#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  explore.wls -- exploration / validation of the diachronic-embedding
    approach:
      1. Orthogonal-Procrustes alignment of every decade onto a reference.
      2. Alignment sanity checks (ref->identity; cross-decade anchor cosine).
      3. Nearest-neighbour drift tables for the target words.
      4. A first global 2-D trajectory plot.
    Run: wolframscript -file wolfram/explore.wls
*)

Get @ FileNameJoin[{DirectoryName[$InputFileName], "load_embeddings.wl"}];

ref = "1990";
decades = $deDecades;
targets = deTargetVocab[];
Print["Decades: ", Row[decades, " "]];
Print["Targets: ", Row[targets, ", "]];

(* ---- unit-normalize rows (float32 rounding) ---- *)
unit[m_] := m / (Sqrt[Total[m^2, {2}]] /. 0. -> 1.);

(* ---- orthogonal Procrustes: rotation taking decade d into ref frame ---- *)
Print["\nLoading anchors + computing Procrustes rotations (ref = ", ref, ") ..."];
anchorsRef = unit @ deAnchors[ref];
rotations = Association @ Table[
   Module[{Ad, m, u, w, v},
     Ad = unit @ deAnchors[d];
     m = Transpose[Ad] . anchorsRef;              (* 300 x 300 *)
     {u, w, v} = SingularValueDecomposition[m];
     d -> u . Transpose[v]                          (* orthogonal R_d *)
   ], {d, decades}];

(* ---- sanity 1: ref aligns to itself (R ~ Identity) ---- *)
idErr = Max@Abs[rotations[ref] - IdentityMatrix[$deDim]];
Print["Sanity 1  ref rotation - I  max|.| = ", ScientificForm[idErr, 3],
      "  (expect ~0)"];

(* ---- sanity 2: cross-decade cosine of the SAME anchor word ----
   Aligned anchors of consecutive decades should be highly similar; without
   alignment the independent SGNS spaces are arbitrarily rotated (cosine ~0). *)
alignedAnchors = Association @ Table[d -> unit[deAnchors[d]] . rotations[d], {d, decades}];
meanCos[a_, b_] := Mean @ Total[a b, {2}];          (* rows are unit length *)
Print["Sanity 2  mean cosine of identical anchor words, consecutive decades:"];
Do[
  Print["   ", decades[[i]], " -> ", decades[[i + 1]], "  ",
        NumberForm[meanCos[alignedAnchors[decades[[i]]], alignedAnchors[decades[[i + 1]]]], {4, 3}]],
  {i, Length[decades] - 1}];
(* baseline: WITHOUT alignment (native spaces) should be ~0 *)
Print["   baseline (un-aligned 1980 vs 1990): ",
      NumberForm[meanCos[unit@deAnchors["1980"], unit@deAnchors["1990"]], {4, 3}]];

(* ---- nearest-neighbour drift in NATIVE per-decade space ----
   Orthogonal rotation preserves within-decade cosine, so neighbours are
   computed in each decade's own coordinates. *)
neighbors[decade_, word_, k_:8] := Module[{pool, M, vocab, vrow, sims, ord},
  pool = deNeighborPool[decade]; M = unit @ pool["matrix"]; vocab = pool["vocab"];
  vrow = unit[deTargets[decade]][[ FirstPosition[targets, word][[1]] ]];
  If[Norm[vrow] < 0.5, Return["(absent)"]];
  sims = M . vrow;
  ord = Reverse @ Ordering[sims, -(k + 5)];
  DeleteCases[vocab[[#]] & /@ ord, word][[;; k]]
];

probeDecades = {"1850", "1900", "1950", "1990"};
Do[
  Print["\n=== ", word, " : nearest neighbours by decade ==="];
  Do[Print["  ", d, ": ", Row[neighbors[d, word], ", "]], {d, probeDecades}],
  {word, targets}];

(* ---- first global 2-D trajectory plot ---- *)
Print["\nBuilding global 2-D trajectory plot ..."];
pts = Reap[
   Do[Module[{v},
     v = unit[deTargets[d]][[ FirstPosition[targets, word][[1]] ]];
     If[Norm[v] > 0.5, Sow[{word, d, v . rotations[d]}]]
   ], {word, targets}, {d, decades}]
 ][[2, 1]];
allVecs = pts[[All, 3]];
proj = PrincipalComponents[allVecs][[All, 1 ;; 2]];
projOf = Association @ MapThread[{#1[[1]], #1[[2]]} -> #2 &, {pts, proj}];

colors = Association @ MapThread[#1 -> #2 &, {targets, ColorData[97] /@ Range@Length@targets}];
traj[word_] := Module[{ds, xy},
  ds = Select[decades, KeyExistsQ[projOf, {word, #}] &];
  xy = projOf[{word, #}] & /@ ds;
  {colors[word],
   Line[xy], PointSize[0.012], Point[xy],
   Text[Style[word, Bold, 12, colors[word]], First[xy], {0, -1.5}],
   Text[Style[StringTake[First[ds], -2] <> "s", 8], First[xy], {0, 1.5}],
   Text[Style[StringTake[Last[ds], -2] <> "s", 8], Last[xy], {0, 1.5}]}
];
g = Graphics[traj /@ targets,
   PlotLabel -> Style["Diachronic drift: target words 1800s-1990s (PCA-2D of aligned vectors)", 13],
   ImageSize -> 900, Frame -> True, Axes -> False,
   FrameLabel -> {"PC1", "PC2"}];
out = FileNameJoin[{$deRepoRoot, "docs", "images", "trajectories_global.png"}];
Export[out, g, "PNG", ImageResolution -> 130];
Print["Wrote ", out];
Print["\nDONE."];
