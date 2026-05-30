#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  trajectories.wls -- the iconic per-word figures + the data products.

    For each target word we draw its decade-by-decade path through a 2-D
    projection of its LOCAL neighbourhood: the word's aligned vector at each
    decade, projected together with the era-specific nearest neighbours that
    anchor each part of the journey. Early-sense neighbours cluster near the
    early path; late-sense neighbours near the late path.

    Method
    ------
    1. Orthogonal-Procrustes-align every decade onto the 1990 reference
       (rotation from the shared 5000-word high-frequency anchor set).
    2. Per decade, find each target's nearest neighbours in NATIVE space
       (rotation is orthogonal, so within-decade cosine is preserved), and
       record each neighbour's aligned vector from the decade it was chosen.
    3. Per target: PCA the {path points} U {neighbour points} to 2-D, plot
       the path with a blue->red time gradient and dash any coverage gaps.

    Outputs
    -------
      docs/images/traj_<word>.png         per-word figure
      docs/images/traj_grid.png           2x3 grid of all six
      data/neighbors.json                 top-8 neighbours, word x decade
      data/trajectories.json              2-D path + neighbour coords per word
*)

Get @ FileNameJoin[{DirectoryName[$InputFileName], "load_embeddings.wl"}];

ref      = "1990";
decades  = $deDecades;
minSpan  = 5;     (* skip words present in fewer than this many decades *)
targets  = deTargetVocab[];
imgDir   = FileNameJoin[{$deRepoRoot, "docs", "images"}];
dataDir  = FileNameJoin[{$deRepoRoot, "data"}];
kTable   = 8;     (* neighbours stored per decade (for the table / json) *)
kFig     = 8;     (* neighbours per decade considered for the figure *)
maxLabels = 24;   (* cap on neighbour labels drawn *)

unit[m_]  := m / (Sqrt[Total[m^2, {2}]] /. 0. -> 1.);
unitV[v_] := With[{n = Sqrt[v . v]}, If[n > 0, v/n, v]];
yr[d_]    := ToExpression[d];
lab[d_]   := StringTake[d, -2] <> "s";

(* ---- rotations onto the reference frame ---- *)
Print["Computing Procrustes rotations (ref = ", ref, ") ..."];
anchorsRef = unit @ deAnchors[ref];
rot = Association @ Table[
   Module[{a, m, u, w, v}, a = unit @ deAnchors[d];
     m = Transpose[a] . anchorsRef; {u, w, v} = SingularValueDecomposition[m];
     d -> u . Transpose[v]], {d, decades}];

(* ---- aligned target vectors + native target vectors ---- *)
tIndex = Association @ MapIndexed[#1 -> #2[[1]] &, targets];
nativeTarget  = Association[];   (* (word,decade) -> native unit vec | Null *)
alignedTarget = Association[];   (* (word,decade) -> aligned unit vec | Null *)
Do[Module[{T = deTargets[d]},
   Do[Module[{v = T[[tIndex[w]]]},
       If[Sqrt[v . v] > 0.5,
         nativeTarget[{w, d}]  = unitV[v];
         alignedTarget[{w, d}] = unitV[v] . rot[d],
         nativeTarget[{w, d}] = Null; alignedTarget[{w, d}] = Null]],
      {w, targets}]], {d, decades}];

(* ---- neighbour search, one pass over decades ---- *)
Print["Finding nearest neighbours per decade ..."];
neighTable = Association @ Table[w -> Association[], {w, targets}]; (* w -> d -> {words} *)
figNeigh   = Association @ Table[w -> {}, {w, targets}];            (* w -> flat list (with dups) *)
neighborVec = Association[];                                        (* word -> aligned vec, first seen *)
Do[Module[{pool, M, vocab, idx},
   pool = deNeighborPool[d]; M = unit @ pool["matrix"]; vocab = pool["vocab"];
   idx = Association @ MapIndexed[#1 -> #2[[1]] &, vocab];
   Do[Module[{tv = nativeTarget[{w, d}], sims, ord, nbrs},
      If[tv =!= Null,
        sims = M . tv;
        ord  = Reverse @ Ordering[sims, -(kTable + 6)];
        nbrs = DeleteCases[vocab[[#]] & /@ ord, w][[;; kTable]];
        neighTable[w][d] = nbrs;
        figNeigh[w] = Join[figNeigh[w], nbrs[[;; UpTo[kFig]]]];
        Do[If[! KeyExistsQ[neighborVec, nb] && KeyExistsQ[idx, nb],
            neighborVec[nb] = unitV[M[[idx[nb]]]] . rot[d]], {nb, nbrs[[;; UpTo[kFig]]]}]
      ]], {w, targets}]
   ], {d, decades}];

(* ---- export neighbour table ---- *)
Export[FileNameJoin[{dataDir, "neighbors.json"}],
   Association @ Table[w -> neighTable[w], {w, targets}], "JSON"];
Print["Wrote data/neighbors.json"];

(* ---- visual styling ---- *)
timeCol[t_] := Blend[{RGBColor[0.13, 0.34, 0.68], RGBColor[0.85, 0.16, 0.11]}, t];
legendBar = Legended[
   DensityPlot[x, {x, 0, 1}, {y, 0, 1}, ColorFunction -> timeCol,
     Frame -> False, PlotPoints -> 2, AspectRatio -> 1/14, ImageSize -> 320],
   Placed["", Below]] // Quiet;
timeLegend = Row[{Style["1800s ", 11, timeCol[0]],
    Graphics[Table[{timeCol[t], Rectangle[{t, 0}, {t + 0.05, 1}]}, {t, 0, 1, 0.05}],
      AspectRatio -> 1/16, ImageSize -> 240],
    Style[" 1990s", 11, timeCol[1]]}];

(* ---- per-word figure ---- *)
plotData = Association[];
plotWord[w_] := Module[
  {ds, pathVecs, nbs, nbVecs, allV, proj, np, pathXY, nbXY, n, segs},
  ds = Select[decades, alignedTarget[{w, #}] =!= Null &];
  pathVecs = alignedTarget[{w, #}] & /@ ds;
  (* figure neighbours: always anchor BOTH endpoints (first + last decade),
     then fill with the most frequently-selected neighbours, capped. *)
  Module[{haveVec, firstNb, lastNb, freqNb},
   haveVec[x_] := KeyExistsQ[neighborVec, x];
   firstNb = Select[neighTable[w][First@ds], haveVec];
   lastNb  = Select[neighTable[w][Last@ds], haveVec];
   freqNb  = (Reverse @ SortBy[Tally @ Select[figNeigh[w], haveVec], Last])[[All, 1]];
   nbs = DeleteDuplicates[Join[firstNb, lastNb, freqNb]][[;; UpTo[maxLabels]]]];
  nbVecs = neighborVec[#] & /@ nbs;
  allV = Join[pathVecs, nbVecs];
  proj = PrincipalComponents[allV][[All, 1 ;; 2]];
  np = Length[pathVecs]; n = np;
  pathXY = proj[[;; np]]; nbXY = proj[[np + 1 ;;]];
  plotData[w] = <|"decades" -> ds, "path" -> pathXY,
     "neighbors" -> MapThread[<|"word" -> #1, "xy" -> #2|> &, {nbs, nbXY}]|>;
  (* path segments, dashed across >10-yr coverage gaps *)
  segs = Table[{
      If[yr[ds[[i + 1]]] - yr[ds[[i]]] > 10, Dashing[{0.012, 0.012}], Dashing[{}]],
      Blend[{timeCol[(i - 1)/(n - 1)], timeCol[i/(n - 1)]}], Opacity[0.5],
      Thickness[0.006], Line[{pathXY[[i]], pathXY[[i + 1]]}]}, {i, n - 1}];
  Graphics[{
     {GrayLevel[0.5], MapThread[Text[Style[#1, Italic, 9.5], #2] &, {nbs, nbXY}]},
     segs, Opacity[1],
     MapIndexed[{timeCol[(#2[[1]] - 1)/(n - 1)], EdgeForm[White], PointSize[0.024],
        Point[#1]} &, pathXY],
     MapIndexed[If[#2[[1]] == 1 || #2[[1]] == n || Mod[#2[[1]], 4] == 1,
         Text[Style[lab[ds[[#2[[1]]]]], Bold, 11, timeCol[(#2[[1]] - 1)/(n - 1)]],
           #1, {0, -1.7}], {}] &, pathXY]
     },
   PlotLabel -> Style["\"" <> w <> "\"  \[Dash]  " <> lab[First@ds] <> "\[Dash]" <> lab[Last@ds], 15, Bold],
   ImageSize -> 580, PlotRangePadding -> Scaled[0.09], Background -> White]
];

renderTargets = Select[targets,
   Count[decades, d_ /; alignedTarget[{#, d}] =!= Null] >= minSpan &];
Print["Rendering per-word figures (", Length@renderTargets, " of ", Length@targets, ") ..."];
figs = Association @ Table[w -> plotWord[w], {w, renderTargets}];
Do[Export[FileNameJoin[{imgDir, "traj_" <> w <> ".png"}],
     Labeled[figs[w], timeLegend, Bottom], "PNG", ImageResolution -> 130];
   Print["  traj_", w, ".png"], {w, targets}];

(* ---- combined grid ---- *)
Export[FileNameJoin[{imgDir, "traj_grid.png"}],
   Labeled[Grid[Partition[Values[figs], 2], Spacings -> {2, 2}, Background -> White],
     timeLegend, Bottom], "PNG", ImageResolution -> 110];
Print["Wrote traj_grid.png"];

(* ---- export trajectory coords ---- *)
Export[FileNameJoin[{dataDir, "trajectories.json"}], plotData, "JSON"];
Print["Wrote data/trajectories.json"];
Print["DONE."];
