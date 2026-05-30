#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  alignment.wls -- diagnostics + figure for the Procrustes alignment
    (Section 3 of the post). Quantifies, for every decade relative to the
    1990s reference:

      * meanCosUnaligned : mean cosine of the 5000 shared anchor words to
                           their 1990s vectors, in RAW (un-rotated) space
      * meanCosAligned   : the same after orthogonal-Procrustes rotation
      * rotFrobenius     : || R - I ||_F  (0 = identity; ~sqrt(2d) for an
                           unrelated/random rotation; 2*sqrt(d) is the
                           absolute maximum, attained only by R = -I)
      * traceRatio       : tr(R)/d  (1 = identity, 0 = random, -1 = -I)

    Writes data/alignment.json and docs/images/alignment_effect.png.
*)

Get @ FileNameJoin[{DirectoryName[$InputFileName], "load_embeddings.wl"}];
decades = $deDecades; ref = "1990"; d = $deDim;
imgDir = FileNameJoin[{$deRepoRoot, "docs", "images"}];
dataDir = FileNameJoin[{$deRepoRoot, "data"}];

unit[m_] := m / (Sqrt[Total[m^2, {2}]] /. 0. -> 1.);
meanCos[a_, b_] := Mean @ Total[a b, {2}];   (* rows unit length *)

anchorsRef = unit @ deAnchors[ref];
rotOf[dec_] := Module[{a, m, u, w, v},
   a = unit @ deAnchors[dec]; m = Transpose[a] . anchorsRef;
   {u, w, v} = SingularValueDecomposition[m]; u . Transpose[v]];

rows = Table[
   Module[{a = unit @ deAnchors[dec], R, un, al, rfro, tr},
     R = rotOf[dec];
     un = meanCos[a, anchorsRef];
     al = meanCos[a . R, anchorsRef];
     rfro = Norm[R - IdentityMatrix[d], "Frobenius"];
     tr = Tr[R]/d;
     <|"decade" -> dec, "year" -> ToExpression[dec],
       "cosUnaligned" -> Round[un, 0.0001], "cosAligned" -> Round[al, 0.0001],
       "rotFrobenius" -> Round[rfro, 0.01], "traceRatio" -> Round[tr, 0.0001]|>],
   {dec, decades}];

randomBaseline = N@Sqrt[2 d];   (* ||R-I|| for a random orthogonal R *)
maxPossible = N[2 Sqrt[d]];     (* ||R-I|| for R = -I *)

Export[FileNameJoin[{dataDir, "alignment.json"}],
   <|"reference" -> ref, "dim" -> d, "anchors" -> $deManifest["anchor_k"],
     "rotFrobenius_randomBaseline" -> Round[randomBaseline, 0.01],
     "rotFrobenius_max" -> Round[maxPossible, 0.01], "rows" -> rows|>, "JSON"];

Print["decade  cosUn  cosAl  ||R-I||  tr(R)/d"];
Do[Print[StringPadLeft[r["decade"], 6], "  ",
     NumberForm[r["cosUnaligned"], {4, 3}], "  ", NumberForm[r["cosAligned"], {4, 3}],
     "   ", NumberForm[r["rotFrobenius"], {5, 2}], "  ", NumberForm[r["traceRatio"], {4, 3}]],
   {r, rows}];
Print["\n||R-I|| random-rotation baseline (sqrt 2d) = ", NumberForm[randomBaseline, {4, 2}]];
Print["||R-I|| absolute maximum (R=-I, 2 sqrt d)    = ", NumberForm[maxPossible, {4, 2}]];
Print["1800 rotation ", NumberForm[rows[[1]]["rotFrobenius"], {4, 2}],
      " = ", NumberForm[100 rows[[1]]["rotFrobenius"]/maxPossible, {3, 1}], "% of max, ",
      NumberForm[100 rows[[1]]["rotFrobenius"]/randomBaseline, {3, 1}], "% of random baseline"];

(* ---- figure: cosine-to-1990 vs decade, raw vs aligned ---- *)
yrs = #["year"] & /@ rows;
unPts = Transpose[{yrs, #["cosUnaligned"] & /@ rows}];
alPts = Transpose[{yrs, #["cosAligned"] & /@ rows}];
fig = ListLinePlot[{unPts, alPts},
   PlotStyle -> {Directive[RGBColor[0.13, 0.34, 0.68], Thickness[0.006]],
                 Directive[RGBColor[0.85, 0.16, 0.11], Thickness[0.006]]},
   PlotMarkers -> {Automatic, 8},
   Frame -> True, GridLines -> Automatic, GridLinesStyle -> GrayLevel[0.9],
   FrameLabel -> {"decade", "mean cosine to the 1990s vectors"},
   PlotLabel -> Style["Alignment makes the decades comparable\n(5000 shared high-frequency anchor words)", 13],
   PlotLegends -> Placed[{"raw (independently trained)", "after Procrustes alignment"}, {0.32, 0.82}],
   PlotRange -> {{1795, 1995}, {0.3, 1.02}}, ImageSize -> 640, AspectRatio -> 0.62];
Export[FileNameJoin[{imgDir, "alignment_effect.png"}], fig, "PNG", ImageResolution -> 130];
Print["\nWrote docs/images/alignment_effect.png and data/alignment.json"];
