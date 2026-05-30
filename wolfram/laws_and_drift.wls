#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  laws_and_drift.wls -- quantitative diachronic analysis over the stable
    CONTENT vocabulary (real dictionary words present in every decade).

    A raw drift ranking over all words is dominated by (a) function words
    ("the", "of") whose ubiquitous-context SGNS vectors are unstable, and
    (b) archaic spellings / foreign tokens. Both are artifacts, not meaning
    change, and they invert the frequency law. So we keep only lowercase
    alphabetic words of length >= 3 that are NOT stopwords and ARE known to
    WordNet (via WL's curated WordData). Polysemy for the innovation law is
    the same WordData sense count, computed in the same pass.

    Products
    --------
      data/drift_ranking.json      top-200 most-drifted content words
      docs/images/drift_top.png    leaderboard bar chart
      docs/images/law_conformity.png  rate vs frequency (binned + scatter)
      docs/images/law_innovation.png  rate vs polysemy (binned + scatter)
*)

Get @ FileNameJoin[{DirectoryName[$InputFileName], "load_embeddings.wl"}];
decades = $deDecades; ref = "1990";
imgDir  = FileNameJoin[{$deRepoRoot, "docs", "images"}];
dataDir = FileNameJoin[{$deRepoRoot, "data"}];
r3[x_] := Round[x, 0.001];

unit[m_] := m / (Sqrt[Total[m^2, {2}]] /. 0. -> 1.);

stop = {"the","of","to","and","in","a","is","that","for","it","as","be","not",
   "was","on","with","he","i","his","at","by","this","had","they","but","from",
   "or","she","which","you","one","we","all","were","her","would","there","their",
   "an","been","has","when","who","will","more","no","if","out","so","what","up",
   "its","about","into","than","them","can","only","other","new","some","could",
   "time","these","two","may","then","do","first","any","my","now","such","like",
   "our","over","man","me","even","most","made","after","also","did","many","off",
   "before","must","well","back","through","years","where","much","your","way",
   "down","should","because","each","just","those","people","mr","how","too",
   "little","state","good","very","make","world","still","own","see","men","work",
   "long","here","between","both","life","being","under","never","same","another",
   "while","last","might","us","great","old","year","come","since","against",
   "go","came","right","used","take","three","states","himself","few","house",
   "use","during","without","again","place","american","around","however","home",
   "small","found","thought","went","say","part","once","general","high","upon",
   "school","every","don","does","got","united","left","number","course","war",
   "until","always","away","something","fact","though","water","less","public",
   "put","think","almost","hand","enough","far","took","head","yet","government",
   "system","set","told","nothing","end","why","called","didn","find","going",
   "look","asked","later","knew","st","shall","thus","hath","unto","thy","thee",
   "his","ye","whom","whose","therein","whereof","hereby","therefore","wherein"};
stopQ = Association @ Table[s -> True, {s, stop}];

(* ---- rotations ---- *)
Print["Computing Procrustes rotations ..."];
anchorsRef = unit @ deAnchors[ref];
rot = Association @ Table[
   Module[{a, m, u, w, v}, a = unit @ deAnchors[d];
     m = Transpose[a] . anchorsRef; {u, w, v} = SingularValueDecomposition[m];
     d -> u . Transpose[v]], {d, decades}];

(* ---- stable common vocabulary ---- *)
Print["Building common vocabulary ..."];
poolVocabs = Association @ Table[d -> deNeighborPool[d]["vocab"], {d, decades}];
common = Fold[Intersection, poolVocabs[First@decades], Values@poolVocabs];

(* ---- content-word filter + polysemy (one WordData pass) ----
   WordData's first calls can return unevaluated while the paclet loads; we
   must NOT memoize that as 0 (a silent failure once produced empty law
   plots). So: pre-warm, cache only successful integer results, and guard. *)
Print["Filtering to content words via WordData (", Length@common, " candidates) ..."];
Do[WordData["computer", "Senses"], 3];   (* pre-warm the paclet *)
If[! ListQ[WordData["computer", "Senses"]],
   Print["FATAL: WordData unavailable (computer/Senses not a list). Aborting."];
   Exit[1]];
senseCount[w_] := Module[{s = Quiet @ WordData[w, "Senses"]},
    If[ListQ[s], senseCount[w] = Length[s], 0]];   (* cache hits only *)
alphaQ[w_] := StringMatchQ[w, RegularExpression["[a-z]+"]] && StringLength[w] >= 3;
contentQ[w_] := alphaQ[w] && ! TrueQ[stopQ[w]] && senseCount[w] > 0;
content = Select[common, contentQ];
Print["Content words kept: ", Length@content];
If[Length[content] < 1000,
   Print["FATAL: only ", Length@content, " content words (expected ~8800). ",
     "WordData likely flaky this run. Aborting so no empty plots are written."];
   Exit[1]];

(* ---- single pass: aligned vectors -> rate, endpoint drift, frequency ---- *)
Print["Pass over decades: change rate + endpoint drift + frequency ..."];
nC = Length@content;
distSum = ConstantArray[0., nC];
fracSum = ConstantArray[0., nC];   (* sum of normalized rank in [0,1], lower=frequent *)
prev = Null; firstVec = Null; lastVec = Null;
Do[Module[{pool, M, vocab, idx, rows, cur, sz},
   pool = deNeighborPool[d]; M = unit @ pool["matrix"]; vocab = pool["vocab"];
   sz = Length@vocab; idx = Association @ MapIndexed[#1 -> #2[[1]] &, vocab];
   rows = idx /@ content;
   cur = M[[rows]] . rot[d];
   fracSum += N[rows]/sz;
   If[prev =!= Null, distSum += (1 - Total[cur prev, {2}])];
   prev = cur;
   If[d === First@decades, firstVec = cur];
   If[d === Last@decades, lastVec = cur];
   ], {d, decades}];

rate    = distSum/(Length[decades] - 1);
meanFrac = fracSum/Length[decades];
freqScore = -Log[meanFrac];                 (* higher = more frequent *)
drift = 1 - Total[firstVec lastVec, {2}];   (* 1800 vs 1990 *)

(* ---- 1. drift leaderboard ---- *)
ord = Reverse @ Ordering[drift];
leaderboard = Table[<|"word" -> content[[i]], "drift" -> r3@drift[[i]],
     "rate" -> Round[rate[[i]], 0.0001], "senses" -> senseCount[content[[i]]]|>,
   {i, ord[[;; Min[200, nC]]]}];
Export[FileNameJoin[{dataDir, "drift_ranking.json"}], leaderboard, "JSON"];
Print["\nTop 40 most-drifted content words 1800s->1990s:"];
Do[Print["  ", StringPadRight[content[[i]], 16], r3@drift[[i]]], {i, ord[[;; 40]]}];

topN = 25; topW = content[[ord[[;; topN]]]]; topD = drift[[ord[[;; topN]]]];
Export[FileNameJoin[{imgDir, "drift_top.png"}],
   BarChart[Reverse@topD, BarOrigin -> Left,
     ChartLabels -> Placed[Reverse@topW, Before], ChartStyle -> ColorData["SolarColors"],
     PlotLabel -> Style["Most-drifted content words, 1800s\[Dash]1990s\n(1 \[Minus] cosine of a word's own 1800 vs 1990 aligned vector)", 13],
     ImageSize -> 740, AspectRatio -> 1.3, Frame -> {{True, False}, {True, False}}],
   "PNG", ImageResolution -> 130];
Print["Wrote docs/images/drift_top.png"];

(* ---- 2. law of conformity: rate vs frequency ---- *)
Print["\nLaw of conformity ..."];
lx = freqScore; ly = Log10[rate];
lmC = LinearModelFit[Transpose[{lx, ly}], x, x];
slopeC = lmC["BestFitParameters"][[2]]; rC = Correlation[lx, ly];
Print["  slope(log10 rate vs freqScore) = ", r3@slopeC];
Print["  Pearson r = ", r3@rC, "   (negative => frequent words change less)"];
(* decile bins for a clean trend line *)
qs = Quantile[lx, Range[0, 10]/10];
binC = Table[Module[{sel = Pick[Transpose[{lx, ly}], (qs[[k]] <= # < qs[[k + 1]]) & /@ lx]},
     If[Length[sel] >= 5, {Mean[sel[[All, 1]]], Mean[sel[[All, 2]]]}, Nothing]], {k, 10}];
Export[FileNameJoin[{imgDir, "law_conformity.png"}],
   Show[
     ListPlot[Transpose[{lx, ly}], PlotStyle -> Directive[Opacity[0.12], ColorData[97][1], PointSize[0.004]],
       Frame -> True, FrameLabel -> {"frequency  (\[Minus]log mean normalized rank)", "log\:2081\:2080 mean change rate"},
       PlotLabel -> Style["Law of conformity: more frequent words drift more slowly", 13],
       ImageSize -> 640, PlotRange -> All],
     ListLinePlot[binC, PlotStyle -> Directive[Red, Thick], PlotMarkers -> {Automatic, 9}],
     Epilog -> {Text[Style[Row[{"slope = ", r3@slopeC, ",  r = ", r3@rC}], 13, Red], Scaled[{0.74, 0.92}]]}],
   "PNG", ImageResolution -> 130];
Print["Wrote docs/images/law_conformity.png"];

(* ---- 3. law of innovation: rate vs polysemy ---- *)
Print["\nLaw of innovation ..."];
poly = senseCount /@ content;
rI = Correlation[Log[poly], Log[rate]]; sp = SpearmanRho[poly, rate];
Print["  Pearson r(log polysemy, log rate) = ", r3@rI];
Print["  Spearman rho = ", r3@sp, "   (positive => polysemous words change faster)"];
maxP = 14;
binI = Table[Module[{sel = Pick[rate, # == k & /@ poly]},
     If[Length[sel] >= 8, {k, Mean[sel]}, Nothing]], {k, 1, maxP}];
Export[FileNameJoin[{imgDir, "law_innovation.png"}],
   Show[
     ListPlot[Transpose[{poly, rate}], PlotStyle -> Directive[Opacity[0.08], ColorData[97][3], PointSize[0.004]],
       Frame -> True, FrameLabel -> {"polysemy  (number of WordNet senses)", "mean change rate"},
       PlotLabel -> Style["Law of innovation: polysemous words drift faster", 13],
       ImageSize -> 640, PlotRange -> {{0, maxP + 1}, All}],
     ListLinePlot[binI, PlotStyle -> Directive[Red, Thick], PlotMarkers -> {Automatic, 9}],
     Epilog -> {Text[Style[Row[{"Spearman \[Rho] = ", r3@sp}], 13, Red], Scaled[{0.72, 0.12}]]}],
   "PNG", ImageResolution -> 130];
Print["Wrote docs/images/law_innovation.png"];
Print["\nDONE."];
