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
   Polysemy = number of WordNet sense-specs WordData[w] returns: a list like
   {{w,Noun},{w,Adjective,Festive},...} whose Length is the sense count.
   (There is NO "Senses" property -- that was the earlier bug; and the specs
   are 2- OR 3-element, so we match {__List}, not {{_,_,_}..}.) WordData's
   first call loads a paclet ("Initializing WordData indices...") but then
   returns correct, stable values, so we pre-warm once and guard the count. *)
Print["Filtering to content words via WordData (", Length@common, " candidates) ..."];
WordData["computer"];   (* pre-warm the paclet *)
senseCount[w_] := senseCount[w] = Module[{r = Quiet @ WordData[w]},
    If[MatchQ[r, {__List}], Length[r], 0]];
If[senseCount["gay"] < 3 || senseCount["computer"] < 1,
   Print["FATAL: WordData polysemy looks wrong (gay=", senseCount["gay"],
     ", computer=", senseCount["computer"], "). Aborting."]; Exit[1]];
alphaQ[w_] := StringMatchQ[w, RegularExpression["[a-z]+"]] && StringLength[w] >= 3;
contentQ[w_] := alphaQ[w] && ! TrueQ[stopQ[w]] && senseCount[w] > 0;
content = Select[common, contentQ];
Print["Content words kept: ", Length@content];
If[Length[content] < 1000,
   Print["FATAL: only ", Length@content, " content words (expected several thousand). ",
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

poly = senseCount /@ content;     (* polysemy: WordNet sense count *)

(* ---- 2. law of conformity: rate vs frequency ---- *)
Print["\nLaw of conformity ..."];
lx = freqScore; ly = Log10[rate];
lmC = LinearModelFit[Transpose[{lx, ly}], x, x];
slopeC = lmC["BestFitParameters"][[2]]; rC = Correlation[lx, ly];
spCF = SpearmanRho[freqScore, rate];
Print["  slope(log10 rate vs freqScore) = ", r3@slopeC];
Print["  Pearson r = ", r3@rC, ";  Spearman = ", r3@spCF,
      "   (negative => frequent words change less)"];
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
     Epilog -> {Text[Style[Row[{"Spearman \[Rho] = ", r3@spCF}], 13, Red], Scaled[{0.72, 0.9}]]}],
   "PNG", ImageResolution -> 130];
Print["Wrote docs/images/law_conformity.png"];

(* ---- 3. law of innovation: rate vs polysemy, controlling for frequency ----
   The RAW polysemy<->rate correlation is negative, because polysemous words
   are also frequent and frequency suppresses change (conformity). This is a
   textbook confound / Simpson's paradox. The law of innovation is a PARTIAL
   relationship: hold frequency fixed. We compute the partial Spearman
   correlation rho(rate, polysemy | frequency) and, for the figure,
   frequency-residualize the (log) change rate before binning by polysemy. *)
Print["\nLaw of innovation (controlling for frequency) ..."];
spRP = SpearmanRho[rate, N@poly];               (* raw rate vs polysemy *)
spRF = SpearmanRho[rate, freqScore];            (* rate vs frequency *)
spPF = SpearmanRho[N@poly, freqScore];          (* polysemy vs frequency *)
partial = (spRP - spRF spPF)/Sqrt[(1 - spRF^2) (1 - spPF^2)];
Print["  raw Spearman(rate, polysemy)      = ", r3@spRP, "  (confounded, negative)"];
Print["  Spearman(rate, frequency)         = ", r3@spRF];
Print["  Spearman(polysemy, frequency)     = ", r3@spPF];
Print["  PARTIAL Spearman(rate,poly|freq)  = ", r3@partial,
      "   (positive => innovation law holds)"];

(* frequency-residualized log change-rate: regress ly on freqScore, keep resid *)
residRate = ly - (lmC["BestFitParameters"][[1]] + lmC["BestFitParameters"][[2]] lx);
maxP = 14;
(* two trend lines: raw rate vs polysemy (confounded, slopes DOWN) and the
   frequency-controlled residual (flat) -- the figure's whole point. *)
binRaw = Table[Module[{sel = Pick[ly, # == k & /@ poly]},
     If[Length[sel] >= 8, {k, Mean[sel]}, Nothing]], {k, 1, maxP}];
binResid = Table[Module[{sel = Pick[residRate, # == k & /@ poly]},
     If[Length[sel] >= 8, {k, Mean[sel]}, Nothing]], {k, 1, maxP}];
Export[FileNameJoin[{imgDir, "law_innovation.png"}],
   Show[
     ListLinePlot[{binRaw, binResid},
       PlotStyle -> {Directive[ColorData[97][3], Thick], Directive[Red, Thick]},
       PlotMarkers -> {Automatic, 9},
       Frame -> True, GridLines -> {None, {0}}, GridLinesStyle -> GrayLevel[0.8],
       FrameLabel -> {"polysemy  (number of WordNet senses)", "mean log\:2081\:2080 change rate"},
       PlotLabel -> Style["Law of innovation does NOT survive a frequency control\n(the raw polysemy effect is a frequency artifact)", 12.5],
       PlotLegends -> Placed[{"raw mean rate", "frequency-controlled (residual)"}, {0.7, 0.78}],
       ImageSize -> 640, PlotRange -> {{0, maxP + 1}, All}],
     Epilog -> {Text[Style[Row[{"raw Spearman \[Rho] = ", r3@spRP,
         ";  partial (freq-controlled) \[Rho] = ", r3@partial}], 11.5, Red], Scaled[{0.5, 0.06}]]}],
   "PNG", ImageResolution -> 130];
Print["Wrote docs/images/law_innovation.png"];

(* ---- export every quoted statistic so the notebook prose can read it ---- *)
Export[FileNameJoin[{dataDir, "laws.json"}], <|
   "nContentWords" -> Length@content,
   "conformity" -> <|"slope" -> r3@slopeC, "pearson" -> r3@rC, "spearman" -> r3@spCF|>,
   "innovation" -> <|"rawSpearman" -> r3@spRP, "rateVsFreqSpearman" -> r3@spRF,
     "polyVsFreqSpearman" -> r3@spPF, "partialSpearman" -> r3@partial|>
|>, "JSON"];
Print["Wrote data/laws.json"];
Print["\nDONE."];
