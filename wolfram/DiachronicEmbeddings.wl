(* ::Package:: *)

(*  DiachronicEmbeddings`  --  all rendering / analysis code for the
    "Diachronic Word Embeddings" Wolfram Community notebook.

    The notebook imports this package once at the top and then, before each
    output, gives only the call of the function (with its arguments). All the
    long code lives here.

    Every function draws from the small, committed derived-data products in
    ../data (the 1.6 GB raw HistWords embeddings are NOT needed):

        trajectories.json   per-word 2-D path + neighbour coordinates
        neighbors.json      per-word, per-decade nearest-neighbour lists
        alignment.json      per-decade raw vs aligned cosine-to-1990 + ||R-I||
        laws.json           summary statistics of the two laws
        laws_points.json    sampled scatter + trend lines for the law figures
        drift_ranking.json  most-drifted content words

    Public functions
    ----------------
        deWords[]                  the 14 tracked words, in display order
        deTrajectoryPlot[word]            full 1800s-1990s trajectory of word
        deTrajectoryPlot[word, yearMax]   reveal decades up to yearMax
        deTrajectoryPlot[word, yearMax, showLabels]
        deExplorer[]               interactive Manipulate over all words
        deHero[]                   2x3 cover grid of six striking words
        deAlignmentPlot[]          raw vs aligned cosine per decade (Section 3)
        deNeighborTable[words]     nearest-neighbour drift table (Section 6)
        deConformityPlot[]         law of conformity (Section 7)
        deInnovationPlot[]         law of innovation (Section 7)
        deDriftChart[n]            n most-drifted content words (Section 8)
        deAlignmentFacts[]         assoc of alignment numbers (for prose)
        deLawFacts[]               assoc of law statistics (for prose)
*)

BeginPackage["DiachronicEmbeddings`"];

deWords::usage           = "deWords[] returns the 14 tracked words in display order.";
deTrajectoryPlot::usage  = "deTrajectoryPlot[word] draws word's 200-year trajectory through its 2-D-projected neighbourhood. deTrajectoryPlot[word, yearMax] reveals decades up to yearMax; a third argument toggles neighbour labels.";
deExplorer::usage        = "deExplorer[] returns the interactive Manipulate exploring every word.";
deHero::usage            = "deHero[] returns the cover grid of six words.";
deAlignmentPlot::usage   = "deAlignmentPlot[] plots mean cosine to the 1990s vectors, raw vs Procrustes-aligned, per decade.";
deNeighborTable::usage   = "deNeighborTable[words] gives the nearest-neighbour drift table (decades 1850/1900/1950/1990).";
deConformityPlot::usage  = "deConformityPlot[] draws the law of conformity (change rate vs frequency).";
deInnovationPlot::usage  = "deInnovationPlot[] draws the law of innovation (frequency-controlled rate vs polysemy).";
deDriftChart::usage      = "deDriftChart[n] charts the n most-drifted content words.";
deAlignmentFacts::usage  = "deAlignmentFacts[] returns an association of alignment numbers used in the prose.";
deLawFacts::usage        = "deLawFacts[] returns an association of the law statistics used in the prose.";

Begin["`Private`"];

(* ---------- locate + load the committed data ---------- *)
$deRepo = ParentDirectory @ DirectoryName[$InputFileName];
$deData = FileNameJoin[{$deRepo, "data"}];
load[f_] := Import[FileNameJoin[{$deData, f}], "RawJSON"];

$traj   = load["trajectories.json"];
$neigh  = load["neighbors.json"];
$align  = load["alignment.json"];
$laws   = load["laws.json"];
$lpts   = load["laws_points.json"];
$drift  = load["drift_ranking.json"];

$order = {"gay", "broadcast", "computer", "awful", "queer", "literally",
   "terrific", "guy", "media", "fun", "cell", "nice", "mouse", "web"};
deWords[] := $order;

(* ---------- shared style ---------- *)
deCol[t_] := Blend[{RGBColor[0.13, 0.34, 0.68], RGBColor[0.85, 0.16, 0.11]}, t];
frac[d_] := (ToExpression[d] - 1800)/190.;
decLabel[d_] := StringTake[d, -2] <> "s";

(* a neighbour label sits EXACTLY at the word's projected position (centred),
   on a translucent white panel so it stays legible over the path and other
   labels. Placing the word at its true coordinate -- rather than offset away
   from a separate dot -- is what makes the words "line up" with the geometry. *)
nbMark[xy_] := {};
nbLabel[word_, xy_] := Text[
   Framed[Style[word, Italic, 8.5, GrayLevel[0.45]],
     Background -> Directive[White, Opacity[0.75]],
     FrameStyle -> Directive[GrayLevel[0.85], Thickness[0.4]], FrameMargins -> 1,
     RoundingRadius -> 2],
   xy];

(* ---------- trajectory ---------- *)
trajRec[w_] := <|
   "decades" -> $traj[w]["decades"],
   "path" -> $traj[w]["path"],
   "neighbors" -> ({#["word"], #["xy"]} & /@ $traj[w]["neighbors"])|>;

deTrajectoryPlot[w_String] := deTrajectoryPlot[w, 1990, True];
deTrajectoryPlot[w_String, yearMax_] := deTrajectoryPlot[w, yearMax, True];
deTrajectoryPlot[w_String, yearMax_, showLabels_] := Module[
   {r = trajRec[w], ds, path, nbrs, n, shown, last},
   ds = r["decades"]; path = r["path"]; nbrs = r["neighbors"]; n = Length[ds];
   shown = Select[Range[n], ToExpression[ds[[#]]] <= yearMax &];
   Graphics[{
      (* neighbour cloud *)
      nbMark[#[[2]]] & /@ nbrs,
      If[showLabels, nbLabel[#[[1]], #[[2]]] & /@ nbrs, {}],
      (* path up to yearMax *)
      If[Length[shown] >= 2,
        Table[{deCol[frac[ds[[i]]]], Opacity[0.5], Thickness[0.006],
           Line[{path[[i]], path[[i + 1]]}]}, {i, Most[shown]}], {}],
      Opacity[1],
      Table[{deCol[frac[ds[[i]]]], EdgeForm[White], PointSize[0.022], Point[path[[i]]]},
         {i, shown}],
      (* endpoint decade labels *)
      If[shown =!= {}, last = Last[shown];
        {Text[Style[decLabel[ds[[1]]], Bold, 11, deCol[frac[ds[[1]]]]], path[[1]], {0, -1.7}],
         Text[Style[decLabel[ds[[last]]], Bold, 13, deCol[frac[ds[[last]]]]], path[[last]], {0, -1.7}]},
        {}]},
    PlotLabel -> Style["\"" <> w <> "\"", 16, Bold, FontFamily -> "Helvetica"],
    PlotRange -> CoordinateBounds[Join[path, nbrs[[All, 2]]], Scaled[0.12]],
    ImageSize -> 560, AspectRatio -> 0.85, Background -> White]];

(* shared blue->red time legend *)
deLegend[] := Row[{Style["1800s ", 11, deCol[0]],
   Graphics[Table[{deCol[t], Rectangle[{t, 0}, {t + 0.05, 1}]}, {t, 0, 1, 0.05}],
     AspectRatio -> 1/16, ImageSize -> 220], Style[" 1990s", 11, deCol[1]]}];

(* ---------- interactive explorer ---------- *)
deExplorer[] := Manipulate[
   deTrajectoryPlot[word, yearMax, neighbourLabels],
   {{word, "gay", "word"}, $order},
   {{yearMax, 1990, "reveal decades up to"}, 1800, 1990, 10, Appearance -> "Labeled"},
   {{neighbourLabels, True, "neighbour labels"}, {True, False}},
   ControlPlacement -> Top, SaveDefinitions -> True];

(* ---------- cover ---------- *)
deHero[] := Labeled[
   Grid[Partition[
      Show[deTrajectoryPlot[#, 1990, True], ImageSize -> 300] & /@
        {"gay", "broadcast", "computer", "guy", "terrific", "media"}, 3],
     Spacings -> {1, 1}, Background -> White],
   deLegend[], Bottom];

(* ---------- Section 3: alignment ---------- *)
deAlignmentPlot[] := Module[{rows, yrs, un, al},
   rows = SortBy[$align["rows"], #["year"] &];
   yrs = #["year"] & /@ rows;
   un = Transpose[{yrs, #["cosUnaligned"] & /@ rows}];
   al = Transpose[{yrs, #["cosAligned"] & /@ rows}];
   ListLinePlot[{un, al},
     PlotStyle -> {Directive[deCol[0], Thickness[0.006]], Directive[deCol[1], Thickness[0.006]]},
     PlotMarkers -> {Automatic, 8}, Frame -> True, GridLines -> Automatic,
     GridLinesStyle -> GrayLevel[0.9],
     FrameLabel -> {"decade", "mean cosine to the 1990s vectors"},
     PlotLabel -> Style["Alignment makes the decades comparable\n(5000 shared high-frequency anchor words)", 13],
     PlotLegends -> Placed[{"raw (independently trained)", "after Procrustes alignment"}, {0.32, 0.82}],
     PlotRange -> {{1795, 1995}, {0.3, 1.02}}, ImageSize -> 620, AspectRatio -> 0.62]];

deAlignmentFacts[] := Module[{row},
   row[d_] := SelectFirst[$align["rows"], #["decade"] == d &];
   <|"rot1800" -> row["1800"]["rotFrobenius"],
     "randomBaseline" -> $align["rotFrobenius_randomBaseline"],
     "max" -> $align["rotFrobenius_max"],
     "cos1800un" -> row["1800"]["cosUnaligned"], "cos1800al" -> row["1800"]["cosAligned"],
     "cos1900un" -> row["1900"]["cosUnaligned"], "cos1900al" -> row["1900"]["cosAligned"]|>];

(* ---------- Section 6: neighbour drift table ---------- *)
deNeighborTable[words_List] := Module[{ndecs = {"1850", "1900", "1950", "1990"}, row},
   row[w_] := Prepend[
      Table[If[KeyExistsQ[$neigh[w], d], StringRiffle[Take[$neigh[w][d], UpTo[5]], ", "], "\[Dash]"],
        {d, ndecs}],
      Style[w, Italic, Bold]];
   Grid[
     Prepend[row /@ words, Style[#, Bold] & /@ Prepend[decLabel /@ ndecs, "word"]],
     Frame -> All, Alignment -> Left, Background -> {None, {RGBColor[0.92, 0.94, 1.0]}},
     Spacings -> {1, 0.8}, ItemSize -> {{6, 14, 14, 14, 14}, Automatic},
     FrameStyle -> GrayLevel[0.7]]];

(* ---------- Section 7: laws ---------- *)
deConformityPlot[] := Module[{c = $lpts["conformity"], pts, bin},
   pts = Transpose[{c["x"], c["y"]}];
   bin = c["bin"];
   Show[
     ListPlot[pts, PlotStyle -> Directive[Opacity[0.12], ColorData[97][1], PointSize[0.004]],
       Frame -> True,
       FrameLabel -> {"frequency  (\[Minus]log mean normalized rank)", "log\:2081\:2080 mean change rate"},
       PlotLabel -> Style["Law of conformity: more frequent words drift more slowly", 13],
       ImageSize -> 620, PlotRange -> All],
     ListLinePlot[bin, PlotStyle -> Directive[Red, Thick], PlotMarkers -> {Automatic, 9}],
     Epilog -> {Text[Style[Row[{"Spearman \[Rho] = ", $laws["conformity"]["spearman"]}], 13, Red],
        Scaled[{0.72, 0.9}]]}]];

deInnovationPlot[] := Module[{n = $lpts["innovation"], pts, maxP},
   maxP = n["maxP"];
   pts = Transpose[{n["poly"], n["resid"]}];
   Show[
     ListLinePlot[{n["binRaw"], n["binResid"]},
       PlotStyle -> {Directive[ColorData[97][3], Thick], Directive[Red, Thick]},
       PlotMarkers -> {Automatic, 9}, Frame -> True,
       GridLines -> {None, {0}}, GridLinesStyle -> GrayLevel[0.8],
       FrameLabel -> {"polysemy  (number of WordNet senses)", "mean log\:2081\:2080 change rate"},
       PlotLabel -> Style["Law of innovation does NOT survive a frequency control\n(the raw polysemy effect is a frequency artifact)", 12.5],
       PlotLegends -> Placed[{"raw mean rate", "frequency-controlled (residual)"}, {0.7, 0.78}],
       ImageSize -> 620, PlotRange -> {{0, maxP + 1}, All}],
     Epilog -> {Text[Style[Row[{"raw Spearman \[Rho] = ", n["rawSpearman"],
         ";  partial (freq-controlled) \[Rho] = ", n["partialSpearman"]}], 11.5, Red],
        Scaled[{0.5, 0.06}]]}]];

deLawFacts[] := <|
   "nContent" -> $laws["nContentWords"],
   "confSpearman" -> $laws["conformity"]["spearman"], "confSlope" -> $laws["conformity"]["slope"],
   "innoRaw" -> $laws["innovation"]["rawSpearman"], "innoPartial" -> $laws["innovation"]["partialSpearman"],
   "polyFreq" -> $laws["innovation"]["polyVsFreqSpearman"]|>;

(* ---------- Section 8: drift leaderboard ---------- *)
deDriftChart[n_Integer] := Module[{top},
   top = Take[$drift, UpTo[n]];
   BarChart[Reverse[#["drift"] & /@ top], BarOrigin -> Left,
     ChartLabels -> Placed[Reverse[#["word"] & /@ top], Before],
     ChartStyle -> ColorData["SolarColors"],
     PlotLabel -> Style["Most-drifted content words, 1800s\[Dash]1990s\n(1 \[Minus] cosine of a word's own 1800 vs 1990 aligned vector)", 13],
     ImageSize -> 720, AspectRatio -> 1.3, Frame -> {{True, False}, {True, False}}]];

End[];
EndPackage[];
