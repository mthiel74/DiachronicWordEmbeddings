#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/diachronic_embeddings.nb (+ .pdf) -- the long-form
    Wolfram Community post on 200 years of semantic drift.

    Cells are accumulated as Cell[...] expressions, then a real notebook is
    created via UsingFrontEnd + CreateDocument, evaluated with NotebookEvaluate
    (so the interactive Manipulate is baked in live), saved, and exported to
    PDF. Static figures are embedded as resized Image[] rasters. Quoted
    alignment numbers are read from data/alignment.json so prose and data
    never drift apart.

    Sections
    --------
      Abstract
      1. The idea: words as moving points
      2. The data: HistWords decade embeddings
      3. Why the decades must be aligned (orthogonal Procrustes)  + figure
      4. How to read a trajectory (one worked example: gay)
      5. Explore all fourteen words  (interactive Manipulate)
      6. Nearest-neighbour drift tables
      7. Two statistical laws of semantic change
      8. The most-drifted words -- and a caveat
      9. Methods and reproduction
     10. References
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
dataDir  = FileNameJoin[{repoRoot, "data"}];
outNb    = FileNameJoin[{repoRoot, "community", "diachronic_embeddings.nb"}];
outPdf   = FileNameJoin[{repoRoot, "community", "diachronic_embeddings.pdf"}];

(* ---------- cell helpers ---------- *)
title[t_]    := Cell[t, "Title"];
subtitle[t_] := Cell[t, "Subtitle"];
hd1[t_]      := Cell[t, "Section"];
hd2[t_]      := Cell[t, "Subsection"];
para[s_]     := Cell[TextData[s], "Text", CellMargins -> {{50, 50}, {6, 6}}];
abstract[s_] := Cell[TextData[s], "Text", FontSize -> 14,
   CellMargins -> {{60, 60}, {8, 10}}, Background -> RGBColor[0.96, 0.97, 1.0]];
bold[s_] := StyleBox[s, FontWeight -> Bold];
ital[s_] := StyleBox[s, FontSlant -> Italic];
mono[s_] := StyleBox[s, FontFamily -> "Courier"];
item[s_] := Cell[TextData[s], "Item", CellMargins -> {{66, 50}, {3, 3}}];
caption[s_] := Cell[TextData[Flatten@{bold["Figure. "], s}], "Caption",
   CellMargins -> {{55, 55}, {2, 16}}];
refItem[s_] := Cell[TextData[s], "Text", CellMargins -> {{60, 50}, {3, 3}}, FontSize -> 11];
(* interpretation callout *)
takeaway[s_] := Cell[TextData[Flatten@{bold["Takeaway.  "], s}], "Text",
   FontSize -> 12, CellMargins -> {{60, 60}, {8, 12}},
   Background -> RGBColor[0.93, 0.97, 0.93],
   CellFrame -> {{3, 0}, {0, 0}}, CellFrameColor -> RGBColor[0.35, 0.6, 0.4]];

displayMath[expr_] := Cell[BoxData @ FormBox[expr, TraditionalForm],
   "DisplayFormula", CellMargins -> {{80, 50}, {8, 8}}];

(* resized static-image cell *)
imgCacheDir = FileNameJoin[{repoRoot, "community", ".img-cache"}];
If[! DirectoryQ[imgCacheDir], CreateDirectory[imgCacheDir]];
resized[path_, w_] := Module[{cached, im, dim},
   cached = FileNameJoin[{imgCacheDir, ToString[w] <> "_" <> FileNameTake[path]}];
   If[FileExistsQ[cached] && FileDate[cached, "Modification"] > FileDate[path, "Modification"],
     Return[cached]];
   im = Import[path]; dim = ImageDimensions[im];
   If[dim[[1]] > w, im = ImageResize[im, w]];
   Export[cached, im, "PNG"]; cached];
imgCell[file_, w_: 680] := Module[{path = FileNameJoin[{imgDir, file}]},
   If[FileExistsQ[path],
     Cell[BoxData @ ToBoxes @ Image[Import @ resized[path, w], ImageSize -> w],
        "Output", ShowCellLabel -> False, TextAlignment -> Center,
        CellMargins -> {{50, 50}, {10, 14}}],
     Cell["[missing figure: " <> file <> "]", "Text"]]];

(* ---------- numbers pulled from the computed data ---------- *)
align = Import[FileNameJoin[{dataDir, "alignment.json"}], "RawJSON"];
arow[dec_] := SelectFirst[align["rows"], #["decade"] == dec &];
nf[x_, d_: 2] := ToString@NumberForm[N[x], {d + 1, d}];
a1800 = arow["1800"]; a1900 = arow["1900"];
laws = Import[FileNameJoin[{dataDir, "laws.json"}], "RawJSON"];
lawConf = laws["conformity"]; lawInno = laws["innovation"];
nContent = laws["nContentWords"];

(* ---------- assemble cell list ---------- *)
$cells = {};
push[c_] := AppendTo[$cells, c];

push @ title["Diachronic Word Embeddings"];
push @ subtitle["200 Years of Semantic Drift, Traced Through a Word-Embedding Space"];

push @ imgCell["hero_still.png", 720];
push @ caption[{"Fourteen words, two centuries (cover; animated version in ",
   mono["docs/images/hero.gif"], "). Each panel projects one word's per-decade ",
   "vector \[Dash] blue in the 1800s, red in the 1990s \[Dash] into a 2-D map of its ",
   "own changing neighbourhood. \[Section]5 lets you explore any of the words interactively."}];

push @ abstract[{
  "Word meanings move. ", ital["Gay"], " once meant carefree; ", ital["broadcast"],
  " meant to scatter seed by hand; a ", ital["computer"], " was a person who did ",
  "arithmetic. This notebook makes that motion visible and measurable. Using the ",
  bold["HistWords"], " embeddings (word2vec / SGNS trained on the Google N-grams ",
  "English corpus, one model per decade from the 1800s to the 1990s), we bring two ",
  "centuries of independently-trained vector spaces into a single comparable frame ",
  "with ", bold["orthogonal Procrustes alignment"], ", then trace individual words ",
  "as ", bold["trajectories"], " through a 2-D projection of that shared space."}];
push @ abstract[{"The drifting paths are the heart of the post (and \[Section]5 turns ",
  "them into an interactive explorer). We then test, on the same aligned embeddings, the ",
  "two ", ital["statistical laws of semantic change"], " of Hamilton, Leskovec & Jurafsky ",
  "(2016): the law of conformity (frequent words drift more slowly) replicates cleanly, ",
  "while the law of innovation (polysemous words drift faster) turns out to be a frequency ",
  "confound in this corpus \[Dash] a useful reminder to control before concluding. ",
  "Everything is computed in the Wolfram Language; the code and data are in the companion ",
  "repository."}];

(* ---- 1 ---- *)
push @ hd1["1. The idea: words as moving points"];
push @ para[{"A ", bold["word embedding"], " places every word at a point in a ",
   "high-dimensional space so that words appearing in similar contexts land close ",
   "together. Cosine similarity between two vectors then behaves like a similarity of ",
   "meaning: in a modern embedding, ", ital["king"], " sits near ", ital["queen"],
   " and ", ital["throne"], ", far from ", ital["pizza"], "."}];
push @ para[{"Now train one embedding on text from the 1850s and another on text from ",
   "the 1990s. A word whose meaning has changed will sit in a different neighbourhood ",
   "on the two maps. ", ital["Gay"], " keeps company with ", ital["cheerful, merry, ",
   "lively"], " in the 1850s and with ", ital["homosexual, lesbian, bisexual"],
   " in the 1990s. If we can line the two maps up, that change becomes a displacement ",
   "we can draw \[Dash] and across twenty decades, a path."}];
push @ takeaway[{"Meaning change shows up as a word ", ital["moving"],
   " relative to the rest of the vocabulary. The whole project is about measuring and ",
   "drawing that motion honestly."}];

(* ---- 2 ---- *)
push @ hd1["2. The data: HistWords decade embeddings"];
push @ para[{"We use the ", bold["HistWords"], " embeddings of Hamilton et al.: ",
   "300-dimensional skip-gram-with-negative-sampling (SGNS) vectors, trained ",
   "separately for each decade on the Google N-grams ", mono["eng-all"], " corpus, ",
   "from the 1800s through the 1990s \[Dash] twenty decade-models, each with a ",
   "100,000-word vocabulary. The vectors are L2-normalized; a word absent from a ",
   "decade is stored as a zero vector and skipped. The raw download is ~1.6 GB and is ",
   "not committed; a thin Python shim converts the NumPy/pickle files to plain float32 ",
   "binaries, and everything downstream is Wolfram Language."}];
push @ para[{"Coverage varies by word. Of the fourteen we track, ", ital["gay"], ", ",
   ital["awful"], ", ", ital["literally"], ", ", ital["nice"], ", ", ital["mouse"],
   " and ", ital["web"], " appear in all twenty decades; ", ital["queer"],
   " from the 1830s, ", ital["broadcast"], " from the 1850s, and ", ital["computer"],
   " at 1900 (the human-\[OpenCurlyDoubleQuote]computer\[CloseCurlyDoubleQuote] sense) ",
   "then continuously from the 1940s (the machine). Gaps are drawn as dashed segments."}];

(* ---- 3 ---- *)
push @ hd1["3. Why the decades must be aligned"];
push @ para[{"Each decade's embedding is trained independently, so its coordinate axes ",
   "are arbitrary: two decade-maps can be rotated and reflected relative to one another ",
   "even when they agree perfectly about which words are near which. (A skip-gram ",
   "objective is invariant under any orthogonal transformation of the space, so nothing ",
   "pins down a shared orientation.) Before a word can be tracked through time, the ",
   "decades must be rotated into a common frame."}];
push @ para[{"We use ", bold["orthogonal Procrustes alignment"], ". Let ", ital["A"],
   " hold the vectors of the 5,000 highest-frequency words present in every decade, for ",
   "one decade, and ", ital["B"], " the same words in the 1990s reference. We seek the ",
   "orthogonal ", ital["R"], " minimising"}];
push @ displayMath[RowBox[{UnderscriptBox["argmin", StyleBox["R", FontSlant -> Italic]],
   " ", SuperscriptBox[RowBox[{"\[LeftDoubleBracketingBar]", "A", " ", "R", " ", "-", " ",
       "B", "\[RightDoubleBracketingBar]"}], "2"], " ",
   RowBox[{"subject\[ThinSpace]to", " ", SuperscriptBox["R", "T"], "R", "=", "I"}]}]];
push @ para[{"This has a closed form from a singular value decomposition: if ",
   ital["U S V"], "\[Transpose] = ", ital["A"], "\[Transpose]", ital["B"], ", then ",
   ital["R"], " = ", ital["U V"], "\[Transpose]", ". Because ", ital["R"],
   " is orthogonal it preserves every length and angle ", ital["within"],
   " a decade \[Dash] it only reorients the space \[Dash] so each decade's nearest ",
   "neighbours are exactly as trained, while becoming comparable ", ital["across"],
   " decades. (Nearest-neighbour queries in \[Section]6 are therefore run in each ",
   "decade's own native space; alignment is needed only to compare frames.)"}];
push @ imgCell["alignment_effect.png", 600];
push @ caption[{"Mean cosine of the 5,000 shared anchor words to their 1990s vectors, ",
   "before (blue) and after (red) Procrustes alignment. Alignment lifts the curve at ",
   "every decade; both necessarily reach 1.0 at the 1990s reference."}];
push @ para[{"How far from comparable are the raw spaces? The rotation that best aligns ",
   "the 1800s onto the 1990s is large: \[LeftDoubleBracketingBar]R \[Minus] ",
   "I\[RightDoubleBracketingBar] = ", nf[a1800["rotFrobenius"]], ". For calibration, ",
   "two unrelated 300-D spaces are related by a random rotation with ",
   "\[LeftDoubleBracketingBar]R \[Minus] I\[RightDoubleBracketingBar] \[TildeTilde] ",
   nf[align["rotFrobenius_randomBaseline"]], ", identical spaces give 0, and a perfect ",
   "mirror-flip gives the maximum ", nf[align["rotFrobenius_max"]], ". So the 1800s axes ",
   "begin almost as unrelated to the 1990s as random \[Dash] alignment is essential, not ",
   "cosmetic \[Dash] and the required rotation then shrinks smoothly toward the present."}];
push @ para[{"Aligning lifts the mean anchor-word cosine to the 1990s at every decade ",
   "(1800s: ", nf[a1800["cosUnaligned"]], " \[Rule] ", nf[a1800["cosAligned"]],
   "; 1900s: ", nf[a1900["cosUnaligned"]], " \[Rule] ", nf[a1900["cosAligned"]],
   "). That the raw (blue) curve already rises, rather than sitting near zero, is itself ",
   "informative: HistWords initialises each decade's training from the previous one, so ",
   "adjacent decades inherit a partial common orientation \[Dash] but that shared frame ",
   "erodes over a century, which is exactly why distant decades must be re-aligned."}];
push @ takeaway[{"Independently trained spaces are not comparable as shipped. One ",
   "orthogonal rotation per decade, fitted on stable high-frequency words, makes them ",
   "comparable while leaving each decade's internal geometry untouched."}];

(* ---- 4 ---- *)
push @ hd1["4. How to read a trajectory"];
push @ para[{"For one word we collect its aligned vector in every decade it occurs, ",
   "gather the nearest neighbours that anchor each era of its meaning, and project that ",
   "whole local neighbourhood to two dimensions with PCA. The word's path then threads ",
   "between an early-meaning cluster and a late-meaning cluster; colour runs from blue ",
   "(1800s) to red (1990s)."}];
push @ para[{bold["What the projection does and does not say."], " The 2-D layout is a ",
   ital["per-word"], " PCA of that word's own cloud, chosen to spread its motion across ",
   "the page; absolute positions and the axes themselves carry no global meaning, and ",
   "2-D distances are an approximation of the true 300-D ones. The trustworthy signal is ",
   bold["which neighbours sit at each end"], " of the path and ", bold["how far the word ",
   "travels between them"], " \[Dash] both of which come straight from the high-",
   "dimensional cosine geometry, not the projection."}];
push @ imgCell["traj_gay.png", 540];
push @ caption[{ital["gay"], ": the path lingers among ", ital["merry, cheerful, ",
   "joyous, lively, gaiety"], " through the blue and purple decades, then swings sharply ",
   "red-ward to ", ital["homosexual, lesbian, gays, bisexual"], " by the 1990s \[Dash] a ",
   "two-century meaning change rendered as a single sweep across the page."}];
push @ takeaway[{"Read a trajectory by its ", ital["endpoints and their neighbours"],
   ", not by the exact shape of the curve. The neighbours are the evidence; the curve is ",
   "just their geometry."}];

(* ---- 5 ---- interactive explorer ---- *)
push @ hd1["5. Explore all fourteen words"];
push @ para[{"Rather than print fourteen separate plots, the construction above is wired ",
   "into one interactive ", mono["Manipulate"], ". Pick a word, scrub the time slider to ",
   "reveal the path decade by decade, and toggle the neighbour labels. The static image ",
   "below shows one state (", ital["gay"], ", full path); evaluate the code cell beneath ",
   "it in the live notebook for the interactive version."}];
push @ para[{"A few worth visiting: ", ital["broadcast"], " (agricultural ", ital["sow / ",
   "seed / scatter"], " \[Rule] ", ital["radio / television / bbc"], "); ", ital["computer"],
   " (a V from human surnames through ", ital["electronic / circuits"], " to ",
   ital["software / internet"], "); ", ital["mouse"], " (keeps ", ital["rat / cat"],
   " but grows a second ", ital["cursor / click / keyboard"], " cluster); ", ital["web"],
   " (spider \[Rule] ", ital["www / browser / server"], "); ", ital["terrific"],
   " and ", ital["awful"], " (the two halves of a terror-to-praise swap); and ",
   ital["guy"], " (the Guy Fawkes effigy \[Rule] ", ital["fellow / chap / dude"], ")."}];

(* build the embedded, self-contained explorer code as evaluatable Input cells *)
traj = Import[FileNameJoin[{dataDir, "trajectories.json"}], "RawJSON"];
wordsOrder = {"gay", "broadcast", "computer", "awful", "queer", "literally",
   "terrific", "guy", "media", "fun", "cell", "nice", "mouse", "web"};
mkRec[r_] := <|"decades" -> r["decades"], "path" -> Round[r["path"], 0.001],
   "neighbors" -> ({#["word"], Round[#["xy"], 0.001]} & /@ r["neighbors"])|>;
trajClean = Association @ Table[w -> mkRec[traj[w]], {w, wordsOrder}];
compressed = Compress[trajClean];
Print["embedded trajData compressed size: ", StringLength[compressed], " chars"];

(* static preview of the explorer (gay, full path) -- so the rendered page /
   web view always shows a real plot even though the live Manipulate below is
   only realised when a reader evaluates it. *)
deCol[t_] := Blend[{RGBColor[0.13, 0.34, 0.68], RGBColor[0.85, 0.16, 0.11]}, t];
previewDraw[w_] := Module[{r = trajClean[w], ds, path, nbrs, n, fr},
   ds = r["decades"]; path = r["path"]; nbrs = r["neighbors"]; n = Length[ds];
   fr[i_] := (ToExpression[ds[[i]]] - 1800)/190.;
   Graphics[{
      {GrayLevel[0.5], Text[Style[#[[1]], Italic, 10], #[[2]]] & /@ nbrs},
      Table[{deCol[fr[i]], Opacity[0.5], Thickness[0.006],
         Line[{path[[i]], path[[i + 1]]}]}, {i, n - 1}], Opacity[1],
      Table[{deCol[fr[i]], EdgeForm[White], PointSize[0.022], Point[path[[i]]]}, {i, n}],
      Text[Style[StringTake[ds[[1]], -2] <> "s", Bold, 11, deCol[0]], path[[1]], {0, -1.7}],
      Text[Style[StringTake[ds[[-1]], -2] <> "s", Bold, 13, deCol[1]], path[[-1]], {0, -1.7}]},
    PlotLabel -> Style[w, 16, Bold, FontFamily -> "Helvetica"],
    PlotRange -> CoordinateBounds[Join[path, nbrs[[All, 2]]], Scaled[0.12]],
    ImageSize -> 560, AspectRatio -> 0.85, Background -> White]];
Export[FileNameJoin[{imgDir, "explorer_preview.png"}], previewDraw["gay"],
   "PNG", ImageResolution -> 130];
Print["wrote docs/images/explorer_preview.png"];

(* ONE self-contained Manipulate. All data + helpers live in Initialization,
   so the result renders even with no prior cell evaluated. A single Input
   cell (not split across cells) avoids the Syntax::sntxi that NotebookEvaluate
   threw on multi-statement string cells. *)
manipCode = StringJoin[
  "Manipulate[\n",
  " deDraw[word, yearMax, neighbourLabels],\n",
  " {{word, \"gay\", \"word\"}, deWords},\n",
  " {{yearMax, 1990, \"reveal decades up to\"}, 1800, 1990, 10, Appearance -> \"Labeled\"},\n",
  " {{neighbourLabels, True, \"neighbour labels\"}, {True, False}},\n",
  " ControlPlacement -> Top, SaveDefinitions -> True,\n",
  " Initialization :> (\n",
  "  deData = Uncompress[", ToString[compressed, InputForm], "];\n",
  "  deWords = ", ToString[wordsOrder, InputForm], ";\n",
  "  deCol[t_] := Blend[{RGBColor[0.13, 0.34, 0.68], RGBColor[0.85, 0.16, 0.11]}, t];\n",
  "  deDraw[w_, yearMax_, showLabels_] := Module[\n",
  "    {r = deData[w], ds, path, nbrs, n, fr, shown, last},\n",
  "    ds = r[\"decades\"]; path = r[\"path\"]; nbrs = r[\"neighbors\"]; n = Length[ds];\n",
  "    fr[i_] := (ToExpression[ds[[i]]] - 1800)/190.;\n",
  "    shown = Select[Range[n], ToExpression[ds[[#]]] <= yearMax &];\n",
  "    Graphics[{\n",
  "       If[showLabels, {GrayLevel[0.5],\n",
  "          (Text[Style[#[[1]], Italic, 10], #[[2]]] & /@ nbrs)}, {}],\n",
  "       If[Length[shown] >= 2, Table[{deCol[fr[i]], Opacity[0.5], Thickness[0.006],\n",
  "          Line[{path[[i]], path[[i + 1]]}]}, {i, Most[shown]}], {}], Opacity[1],\n",
  "       Table[{deCol[fr[i]], EdgeForm[White], PointSize[0.022], Point[path[[i]]]},\n",
  "          {i, shown}],\n",
  "       If[shown =!= {}, last = Last[shown];\n",
  "          {Text[Style[StringTake[ds[[1]], -2] <> \"s\", Bold, 11, deCol[fr[1]]],\n",
  "             path[[1]], {0, -1.7}],\n",
  "           Text[Style[StringTake[ds[[last]], -2] <> \"s\", Bold, 13, deCol[fr[last]]],\n",
  "             path[[last]], {0, -1.7}]}, {}]},\n",
  "     PlotLabel -> Style[w, 16, Bold, FontFamily -> \"Helvetica\"],\n",
  "     PlotRange -> CoordinateBounds[Join[path, nbrs[[All, 2]]], Scaled[0.12]],\n",
  "     ImageSize -> 560, AspectRatio -> 0.85, Background -> White]];\n",
  "  )]"
];

push @ caption[{"The \[Section]5 explorer. In the live notebook the cell below renders ",
   "as an interactive ", mono["Manipulate"], " (choose a word, scrub the decade slider, ",
   "toggle neighbour labels); a static preview is shown here for the PDF / web view."}];
(* This Input cell is converted to proper input boxes (so it is a clean,
   syntax-highlighted, evaluatable cell) and then evaluated on build by
   NotebookEvaluate, so the rendered notebook shows the live Manipulate
   rather than an unevaluated red-bracketed box. *)
manipCellBoxes = ToExpression[
   "FrontEndExecute@FrontEnd`ReparseBoxStructurePacket[" <>
   ToString[manipCode, InputForm] <> "][[1]]"];
push @ Cell[BoxData[manipCellBoxes], "Input", CellMargins -> {{50, 30}, {6, 6}},
   FontSize -> 11];

(* ---- 6 ---- *)
push @ hd1["6. Nearest-neighbour drift tables"];
push @ para[{"The trajectories are the geometry of a simpler fact: the list of a word's ",
   "nearest neighbours turns over from decade to decade. Neighbours are computed by ",
   "cosine similarity in each decade's own native space. Sampled every fifty years:"}];
neigh = Import[FileNameJoin[{dataDir, "neighbors.json"}], "RawJSON"];
ndecs = {"1850", "1900", "1950", "1990"};
nbRow[w_] := Prepend[
   Table[If[KeyExistsQ[neigh[w], d],
      StringRiffle[Take[neigh[w][d], UpTo[5]], ", "], "\[Dash]"], {d, ndecs}],
   Style[w, Italic, Bold]];
nbWords = {"gay", "broadcast", "awful", "computer", "guy", "terrific", "media", "queer"};
nbTable = Grid[
   Prepend[nbRow /@ nbWords,
     Style[#, Bold] & /@ Prepend[(StringTake[#, -2] <> "s") & /@ ndecs, "word"]],
   Frame -> All, Alignment -> Left, Background -> {None, {RGBColor[0.92, 0.94, 1.0]}},
   Spacings -> {1, 0.8}, ItemSize -> {{6, 14, 14, 14, 14}, Automatic},
   FrameStyle -> GrayLevel[0.7]];
push @ Cell[BoxData @ ToBoxes @ Style[nbTable, 10], "Output", ShowCellLabel -> False,
   TextAlignment -> Center, CellMargins -> {{40, 40}, {10, 14}}];
push @ caption[{"Top-5 cosine nearest neighbours by decade. The full table for all ",
   "fourteen words is in ", mono["data/neighbors.json"], "."}];
push @ takeaway[{"Trajectories can be cross-checked against plain word lists. When the ",
   "neighbours of ", ital["broadcast"], " go from ", ital["sow, scatter"], " to ",
   ital["radio, television"], ", the drawn path is just reporting that shift."}];

(* ---- 7 ---- *)
push @ hd1["7. Two statistical laws of semantic change"];
push @ para[{"Hamilton, Leskovec & Jurafsky (2016) propose two regularities of semantic ",
   "change. Both can be tested directly on the aligned embeddings, measured over the ",
   bold[ToString[nContent]], " dictionary content words present in every decade (function ",
   "words and archaic/foreign tokens removed via a stoplist and ", mono["WordData"],
   " membership). A word's ", ital["rate of change"], " is the mean cosine distance ",
   "between its aligned vectors in consecutive decades; its ", ital["frequency"],
   " is proxied by its mean rank, and its ", ital["polysemy"],
   " by its number of WordNet senses. Re-deriving the laws independently is also a good ",
   "way to watch a confound masquerade as a result."}];
push @ hd2["The law of conformity \[Dash] holds"];
push @ para[{bold["More frequent words change more slowly."], " Change-rate falls steeply ",
   "with frequency (Spearman \[Rho] = ", nf[lawConf["spearman"]], ", slope ",
   nf[lawConf["slope"]], " in log-rate). ", ital["Why:"], " a high-frequency word is seen ",
   "in a huge range of contexts, so its vector is an average over many uses and is ",
   "statistically well-anchored. Part of the strength is also methodological \[Dash] rare ",
   "words are estimated from little data, so their vectors are noisier and ", ital["look"],
   " like they move more \[Dash] which is itself worth stating plainly."}];
push @ imgCell["law_conformity.png", 560];
push @ caption[{"Rate of semantic change vs. frequency (\[Minus]log mean rank). Red ",
   "markers are decile means."}];
push @ hd2["The law of innovation \[Dash] a cautionary tale"];
push @ para[{"The published law says ", bold["more polysemous words change faster"],
   ". In this corpus the ", ital["raw"], " correlation comes out the wrong way: change-",
   "rate vs. polysemy has Spearman \[Rho] = ", nf[lawInno["rawSpearman"]],
   " \[Dash] negative. The culprit is a ", bold["confound"], ": polysemous words tend to ",
   "be frequent (\[Rho] = ", nf[lawInno["polyVsFreqSpearman"]],
   "), and frequency strongly suppresses change. In the raw comparison the frequency ",
   "effect simply dominates \[Dash] a textbook Simpson's paradox."}];
push @ para[{"Controlling for frequency (partial Spearman correlation, holding frequency ",
   "rank fixed) the spurious negative trend vanishes \[Dash] but it does not turn ",
   "convincingly positive either: partial \[Rho] = ", nf[lawInno["partialSpearman"]],
   ", essentially zero. ", bold["So in the Google-N-grams eng-all corpus, with WordNet ",
   "sense counts as the polysemy measure, the law of innovation does not replicate."],
   " That is a more honest \[Dash] and more useful \[Dash] result than forcing the sign: ",
   "the law may need a different corpus, a better polysemy measure, or the original ",
   "authors' frequency-and-polysemy-matched controls to surface."}];
push @ imgCell["law_innovation.png", 560];
push @ caption[{"The raw polysemy trend (blue) slopes the \"wrong\" way only because ",
   "polysemous words are frequent; the frequency-controlled residual (red) is flat. Raw ",
   "Spearman ", nf[lawInno["rawSpearman"]], ", partial ", nf[lawInno["partialSpearman"]], "."}];
push @ takeaway[{"Conformity replicates cleanly; innovation does not survive a frequency ",
   "control here. A large raw correlation is easy to get and easy to misread \[Dash] ",
   "always ask what else moves with your variable before naming an effect."}];

(* ---- 8 ---- *)
push @ hd1["8. The most-drifted words \[Dash] and a caveat"];
push @ para[{"Ranking every stable content word by how far it moved between its 1800s and ",
   "1990s aligned vectors surfaces two phenomena at once. Some entries are genuine ",
   "meaning change (", ital["advert"], " verb \[Rule] noun, ", ital["peer"],
   " noble \[Rule] equal, ", ital["media"], ", ", ital["broadcast"],
   "); others are lexical obsolescence or register shift, where an archaic word (",
   ital["twelvemonth, fourthly, civilities"], ") simply stopped occurring in the same ",
   "kinds of text. A large embedding displacement means ",
   "\[OpenCurlyDoubleQuote]the contexts changed\[CloseCurlyDoubleQuote] \[Dash] which is ",
   "necessary, but not sufficient, for \[OpenCurlyDoubleQuote]the meaning ",
   "changed\[CloseCurlyDoubleQuote]. Distinguishing the two is where embeddings end and ",
   "lexicography begins."}];
push @ imgCell["drift_top.png", 560];
push @ caption[{"The 25 most-drifted stable content words, 1800s\[Dash]1990s. Full ",
   "ranking in ", mono["data/drift_ranking.json"], "."}];
push @ takeaway[{"Embeddings are a powerful detector of ", ital["that"],
   " something changed about a word's usage; deciding ", ital["what"],
   " changed still needs the neighbour lists, the trajectories, and human judgement."}];

(* ---- 9 ---- *)
push @ hd1["9. Methods and reproduction"];
push @ para[{"The science is pure Wolfram Language; the only Python is a format shim for ",
   "the NumPy/pickle files. Five steps:"}];
push @ item[{mono["wolframscript -file wolfram/fetch_data.wls"],
   "  \[Dash] download + unzip the HistWords eng-all SGNS embeddings"}];
push @ item[{mono["python3 src/convert_histwords.py"],
   "  \[Dash] .npy/.pkl \[Rule] float32 binaries + vocab (anchors, targets, pools)"}];
push @ item[{mono["wolframscript -file wolfram/trajectories.wls"],
   "  \[Dash] Procrustes alignment, projection, the trajectory figures + JSON"}];
push @ item[{mono["wolframscript -file wolfram/laws_and_drift.wls"],
   "  \[Dash] drift leaderboard + the two statistical laws"}];
push @ item[{mono["wolframscript -file wolfram/alignment.wls"], " and ",
   mono["wolfram/hero.wls"], "  \[Dash] the alignment diagnostic and the cover animation"}];
push @ para[{"Repository: ", mono["github.com/mthiel74/DiachronicWordEmbeddings"],
   ". Derived products (neighbour tables, trajectory coordinates, alignment diagnostics, ",
   "drift ranking) are committed as small JSON files; the 1.6 GB raw download is ",
   "regenerable. The interactive explorer in \[Section]5 embeds its 2-D coordinates, so ",
   "the notebook is self-contained."}];

(* ---- 10 ---- *)
push @ hd1["10. References"];
push @ refItem[{"W. L. Hamilton, J. Leskovec, D. Jurafsky (2016). ",
   ital["Diachronic Word Embeddings Reveal Statistical Laws of Semantic Change."],
   " ACL 2016. arXiv:1605.09096."}];
push @ refItem[{"HistWords project and data: ",
   "https://nlp.stanford.edu/projects/histwords/ (Stanford NLP)."}];
push @ refItem[{"P. H. Sch\[ODoubleDot]nemann (1966). ",
   ital["A generalized solution of the orthogonal Procrustes problem."],
   " Psychometrika 31(1). (The SVD alignment used in \[Section]3.)"}];
push @ refItem[{"T. Mikolov et al. (2013). ",
   ital["Distributed Representations of Words and Phrases and their Compositionality."],
   " NeurIPS 2013. (word2vec / SGNS.)"}];
push @ refItem[{"V. Kulkarni et al. (2015). ",
   ital["Statistically Significant Detection of Linguistic Change."], " WWW 2015."}];
push @ refItem[{"J.-B. Michel et al. (2011). ",
   ital["Quantitative Analysis of Culture Using Millions of Digitized Books."],
   " Science 331. (The Google N-grams corpus.)"}];

(* ---- save: real notebook via the front end. NotebookEvaluate runs the one
   \[Section]5 Manipulate Input cell (now proper input boxes, not a raw string,
   which is what previously triggered Syntax::sntxi) so the saved notebook shows
   the LIVE interactive Manipulate as output rather than an unevaluated
   red-bracketed box. The 21 figure/table Output cells are already boxed and
   are not re-run. ---- *)
Print["assembled ", Length@$cells, " cells; building notebook ..."];
UsingFrontEnd @ Module[{doc},
   doc = CreateDocument[$cells, Visible -> False,
      StyleDefinitions -> "Default.nb", WindowSize -> {840, 900}];
   NotebookEvaluate[doc, InsertResults -> True];
   NotebookSave[doc, outNb];
   Export[outPdf, doc, "PDF"];
   NotebookClose[doc];
];
ok[f_] := FileExistsQ[f] && FileByteCount[f] > 20000;
Print[If[ok[outNb], "wrote ", "FAILED "], outNb, "  ", FileByteCount[outNb], " bytes"];
Print[If[ok[outPdf], "wrote ", "FAILED "], outPdf, "  ", FileByteCount[outPdf], " bytes"];
If[! (ok[outNb] && ok[outPdf]), Exit[1]];
