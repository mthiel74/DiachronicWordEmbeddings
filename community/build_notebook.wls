#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/diachronic_embeddings.nb (+ .pdf) -- the long-form
    Wolfram Community post on 200 years of semantic drift.

    Architecture mirrors the ENSO-emergence builder: open an invisible
    front-end document and NotebookWrite fully-formed Cell[...] expressions
    into it; images are embedded as resized Image[] rasters.

    Sections
    --------
      Abstract
      1. The idea: words as moving points
      2. The data: HistWords decade embeddings
      3. Why decades must be aligned (orthogonal Procrustes)
      4. Reading a trajectory: six words, six stories
      5. The full gallery (twelve words)
      6. Nearest-neighbour drift tables
      7. Statistical laws of semantic change
      8. The most-drifted words, and a caveat
      9. How to reproduce / methods
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

displayMath[expr_] := Cell[BoxData @ FormBox[expr, TraditionalForm],
   "DisplayFormula", CellMargins -> {{80, 50}, {8, 8}}];

(* resized image cell *)
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

(* ---------- assemble ---------- *)
$cells = {};
push[c_] := AppendTo[$cells, c];

push @ title["Diachronic Word Embeddings"];
push @ subtitle["200 Years of Semantic Drift, Traced Through a Word-Embedding Space"];

push @ abstract[{
  "Word meanings move. ", ital["Gay"], " once meant carefree; ", ital["broadcast"],
  " meant to scatter seed by hand; a ", ital["computer"],
  " was a person who did arithmetic. This notebook makes that motion visible. ",
  "Using the HistWords decade-by-decade embeddings (word2vec / SGNS trained on ",
  "the Google N-grams English corpus, one model per decade from the 1800s to the ",
  "1990s), we bring two centuries of independently-trained embedding spaces into a ",
  "single comparable frame with ", bold["orthogonal Procrustes alignment"],
  ", then trace individual words as ", bold["trajectories"],
  " through a 2-D projection of that shared space. The paths \[Dash] a word drifting ",
  "past the neighbours that anchor each era of its meaning \[Dash] are the point of the ",
  "project. We close by recovering, from the same data, the two ",
  ital["statistical laws of semantic change"], " of Hamilton, Leskovec & Jurafsky (2016). ",
  "Everything here is computed in the Wolfram Language."}];

push @ imgCell["hero_still.png", 720];
push @ caption[{"Six words, two centuries. Each panel projects one word's decade ",
   "vectors (blue = 1800s \[Rule] red = 1990s) into a 2-D map of its own changing ",
   "neighbourhood. The animated version (", mono["docs/images/hero.gif"],
   ") draws each path on decade at a time."}];

(* ---- 1 ---- *)
push @ hd1["1. The idea: words as moving points"];
push @ para[{"A word embedding places every word at a point in a high-dimensional ",
   "space so that words used in similar contexts sit close together. Train one ",
   "embedding on text from the 1850s and another on text from the 1990s and you get ",
   "two maps of the language \[Dash] and a word that has changed its meaning will sit ",
   "in a different neighbourhood on the two maps. ", ital["Gay"],
   " sits among ", ital["cheerful, merry, lively"], " on the 1850s map and among ",
   ital["homosexual, lesbian, bisexual"], " on the 1990s map. If we can line the maps ",
   "up, we can draw the move as an arrow \[Dash] and across twenty decades, as a path."}];

(* ---- 2 ---- *)
push @ hd1["2. The data: HistWords decade embeddings"];
push @ para[{"We use the ", bold["HistWords"], " embeddings of Hamilton et al.: ",
   "300-dimensional skip-gram-with-negative-sampling (SGNS) vectors, trained ",
   "separately for each decade on the Google N-grams ", mono["eng-all"], " corpus, ",
   "covering the 1800s through the 1990s \[Dash] twenty decade-models, each with a ",
   "100,000-word vocabulary. The vectors are L2-normalized; words absent from a ",
   "decade are stored as zero vectors and skipped. The raw download is ~1.6 GB and ",
   "is not committed; a thin Python shim converts the NumPy/pickle files to plain ",
   "float32 binaries, and all of the analysis below is Wolfram Language."}];
push @ para[{"Of the six headline words, ", ital["gay"], ", ", ital["awful"],
   " and ", ital["literally"], " appear in all twenty decades; ", ital["queer"],
   " from the 1830s, ", ital["broadcast"], " from the 1850s, and ", ital["computer"],
   " at 1900 (the human-\[OpenCurlyDoubleQuote]computer\[CloseCurlyDoubleQuote] sense) ",
   "then continuously from the 1940s (the machine)."}];

(* ---- 3 ---- *)
push @ hd1["3. Why the decades must be aligned"];
push @ para[{"Each decade's embedding is trained independently, so its coordinate ",
   "axes are arbitrary: the two maps can be rotated and reflected relative to one ",
   "another even when they agree perfectly about which words are near which. Before ",
   "any word can be tracked through time, the decades must be rotated into a common ",
   "frame. We use ", bold["orthogonal Procrustes alignment"],
   ": for the matrices ", ital["A"], " (a decade) and ", ital["B"],
   " (the 1990s reference) holding the vectors of the 5,000 highest-frequency words ",
   "present in every decade, we seek the orthogonal ", ital["R"], " minimising"}];
push @ displayMath[RowBox[{UnderscriptBox["argmin", StyleBox["R", FontSlant -> Italic]],
   " ", SuperscriptBox[RowBox[{"\[LeftDoubleBracketingBar]", "A", " ", "R", " ", "-", " ",
       "B", "\[RightDoubleBracketingBar]"}], "2"], " ",
   RowBox[{"with", " ", SuperscriptBox["R", "T"], "R", "=", "I"}]}]];
push @ para[{"whose closed-form solution is ", ital["R"], " = ", ital["U"],
   " ", ital["V"], SuperscriptBox["", "T"], " from the singular value decomposition ",
   ital["U S V"], SuperscriptBox["", "T"], " = ", ital["A"], SuperscriptBox["", "T"],
   " ", ital["B"], ". Because ", ital["R"], " is orthogonal it preserves all within-",
   "decade distances \[Dash] it only reorients the space \[Dash] so nearest neighbours ",
   "stay exactly as trained, while becoming comparable across decades."}];
push @ para[{"This step is necessary, not cosmetic. The rotations are large (the 1800s ",
   "map is rotated by nearly the maximum possible amount onto the 1990s map), and ",
   "alignment measurably raises the cross-decade cosine similarity of stable words ",
   "(e.g. the average for the 5,000 anchor words rises from 0.50 to 0.65 between the ",
   "1900s and the 1990s)."}];

(* ---- 4 ---- *)
push @ hd1["4. Reading a trajectory: six words, six stories"];
push @ para[{"For each word we collect its aligned vector in every decade in which it ",
   "occurs, gather the nearest neighbours that anchor each era, and project the whole ",
   "local neighbourhood to two dimensions with PCA. The word's path threads between ",
   "an early-meaning cluster and a late-meaning cluster; the colour runs blue (1800s) ",
   "to red (1990s), and dashed segments mark gaps in coverage."}];

push @ imgCell["traj_gay.png", 560];
push @ caption[{ital["gay"], " lingers among ", ital["merry, cheerful, lively, joyous"],
   " through the early-20th-century before swinging sharply toward ",
   ital["homosexual, lesbian, gays"], " in the final decades."}];

push @ imgCell["traj_broadcast.png", 560];
push @ caption[{ital["broadcast"], " begins as an agricultural term among ",
   ital["sow, seed, scatter, cuttings"], " and moves wholesale into ",
   ital["radio, television, bbc, broadcasting"], " \[Dash] the word the new technology ",
   "borrowed."}];

push @ imgCell["traj_computer.png", 560];
push @ caption[{ital["computer"], " traces a V: from human ",
   "\[OpenCurlyDoubleQuote]computers\[CloseCurlyDoubleQuote] (surnames such as ",
   ital["williams, barney, burt"], ") at 1900, down through ",
   ital["electronic, circuits, digital"], " mid-century, up to ",
   ital["software, internet, database"], ". The dashed leg is the 1900\[Dash]1940 gap."}];

push @ imgCell["traj_guy.png", 560];
push @ caption[{ital["guy"], " moves from the Guy Fawkes ", ital["effigy, bonfires, guise"],
   " to the everyday ", ital["fellow, chap, bloke, dude"], "."}];

push @ imgCell["traj_terrific.png", 560];
push @ caption[{ital["terrific"], " ameliorates from ",
   ital["dreadful, terrible, fearful"], " to ", ital["marvelous, wonderful, splendid"],
   " \[Dash] the same terror-to-praise path ", ital["awful"], " took in reverse."}];

push @ imgCell["traj_media.png", 560];
push @ caption[{ital["media"], " starts as a laboratory plural (",
   ital["agar, culture, opacities, refractive"], ") and becomes ",
   ital["newspapers, television, coverage, publicity"], "."}];

push @ imgCell["traj_mouse.png", 560];
push @ caption[{ital["mouse"], " keeps its old company \[Dash] ",
   ital["rat, cat, squirrel, weasel"], " \[Dash] then in the final decades grows ",
   "a second cluster of ", ital["cursor, click, keyboard, pointer, button"], "."}];

push @ imgCell["traj_web.png", 560];
push @ caption[{ital["web"], " runs from the spider's ",
   ital["threads, woven, weave, mesh"], " to the ",
   ital["www, url, html, browser, server, internet"], " of the 1990s."}];

(* ---- 5 ---- *)
push @ hd1["5. The full gallery"];
push @ para[{"The same construction for all fourteen words. Alongside those above: ",
   ital["awful"], " (awe \[Rule] very bad), ", ital["queer"], " (odd \[Rule] LGBTQ), ",
   ital["literally"], " (exactly \[Rule] intensifier), ", ital["fun"],
   " (jest/mischief \[Rule] enjoyment), ", ital["cell"],
   " (prison \[Rule] biology \[Rule] battery) and ", ital["nice"],
   " (fastidious \[Rule] pleasant)."}];
push @ imgCell["traj_grid.png", 720];
push @ caption[{"Fourteen English words drifting through their own neighbourhoods, ",
   "1800s (blue) to 1990s (red)."}];

(* ---- 6 ---- *)
push @ hd1["6. Nearest-neighbour drift tables"];
push @ para[{"The trajectories are summaries of a simpler fact: the list of a word's ",
   "nearest neighbours changes from decade to decade. The neighbours are computed by ",
   "cosine similarity in each decade's own (native) space \[Dash] alignment is needed ",
   "only for the cross-decade picture. A few words, sampled every fifty years:"}];

(* neighbour table from data/neighbors.json *)
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

(* ---- 7 ---- *)
push @ hd1["7. Two statistical laws of semantic change"];
push @ para[{"Hamilton, Leskovec & Jurafsky (2016) report two regularities that hold ",
   "across languages and corpora. Both fall out of the same aligned embeddings, ",
   "measured over the ~8,800 dictionary content words present in every decade ",
   "(function words and archaic/foreign tokens removed via a stoplist and WordNet ",
   "membership). A word's ", ital["rate of change"], " is the mean cosine distance ",
   "between its aligned vectors in consecutive decades."}];
push @ hd2["The law of conformity"];
push @ para[{bold["Frequent words change more slowly."], " Plotting the log rate of ",
   "change against a frequency proxy (negative log mean rank) gives a negative slope ",
   "(\[TildeTilde] \[Minus]0.05, Pearson r \[TildeTilde] \[Minus]0.21): the more ",
   "common a word, the more anchored its meaning."}];
push @ imgCell["law_conformity.png", 560];
push @ caption[{"Rate of semantic change vs. frequency. Red markers are decile means."}];
push @ hd2["The law of innovation"];
push @ para[{bold["Polysemous words change faster."], " Using each word's number of ",
   "WordNet senses (from the Wolfram Language ", mono["WordData"],
   " knowledgebase) as a polysemy measure, the rate of change rises with polysemy ",
   "(Spearman \[Rho] \[TildeTilde] 0.20): words that already carry many senses are the ",
   "ones most apt to acquire another."}];
push @ imgCell["law_innovation.png", 560];
push @ caption[{"Rate of semantic change vs. number of WordNet senses; red markers are ",
   "per-sense-count means."}];
push @ para[{"The effects are modest with a rank-based frequency proxy on a single ",
   "corpus, but both reproduce the published direction."}];

(* ---- 8 ---- *)
push @ hd1["8. The most-drifted words \[Dash] and a caveat"];
push @ para[{"Ranking every stable content word by how far it moved between its 1800s ",
   "and 1990s aligned vectors surfaces two phenomena at once. Some entries are genuine ",
   "meaning change (", ital["advert"], " verb \[Rule] noun, ", ital["peer"],
   " noble \[Rule] equal, ", ital["media"], ", ", ital["broadcast"],
   "); others are lexical obsolescence or register shift, where an archaic word (",
   ital["twelvemonth, fourthly, civilities"],
   ") simply stopped appearing in the same kinds of text. A large embedding ",
   "displacement means \[OpenCurlyDoubleQuote]the contexts changed\[CloseCurlyDoubleQuote], ",
   "which is necessary but not sufficient for \[OpenCurlyDoubleQuote]the meaning ",
   "changed\[CloseCurlyDoubleQuote]."}];
push @ imgCell["drift_top.png", 560];
push @ caption[{"The 25 most-drifted stable content words, 1800s\[Dash]1990s. Full ",
   "ranking in ", mono["data/drift_ranking.json"], "."}];

(* ---- 9 ---- *)
push @ hd1["9. How to reproduce"];
push @ para[{"The pipeline is four steps; the science is pure Wolfram Language and the ",
   "only Python is a format shim for the NumPy/pickle files."}];
push @ item[{mono["wolframscript -file wolfram/fetch_data.wls"],
   "  \[Dash] download + unzip the HistWords eng-all SGNS embeddings"}];
push @ item[{mono["python3 src/convert_histwords.py"],
   "  \[Dash] .npy/.pkl \[Rule] float32 binaries + vocab"}];
push @ item[{mono["wolframscript -file wolfram/trajectories.wls"],
   "  \[Dash] align, project, render the trajectory figures + JSON"}];
push @ item[{mono["wolframscript -file wolfram/laws_and_drift.wls"],
   "  \[Dash] the laws of change and the drift leaderboard"}];
push @ para[{"Repository: ", mono["github.com/mthiel74/DiachronicWordEmbeddings"],
   ". Derived data products (neighbour tables, trajectory coordinates, drift ranking) ",
   "are committed as small JSON files; the 1.6 GB raw download is regenerable."}];

(* ---- 10 ---- *)
push @ hd1["10. References"];
push @ refItem[{"W. L. Hamilton, J. Leskovec, D. Jurafsky (2016). ",
   ital["Diachronic Word Embeddings Reveal Statistical Laws of Semantic Change."],
   " ACL 2016. arXiv:1605.09096."}];
push @ refItem[{"HistWords project and data: ",
   "https://nlp.stanford.edu/projects/histwords/ (Stanford NLP)."}];
push @ refItem[{"T. Mikolov et al. (2013). ",
   ital["Distributed Representations of Words and Phrases and their Compositionality."],
   " NeurIPS 2013. (word2vec / SGNS.)"}];
push @ refItem[{"J.-B. Michel et al. (2011). ",
   ital["Quantitative Analysis of Culture Using Millions of Digitized Books."],
   " Science 331. (The Google N-grams corpus.)"}];

(* ---- save: build one Notebook[] expression, then write both .nb and
   .pdf through the front end (wolframscript provides one via UsingFrontEnd).
   The plain Export[outNb, nbExpr] path returned $Failed under headless
   wolframscript, so we drive an actual notebook object instead. ---- *)
nbExpr = Notebook[$cells, StyleDefinitions -> "Default.nb",
   WindowSize -> {840, 900}, WindowMargins -> Automatic];

UsingFrontEnd @ Module[{doc},
   doc = CreateDocument[$cells, Visible -> False,
      StyleDefinitions -> "Default.nb", WindowSize -> {840, 900}];
   NotebookSave[doc, outNb];
   Export[outPdf, doc, "PDF"];
   NotebookClose[doc];
];

ok[f_] := FileExistsQ[f] && FileByteCount[f] > 20000;
Print["cells: ", Length@$cells];
Print[If[ok[outNb], "wrote ", "FAILED "], outNb, "  ",
   If[FileExistsQ[outNb], Row[{FileByteCount[outNb], " bytes"}], ""]];
Print[If[ok[outPdf], "wrote ", "FAILED "], outPdf, "  ",
   If[FileExistsQ[outPdf], Row[{FileByteCount[outPdf], " bytes"}], ""]];
If[! (ok[outNb] && ok[outPdf]), Exit[1]];
