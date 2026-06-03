#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Produce community/DiachronicEmbeddings.wl -- a SELF-CONTAINED copy of the
    rendering package wolfram/DiachronicEmbeddings.wl with the six small data
    products from data/ embedded inline. This is the .wl a reader uploads
    NEXT TO the notebook on Wolfram Community: notebook + this one file run on
    their own, with no repository checkout and no separate data files.

    The only difference from the dev package is the data-loading block: the
    six `load[...]` calls that read ../data/*.json are replaced by the same
    JSON text embedded as string literals + an ImportString loader. Everything
    else (all the rendering code) is copied verbatim, so the two never drift. *)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
srcPkg   = FileNameJoin[{repoRoot, "wolfram", "DiachronicEmbeddings.wl"}];
outPkg   = FileNameJoin[{repoRoot, "community", "DiachronicEmbeddings.wl"}];
dataDir  = FileNameJoin[{repoRoot, "data"}];

src = Import[srcPkg, "Text"];

(* the exact block to replace (file-reading loader) *)
oldBlock = StringJoin[
  "(* ---------- locate + load the committed data ---------- *)\n",
  "$deRepo = ParentDirectory @ DirectoryName[$InputFileName];\n",
  "$deData = FileNameJoin[{$deRepo, \"data\"}];\n",
  "load[f_] := Import[FileNameJoin[{$deData, f}], \"RawJSON\"];\n",
  "\n",
  "$traj   = load[\"trajectories.json\"];\n",
  "$neigh  = load[\"neighbors.json\"];\n",
  "$align  = load[\"alignment.json\"];\n",
  "$laws   = load[\"laws.json\"];\n",
  "$lpts   = load[\"laws_points.json\"];\n",
  "$drift  = load[\"drift_ranking.json\"];"];

If[! StringContainsQ[src, oldBlock],
   Print["FAILED: data-loading block not found verbatim in ", srcPkg,
     " -- did the dev package change? Aborting."]; Exit[1]];

(* embed each JSON file as an InputForm-escaped string literal *)
lit[file_] := ToString[Import[FileNameJoin[{dataDir, file}], "Text"], InputForm];

newBlock = StringJoin[
  "(* ---------- embedded committed-data products (self-contained) ----------\n",
  "   The six small JSON products that the companion repository keeps under\n",
  "   data/ are embedded below as text, so this package together with the\n",
  "   notebook runs on its own -- no repository checkout, no separate data\n",
  "   files. They are DERIVED products (2-D trajectory coordinates, nearest-\n",
  "   neighbour word lists, per-decade alignment diagnostics, and summary\n",
  "   statistics of the two laws) computed from the HistWords eng-all SGNS\n",
  "   embeddings, which Hamilton, Leskovec & Jurafsky release into the public\n",
  "   domain under the Open Data Commons PDDL v1.0. See the notebook's\n",
  "   \"Data, licensing, and acknowledgements\" section. *)\n",
  "load[json_String] := ImportString[json, \"RawJSON\"];\n",
  "\n",
  "$traj  = load[", lit["trajectories.json"], "];\n\n",
  "$neigh = load[", lit["neighbors.json"], "];\n\n",
  "$align = load[", lit["alignment.json"], "];\n\n",
  "$laws  = load[", lit["laws.json"], "];\n\n",
  "$lpts  = load[", lit["laws_points.json"], "];\n\n",
  "$drift = load[", lit["drift_ranking.json"], "];"];

out = StringReplace[src, oldBlock -> newBlock];

(* prepend a short banner noting this is the generated, uploadable copy *)
banner = StringJoin[
  "(* ::Package:: *)\n\n",
  "(*  DiachronicEmbeddings`  --  SELF-CONTAINED build for Wolfram Community.\n",
  "    GENERATED from wolfram/DiachronicEmbeddings.wl by community/bundle_package.wls;\n",
  "    do not edit by hand. Upload this file next to diachronic_embeddings.nb. *)\n\n"];
out = banner <> StringReplace[out, StartOfString ~~ "(* ::Package:: *)\n" -> ""];

Export[outPkg, out, "Text"];

(* sanity: load the generated package in a clean check and exercise one fn *)
Get[outPkg];
ok = (Length[DiachronicEmbeddings`deWords[]] === 14) &&
     Head[DiachronicEmbeddings`deTrajectoryPlot["gay"]] === Graphics;
Print[If[ok, "wrote ", "FAILED "], outPkg, "  ", FileByteCount[outPkg], " bytes",
   "  (deWords -> ", Length[DiachronicEmbeddings`deWords[]], " words)"];
If[! ok, Exit[1]];
