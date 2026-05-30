#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  fetch_data.wls -- download + unzip the HistWords eng-all SGNS embeddings.

    Writes the ~1.6 GB zip and its unpacked per-decade files into the
    git-ignored data/raw/. Idempotent: skips the download if the zip is
    already present and the right size, and skips the unzip if the decade
    files are already there.

    Source: Hamilton, Leskovec & Jurafsky (2016), HistWords.
            https://nlp.stanford.edu/projects/histwords/
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
rawDir   = FileNameJoin[{repoRoot, "data", "raw"}];
sgnsDir  = FileNameJoin[{rawDir, "sgns"}];
zipPath  = FileNameJoin[{rawDir, "eng-all_sgns.zip"}];
url      = "http://snap.stanford.edu/historical_embeddings/eng-all_sgns.zip";
expectedSize = 1686718098;  (* bytes *)

If[! DirectoryQ[rawDir], CreateDirectory[rawDir, CreateIntermediateDirectories -> True]];

(* ---- download ---- *)
If[FileExistsQ[zipPath] && FileByteCount[zipPath] == expectedSize,
  Print["Zip already present (", FileByteCount[zipPath], " bytes) \[Dash] skipping download."],
  Print["Downloading ", url, " (~1.6 GB) ..."];
  Module[{resp},
    resp = URLDownload[url, zipPath];   (* large binary; URLDownload is fine here *)
    If[! FileExistsQ[zipPath] || FileByteCount[zipPath] < expectedSize/2,
      Print["ERROR: download failed or truncated (",
        If[FileExistsQ[zipPath], FileByteCount[zipPath], 0], " bytes)."];
      Exit[1]];
    Print["Downloaded ", FileByteCount[zipPath], " bytes."]]];

(* ---- unzip ---- *)
have = If[DirectoryQ[sgnsDir], Length @ FileNames["*-w.npy", sgnsDir], 0];
If[have >= 20,
  Print["Decade files already unpacked (", have, " .npy) \[Dash] skipping unzip."],
  Print["Extracting ..."];
  ExtractArchive[zipPath, rawDir, OverwriteTarget -> True];
  Print["Extracted ", Length @ FileNames["*-w.npy", sgnsDir], " decade matrices to ", sgnsDir]];

Print["Done. Next: python3 src/convert_histwords.py"];
