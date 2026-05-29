(* ::Package:: *)

(*  load_embeddings.wl  --  shared loader for the WL-friendly binaries
    written by src/convert_histwords.py into data/raw/wl/.

    Plain little-endian float32, row-major, 300-dim rows. Vocab files are
    one word per line in the same row order as the matrix.

    Exposed (Global` context, loaded via Get):
      $deRepoRoot           repo root path
      $deWLDir              data/raw/wl
      $deManifest           parsed manifest.json (assoc)
      $deDecades            {"1800",...,"1990"}
      $deDim                300
      deLoadMatrix[path]            -> N x 300 packed real matrix
      deLoadVocab[path]            -> {words...}
      deAnchors[decade]            -> N x 300 matrix (fixed anchor order)
      deAnchorVocab[]              -> anchor word list
      deTargets[decade]            -> 6 x 300 matrix (zero rows = absent)
      deTargetVocab[]              -> {"gay",...}
      deNeighborPool[decade]       -> <|"matrix"->M, "vocab"->words|>
*)

$deRepoRoot = ParentDirectory @ DirectoryName[$InputFileName];
$deWLDir    = FileNameJoin[{$deRepoRoot, "data", "raw", "wl"}];

$deManifest = Import[FileNameJoin[{$deWLDir, "manifest.json"}], "RawJSON"];
$deDecades  = $deManifest["decades"];
$deDim      = $deManifest["dim"];

(* Little-endian float32, partitioned into 300-d rows. Developer`ToPackedArray
   keeps the linear-algebra fast. *)
deLoadMatrix[path_String] := Module[{flat},
  flat = BinaryReadList[path, "Real32", ByteOrdering -> -1];
  Developer`ToPackedArray @ N @ Partition[flat, $deDim]
];

deLoadVocab[path_String] := Import[path, "Lines"];

deAnchors[decade_String]   := deLoadMatrix @ FileNameJoin[{$deWLDir, "anchors_" <> decade <> ".bin"}];
deAnchorVocab[]            := deLoadVocab  @ FileNameJoin[{$deWLDir, "anchors_vocab.txt"}];
deTargets[decade_String]   := deLoadMatrix @ FileNameJoin[{$deWLDir, "targets_" <> decade <> ".bin"}];
deTargetVocab[]            := deLoadVocab  @ FileNameJoin[{$deWLDir, "targets_vocab.txt"}];

deNeighborPool[decade_String] := <|
  "matrix" -> deLoadMatrix @ FileNameJoin[{$deWLDir, decade <> ".bin"}],
  "vocab"  -> deLoadVocab  @ FileNameJoin[{$deWLDir, decade <> "-vocab.txt"}]
|>;
