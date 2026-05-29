#!/usr/bin/env wolframscript
Get @ FileNameJoin[{DirectoryName[$InputFileName], "load_embeddings.wl"}];
targets = deTargetVocab[];
ti = Association @ MapIndexed[#1 -> #2[[1]] &, targets];
Print["Target presence (norm of aligned-source vector) per decade:"];
Print["          ", Row[StringTake[#, -2] & /@ $deDecades, "  "]];
Do[Module[{T},
   Print[StringPadRight[w, 10],
     Row[Table[T = deTargets[d];
        If[Norm[T[[ti[w]]]] > 0.5, " Y", " ."], {d, $deDecades}], "  "]]],
  {w, targets}];
