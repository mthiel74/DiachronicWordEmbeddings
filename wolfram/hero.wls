#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  hero.wls -- animated hero figure for the post / README.

    Reads the pre-computed 2-D projections in data/trajectories.json and
    animates six striking words drawing their trajectories decade by decade
    (blue 1800s -> red 1990s) against their static neighbour clouds.

    Writes docs/images/hero.gif and docs/images/hero_still.png (final frame).
    No embedding maths here -- purely a renderer of trajectories.json.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
data = Import[FileNameJoin[{repoRoot, "data", "trajectories.json"}], "RawJSON"];

heroWords = {"gay", "broadcast", "computer", "guy", "terrific", "media"};
gdecades  = ToString /@ Range[1800, 1990, 10];
yr[d_] := ToExpression[d];
frac[d_] := (yr[d] - 1800)/190.;
timeCol[t_] := Blend[{RGBColor[0.13, 0.34, 0.68], RGBColor[0.85, 0.16, 0.11]}, t];
lab[d_] := StringTake[d, -2] <> "s";

(* fixed plot range per word so panels don't jump during the animation *)
rangeOf[w_] := Module[{xy},
   xy = Join[data[w]["path"], #["xy"] & /@ data[w]["neighbors"]];
   {{Min[xy[[All, 1]]], Max[xy[[All, 1]]]}, {Min[xy[[All, 2]]], Max[xy[[All, 2]]]}}];
ranges = Association @ Table[w -> rangeOf[w], {w, heroWords}];

panel[w_, dG_] := Module[{ds, path, nbrs, shown, n, last},
  ds = data[w]["decades"]; path = data[w]["path"]; nbrs = data[w]["neighbors"];
  shown = Select[Range@Length@ds, yr[ds[[#]]] <= yr[dG] &];
  n = Length@ds;
  Graphics[{
     {GrayLevel[0.55], Table[Text[Style[nb["word"], Italic, 8], nb["xy"]], {nb, nbrs}]},
     If[Length[shown] >= 2,
       {Thickness[0.007], Opacity[0.5],
        Table[{timeCol[frac[ds[[i]]]], Line[{path[[i]], path[[i + 1]]}]},
          {i, shown[[1 ;; -2]]}]}, {}],
     Opacity[1],
     Table[{timeCol[frac[ds[[i]]]], EdgeForm[White], PointSize[0.02], Point[path[[i]]]},
       {i, shown}],
     If[shown =!= {}, last = Last@shown;
       {timeCol[frac[ds[[last]]]], PointSize[0.04], EdgeForm[{Thick, White}], Point[path[[last]]],
        Text[Style[lab[ds[[last]]], Bold, 13, timeCol[frac[ds[[last]]]]], path[[last]], {0, -1.8}]}, {}]
     },
   PlotLabel -> Style["\"" <> w <> "\"", 14, Bold],
   PlotRange -> ranges[w], PlotRangePadding -> Scaled[0.12],
   ImageSize -> 330, Background -> White]
];

legend = Row[{Style["1800s ", 11, timeCol[0]],
   Graphics[Table[{timeCol[t], Rectangle[{t, 0}, {t + 0.05, 1}]}, {t, 0, 1, 0.05}],
     AspectRatio -> 1/16, ImageSize -> 220], Style[" 1990s", 11, timeCol[1]]}];

frame[dG_] := Labeled[
   Grid[Partition[panel[#, dG] & /@ heroWords, 3], Spacings -> {1, 1}, Background -> White],
   {Style["200 years of semantic drift \[Dash] " <> lab[dG], 17, Bold], legend},
   {Top, Bottom}];

Print["Rendering ", Length@gdecades, " frames ..."];
frames = frame /@ gdecades;
still = Rasterize[Last@frames, ImageResolution -> 120];
Export[FileNameJoin[{repoRoot, "docs", "images", "hero_still.png"}], still, "PNG"];
(* hold the first and last frame a little longer *)
durs = Join[{1.2}, ConstantArray[0.45, Length@frames - 2], {2.5}];
Export[FileNameJoin[{repoRoot, "docs", "images", "hero.gif"}], frames,
   "DisplayDurations" -> durs, "AnimationRepetitions" -> Infinity];
Print["Wrote docs/images/hero.gif and hero_still.png"];
