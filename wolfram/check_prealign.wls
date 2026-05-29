#!/usr/bin/env wolframscript
Get @ FileNameJoin[{DirectoryName[$InputFileName], "load_embeddings.wl"}];
unit[m_] := m / (Sqrt[Total[m^2, {2}]] /. 0. -> 1.);
ref = "1990"; aR = unit @ deAnchors[ref];
rot[d_] := Module[{a, m, u, w, v},
   a = unit @ deAnchors[d]; m = Transpose[a] . aR;
   {u, w, v} = SingularValueDecomposition[m]; u . Transpose[v]];
mc[a_, b_] := Mean @ Total[a b, {2}];
Print["Rotation distance from Identity (Frobenius ||R-I||), max possible ~", N@Sqrt[2*300]];
Do[Print["  ", d, "  ", NumberForm[Norm[rot[d] - IdentityMatrix[300], "Frobenius"], {5, 3}]],
   {d, {"1800", "1850", "1900", "1950", "1980", "1990"}}];
Print["\nAnchor cosine, un-aligned vs aligned:"];
check[d1_, d2_] := Print["  ", d1, " vs ", d2,
   "  un-aligned ", NumberForm[mc[unit@deAnchors[d1], unit@deAnchors[d2]], {4, 3}],
   "   aligned ", NumberForm[mc[unit[deAnchors[d1]] . rot[d1], unit[deAnchors[d2]] . rot[d2]], {4, 3}]];
check["1980", "1990"];
check["1900", "1990"];
check["1800", "1990"];
check["1800", "1900"];
