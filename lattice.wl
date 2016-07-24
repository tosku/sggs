(* ::Package:: *)

BeginPackage["Lattice`"]

getLattice::usage = "getLattice[ L,d] returns a bimodal EA lattice as a Graph.";
nextVertex::usage = "nextVertex[i,j,L] returns the next vertex in j direction (dimension) or False if v is boundary"
previousVertex::usage = "previousVertex[i,j,L] returns the previous vertex in j direction (dimension) or False if v is boundary"

Begin["`Private`"];

getLattice[L_,d_] := G[L,d];

J1=1;
J2=-1;
p=1/2;
(*True if node (i) is boundary, j dimension iterator*)
isBoundaryEdge[i_, j_, L_] := L^j - L^(j - 1) - Mod[i - 1, L^j] <= 0;

nextVertex[i_,j_,L_] := If[isBoundaryEdge[i, j, L], False, i + L^(j - 1)];

previousVertex[i_,j_,L_] := If[isBoundaryEdge[i - L^(j - 1), j, L], False, i - L^(j - 1)];


makeInternalEdges[i_, d_, L_] :=
  Fold[If[isBoundaryEdge[i, #2, L], #1,
     Append[#1, {i, i + L^(#2 - 1)}]] &, {}, Range[1, d]];

interalEdges[L_,d_] :=
  Flatten[Fold[Append[#1, makeInternalEdges[#2, d, L]] &, {},
    Range[1, L^d]], 1];

makeBoundaryPeriodicEdges[i_, d_, L_] :=
 Fold[If[isBoundaryEdge[i, #2, L],
    Append[#1, {i - (L^#2 - L^(#2 - 1)), i}], #1] &, {}, Range[1, d]];

boundaryEdges[L_,d_] :=
  Flatten[Fold[Append[#1, makeBoundaryPeriodicEdges[#2, d, L]] &, {},
    Range[1, L^d]], 1];
ed[L_,d_] := interalEdges[L,d];

(*
ed[L_,d_] := Join[interalEdges[L,d], boundaryEdges[L,d]]
*)

cc[v_] := {v,
   PropertyValue[{gr, #}, EdgeWeight] & /@
    EdgeList[gr, v \[DirectedEdge] _]};

Jij[g_, e_] := PropertyValue[{g, e}, EdgeWeight];

V[L_,d_] := UndirectedEdge[First[#], Last[#]] & /@
   RandomSample[ed[L,d],Length[ed[L,d]]];
Caps[L_,d_] := 
Join[
	ConstantArray[
			J1, Round[
				(1-p)*Length[ed[L,d]]
			]
	],
    ConstantArray[
			J2, Round[
				p*Length[ed[L,d]]
			]
	] 
];

  G[L_,d_] := Graph[V[L,d] , EdgeCapacity-> Caps[L,d] ];

End[]
EndPackage[]






