(* ::Package:: *)

BeginPackage["Lattice`"]

edgesToList::usage = "edgesToList[edges] returns a list of edges as lists given graph edges i.e. from (EdgeList[g])";
listToEdges::usage = "listToEdges[edgeList] returns graph edges given list of edges as list";
getFreeBoundaryLattice::usage = "getFreeBoundaryLattice[ L,d] returns a bimodal EA lattice as a Graph.";
nextVertexFreeBoundary::usage = "nextVertexFreeBoundary[i,j,L] returns the next vertex in j direction (dimension) or False if v is boundary";
previousVertexFreeBoundary::usage = "previousVertexFreeBoundary[i,j,L] returns the previous vertex in j direction (dimension)";
getPCBLattice::usage = "getPCBLattice[ L,d] returns a bimodal EA lattice as a Graph.";
isBoundaryEdge::usage = "isBoundaryEdge[i, j, L] True if node (i) is boundary, j direction";
nextVertex::usage = "nextVertex[i,j,L] returns the next vertex in j direction (dimension)";
previousVertex::usage = "previousVertex[i,j,L] returns the previous vertex in j direction (dimension) or False if v is boundary"
getEdgeDirection::usage = "getEdgeDirection[edge,L,d] returns direction of edge";
isSurfaceEdge::usage = "isSurfaceEdge[edge, L, d] True if edge is on the surface of the lattice";
Jij::usage = "Jij[g,e] returns weight of an edge";
labelVertices::usage = "labelVertices[graph] returns the graph with its vertices labeled";
labelEdges::usage = "labelEdges[graph] returns the graph with its edges labeled";
periodicEdges::usage = "periodicEdges[L ,d] returns periodic edges";
Spin::usage = "Spin[vertex, configuration] returns the spin value of a vertex given a configuration";
Energy::usage = "Energy[lattice, configuration] returns lattice Energy given a spin configuration";



Begin["`Private`"];


edgesToList[edges_]:=Apply[List,#]&/@ edges;
listToEdges[edgeList_]:=Apply[UndirectedEdge, #]&/@edgeList;
labelVertices[graph_]:= Fold[SetProperty[{#1,#2},VertexLabels->ToString[#2]]&, graph,VertexList[graph]];
labelEdges[graph_]:=Fold[SetProperty[{#1,#2},EdgeLabels->PropertyValue[{#1,#2},EdgeWeight]]&, graph,EdgeList[graph]];
getFreeBoundaryLattice[L_,d_] := G[edfb[L,d]];
getPCBLattice[L_,d_] := G[edpcb[L,d]];
Jij[g_, e_] := PropertyValue[{g, e}, EdgeWeight];
J1=1;
J2=-1;
p=1/2;
(*True if node (i) is boundary, j dimension iterator*)
isBoundaryEdge[i_, j_, L_] := L^j - L^(j - 1) - Mod[i - 1, L^j] <= 0;

nextVertexFreeBoundary[i_,j_,L_] := If[isBoundaryEdge[i, j, L], False, i + L^(j - 1)];

previousVertexFreeBoundary[i_,j_,L_] := If[isBoundaryEdge[i - L^(j - 1), j, L], False, i - L^(j - 1)];

nextVertex[i_,j_,L_] := If[isBoundaryEdge[i, j, L], i - (L^j- L^(j- 1)), i + L^(j - 1)];

previousVertex[i_,j_,L_] := If[isBoundaryEdge[i - L^(j - 1), j, L], i +(L^j- L^(j- 1)), i - L^(j - 1)];

getEdgeDirection[edge_,L_,d_]:=Fold[If[Last[Sort[edge]]==nextVertex[First[Sort[edge]],#2,L],#2,#1]&,0,Range[1,d]]

isSurfaceEdge[edge_ ,L_ ,d_] := Fold[If[
	(isBoundaryEdge[First[edge],#2,L] && isBoundaryEdge[Last[edge],#2,L])||
	(isBoundaryEdge[previousVertex[First[edge],#2,L],#2,L] && isBoundaryEdge[previousVertex[Last[edge],#2,L],#2,L])
,True,#1]&,False,Range[1,d]]



makeInternalEdges[i_, d_, L_] :=
  Fold[If[isBoundaryEdge[i, #2, L], #1,
     Append[#1, {i, i + L^(#2 - 1)}]] &, {}, Range[1, d]];

interalEdges[L_,d_] :=
  Flatten[Fold[Append[#1, makeInternalEdges[#2, d, L]] &, {},
    Range[1, L^d]], 1];

makeBoundaryPeriodicEdges[i_, d_, L_] :=
 Fold[If[isBoundaryEdge[i, #2, L],
    Append[#1, {i - (L^#2 - L^(#2 - 1)), i}], #1] &, {}, Range[1, d]];

periodicEdges[L_,d_] :=
  Flatten[Fold[Append[#1, makeBoundaryPeriodicEdges[#2, d, L]] &, {},
    Range[1, L^d]], 1];
edfb[L_,d_] := interalEdges[L,d];


edpcb[L_,d_] := Join[interalEdges[L,d], periodicEdges[L,d]];


cc[v_] := {v,
   PropertyValue[{gr, #}, EdgeWeight] & /@
    EdgeList[gr, v \[DirectedEdge] _]};



V[edges_] := UndirectedEdge[First[#], Last[#]] & /@
   RandomSample[edges,Length[edges]];
Caps[edges_] := 
Join[
	ConstantArray[
			J1, Round[
				(1-p)*Length[edges]
			]
	],
    ConstantArray[
			J2, Round[
				p*Length[edges]
			]
	] 
];

  G[edges_] := Graph[V[edges] , EdgeWeight-> Caps[edges] ];
 
Spin[vertex_,configuration_]:=configuration[[vertex]];
Energy[lattice_, configuration_]:= -Total[Jij[lattice,#]*Spin[First[#],configuration]*Spin[Last[#],configuration] &/@ EdgeList[lattice]]

End[]
EndPackage[]






