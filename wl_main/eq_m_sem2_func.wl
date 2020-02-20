
(*
ee5Eq = epsilon*D[y[z], {z, 5}] + gamma*D[y[z], {z, 4}] + delta*D[y[z], {z, 3}] + xi*D[y[z], {z, 2}] + beta*D[y[z], {z, 1}] + alpha*y[z]^2 - C0*y[z] + C1;
pOrder = 5;
SolveParamList = {};
PartSolveParamList = {};
TargetEq = ee5Eq;
*)
(*
polyFindSub = TargetEq/.y->Function[{ksi}, a0*ksi^(-pp)];
polyFindList = Exponent[polyFindSub, z^(-1)];
*)
(*
ACoeffList = { A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10 };
*)
(*
ParamList = SolveParamList;
PartParamList = PartSolveParamList;
*)
(*
PdeToolsLoopLimit = 20;
*)

GP = Function[{x}, Cases[DeleteDuplicates@Cases[x, _Symbol, Infinity], Except[z]]];
PP = Function[{x}, Exponent[x/.y->Function[{ksi}, a0*ksi^(-pp0)], z^(-1)]];

funcGetOdeOrder = Function[{TargetEq},
TmpParams = GP[TargetEq];
uu = TargetEq/.Map[(#1->1)&, TmpParams];
RetVal = Cases[Map[Function[{x}, If[Head[FullForm[x][[1, 0, 0]]] === Derivative, Level[FullForm[x][[1, 0, 0]], Depth[FullForm[x][[1, 0, 0]]]] ]], Level[uu, Depth[uu]]], Except[Null]];
RetVal = Join@@RetVal;
OdeOrder = Max[RetVal];
OdeOrder
];

funcGetAllParams = Function[{TargetEq},
  RetVal = GP[TargetEq];
  RetVal
  ];

funcGetACoeffList = Function[{pOrder},
  RetVal = Map[(ToExpression[StringJoin[{"A", ToString[#1]}]])&, Range[0, pOrder + 3, 1]];
  RetVal
];

funcPoleTip = Function[{TargetEq},
(*Map[(If[#1 == 2, -1, 1]*Map[Function[{x}, {TargetEq[[x]],  Exponent[TargetEq[[x]]/.y->Function[{ksi}, a0*ksi^(-pp0)], z^(-1)]}],Range[1, Length[TargetEq]]][[All,#1]])&, {1, 2}]];*)
RetVal = Map[(If[#1 == 2, -1, 1]*Map[Function[{x}, {TargetEq[[x]],  Exponent[TargetEq[[x]]/.y->Function[{ksi}, a0*ksi^(-pp0)], z^(-1)]}],Range[1, Length[TargetEq]]][[All,#1]])&, {1, 2}];
For[i = 1, i < Length[TargetEq], i++, Print[RetVal[[1]][[i]], "=>", RetVal[[2]][[i]]]];
Print[Min[RetVal[[2]]]];
RetVal
];

funcGetRicSub = Function[{PdeToolsLoopLimit},
	PdeToolsRicEq = D[Y[z], {z, 1}] + Y[z]^2 - b;
	PdeToolsRicSub = -Coefficient[PdeToolsRicEq, D[Y[z], {z, 1}], 0];
	PdeToolsRz = PdeToolsRicSub;
	PdeToolsTmpList = { PdeToolsRz };
	For[i = 0, i < PdeToolsLoopLimit, i++, PdeToolsTmpList = Append[PdeToolsTmpList, D[PdeToolsTmpList[[-1]], {z, 1}]/.D[Y[z], {z, 1}]->PdeToolsRz]];
	PdeToolsRzz = D[PdeToolsRz, {z, 1}]/.D[Y[z], {z, 1}]->PdeToolsRz;
	PdeToolsRzzz = D[PdeToolsRzz, {z, 1}]/.D[Y[z], {z, 1}]->PdeToolsRz;
	PdeToolsRzzzz = D[PdeToolsRzzz, {z, 1}]/.D[Y[z], {z, 1}]->PdeToolsRz;
	PdeToolsRicSubList1 = Map[Function[{x}, D[Y[z], {z, x}]->PdeToolsTmpList[[x]]], Range[1, Length[PdeToolsTmpList]]];
	ricSubList1 = PdeToolsRicSubList1;
	ricSubList1
];

funcGetEqList = Function[{ACoeffList, pOrder, TargetEq, ricSubList1},
	subEx = Plus@@Map[(ACoeffList[[#1 + 1]]*Y[x]^#1)&, Range[0, pOrder]];
	subEx = Function[{x}, Evaluate[subEx]];
	subTargetEq0 = TargetEq/.y->subEx;
	subTargetEq1 = Collect[subTargetEq0/.ricSubList1, Y[z]];
	CoeffList = Map[(Coefficient[subTargetEq1, Y[z], #1])&, Range[0, Length[MonomialList[subTargetEq1, Y[z]]] - 1]];
	EqList0 = Map[(Coefficient[subTargetEq1, Y[z], #1] == 0)&, Range[0, Length[MonomialList[subTargetEq1, Y[z]]] - 1]];
	EqList0 = Simplify@EqList0;
	EqList = Append[EqList0, ACoeffList[[pOrder + 1]] != 0];
	RetVal = {EqList, EqList0, subEx};
	RetVal
];



funcGetAexp = Function[{ACoeffList, pOrder, TargetEq, EqList0},
	Es0 = Part[EqList0, Range[-1, -pOrder - 1, -1]];
	Ac0 = Reverse[Part[ACoeffList, Range[1, pOrder + 1]]];
	(*
	Ac1 = { Solve[{Es0[[1]], Ac0[[1]] != 0}, Ac0[[1]]] };
	For[i = 2, i <= Length[Ac0], i++, Ac1 = Append[Ac1, Solve[Es0[[i]], Ac0[[i]]]]];
	*)

(* !!!!!!!!!!!!!!!!!!!!!!!! *)
(* Ac1 = Solve[Join[Es0, { Ac0[[1]] != 0 }], Ac0]; *)
(* !!!!!!!!!!!!!!!!!!!!!!!! *)

  Ac1 = {};
	Es1 = Part[EqList0, Range[-pOrder - 2, -Length[EqList0], -1]];

  (*
  Es2 = Es1/.Join@@Ac1;
  Es3 = Simplify@Es2;

  Ac2 = Reverse[Ac1[[1]]];
  Ac3 = Map[(Fold[Function[{x, y}, x/.y], t[[#1]], Part[t, Range[#1 + 1, Length[t]]]])&, Range[1, Length[t]]];
  *)
  (*
  tmpp0 = Map[(PP[TargetEq][[-1]][[#1]])&, Range[1, Length[PP[TargetEq][[-1]]]]];
  *)
  (*
  TMP0 = Tuples[Map[(Solve[Es0[[#1]], Ac0[[#1]]])&, Range[1, Length[Ac0]]]];
  *)
TMP0 = Tuples[Map[(Solve[Es0[[#1]], Ac0[[#1]]])&, Range[1, Length[Ac0]]]];

(* !!!!!!!!!!!!!!!!!!!!!!!! *)
(* TMP1 = Map[Function[{x}, Map[(Fold[Function[{x, y}, x/.y], x[[#1]], Part[TMP0[[2]], Reverse[Cases[Range[1, Length[x]], Except[#1]]]]])&, Range[1, Length[x]]]], TMP0]; *)
(* !!!!!!!!!!!!!!!!!!!!!!!! *)

funcGetAexp0 = Function[{x}, RetVal = {x[[1]]}; q = x;
  If[Length[x] > 1, For[i = 1, i <= Length[x], i++, If[Length[q]  > 1, ( q = q[[2 ;;]] /. RetVal[[i]]; RetVal = Append[RetVal, q[[1]]];), Break[]]], x];
    RetVal
];
funcRemoveErrorCases = Function[{y},
REPLACEVALUE = {};
  REPLACEVALUE = Map[({#1->0})&, Ac0];
  RetVal = Map[Function[{x}, If[MemberQ[Values[Association[x]], Indeterminate] || MemberQ[Values[Association[x]], ComplexInfinity] || MemberQ[Values[Association[x]], Infinity], REPLACEVALUE, x]], y];
    RetVal
];

TMP1 = Map[(funcGetAexp0[#1])&, TMP0];
TMP1 = funcRemoveErrorCases[TMP1];

TMP2 = Map[(Simplify[#1])&, TMP1];

(*
Map[(Fold[Function[{x, y}, Simplify[x/.y]], TMP0[[2]][[#1 + 1]], Part[TMP0[[2]], Range[1, #1]]])&, Range[0, Length[TMP0[[2]]]]]
Fold[Function[{x, y}, Simplify[x/.y]], TMP0[[2]][[3]], Part[TMP0[[2]], Reverse[Cases[Range[1, 6], Except[3]]]]]
Map[(Fold[Function[{x, y}, x/.y], TMP0[[2]][[#1]], Part[TMP0[[2]], Reverse[Cases[Range[1, 6], Except[#1]]]]])&, Range[1, Length[TMP0[[2]]]]]
*)
RetVal = {TMP0, TMP1, TMP2, Es0, Es1, Ac0, Ac1};
RetVal
];

funcGetAexpOld = Function[{ACoeffList, pOrder, TargetEq, EqList0},
	Es0 = Part[EqList0, Range[-1, -pOrder - 1, -1]];
	Ac0 = Reverse[Part[ACoeffList, Range[1, pOrder + 1]]];
	(*
	Ac1 = { Solve[{Es0[[1]], Ac0[[1]] != 0}, Ac0[[1]]] };
	For[i = 2, i <= Length[Ac0], i++, Ac1 = Append[Ac1, Solve[Es0[[i]], Ac0[[i]]]]];
	*)

(* !!!!!!!!!!!!!!!!!!!!!!!! *)
(* Ac1 = Solve[Join[Es0, { Ac0[[1]] != 0 }], Ac0]; *)
(* !!!!!!!!!!!!!!!!!!!!!!!! *)

  Ac1 = {};
	Es1 = Part[EqList0, Range[-pOrder - 2, -Length[EqList0], -1]];

  (*
  Es2 = Es1/.Join@@Ac1;
  Es3 = Simplify@Es2;

  Ac2 = Reverse[Ac1[[1]]];
  Ac3 = Map[(Fold[Function[{x, y}, x/.y], t[[#1]], Part[t, Range[#1 + 1, Length[t]]]])&, Range[1, Length[t]]];
  *)
  (*
  tmpp0 = Map[(PP[TargetEq][[-1]][[#1]])&, Range[1, Length[PP[TargetEq][[-1]]]]];
  *)
  (*
  TMP0 = Tuples[Map[(Solve[Es0[[#1]], Ac0[[#1]]])&, Range[1, Length[Ac0]]]];
  *)
TMP0 = Tuples[Map[(Solve[Es0[[#1]], Ac0[[#1]]])&, Range[1, Length[Ac0]]]];
TMP1 = Map[Function[{x}, Map[(Fold[Function[{x, y}, x/.y], x[[#1]], Part[TMP0[[2]], Reverse[Cases[Range[1, Length[x]], Except[#1]]]]])&, Range[1, Length[x]]]], TMP0];

(* !!!!!!!!!!!!!!!!!!!!!!!! *)
(*
funcGetAexp0 = Function[{x}, RetVal = {x[[1]]}; q = x;
  If[Length[x] > 1, For[i = 1, i <= Length[x], i++, If[Length[q]  > 1, ( q = q[[2 ;;]] /. RetVal[[i]]; RetVal = Append[RetVal, q[[1]]];), Break[]]], x];
    RetVal
];
funcRemoveErrorCases = Function[{y},
REPLACEVALUE = {};
  REPLACEVALUE = Map[({#1->0})&, Ac0];
  RetVal = Map[Function[{x}, If[MemberQ[Values[Association[x]], Indeterminate] || MemberQ[Values[Association[x]], ComplexInfinity] || MemberQ[Values[Association[x]], Infinity], REPLACEVALUE, x]], y];
    RetVal
];

TMP1 = Map[(funcGetAexp0[#1])&, TMP0];
TMP1 = funcRemoveErrorCases[TMP1];
*)
(* !!!!!!!!!!!!!!!!!!!!!!!! *)


TMP2 = Map[(Simplify[#1])&, TMP1];

(*
Map[(Fold[Function[{x, y}, Simplify[x/.y]], TMP0[[2]][[#1 + 1]], Part[TMP0[[2]], Range[1, #1]]])&, Range[0, Length[TMP0[[2]]]]]
Fold[Function[{x, y}, Simplify[x/.y]], TMP0[[2]][[3]], Part[TMP0[[2]], Reverse[Cases[Range[1, 6], Except[3]]]]]
Map[(Fold[Function[{x, y}, x/.y], TMP0[[2]][[#1]], Part[TMP0[[2]], Reverse[Cases[Range[1, 6], Except[#1]]]]])&, Range[1, Length[TMP0[[2]]]]]
*)
RetVal = {TMP0, TMP1, TMP2, Es0, Es1, Ac0, Ac1};
RetVal
];

funcGetParamEqSys = Function[{TMP2, Es1, FlagSimplify},
	ParamEqSys = Map[Function[{x}, Es1/.(Join@@x)], TMP2];
(*
ParamEqSysSimp = Map[(Simplify[#1])&, ParamEqSys];
*)
	RetVal = ParamEqSys;
	If[FlagSimplify, RetVal = Map[(Simplify[#1])&, ParamEqSys]];
	RetVal
];



funcTrySolveForParam = Function[{ParamEqSys, ParamForSolve, FlagSimplify},
	ParamValues = Map[(Solve[#1, ParamForSolve])&, ParamEqSys];
(*
ParamValuesSimp = Map[(Simplify[#1])&, ParamValues];
*)
	RetVal = ParamValues;
	If[FlagSimplify, RetVal = Map[(Simplify[#1])&, ParamValues]];
	RetVal
];



funcFormResult = Function[{pOrder, TMP2, ParamValues},
(*
TMP3 = Map[Function[{x}, Map[(Join[Join@@TMP2[[x]], #1])&, ParamValues[[x]]]], Range[1, Length[TMP2]]];
*)
TMP3 = Join@@(Map[Function[{x}, Map[(Join[Join@@TMP2[[x]], #1])&, ParamValues[[x]]]], Range[1, Length[TMP2]]]);
TMP30 = Map[Function[{x}, Part[x, Range[pOrder + 2, Length[x]]]], TMP3];
TMP31 = Map[Function[{y}, Part[TMP3[[y]], Range[1, pOrder + 1]]/.Map[Function[{x}, Part[x, Range[pOrder + 2, Length[x]]]], TMP3][[y]]], Range[1, Length[TMP3]]];
TMP4 = Map[(Join[TMP31[[#1]], TMP30[[#1]]])&, Range[1, Length[TMP30]]];
(*
RES = TMP4;
TMP5 = Cases[Map[Function[{x}, If[x[[1]][[2]] != 0, x]], RES], Except[Null]];
*)
TMP5 = Cases[Map[Function[{x}, If[Not[NumericQ[x[[1]][[2]]]] || ( NumericQ[x[[1]][[2]]] && x[[1]][[2]] != 0), x]], TMP4], Except[Null]];
RES = TMP5;
RES
];


funcCheckRes = Function[{TargetEq, RES, ricEq, subEx, CheckParam},

	tmpRes = RES;
	ricSol = DSolve[ricEq == 0, Y[z], z][[1]]/.C[1]->Cric;
	tmpSub0 = subEx/.x->z/.ricSol;
	CheckFunc = Function[{x}, Simplify[(TargetEq/.x)/.y->(tmpSub0/.x)]];
	(*
	CheckRes = Map[Function[{x}, Simplify[(TargetEq/.x)/.y->(tmpSub0/.x)]], tmpRes];
	*)
	(*
	CheckRes = Map[CheckFunc, tmpRes];
	*)
	RetVal = Null;
	If[CheckParam === "all", Return[Map[CheckFunc, tmpRes]]];
	ParseParam = ToExpression[CheckParam];
	If[NumberQ[ParseParam], If[ParseParam >= 1 && ParseParam <= Length[tmpRes], Return[{CheckFunc[tmpRes[[ParseParam]]]}]]];
	RetVal
	];

CHECK = Function[{TargetEq, pOrder, RES, CheckParam},
  ricEq = D[Y[z], {z, 1}] + Y[z]^2 - b;
  PdeToolsLoopLimit = funcGetOdeOrder[TargetEq] + 2;
  ACoeffList = funcGetACoeffList[pOrder];
  ricSubList1 = funcGetRicSub[PdeToolsLoopLimit];
  EqLists = funcGetEqList[ACoeffList, pOrder, TargetEq, ricSubList1];
  subEx = EqLists[[3]];
  RetVal = funcCheckRes[TargetEq, RES, ricEq, subEx, CheckParam];
  RetVal
];

(*
CHECK[TargetEq_, pOrder_, RES_, CheckParam_ : "all"] := CHECK0[TargetEq, pOrder, RES, ChechParam];
*)


  (*
  TEST0 = Cases[Map[Function[{x}, If[Length[x] > 0 && Length[x] <= Length[ParamEqSys], x]], Subsets[AllParamsSet]], Except[Null]];
  TEST1 = Cases[Map[(If[Length[#1] == Length[ParamEqSys], #1])&, TEST0], Except[Null]];
  TEST2 = Map[Function[{x}, Map[Function[{y}, TimeConstrained[Solve[y, x], TIMEOUT]], ParamEqSys]], TEST1];
  TEST2 = Map[Function[{x}, Map[Function[{y}, TimeConstrained[Solve[y, x], TIMEOUT]], {ParamEqSys[[2]]}]], TEST1];

  TEST2 = Cases[Map[(If[Length[#1] == Length[ParamEqSys[[1]]] - 1, #1])&, TEST0], Except[Null]];
  TEST3 = Map[Function[{x}, Map[Function[{y}, TimeConstrained[Solve[y, x], TIMEOUT]], {ParamEqSys[[2]]}]], TEST2];
  *)


  (*
  TEST2 = Map[(Solve[#1, TEST1[[1]]])&, ParamEqSys];
  TEST2 = TimeConstrained[Map[(Solve[#1, TEST1[[1]]])&, ParamEqSys], 4]; // Timing
  t4 = (TEST2 = TimeConstrained[Map[(Solve[#1, TEST1[[1]]])&, ParamEqSys], 4]; // Timing);
  *)
