
Get["eq_m_sem2_func.wl"];
Get["eq_m_sem2_data.wl"];
Get["eq_m_sem2_int.wl"]

(*--------------------------------------------------------------------------------------------------------*)

ReloadAll0 = Function[{ReloadParam},
If[ReloadParam === Null, Get["eq_m_sem2.wl"], If[ReloadParam === "all",
Get["eq_m_sem2_func.wl"];
Get["eq_m_sem2_data.wl"];
Get["eq_m_sem2_int.wl"];
Get["eq_m_sem2.wl"];]]
];
ReloadAll[ReloadParam_ : Null] := ReloadAll0[ReloadParam];

ISER = Function[{RetVal}, RetVal["FlagEmptyRes"]];

funcTest = Function[{TargetEq, pOrder, ParamForSolve},

	pOrder = pOrder;

	PdeToolsLoopLimit = funcGetOdeOrder[TargetEq] + 2;

	AllParamsList = funcGetAllParams[TargetEq];
	AllParamsSet = AllParamsList;
	FullParamsSet = Join[AllParamsList, {b}];
	ACoeffList = funcGetACoeffList[pOrder];
	ricSubList1 = funcGetRicSub[PdeToolsLoopLimit];
	EqLists = funcGetEqList[ACoeffList, pOrder, TargetEq, ricSubList1];
	EqList = EqLists[[1]];
	EqList0 = EqLists[[2]];
	subEx = EqLists[[3]];
	RetVal = funcGetAexp[ACoeffList, pOrder, TargetEq, EqList0];
	TMP0 = RetVal[[1]];
	TMP1 = RetVal[[2]];
	TMP2 = RetVal[[3]];
	Es0 = RetVal[[4]];
	Es1 = RetVal[[5]];
	Ac0 = RetVal[[6]];
	Ac1 = RetVal[[7]];
	ParamEqSys = funcGetParamEqSys[TMP2, Es1, False];
	ParamValues = funcTrySolveForParam[ParamEqSys, ParamForSolve, False];
	RES = funcFormResult[pOrder, TMP2, ParamValues];
	FlagEmptyRes = Join@@RES === {} || RES === {};
	FlagEmptyParamValues = Join@@ParamValues || ParamValues === {};
	RetVal = <|"RES"->RES, "TMP2"->TMP2, "ParamEqSys"->ParamEqSys, "ParamValues"->ParamValues,
	 "subEx"->subEx, "EqList0"->EqList0, "AllParamsSet"->AllParamsSet, "pOrder"->pOrder,
	 "ParamForSolve"->ParamForSolve, "TargetEq"->TargetEq, "FullParamsSet"->FullParamsSet,
	 "FlagEmptyRes"->FlagEmptyRes, "FlagEmptyParamValues"->FlagEmptyParamValues |>;
	 RetVal
];

(******************************************************************************)
TargetEq = KdVBEq;
pOrder = 2;
ParamForSolve = { b, alpha };
TargetEq = nsEq;
pOrder = 1;
ParamForSolve = { alpha, b };
TargetEq = REq;
pOrder = 2;
ParamForSolve = { alpha, gamma }
TargetEq = kbEq;
pOrder = 2;
ParamForSolve = { b };
TargetEq = zkEq;
pOrder = 2;
ParamForSolve = { b, lambda1, lambda2 };
(* TargetEq = rdEq;
pOrder = 2;
ParamForSolve = { b, alpha, beta, C0 }; *)
TargetEq = ee5Eq;
pOrder = 5;
ParamForSolve = { epsilon, gamma, delta, xi, beta, b };
TargetEq = cfEq;
pOrder = 2;
ParamForSolve = { beta, C0, b };
TargetEq = kdv5Eq;
pOrder = 2;
ParamForSolve = { b, C0, gamma };
TargetEq = ksEq;
pOrder = 3;
ParamForSolve = { sigma, b, C1 };
TargetEq = genericEq2;
pOrder = 2;
ParamForSolve = { xi, b, omega, phi, epsilon, gamma, delta, alpha  };
TargetEq = genericEq3;
pOrder = 3;
ParamForSolve = { b, gamma, xi, delta  };
(******************************************************************************)


pOrder = pOrder;


PdeToolsLoopLimit = funcGetOdeOrder[TargetEq] + 2;

AllParamsList = funcGetAllParams[TargetEq];
AllParamsSet = AllParamsList;
FullParamsSet = Join[AllParamsList, {b}];
ACoeffList = funcGetACoeffList[pOrder];
ricSubList1 = funcGetRicSub[PdeToolsLoopLimit];
EqLists = funcGetEqList[ACoeffList, pOrder, TargetEq, ricSubList1];
EqList = EqLists[[1]];
EqList0 = EqLists[[2]];
subEx = EqLists[[3]];
RetVal = funcGetAexp[ACoeffList, pOrder, TargetEq, EqList0];

TMP0 = RetVal[[1]];
TMP1 = RetVal[[2]];
TMP2 = RetVal[[3]];
Es0 = RetVal[[4]];
Es1 = RetVal[[5]];
Ac0 = RetVal[[6]];
Ac1 = RetVal[[7]];
ParamEqSys = funcGetParamEqSys[TMP2, Es1, False];
ParamValues = funcTrySolveForParam[ParamEqSys, ParamForSolve, False];

RES = funcFormResult[pOrder, TMP2, ParamValues];



(******************************************************************************)

Function[{eq, params},
	t0 = Factor[Expand[eq]][[1]];
	t1 = Numerator[t0];
	t2 = Depth[t1];
	t3 = Level[t1, t2];
	t4 = Association[Map[(#1 -> DeleteDuplicates[Exponent[t3, #1]])&, params]];
	t5 = Transpose[{Keys[t4], Values[t4]}];
	t6 = Cases[Map[(If[#1[[2]] === {0}, Null, {#1[[1]], Cases[#1[[2]], Except[0]]}])&, t5], Except[Null]];
	RetVal = {t4, t6};
	RetVal
];


(*
RetValOld = funcGetAexpOld[ACoeffList, pOrder, TargetEq, EqList0];
TMP0Old = RetValOld[[1]];
TMP1Old = RetValOld[[2]];
TMP2Old = RetValOld[[3]];
Es0Old = RetValOld[[4]];
Es1Old = RetValOld[[5]];
Ac0Old = RetValOld[[6]];
Ac1Old = RetValOld[[7]];
ParamEqSysOld = funcGetParamEqSys[TMP2Old, Es1Old, False];
ParamValuesOld = funcTrySolveForParam[ParamEqSysOld, ParamForSolve, False];

RESOld = funcFormResult[pOrder, TMP2Old, ParamValuesOld];
*)

(*
		TIMEOUT = 1;
*)


(*
		TEST0 = Cases[Map[Function[{x}, If[Length[x] > 0 && Length[x] <= Length[ParamEqSys], x]], Subsets[AllParamsSet]], Except[Null]];
		TEST1 = Cases[Map[(If[Length[#1] == Length[ParamEqSys], #1])&, TEST0], Except[Null]];
		TEST2 = Cases[Map[(If[Length[#1] == Length[ParamEqSys[[1]]] - 1, #1])&, TEST0], Except[Null]];
		TEST3 = Cases[Map[(If[Length[#1] == Length[ParamEqSys[[1]]], #1])&, TEST0], Except[Null]];

		TIMING4 = (TEST4 = Map[Function[{x}, Map[Function[{y}, TimeConstrained[Solve[y, x], TIMEOUT]], {ParamEqSys[[3]]}]], TEST3]; // Timing);
		INDEX4 = Cases[Map[Function[{x}, If[Not[TEST4[[x]][[1]] === $Aborted], x]], Range[1, Length[TEST4]]], Except[Null]];
*)




(*--------------------------------------------------------------------------------------------------------*)
