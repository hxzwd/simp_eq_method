

Get["eq_m_sem2_data.wl"];
Get["eq_m_sem2_func.wl"];

PresetEqs = {{"ksEq", ksEq, 3}, {"ee5", ee5Eq, 5}, {"task", taskEq, 1}, {"REq", REq, 2}, {"KdVBEq", KdVBEq, 2},
            {"nsEq", nsEq, 1}, {"kbEq", kbEq, 2}, {"zkEq", zkEq, 2}, {"rdEq", rdEq, 2}, {"kdv5Eq", kdv5Eq, 2},
            {"ee4Eq", ee4Eq, 4}, {"cfEq", cfEq, 2}, {"genericEq6", genericEq6, 6}, {"genericEq2", genericEq2, 2},
            {"genericEq3", genericEq3, 3}};

funcAskEq = Function[{PresetEqs},
  Print["Enter equation or use default preset for test."];
  Map[(Print[#1, ". [\"", PresetEqs[[#1]][[1]], "\", p = ", PresetEqs[[#1]][[3]], "]\t", PresetEqs[[#1]][[2]], " = 0"])&, Range[1, Length[PresetEqs]]];
  CMD = InputString["Enter equation [or name of preset equation]: "];
  RetVal = ToExpression[CMD];
  FlagPreset = False;
  If[MemberQ[PresetEqs[[All,1]], CMD], RetVal = (PresetEqs[[All,2]])[[Position[PresetEqs[[All,1]], CMD][[1]][[1]]]]; FlagPreset = True];
  RetVal2 = { RetVal, FlagPreset, CMD };
  RetVal2
  ];


funcAskPoleOrder = Function[{TargetEq},
  Print["Enter pole order of equation:\n", TargetEq];
  CMD = InputString["Show tip for finding pole order [y/n]?"];
  If[MemberQ[{"y", "yes", "YES", "Y", "1"}, CMD], funcPoleTip[TargetEq]];
  CMD = InputString["Enter pole order: "];
  RetVal = Null;
  If[NumberQ[ToExpression[CMD]], RetVal = ToExpression[CMD]];
  RetVal
];

funcAskParamsForSolve = Function[{TargetEq},
  Print["Enter not independed params (including param \"b\" of Riccati equation)."];
  Print["All params of target equation: ", funcGetAllParams[TargetEq]];
  Print["All params of task: ", Join[{b}, funcGetAllParams[TargetEq]]];
  CMD = InputString["Enter params for solving: "];
  RetVal = Null;
  If[Head[ToExpression[CMD]] === List, If[SubsetQ[Join[{b}, funcGetAllParams[TargetEq]], ToExpression[CMD]], RetVal = ToExpression[CMD], RetVal = Null], RetVal = Null];
  RetVal
];

funcParamEqSysProc = Function[{ParamEqSys},
  Print["Number of sets param equation systems:\t", Length[ParamEqSys]];
  Map[(Print["Num of equations in system ", #1, "[variable: ", StringJoin["sys", ToString[#1]], "; params: ", GP[ParamEqSys[[#1]]], "]:\t", Length[ParamEqSys[[#1]]]]; Evaluate[ToExpression[StringJoin["sys", ToString[#1], " = ParamEqSys[[", ToString[#1], "]]"]]])&, Range[1, Length[ParamEqSys]]];
  Print["Start dialog for process system..."];
  Print["Enter Return[] for exit from dialog"];
  Print["Set RetVal variable to return value from funcParamEqSysProc [RetVal = <value>;]"];
  RetVal = Null;
  Dialog[];

  RetVal
];



funcIntShowRes = Function[{TargetEq, pOrder, RES},
	Print["Target equation ( pole order p = ", pOrder, "):\n"];
	Print[TargetEq, " = 0"];
	Print["\n"];

	Print["y[z] in form:\n\t"];
	Print[StringJoin[{"y[z] = "}, Riffle[Join[{ToString[A0]}, Map[("A"<>ToString[#1]<>"*Y[z]^"<>ToString[#1])&, Range[1, pOrder]]], " + "]], "\n"];

	Print["Solutions found:\n"];

	H1 = Map[("Solution #"<>ToString[#1])&, Range[1, Length[RES]]];

	tt101 = Function[{x}, TableForm[{Map[(#1[[2]])&, RES[[x]]]}, TableAlignments -> {Right, Center}, TableHeadings -> {{ H1[[x]] }, Map[(#1[[1]])&, RES[[x]]]}]];

	For[i = 1, i <= Length[RES], i++, Print[tt101[i]]; Print["\n"]];

(*
	tt102 = ToExpression[StringJoin[{"y[z] = "}, Riffle[Join[{ToString[A0]}, Map[("A"<>ToString[#1]<>"*Y[z]^"<>ToString[#1])&, Range[1, pOrder]]], " + "]]];
*)

	tt102 = ToExpression[StringJoin[{""}, Riffle[Join[{ToString[A0]}, Map[("A"<>ToString[#1]<>"*Y[z]^"<>ToString[#1])&, Range[1, pOrder]]], " + "]]];
	RetVal = tt102;

	ttRicSol = (DSolve[ricEq == 0, Y[z], z]/.C[1]->Criccati)[[1]][[1]];
	ttRicSolStr = "Y[z] = "<>ToString[ttRicSol[[2]]];


	Print["Solutions found (expanded)    ", ttRicSolStr, ":\n"];

	For[i = 1, i <= Length[RES], i++, Print[TableForm[{ToString[tt102/.RES[[i]]]}, TableAlignments -> {Right, Center}, TableHeadings->{{H1[[i]]<>":  y[z] ="}, None}]]; Print["\n"]];

	(*!!!!!!!!!!!!!!!!!!!!!

	Print["Solutions found (full form):\n"];


	For[i = 1, i <= Length[RES], i++, Print[TableForm[{ToString[tt102/.ttRicSol/.RES[[i]]]}, TableAlignments -> {Right, Center}, TableHeadings->{{H1[[i]]<>":  y[z] ="}, None}]]; Print["\n"]];
	*)



	(*

	RetVal = Map[Function[{x},TableForm[{Map[ (#1[[2]])&, RES[[x]] ]},TableAlignments->{Right, Center},TableHeading->{{H1[[x]]}, Map[(#1[[1]])&, RES[[x]] ]}]],Range[1, Length[RES]];

	TMPT1 = Map[ (#1[[1]])&, RES[[1]] ];
	TMPT1 = Map[ (#1[[2]])&, RES[[1]] ];
	TableForm[tt4, TableAlignments -> {Right, Center}, TableHeadings -> {{ 1, 2 }, {A1, A0, alpha}} ];

	*)

	RetVal
];



funcIntCheckRes = Function[{TargetEq, pOrder, RES},
	Print["Num of solutions is: ", Length[RES]];
	Print["Enter solutions numbers for checking (example: 1, 2, 10 or all):\n"];
	tt103 = InputString[];

	tt104 = Map[(ToString[#1])&, ToExpression["{"<>tt103<>"}"]];
	ttParam = "";
	ttCheck = {};
	If[MemberQ[tt104, "all"], ttParam = Range[1, Length[RES]], ttParam = Map[(ToExpression[#1])&, tt104]];
	(*
	If[ttParam == "all", ttCheck = CHECK[TargetEq, pOrder, RES, "all"], ttCheck = Map[Function[{i}, CHECK[TargetEq, pOrder, RES, i]], ttParam]];
	*)

	Print["Check solutions: ", ttParam];

	ttCheck = Map[Function[{i}, CHECK[TargetEq, pOrder, RES, i][[1]][[1]]], ttParam];
	ttCheckRes = Map[Function[{i}, If[i == 0, "PASS", "FAIL"]], ttCheck];

	H1 = Map[("Solution #"<>ToString[#1])&, ttParam];
	For[i = 1, i <= Length[ttCheckRes], i++, Print[TableForm[{ttCheckRes[[i]]},TableAlignments->{Right, Center},TableHeadings->{{H1[[i]]<>" :"}, {"Check result"}}]]; Print["\n"]];

	RetVal = { ttParam, ttCheck };
	RetVal
];



funcIntTrySolve = Function[{TargetEq, pOrder, ParamForSolve},
	defaultTimeOut = 60;
	ttTimeOut = defaultTimeOut;
	Print["Try to solve target equation for selected parameters:\n"];
	Print["Input computation time limit (seconds):\n"];
	tt108 = InputString["Press enter use default timeout ("<>ToString[defaultTimeOut]<>" s.): "];
	If[NumberQ[ToExpression[tt108]], ttTimeOut = ToExpression[tt108]];
	Print["Timeout value is set to "<>ToString[ttTimeOut]<>"\n"];

	tt109 = TimeConstrained[funcTest[TargetEq, pOrder, ParamForSolve], ttTimeOut, Print["TIME OUT!"]];


	RetVal = tt109;
	RetVal
];

funcIntReduceParams = Function[{TargetEq, pOrder, ParamForSolve},

	Print["Target equation:\n"];
	Print[TargetEq, " = 0\n"];
	Print["Pole order of target equation: p = ", pOrder, "\n"];
	Print["Parameters for solve: "<>ToString[ParamForSolve]<>"\n"];
	Print["Parameters of target equation:\n"]
	Print[ToString[GP[TargetEq]]<>"\n"];
	Print["All parameters of task:\n"]
	Print[ToString[Join[funcGetACoeffList[pOrder][[1;;pOrder + 1]], {b}, GP[TargetEq]]]<>"\n"];

	Print["Set numeric value for some parameters of target equation:\n"];
	Print["Example: alpha = 0, C0 = 3.14\n"];
	tt110 = InputString[];

	If[tt110 == "", Return[Null]];

	tt111 = Map[(StringTrim[#1])&, StringSplit[StringReplace[tt110, "="->"->"], ","]];
	tt112 = Map[(ToExpression[#1])&, tt111];
	newTargetEq = TargetEq/.Map[(ToExpression[#1])&, tt111];


	newpOrder = funcAskPoleOrder[newTargetEq];

	Print["Old parameters for solve:\n"];
	Print[ToString[ParamForSolve]<>"\n"];
	tt113 = InputString["Use reduced system of old parameters? [(y or enter)/n]:"];

	newParamForSolve = ParamForSolve/.tt112;
	newParamForSolve = Cases[Map[(If[NumberQ[#1], Null, #1])&, newParamForSolve], Except[Null]];


	If[tt113 == "n", newParamForSolve = funcAskParamsForSolve[newTargetEq]];


	Print["New target equation:\n"];
	Print[newTargetEq, " = 0\n"];
	Print["New pole order of target equation:\n"];
	Print["p = ", newpOrder, "\n"];
	Print["New parameters for solve:\n"];
	Print[ToString[newParamForSolve]<>"\n"];


	RetVal = { newTargetEq, newpOrder, newParamForSolve };
	RetVal

];
