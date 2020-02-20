

Get["eq_m_sem2_data.wl"];
Get["eq_m_sem2_func.wl"];

PresetEqs = {{"KS", ksEq, 3}, {"ee5", ee5Eq, 5}, {"task", taskEq, 1}, {"REq", REq, 2}, {"KdVBEq", KdVBeq, 2},
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
  RetVal2 = { RetVal, FlagPreset };
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

funcParamEqSysProc = Function[{ParamEqSys}
  Print["Number of sets param equation systems:\t", Length[ParamEqSys]];
  Map[(Print["Num of equations in system ", #1, "[variable: ", StringJoin["sys", ToString[#1]], "; params: ", GP[ParamEqSys[[#1]]], "]:\t", Length[ParamEqSys[[#1]]]]; Evaluate[ToExpression[StringJoin["sys", ToString[#1], " = ParamEqSys[[", ToString[#1], "]]"]]])&, Range[1, Length[ParamEqSys]]];
  Print["Start dialog for process system..."];
  Print["Enter Return[] for exit from dialog"];
  Print["Set RetVal variable to return value from funcParamEqSysProc [RetVal = <value>;]"];
  RetVal = Null;
  Dialog[];

  RetVal
];
