
Get["eq_m_sem2.wl"];



funcMainLoop0 = Function[{},

  askEqResult = funcAskEq[PresetEqs];
  targetEq = askEqResult[[1]];
  flagPreset = askEqResult[[2]];
  CMD = askEqResult[[3]];
  tmp100 = Null;
  If[flagPreset == False, tmp100 = funcAskPoleOrder[targetEq]];
  If[flagPreset == True,
  tmpInd = Cases[Map[(If[PresetEqs[[All,1]][[#1]] == ToString[CMD], #1] )&, Range[1, Length[PresetEqs[[All, 1]]]]], Except[Null]];
  tmp100 = PresetEqs[[All, 3]][[tmpInd[[1]]]]; Print["Pole order (preset equation case) is ", tmp100, "\n"];
  ];
  tmp101 = { targetEq, tmp100 };

  tmp102 = funcAskParamsForSolve[targetEq];

  tmp103 = funcIntTrySolve[targetEq, tmp100, tmp102];

  status = "ok";

  If[tmp103 == Null, status = "fail"];
  If[tmp103["RES"] == {}, status = "fail"];

  tmp104 = {};
  tmp105 = Null;

  If[Head[tmp103] == Association, tmp104 = tmp103["RES"]];
  If[tmp104 != {}, tmp105 = funcIntShowRes[targetEq, tmp100, tmp104]];

  RetVal = { status, targetEq, tmp100, tmp102, tmp103, tmp104 };

  RetVal
];



funcMainLoopFail0 = Function[{targetEq, tmp100, tmp102},

  tmp103 = funcIntTrySolve[targetEq, tmp100, tmp102];

  status = "ok";

  If[tmp103 == Null, status = "fail"];
  If[tmp103["RES"] == {}, status = "fail"];

  tmp104 = {};
  tmp105 = Null;

  If[Head[tmp103] == Association, tmp104 = tmp103["RES"]];
  If[tmp104 != {}, tmp105 = funcIntShowRes[targetEq, tmp100, tmp104]];

  RetVal = { status, targetEq, tmp100, tmp102, tmp103, tmp104 };

  RetVal
];

funcIntSolveFail = Function[{failMethod, TargetEq, pOrder, ParamForSolve},

  tmpLoop = Null;

  If[failMethod == "reduce",
    tmpLoop = For[i = 1, i > 0, i += 0,

    newData = funcIntReduceParams[TargetEq, pOrder, ParamForSolve];
    tmp0 = funcMainLoopFail0[newData[[1]], newData[[2]], newData[[3]]];

    If[tmp0[[1]] == "ok", Break[tmp0]];

    If[tmp0[[1]] == "fail", Print["Solve errors\n"]];

    tmpCmd = InputString[": "];

    If[tmpCmd == "quit", Break[], If[tmpCmd == "exit", Break[], Continue[]]];


  ];

  ];

	RetVal = {};
  RetVal = tmpLoop;
	RetVal
];


funcMainLoop = Function[{},

(* flagTryReduce = False;
tmpReduce = Null; *)

status = "";

tmpLoop = For[i = 1, i > 0, i += 0,

  (* If[Not[flagTryReduce],
  tmp0 = funcMainLoop0[],
  tmp0 = funcIntReduceParams[tmpReduce[[1]], tmpReduce[[2]], tmpReduce[[3]]]
  ]; *)
  tmp0 = funcMainLoop0[];

  status = tmp0[[1]];

  If[tmp0[[1]] == "ok", Break[tmp0]];

  If[tmp0[[1]] == "fail", Print["Solve errors\n"]];

  tmpCmd = InputString[": "];

  If[tmpCmd == "reduce", tmp00 = funcIntSolveFail["reduce", tmp0[[2]], tmp0[[3]], tmp0[[4]]]; Break[tmp00]];

  (* If[tmpCmd == "reduce", flagTryReduce = True;
    tmpReduce = { tmp0[[2]], tmp0[[3]], tmp[[4]]}; Continue[]]; *)

  If[tmpCmd == "quit", Break[], If[tmpCmd == "exit", Break[], Continue[]]];


];
  tmpCmd = "";

  If[status == "ok", tmpCmd = InputString["Check results (y/n)?: "]];
  If[tmpCmd == "y", funcIntCheckRes[tmpLoop[[2]], tmpLoop[[3]], tmpLoop[[-1]]]];

  RetVal = Null;
  RetVal = tmpLoop;
  RetVal
];
