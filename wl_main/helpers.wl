


fP2E = Function[{x}, Map[(#1 == 0)&, x]];
fE2P = Function[{x}, Map[(#1[[1]] - #1[[2]])&, x]];
