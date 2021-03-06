(*********************************************************************************************************************************)
ricEq = D[Y[z], {z, 1}] + Y[z]^2 - b;
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
ee5Eq = epsilon*D[y[z], {z, 5}] + gamma*D[y[z], {z, 4}] + delta*D[y[z], {z, 3}] + xi*D[y[z], {z, 2}] + beta*D[y[z], {z, 1}] + alpha*y[z]^2 - C0*y[z] + C1;
pOrder = 5;
TargetEq = ee5Eq;
ParamForSolve = { b, xi, gamma };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
taskEq = k^3*D[y[z], {z, 3}] + 6*k*y[z]*D[y[z], {z, 1}] + omega*D[y[z], {z, 1}] - alpha - beta*y[z] - gamma*y[z]^2 - delta*y[z]^3 + 6*y[z]^4;
pOrder = 1;
TargetEq = taskEq;
ParamForSolve = {omega, gamma, b};
AllParamsSet = { k, omega, alpha, beta, gamma, delta, b };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
ksEq = D[y[z], {z, 3}] + sigma*D[y[z], {z, 2}] + D[y[z], {z, 1}] + y[z]^2/2 - C0*y[z] + C1;
pOrder = 3;
TargetEq = ksEq;
ParamForSolve = { sigma, C1, b };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
REq = D[y[z], {z, 1}]^2 - 4*y[z]^3 - alpha*y[z]^2 - beta*y[z] - gamma;
pOrder = 2;
TargetEq = REq;
EqParamList = { alpha, beta, gamma };
SolveParamList = { A0, A1, A2, alpha, gamma }
(*********************************************************************************************************************************)


(*********************************************************************************************************************************)
KdVBEq = D[y[z], {z, 2}] + 3*y[z]^2 - alpha*D[y[z], {z, 1}] - C0*y[z] + C1;
pOrder = 2;
TargetEq = KdVBEq;
SolveParamList = { A0, A1, A2, alpha, C1 };
SolveParamList = { A0, A1, A2, alpha, b };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
nsEq = D[y[z], {z, 2}] + alpha*y[z] + beta*y[z]^3;
pOrder = 1;
TargetEq = nsEq;
SolveParamList = { A0, A1, A2, alpha, b };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
kbPdeEq = D[(D[u[x, y, t], {t, 1}] + u[x, y, t]*D[u[x, y, t], {x, 1}] + alpha*D[u[x, y, t], {x, 3}] - beta*D[u[x, y, t], {x, 2}]), {x, 1}] + gamma*D[u[x, y, t], {y, 2}];
subKbEq0 = kbPdeEq/.u->Function[{x, y, t}, y[x + C0*y - C1*t]];
subKbEq1 = subKbEq0/.(x + C0*y - C1*t)->z;
kbEq = subKbEq1/.C0^2->C0;
pOrder = 2;
TargetEq = kbEq;
SolveParamList = { A0, A1, A2, b, alpha, C0, C1 };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
zkPdeEq = D[u[x, y, z, t], {t, 1}] + alpha*u[x, y, z, t]*D[u[x, y, z, t], {x, 1}] + D[u[x, y, z, t], {x, 3}] + D[u[x, y, z, t], {x, 1}, {y, 2}] + D[u[x, y, z, t], {x, 1}, {z, 2}];
subZkEq0 = zkPdeEq/.u->Function[{x, y, z, t}, y[lambda1*x + lambda2*y + lambda3*z + lambda4*t + delta]];
subZkEq1 = subZkEq0/.(lambda1*x + lambda2*y + lambda3*z + lambda4*t + delta)->z;
zkEq = subZkEq1;
pOrder = 2;
TargetEq = zkEq;
SolveParamList = { A0, A1, A2, b, lambda1, lambda2 };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
rdPdeEq = D[u[x, t], {t, 1}] - D[(u[x, t])^2, {x, 2}] - alpha*u[x, t] + beta*u[x, t]^2;
(* beta > 0 *)
subRdEq0 = rdPdeEq/.u->Function[{x, t}, y[x - C0*t]];
subRdEq1 = subRdEq0/.(x - C0*t)->z;
rdEq = subRdEq1;
pOrder = 2;
TargetEq = rdEq;
SolveParamList = { A0, A1, A2, b, alpha, beta, C0 };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
kdv5Eq = D[y[z], {z, 5}] + gamma*y[z]*D[y[z], {z, 3}] + beta*D[y[z], {z, 1}]*D[y[z], {z, 2}] + alpha*y[z]^2*D[y[z], {z, 1}] - C0*D[y[z], {z, 1}];
pOrder = 2;
TargetEq = kdv5Eq;
SolveParamList = { A0, A1, A2, b, C0, gamma };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
ee5Eq = epsilon*D[y[z], {z, 5}] + gamma*D[y[z], {z, 4}] + delta*D[y[z], {z, 3}] + xi*D[y[z], {z, 2}] + beta*D[y[z], {z, 1}] + alpha*y[z]^2 - C0*y[z] + C1;
ee5Eq = ee5Eq/.{ gamma -> 0, xi -> 0, alpha -> 1/2 };
pOrder = 5;
TargetEq = ee5Eq;
SolveParamList = { A0, A1, A2, A3, A4, A5, epsilon, gamma, delta, xi, beta, b };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
ee4Eq = alpha*D[y[z], {z, 4}] + beta*y[z]^2 + gamma*D[y[z], {z, 2}] + delta*D[y[z], {z, 1}] - C0*y[z] + C1;
pOrder = 4;
TargetEq = ee4Eq;
SolveParamList = { A0, A1, A2, A3, A4, b, alpha, beta, gamma, delta, C0, C1 };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
cfEq = gamma*D[y[z], {z, 3}] + beta*D[y[z], {z, 2}] + alpha*D[y[z], {z, 1}] + 1/2*epsilon*y[z]^2 + y[z]*D[y[z], {z, 1}] - C0*y[z] + C1;
pOrder = 2;
TargetEq = cfEq;
SolveParamList = { A0, A1, A2, gamma, beta, alpha, epsilon, C0, C1, b };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
genericEq6 = D[y[z], {z, 8}] + alpha*D[y[z], {z, 2}]*y[z] + beta + gamma*D[y[z], {z, 3}];
SolveParamList = { A0, A1, A2, A3, A4, A5, A6, b, alpha, beta, gamma };
genericEq6 = D[y[z], {z, 14}] + D[y[z], {z, 2}]*y[z]^2;
pOrder = 6;
TargetEq = genericEq6;
SolveParamList = { A0, A1, A2, A3, A4, A5, A6, b };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
genericEq2 = D[y[z], {z, 8}] + D[y[z], {z, 2}]*y[z]^3;
SolveParamList = { A0, A1, A2, b };
genericEq2 = D[y[z], {z, 12}] + alpha*D[y[z], {z, 2}]*y[z]^5 + beta*y[z]*D[y[z], {z, 1}] + y[z]^4 + gamma*D[y[z], {z, 11}] + C0 + C1*y[z] + C2*y[z]^2 + C3*y[z]^3;
SolveParamList = { A0, A1, A2, b, alpha, beta, gamma, C0, C1, C2, C3 };
genericEq2 = D[y[z], {z, 12}] + D[y[z], {z, 2}]*y[z]^5 + y[z]*D[y[z], {z, 1}] + y[z]^4 + alpha*D[y[z], {z, 11}] + y[z] + y[z]^2 + y[z]^3;
genericEq2 = D[y[z], {z, 12}] + D[y[z], {z, 2}]*y[z]^5 + alpha*D[y[z], {z, 3}]*y[z]^4 + D[y[z], {z, 4}]*y[z]^3 + beta*D[y[z], {z, 3}]*y[z]^2 + D[y[z], {z, 4}]*y[z] + gamma*D[y[z], {z, 5}];
genericEq2 = D[y[z], {z, 4}]^2 + ksi*D[y[z], {z, 2}]*y[z]^4 + alpha*D[y[z], {z, 3}]*y[z]^3  + beta*D[y[z], {z, 3}]*y[z] + gamma*y[z]^2 + delta*y[z] + epsilon*y[z]^3 + xi + phi*D[y[z], {z, 2}]*D[y[z], {z, 1}] + omega*D[y[z], {z, 2}]*D[y[z], {z, 2}];
pOrder = 2;
TargetEq = genericEq2;
PartSolveParamList = { alpha, b, gamma, delta, beta, epsilon, xi, phi, ksi };
SolveParamList = { A0, A1, A2, b };
(*********************************************************************************************************************************)

(*********************************************************************************************************************************)
genericEq3 = y[z]^6 + D[y[z], {z, 10}]*D[y[z], {z, 2}] + alpha*D[y[z], {z, 5}]^2 + beta*y[z]^3*D[y[z], {z, 3}] + gamma*y[z]^2*D[y[z], {z, 4}] + delta*D[y[z], {z, 2}]*D[y[z], {z, 7}]
genericEq3 = y[z]^5 + D[y[z], {z, 7}]*D[y[z], {z, 2}];
genericEq3 = y[z]^4 + alpha*D[y[z], {z, 4}]*D[y[z], {z, 2}] + beta*D[y[z], {z, 1}]*D[y[z], {z, 2}] + gamma*y[z]^3 + delta*y[z]^2 + epsilon*D[y[z], {z, 3}]^2 + xi;
pOrder = 3;
TargetEq = genericEq3;
SolveParamList = { A0, A1, A2, A3, b };
(*********************************************************************************************************************************)
