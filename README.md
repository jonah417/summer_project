# optimist

Repo to investigate optimisation issues in `mrds`. Comparisons are made to Distance for Windows' `MCDS.exe`.

"Data" (in the sense of model run outputs) is collected in `testbase`. These are "analysed" (in the sense of compared) in `analyse`. This is done in a sequential way so there may be little to see in the code. For that reason a summary of changes (reflected in the [`optimist` branch](https://github.com/DistanceDevelopment/mrds/tree/optimist) of `mrds`) is given here.

- Calculation of Hermite polynomial adjustments.
  - Implementation was from the formula, so not numerically robust.
  - Implementation used was for "physicists" rather than "probabilists" Hermite polynomials (see e.g., https://en.wikipedia.org/wiki/Hermite_polynomials), which is not as in `MCDS.exe` (and may also have numerical implications).
  - General function for computation of Hermite polynomials of any order (via recursive definition) is not necessary since we only need a few (order 4, 6, 8 usually) so these are now just hard-coded.
  - Remove setting of Hermite starting pars to 1 in `detfct.fit` (unclear why this was happening
- Refinement of adjustment-key-all outer optimisation
  - Previously non-optimized parameters were held constant (i.e., overwritten within the likelihood calculation). This probably had bad effects on derivative calculations.
  - Now only the parameters in question (key or adjustment) are optimized over, so the parameter space is smaller and derivatives are correct. Changes in `detfct.fit.opt` and `flnl`.
- Refinement of "outer" optimization (`detfct.fit`)
  - Now using best previous parameter sets (by likelihood) rather than last values.
  - Use optimizer's convergence diagnostic to assess outer convergence.
- Further refinement of "inner" optimization (`detfct.fit.opt`)
  - simplification of stopping rules (one while() loop rather than two)
  - parameters are nudged only when bounds have not been hit, if bounds have been hit then they are expanded
- Covariate rescaling (via old code from optimx) was written so the scaling was 1/scale not scale. This presumably was causing a lot of problems.
  - Changed code so that the scaling kicks-in at smaller scales (relative scale 2 rather than 3).
