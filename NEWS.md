# prioriactions 0.5.0

-   Now we add the non-academic cbc solver as option in `solve()` function.
-   Update `solve()` function reference.
-   Add the `CONTRIBUTING` file. 
-   New **solver benchmarking** vignette is added.
-   Now `blm` parameter in `problem()` is not rounded.
-   Now the parameter `NodefileStart` is incorporated internally in the **gurobi** solver. Set in 0.5.
-   Update vignettes.

# prioriactions 0.4.2

-   Fix gurobi solver params in `solve()` function.
-   Update `inputData()` function reference.
-   Now parameters *a*, *b*, *c*, *d* for `sensitivity` input are called *delta1*, *delta2*, *delta3*, *delta4* respectively.

# prioriactions 0.4.1.1

-   Fix URL in vignette.

# prioriactions 0.4.1

-   Now `solve()` function allows CPLEX solver.
-   Update `inputData()` description about *d* parameter.
-   Update documentation.
-   Now `solve()` function exports solution scores.
-   Now we sort *id's* of inputs data in `inputData()` function.

# prioriactions 0.4

-   Fixed `problem()` function. Now it works with recovery objectives and data non-filtered, i.e. with non-threatened features.

# prioriactions 0.3.3

-   Release candidate for CRAN.

# prioriactions 0.3.2

-   Release candidate for CRAN.

# prioriactions 0.3.1

-   Release candidate for CRAN.

# prioriactions 0.3.0

-   Update all functions documentation.
-   Rename `problem` function as `inputData()` function for intuitiveness.
-   Add `problem()` function to create different mathematical models instead of `minimizeCosts` and `maximizeBenefits`.
-   Now `getPerformance()` function incorporate information of previous four functions (removed): `getGap`, `getStatus`, `getTimeSolving` and `getObjectiveValue`.
-   Now `getModelInfo()` function incorporate information of previous three functions (removed): `getModelSense()`, `getNconstraints` and `getNvariables`.
-   Add `getCost()` function to get information about the solution costs. The `getActionsCost`, `getPlanningUnitsCosts` and `getTotalCosts` functions are deprecated.
-   Add `getConnectivity()` function to get information about the solution connectivity. The `getPlanningUnitsConnectivity`, `getActionsConnectivity` and `getTotalConnectivity` are deprecated.
-   Add `getSolutionBenefit()` function to get information about the solution benefits. The `getBenefits` and `getTotalBenefits` functions are deprecated.
-   Add `getPotentialBenefit()` function to get maximum benefits to achieve given a data.
-   Add two different targets: recovery and conservation; the first related to the actions to abated threads, and the second related to keep planing units in benefit to features (without do actions).

# prioriactions 0.2.0

-   Update *Mitchell River*, *Benefits and sensitivities* and *Get started* vignettes.
-   Update all functions documentation.
-   Add `threat` input with information on different threats within the exercise.
-   Now rcpp functions work with pointers which allows to use less memory.
-   Add parameters *a*, *b*, *c*, *d* for `sensitivity` input added. These parameters allow work with different sensitivities curves of threats-features. In addition to the possibility to work with non-binary threat intensities.
-   Now the `sensitivity` is an optional input.
-   New `maximizeBenefits()` model function.
-   Rename`min_costs` model function as `minimizeCosts()` model function for consistency.
-   New `portfolio-class` objects to handle multiple solutions.
-   New `evalBlm()` function to create and solve multiple models with different `blm` parameters.
-   New `evalTarget()` function to create and solve multiple models with different `target` parameters.
-   New `evalBudget()` function to create and solve multiple models with different `budget` parameters.
-   Now all methods of the `solution-class` are available as functions for the `solution-class` and `porfolio-class`.

# prioriactions 0.1.2

-   Update *Mitchell River* vignette.
-   Add three new parameters to the function `solve()`; *name_output_file* is the name of the main output that is exported only if the *output_file* parameter is TRUE, and *name_log* is the name of the solver log (only using gurobi solver) that is exported only if the *log_file* parameter is TRUE.
-   Fix `getSolutionActions()` and `getSolutionUnits()` functions of the `solution` object. Now these round the values assigned to each variable to the integer nearest.
-   Fix `benefit()` function of the `solution` object. Now the calculation considers the amount of features and threats other than 1 and 0.
-   Update `OptimizationProblemRcpp.cpp` file. Now the models considers the amount of features and threats other than 1 and 0.
-   Fix typo that named files and variables *sensibility* instead of *sensitivity*. All documentation was updated.
-   Fix issue where the `createtxt()` function did not close the export file correctly.

# prioriactions 0.1.1

-   Rcpp implementation working.
-   Fix internals id's for the proper functioning of the model creation.

# prioriactions 0.1.0

-   Initial package version.
