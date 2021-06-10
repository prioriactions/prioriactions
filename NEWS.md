# prioriactions 0.1.2

- Update *Mitchel River* vignette.
- Add three new parameters to the function `solve()`; *name_output_file* is the name of the main output that is exported only if the *output_file* parameter is TRUE, and *name_log* is the name of the solver log (only using gurobi solver) that is exported only if the *log_file* parameter is TRUE.
- Fix `getSolutionActions()` and `getSolutionUnits()` functions of the `solution` object. Now these round the values assigned to each variable to the integer nearest.
- Fix `benefit()` function of the `solution` object. Now the calculation considers the amount of features and threats other than 1 and 0.
- Update `OptimizationProblemRcpp.cpp` file. Now the models considers the amount of features and threats other than 1 and 0.
- Fix typo that named files and variables *sensibility* instead of *sensitivity*. All documentation was updated.
- Fix issue where the `createtxt()` function did not close the export file correctly.

# prioriactions 0.1.1

- Rcpp implementation working.
- Fix internals id's for the proper functioning of the model creation.

# prioriactions 0.1.0

- Initial package version.
