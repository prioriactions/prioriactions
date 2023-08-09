# load packages
library(testthat)
library(prioriactions)

# load solver packages
require(gurobi)
require(Rsymphony)
require(Rcplex)
require(rcbc)

test_check("prioriactions")
