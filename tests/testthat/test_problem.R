context("problem")

test_that("x=data.frame, features=data.frame (single zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.1, NA, runif(8)))
  species <- data.frame(id = seq_len(5), name = letters[1:5], targets = 0.5)
  rij <- expand.grid(pu = seq_len(9), species = seq_len(5))
  rij$amount <- runif(nrow(rij))
  # create problem
  x <- problem(pu, species, rij, "cost")
  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
  # tests for integer fields
  expect_equal(x$feature_names(), letters[1:5])
  expect_equal(x$zone_names(), "cost")
  expect_equal(x$number_of_features(), 5)
  expect_equal(x$number_of_zones(), 1)
  expect_equal(x$number_of_planning_units(), 9)
  expect_equal(x$planning_unit_indices(), which(!is.na(pu$cost)))
  expect_equal(x$number_of_total_units(), 10)
  # tests for planning_unit_costs field
  expect_equivalent(x$planning_unit_costs(), matrix(pu$cost[-2], ncol = 1))
  expect_equal(colnames(x$planning_unit_costs()), "cost")
  # tests for feature_abundances_in_planning_units field
  rij2 <- rij[rij$pu != 2, ]
  expect_equivalent(x$feature_abundances_in_planning_units(),
                    Matrix::rowSums(Matrix::sparseMatrix(i = rij2[[2]],
                                                         j = rij2[[1]],
                                                         x = rij2[[3]])))
  expect_equal(rownames(x$feature_abundances_in_planning_units()), letters[1:5])
  expect_equal(colnames(x$feature_abundances_in_planning_units()), "cost")
  # tests for feature_abundances_in_total_units field
  expect_equivalent(x$feature_abundances_in_total_units(),
                    Matrix::rowSums(Matrix::sparseMatrix(i = rij[[2]],
                                                         j = rij[[1]],
                                                         x = rij[[3]])))
  expect_equal(rownames(x$feature_abundances_in_total_units()), letters[1:5])
  expect_equal(colnames(x$feature_abundances_in_total_units()), "cost")
  # tests for rij_matrix field
  rij2 <- rij[rij$pu != 2, ]
  rij2$pu <- match(rij2$pu, pu$id[-2])
  expect_equivalent(x$data$rij_matrix[[1]],
                    Matrix::sparseMatrix(i = rij2[[2]], j = rij2[[1]],
                                         x = rij2[[3]],
                                         dims = c(5, 9)))
  expect_equal(names(x$data$rij_matrix), "cost")
  expect_equal(rownames(x$data$rij_matrix[[1]]), letters[1:5])
  # test that calling targets before they have been initialized throws error
  expect_error(x$feature_targets())
})
