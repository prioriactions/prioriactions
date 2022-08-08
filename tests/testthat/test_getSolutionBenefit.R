test_that("evaluate function whith porftolio object and type = total", {
  skip_on_cran()
  skip_if_no_commercial_solvers_installed()

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  sim_features_data$target_recovery <- c(40, 20, 50, 30)

  # eval different blm values
  blm_values = sample(1:10, 2, replace = TRUE)/10

  port <- suppressWarnings(evalBlm(pu = sim_pu_data,
                                   features = sim_features_data,
                                   dist_features = sim_dist_features_data,
                                   threats = sim_threats_data,
                                   dist_threats = sim_dist_threats_data,
                                   boundary = sim_boundary_data,
                                   values = blm_values,
                                   output_file = FALSE))

  f <- getSolutionBenefit(port, type = "total")

  # tests
  expect_s3_class(port, "Portfolio")
  expect_equal(length(port$data), 2)
  for (i in seq_along(port$data))
    expect_s3_class(port$data[[i]], "Solution")
  expect_setequal(f$solution_name, paste0("Blm", base::round(blm_values, 3)))
  expect_equal(nrow(f), 2*nrow(sim_features_data))
  expect_true(all(f$benefit.recovery + f$benefit.conservation == f$benefit.total))
})

test_that("evaluate function whith portfolio object and type = local", {
  skip_on_cran()
  skip_if_no_commercial_solvers_installed()

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  sim_features_data$target_recovery <- c(40, 20, 50, 30)

  # eval different blm values
  blm_values = sample(1:10, 2, replace = TRUE)/10

  port <- suppressWarnings(evalBlm(pu = sim_pu_data,
                                   features = sim_features_data,
                                   dist_features = sim_dist_features_data,
                                   threats = sim_threats_data,
                                   dist_threats = sim_dist_threats_data,
                                   boundary = sim_boundary_data,
                                   values = blm_values,
                                   output_file = FALSE))

  f <- getSolutionBenefit(port, type = "local")

  # tests
  expect_s3_class(port, "Portfolio")
  expect_equal(length(port$data), 2)
  for (i in seq_along(port$data))
    expect_s3_class(port$data[[i]], "Solution")
  expect_setequal(f$solution_name, paste0("Blm", base::round(blm_values, 3)))
  expect_equal(nrow(f), 2*(nrow(sim_dist_features_data)))
  expect_true(all(f$benefit.recovery + f$benefit.conservation == f$benefit.total))
})

test_that("evaluate function whith solution object and type = total", {

  skip_if_no_commercial_solvers_installed()

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  sim_features_data$target_recovery <- c(40, 20, 50, 30)

  s <- suppressWarnings(prioriactions(pu = sim_pu_data,
                                      features = sim_features_data,
                                      dist_features = sim_dist_features_data,
                                      threats = sim_threats_data,
                                      dist_threats = sim_dist_threats_data,
                                      boundary = sim_boundary_data,
                                      sensitivity = sim_sensitivity_data,
                                      output_file = FALSE))

  f <- getSolutionBenefit(s, type = "total")

  # tests
  expect_s3_class(s, "Solution")
  expect_setequal(f$solution_name, "sol")
  expect_equal(nrow(f), nrow(sim_features_data))
  expect_true(all(f$benefit.recovery + f$benefit.conservation == f$benefit.total))
})

test_that("evaluate function whith solution object and type = local", {

  skip_if_no_commercial_solvers_installed()

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  sim_features_data$target_recovery <- c(40, 20, 50, 30)

  s <- suppressWarnings(prioriactions(pu = sim_pu_data,
                                      features = sim_features_data,
                                      dist_features = sim_dist_features_data,
                                      threats = sim_threats_data,
                                      dist_threats = sim_dist_threats_data,
                                      boundary = sim_boundary_data,
                                      sensitivity = sim_sensitivity_data,
                                      output_file = FALSE))

  f <- getSolutionBenefit(s, type = "local")

  # tests
  expect_s3_class(s, "Solution")
  expect_setequal(f$solution_name, "sol")
  expect_equal(nrow(f), nrow(sim_dist_features_data))
  expect_true(all(f$benefit.recovery + f$benefit.conservation == f$benefit.total))
})
