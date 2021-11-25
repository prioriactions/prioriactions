test_that("evaluate function whith porftolio object", {
  skip_on_cran()

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

  f <- getModelInfo(port)

  # tests
  expect_s3_class(port, "Portfolio")
  expect_equal(length(port$data), 2)
  for (i in seq_along(port$data))
    expect_s3_class(port$data[[i]], "Solution")
  expect_setequal(f$solution_name, paste0("Blm", base::round(blm_values, 3)))
  expect_equal(nrow(f), 2)
  expect_equal(f$model_sense[1],
               ifelse(port$data[[1]]$OptimizationClass$data$modelsense == "min", "minimization", "maximization"))
  expect_equal(f$model_sense[2],
               ifelse(port$data[[2]]$OptimizationClass$data$modelsense == "min", "minimization", "maximization"))
  expect_equal(f$n_constraints[1],
               as.character(base::nrow(port$data[[1]]$OptimizationClass$data$A)))
  expect_equal(f$n_constraints[2],
               as.character(base::nrow(port$data[[2]]$OptimizationClass$data$A)))
  expect_equal(f$n_variables[1],
               as.character(base::ncol(port$data[[1]]$OptimizationClass$data$A)))
  expect_equal(f$n_variables[2],
               as.character(base::ncol(port$data[[2]]$OptimizationClass$data$A)))
})

test_that("evaluate function whith solution object", {

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  sim_features_data$target_recovery <- c(40, 20, 50, 30)

  d <- suppressWarnings(inputData(pu = sim_pu_data,
                                  features = sim_features_data,
                                  dist_features = sim_dist_features_data,
                                  threats = sim_threats_data,
                                  dist_threats = sim_dist_threats_data,
                                  boundary = sim_boundary_data,
                                  sensitivity = sim_sensitivity_data))
  p <- suppressWarnings(problem(d))
  s <- solve(p, output_file = FALSE)

  f <- getModelInfo(s)

  # tests
  expect_s3_class(s, "Solution")
  expect_setequal(f$solution_name, "sol")
  expect_equal(nrow(f), 1)
  expect_equal(f$model_sense,
               ifelse(s$OptimizationClass$data$modelsense == "min", "minimization", "maximization"))
  expect_equal(f$n_constraints, base::nrow(s$OptimizationClass$data$A))
  expect_equal(f$n_variables, base::ncol(s$OptimizationClass$data$A))
})

test_that("evaluate function whith OptimizationProblem object", {

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  sim_features_data$target_recovery <- c(40, 20, 50, 30)

  d <- suppressWarnings(inputData(pu = sim_pu_data,
                                  features = sim_features_data,
                                  dist_features = sim_dist_features_data,
                                  threats = sim_threats_data,
                                  dist_threats = sim_dist_threats_data,
                                  sensitivity = sim_sensitivity_data,
                                  boundary = sim_boundary_data))
  p <- suppressWarnings(problem(d))
  f <- getModelInfo(p)

  # tests
  expect_s3_class(p, "OptimizationProblem")
  expect_equal(nrow(f), 1)
  expect_equal(f$model_sense,
               ifelse(p$data$modelsense == "min", "minimization", "maximization"))
  expect_equal(f$n_constraints, base::nrow(p$data$A))
  expect_equal(f$n_variables, base::ncol(p$data$A))
})
