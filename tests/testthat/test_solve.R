test_that("new solution-class", {
  # data
  x <- pproto(NULL, Solution, data = list())
  # tests
  expect_s3_class(x, "Solution")
})

test_that("data.frame inputs", {

  skip_if_not_installed("Rcplex")
  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  sim_features_data$target_recovery <- c(40, 20, 50, 30)

  x <- suppressWarnings(inputData(pu = sim_pu_data,
                                  features = sim_features_data,
                                  dist_features = sim_dist_features_data,
                                  threats = sim_threats_data,
                                  dist_threats = sim_dist_threats_data,
                                  sensitivity = sim_sensitivity_data,
                                  boundary = sim_boundary_data))

  p <- suppressWarnings(problem(x))
  s <- solve(p, output_file = FALSE)


  # tests
  expect_s3_class(s, "Solution")
  expect_equal(s$data$sol[1:10], c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1))
})

test_that("verify cplex and rphymphony", {
  skip_on_ci()
  skip_on_cran()

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  sim_features_data$target_recovery <- c(40, 20, 50, 30)

  x <- suppressWarnings(inputData(pu = sim_pu_data,
                                  features = sim_features_data,
                                  dist_features = sim_dist_features_data,
                                  threats = sim_threats_data,
                                  dist_threats = sim_dist_threats_data,
                                  sensitivity = sim_sensitivity_data,
                                  boundary = sim_boundary_data))

  p <- suppressWarnings(problem(x))
  s1 <- solve(p, output_file = FALSE, solver = "cplex")
  s2 <- solve(p, output_file = FALSE, solver = "symphony")

  # tests
  expect_s3_class(s1, "Solution")
  expect_s3_class(s2, "Solution")
  expect_equal(s1$data$sol[1:10], s2$data$sol[1:10])
})
