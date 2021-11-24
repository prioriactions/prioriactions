

test_that("random values in data.frames", {

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  x <- suppressWarnings(inputData(pu = sim_pu_data,
                                  features = sim_features_data,
                                  dist_features = sim_dist_features_data,
                                  threats = sim_threats_data,
                                  dist_threats = sim_dist_threats_data,
                                  sensitivity = sim_sensitivity_data,
                                  boundary = sim_boundary_data))

  p <- suppressWarnings(problem(x,
                                blm = 1,
                                curve = 1,
                                segments = 1))

  # tests
  expect_equal(p$data$args$blm, 1)
  expect_equal(p$data$args$curve, 1)
  expect_equal(p$data$args$segments, 1)
  expect_equal(p$data$args$model_type, "minimizeCosts")
  expect_equal(p$data$modelsense, "min")
})
