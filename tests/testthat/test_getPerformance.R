test_that("evaluate function whith porftolio object", {
  skip_on_cran()

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

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

  f <- getPerformance(port)

  # tests
  expect_s3_class(port, "Portfolio")
  expect_equal(length(port$data), 2)
  for (i in seq_along(port$data))
    expect_s3_class(port$data[[i]], "Solution")
  expect_setequal(f$solution_name, paste0("Blm", base::round(blm_values, 3)))
  expect_equal(nrow(f), 2)
  expect_equal(f$objective_value[1],
               as.character(base::round(port$data[[1]]$data$objval, 3)))
  expect_equal(f$objective_value[2],
               as.character(base::round(port$data[[2]]$data$objval, 3)))
  expect_equal(f$gap[1],
               as.character(base::round(port$data[[1]]$data$gap * 100, 3)))
  expect_equal(f$gap[2],
               as.character(base::round(port$data[[2]]$data$gap * 100, 3)))
  expect_equal(f$solving_time[1],
               as.character(base::round(port$data[[1]]$data$runtime, 3)))
  expect_equal(f$solving_time[2],
               as.character(base::round(port$data[[2]]$data$runtime, 3)))
})

test_that("evaluate function whith solution object", {

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  s <- suppressWarnings(prioriactions(pu = sim_pu_data,
                                      features = sim_features_data,
                                      dist_features = sim_dist_features_data,
                                      threats = sim_threats_data,
                                      dist_threats = sim_dist_threats_data,
                                      boundary = sim_boundary_data,
                                      sensitivity = sim_sensitivity_data,
                                      output_file = FALSE))

  f <- getPerformance(s)

  # tests
  expect_s3_class(s, "Solution")
  expect_setequal(f$solution_name, "sol")
  expect_equal(nrow(f), 1)
  expect_equal(f$objective_value,
               base::round(s$data$objval, 3))
  expect_equal(f$gap, base::round(s$data$gap * 100, 3))
  expect_equal(f$solving_time, base::round(s$data$runtime, 3))
})