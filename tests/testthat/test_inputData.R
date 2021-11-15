test_that("new data-class", {
  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)

  d <- inputData(pu = sim_pu_data,
                 features = sim_features_data,
                 dist_features = sim_dist_features_data,
                 threats = sim_threats_data,
                 dist_threats = sim_dist_threats_data,
                 boundary = sim_boundary_data)

  x <- pproto(NULL, Data, data = list(pu = sim_pu_data,
                                      features = sim_features_data,
                                      dist_features = sim_dist_features_data,
                                      threats = sim_threats_data,
                                      dist_threats = sim_dist_threats_data,
                                      boundary = sim_boundary_data))
  # tests
  expect_equal(d, x)
})

test_that("get methods", {

  # create data
  data(sim_pu_data, sim_features_data, sim_dist_features_data,
       sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
       sim_boundary_data)


  # tests
  expect_equal(x$getData("pu"), pu_sim)
  expect_equal(x$getData("features"), features_sim)
  expect_equal(x$getData("dist_features"), dist_features_sim)
  expect_equal(x$getData("threats"), threats_sim)
  expect_equal(x$getData("dist_threats"), dist_threats_sim)
  expect_equal(x$getData("boundary"), boundary_sim)
  expect_equal(x$getActionsAmount(), nrow(dist_threats_sim))
  expect_equal(x$getFeatureAmount(), nrow(features_sim))
  expect_equal(x$getFeatureNames(), features_sim$name)
  expect_equal(x$getMonitoringCosts(), pu_sim$monitoring_cost)
  expect_equal(x$getPlanningUnitsAmount(), nrow(pu_sim))
  expect_equal(x$getActionCosts(), dist_threats_sim$action_cost)
  expect_equal(x$getThreatNames(), threats_sim$name)
  expect_equal(x$getThreatsAmount(), nrow(threats_sim))

  # verify that object can be printed
  suppressMessages(print(x))
  suppressMessages(x)
})
