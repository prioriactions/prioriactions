test_that("new data-class", {
  # data
  x <- pproto(NULL, Data, data = list())
  # tests
  expect_s3_class(x, "Data")
})

test_that("get methods", {
  # simulate data
  status_pu <- sample(0:3, 10, replace = TRUE)
  weight <- ifelse(status_pu == 3, 0, 1)
  status_action <- sample(0:3, 10, replace = TRUE)*weight
  bound <- expand.grid(seq_len(10), seq_len(10))
  colnames(bound) <- c("id1", "id2")

  pu_sim <- data.frame(
    id = seq_len(10),
    monitoring_cost = c(0.1, sample(1:10, 9, replace = TRUE)),
    status = ifelse(status_pu == 1, 0 , status_pu))
  features_sim <- data.frame(
    id = seq_len(2),
    target_recovery = sample(1:10, 2, replace = TRUE),
    name = letters[seq_len(2)])
  dist_features_sim <- data.frame(
    pu = rep(seq_len(10), 2),
    feature = c(rep(1, 10), rep(2, 10)),
    amount = sample(1:3, 20, replace = TRUE))
  threats_sim <- data.frame(
    id = seq_len(1),
    blm_actions = runif(1),
    name = letters[seq_len(1)])
  dist_threats_sim <- data.frame(
    pu = seq_len(10),
    threat = rep(1, 10),
    amount = rep(1, 10),
    action_cost = sample(1:10,10, replace = TRUE),
    status = ifelse(status_action == 1, 0 , status_action))
  boundary_sim <- data.frame(
    bound,
    boundary = sample(1:10,nrow(bound), replace = TRUE))

  l <- list(
    pu = pu_sim,
    features = features_sim,
    dist_features = dist_features_sim,
    threats = threats_sim,
    dist_threats = dist_threats_sim,
    boundary = boundary_sim)

  x <- pproto(NULL, Data, data = l)

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
