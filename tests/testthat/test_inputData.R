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
    blm_actions = sample(1:10, 1, replace = TRUE),
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

  d <- suppressWarnings(inputData(pu = pu_sim,
                                  features = features_sim,
                                  dist_features = dist_features_sim,
                                  threats = threats_sim,
                                  dist_threats = dist_threats_sim,
                                  boundary = boundary_sim))

  # tests
  #pu
  expect_equal(d$data$pu$monitoring_cost, base::round(pu_sim$monitoring_cost, 3))
  expect_equal(d$data$pu$internal_id, seq_len(nrow(pu_sim)))
  #features
  expect_equal(d$data$features$target_recovery, base::round(features_sim$target_recovery, 3))
  expect_true(all(d$data$features$target_conservation == 0))
  expect_equal(d$data$features$internal_id, seq_len(nrow(features_sim)))
  expect_equal(d$data$features$name, letters[1:nrow(features_sim)])
  #dist_features
  expect_equal(d$data$dist_features$amount, base::round(dist_features_sim$amount, 3))
  expect_true(all(d$data$dist_features$pu %in% pu_sim$id))
  expect_true(all(d$data$dist_features$feature %in% features_sim$id))
  #threats
  expect_equal(d$data$threats$blm_actions, base::round(threats_sim$blm_actions, 3))
  expect_equal(d$data$threats$internal_id, seq_len(nrow(threats_sim)))
  expect_equal(d$data$threats$name, letters[1:nrow(threats_sim)])
  #dist_threats
  expect_equal(d$data$dist_threats$action_cost, base::round(dist_threats_sim$action_cost, 3))
  expect_true(all(d$data$dist_threats$pu %in% pu_sim$id))
  expect_true(all(d$data$dist_threats$threat %in% threats_sim$id))
  #sensitivity
  expect_true(all(d$data$sensitivity$delta1 == 0))
  expect_true(all(d$data$sensitivity$delta2 == 1))
  expect_true(all(d$data$sensitivity$delta3 == 0))
  expect_true(all(d$data$sensitivity$delta4 == 1))
  expect_true(all(d$data$sensitivity$internal_feature %in% features_sim$id))
  expect_true(all(d$data$sensitivity$internal_threat %in% threats_sim$id))
})
