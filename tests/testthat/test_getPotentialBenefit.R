test_that("random values without conservation benefits, threat amount equal to 1 and no locked actions", {

  # simulate data
  bound <- expand.grid(seq_len(10), seq_len(10))
  colnames(bound) <- c("id1", "id2")

  pu_sim <- data.frame(
    id = seq_len(10),
    monitoring_cost = c(0.1, sample(1:10, 9, replace = TRUE)),
    status = rep(0, 10))
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
    status = rep(0, 10))
  boundary_sim <- data.frame(
    bound,
    boundary = sample(1:10,nrow(bound), replace = TRUE))

  d <- suppressWarnings(inputData(pu = pu_sim,
                                  features = features_sim,
                                  dist_features = dist_features_sim,
                                  threats = threats_sim,
                                  dist_threats = dist_threats_sim,
                                  boundary = boundary_sim))

  f <- getPotentialBenefit(d)

  # tests
  expect_s3_class(d, "Data")
  expect_equal(nrow(f), d$getFeatureAmount())
  expect_equal(f$dist[1], sum(d$data$dist_features$feature == 1, na.rm = TRUE))
  expect_equal(f$dist[2], sum(d$data$dist_features$feature == 2, na.rm = TRUE))
  expect_equal(f$dist, f$dist_threatened)
  expect_equal(f$maximum.recovery.benefit, f$maximum.benefit)
})

test_that("random values with conservation benefits, threat amount equal to 1 and no locked actions", {

  # simulate data
  bound <- expand.grid(seq_len(10), seq_len(10))
  colnames(bound) <- c("id1", "id2")

  pu_sim <- data.frame(
    id = seq_len(10),
    monitoring_cost = c(0.1, sample(1:10, 9, replace = TRUE)),
    status = rep(0, 10))
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
    blm_actions = sample(1:10, 1),
    name = letters[seq_len(1)])
  dist_threats_sim <- data.frame(
    pu = seq_len(10),
    threat = rep(1, 10),
    amount = c(0, rep(1, 9)),
    action_cost = sample(1:10,10, replace = TRUE),
    status = rep(0, 10))
  boundary_sim <- data.frame(
    bound,
    boundary = sample(1:10,nrow(bound), replace = TRUE))

  d <- suppressWarnings(inputData(pu = pu_sim,
                                  features = features_sim,
                                  dist_features = dist_features_sim,
                                  threats = threats_sim,
                                  dist_threats = dist_threats_sim,
                                  boundary = boundary_sim))

  f <- getPotentialBenefit(d)

  # tests
  expect_s3_class(d, "Data")
  expect_equal(nrow(f), d$getFeatureAmount())
  expect_equal(f$dist[1], sum(d$data$dist_features$feature == 1, na.rm = TRUE))
  expect_equal(f$dist[2], sum(d$data$dist_features$feature == 2, na.rm = TRUE))
  expect_true(all(f$dist >= f$dist_threatened))
  expect_true(all(f$maximum.benefit >= f$maximum.recovery.benefit))
  expect_true(any(f$maximum.conservation.benefit > 0))
})

test_that("compare benefits with threat amount not equal to 1 and no locked actions", {

  # simulate data
  bound <- expand.grid(seq_len(10), seq_len(10))
  colnames(bound) <- c("id1", "id2")

  pu_sim <- data.frame(
    id = seq_len(10),
    monitoring_cost = c(0.1, sample(1:10, 9, replace = TRUE)),
    status = rep(0, 10))
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
    amount = c(0, rep(1, 9)),
    action_cost = sample(1:10,10, replace = TRUE),
    status = rep(0, 10))
  boundary_sim <- data.frame(
    bound,
    boundary = sample(1:10,nrow(bound), replace = TRUE))
  dist_threats_sim2 <- data.frame(
    pu = seq_len(10),
    threat = rep(1, 10),
    amount = c(0,sample(1:20, 9)),
    action_cost = sample(1:10,10, replace = TRUE),
    status = 0)

  d1 <- suppressWarnings(inputData(pu = pu_sim,
                                   features = features_sim,
                                   dist_features = dist_features_sim,
                                   threats = threats_sim,
                                   dist_threats = dist_threats_sim,
                                   boundary = boundary_sim))

  d2 <- suppressWarnings(inputData(pu = pu_sim,
                                   features = features_sim,
                                   dist_features = dist_features_sim,
                                   threats = threats_sim,
                                   dist_threats = dist_threats_sim2,
                                   boundary = boundary_sim))

  f1 <- getPotentialBenefit(d1)
  f2 <- getPotentialBenefit(d2)

  # tests
  expect_equal(f1$feature, f2$feature)
  expect_equal(f1$dist, f2$dist)
  expect_equal(f1$dist_threatened, f2$dist_threatened)
  expect_equal(f1$maximum.conservation.benefit, f2$maximum.conservation.benefit)
  expect_true(all(f1$maximum.recovery.benefit >= f2$maximum.recovery.benefit))
  expect_true(all(f1$maximum.benefit >= f2$maximum.benefit))
})

test_that("compare benefits with threat amount equal to 1 and locked actions", {

  # simulate data
  status_pu <- sample(0:3, 10, replace = TRUE)
  status_pu <- ifelse(status_pu == 1, 0, status_pu)
  status_action <- sample(0:3, 10, replace = TRUE)
  status_action <- ifelse(status_action == 1, 0, status_action)
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
    status = rep(0, 10))
  boundary_sim <- data.frame(
    bound,
    boundary = sample(1:10,nrow(bound), replace = TRUE))
  pu_sim2 <- data.frame(
    id = seq_len(10),
    monitoring_cost = c(0.1, sample(1:10, 9, replace = TRUE)),
    status = ifelse(status_pu == 1, 0 , status_pu))
  dist_threats_sim2 <- data.frame(
    pu = seq_len(10),
    threat = rep(1, 10),
    amount = rep(1, 10),
    action_cost = sample(1:10,10, replace = TRUE),
    status = ifelse(status_action == 1, 0 , status_action))

  d1 <- suppressWarnings(inputData(pu = pu_sim,
                                   features = features_sim,
                                   dist_features = dist_features_sim,
                                   threats = threats_sim,
                                   dist_threats = dist_threats_sim,
                                   boundary = boundary_sim))

  d2 <- suppressWarnings(inputData(pu = pu_sim2,
                                   features = features_sim,
                                   dist_features = dist_features_sim,
                                   threats = threats_sim,
                                   dist_threats = dist_threats_sim2,
                                   boundary = boundary_sim))

  f1 <- getPotentialBenefit(d1)
  f2 <- getPotentialBenefit(d2)

  # tests
  expect_equal(f1$feature, f2$feature)
  expect_equal(f1$dist, f2$dist)
  expect_true(all(f1$dist_threatened >= f2$dist_threatened))
  expect_true(all(f1$maximum.recovery.benefit >= f2$maximum.recovery.benefit))
  expect_true(all(f1$maximum.benefit >= f2$maximum.benefit))
})
