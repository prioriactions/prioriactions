test_that("testing minimize Costs", {
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
    amount = c(0, rep(1, 9)),
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

  p <- suppressWarnings(problem(d))

  # tests
  ben <- getPotentialBenefit(d)
  expect_true(all(p$ConservationClass$data$features$target_recovery <= ben$maximum.recovery.benefit))
})

test_that("testing maximize Benefits", {
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
    amount = c(rep(1, 10)),
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

  budget <- sample(2:10, 1)
  p <- suppressWarnings(problem(d, budget = 0.1, model_type = "maximizeBenefits"))

  # tests
  actions_locked_in <- which(d$data$dist_threats$status == 2)
  locked_in_actions_cost <- sum(base::round(d$data$dist_threats$action_cost, 3)[actions_locked_in])
  pu_locked_in <- unique(c(which(d$data$pu$status == 2), actions_locked_in))
  locked_in_unit_cost <- sum(base::round(d$data$pu$monitoring_cost, 3)[pu_locked_in])
  expect_true(locked_in_unit_cost + locked_in_actions_cost <= p$data$args$budget)
})
