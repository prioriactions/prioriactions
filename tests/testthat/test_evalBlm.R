test_that("evaluate methods with random values", {
  skip_on_cran()
  skip_if_no_commercial_solvers_installed()

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
    amount = sample(1:10, 20, replace = TRUE))
  threats_sim <- data.frame(
    id = seq_len(1),
    blm_actions = sample(1:10, 1, replace = TRUE),
    name = letters[seq_len(1)])
  dist_threats_sim <- data.frame(
    pu = seq_len(10),
    threat = rep(1, 10),
    amount = rep(1, 10),
    action_cost = sample(1:10, 10, replace = TRUE),
    status = ifelse(status_action == 1, 0 , status_action))
  boundary_sim <- data.frame(
    bound,
    boundary = sample(1:10, nrow(bound), replace = TRUE))

  # eval different blm values
  blm_values = sample(0:10, 3, replace = FALSE)

  port <- suppressWarnings(evalBlm(pu = pu_sim,
                                   features = features_sim,
                                   dist_features = dist_features_sim,
                                   threats = threats_sim,
                                   dist_threats = dist_threats_sim,
                                   boundary = boundary_sim,
                                   values = blm_values,
                                   output_file = FALSE))

  # tests
  expect_s3_class(port, "Portfolio")
  expect_equal(length(port$data), 3)
  for (i in seq_along(port$data))
    expect_s3_class(port$data[[i]], "Solution")
  expect_type(port$getBlms(), "integer")
  expect_equal(port$getBlms(), blm_values)
  expect_equal(port$getNames(), paste0("Blm", blm_values))
})

test_that("evaluate solutions values", {
  skip_on_cran()
  skip_if_no_commercial_solvers_installed()

  # simulate data
  bound <- expand.grid(seq_len(10), seq_len(10))
  colnames(bound) <- c("id1", "id2")

  pu_sim <- data.frame(
    id = seq_len(10),
    monitoring_cost = c(0.1, 1, 2, 3, 2, 1, 2, 3, 2, 1),
    status = c(0, 0, 3, 2, 0, 3, 3, 0, 2, 2))
  features_sim <- data.frame(
    id = seq_len(2),
    target_recovery = c(0.2, 0.5),
    name = letters[seq_len(2)])
  dist_features_sim <- data.frame(
    pu = rep(seq_len(10), 2),
    feature = c(rep(1, 10), rep(2, 10)),
    amount = 1)
  threats_sim <- data.frame(
    id = seq_len(1),
    blm_actions = 0.5,
    name = letters[seq_len(1)])
  dist_threats_sim <- data.frame(
    pu = seq_len(10),
    threat = rep(1, 10),
    amount = rep(1, 10),
    action_cost = c(1.5, 2.5, 2.5, 1.5, 2.5, 2.5, 1.5, 2.5, 1.5, 2.5),
    status = c(0, 0, 3, 0, 0, 3, 3, 2, 2, 2))
  boundary_sim <- data.frame(
    bound,
    boundary = 0.5)

  # eval different blm values
  w <- capture_warnings(port <- evalBlm(pu = pu_sim,
                                        features = features_sim,
                                        dist_features = dist_features_sim,
                                        threats = threats_sim,
                                        dist_threats = dist_threats_sim,
                                        boundary = boundary_sim,
                                        values = c(0, 4),
                                        output_file = FALSE))

  # tests
  s1 <- port$data[[1]]$data
  s2 <- port$data[[2]]$data

  expect_equal(s1$sol[1:20], c(0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1))
  expect_equal(s2$sol[1:20], c(1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1))
  expect_gt(s2$objval, s1$objval)
  expect_match(w, "The blm argument was set to 0", all = FALSE)
})
