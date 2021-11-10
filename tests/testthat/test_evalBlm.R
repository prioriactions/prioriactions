test_that("works", {
  skip_on_cran()

  # make data
  data(sim_pu_data, sim_features_data, sim_dist_features_data, sim_threats_data,
       sim_dist_threats_data, sim_sensitivity_data, sim_boundary_data)

  p1 <- inputData(sim_pu_data, sim_features_data, sim_dist_features_data, sim_threats_data,
            sim_dist_threats_data, sim_sensitivity_data, sim_boundary_data)




  # simulate data
  pu <- data.frame(id = seq_len(4), cost = c(1, 2, NA, 3))
  species <- data.frame(id = seq_len(2), name = letters[1:2])
  rij <- data.frame(pu = rep(1:4, 2), species = rep(1:2, each = 4),
                    amount = c(1, 2, 0, 0, 0, 0, 1, 1))
  # create problem
  s <- problem(pu, species, "cost", rij = rij) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 1)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve()
  # run tests
  expect_is(s, "data.frame")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

