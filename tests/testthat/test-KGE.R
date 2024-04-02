test_that("KGE computation for regular data",{
  sim <- c(0.5, 0.5, 10, 15, 0.5, 20, 25, 0.1, 15, 10)
  obs <- c(1, 0.1, 0.1, 20, 0.6, 30, 20, 0.5, 30, 8)
  kge_recharge <- rechaRge::KGE(sim, obs)

  kge_expected <- 0.6488572
  expect_true(all.equal(round(kge_recharge, 6), round(kge_expected, 6)))
})

test_that("KGE computation for wrong sim data format",{
  sim <- c(0.5, 0.5, 10, 15, 0.5, 20, 25, 0.1, 15, 10i)
  obs <- c(1, 0.1, 0.1, 20, 0.6, 30, 20, 0.5, 30, 8)
  expect_error(rechaRge::KGE(sim, obs),
               regexp = "Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')",
               fixed = TRUE)
})

test_that("KGE computation for wrong obs data format",{
  sim <- c(0.5, 0.5, 10, 15, 0.5, 20, 25, 0.1, 15, 10)
  obs <- c(1, 0.1, 0.1, 20, 0.6, 30, 20, 0.5, 30, 8i)
  expect_error(rechaRge::KGE(sim, obs),
               regexp = "Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')",
               fixed = TRUE)
})

test_that("KGE computation for data not the same length",{
  sim <- c(0.5, 0.5, 10, 15, 0.5, 20, 25, 0.1, 15, 10)
  obs <- c(1, 0.1, 0.1, 20, 0.6, 30, 20, 0.5, 30, 8, 20)
  expect_error(rechaRge::KGE(sim, obs), regexp = "Invalid argument: length(sim) != length(obs)", fixed = TRUE)
})

test_that("KGE computation with mean(obs)==0",{
  sim <- c(0.5, 0.5, 10, 15, 0.5, 20, 25, 0.1, 15, 10)
  obs <- c(1, -1, 0.1, -0.1, 30, -30, 20, -20, 30, -30)
  expect_warning(rechaRge::KGE(sim, obs), regexp = "Warning: mean(obs) == 0 ; Beta = Inf", fixed = TRUE)
})

test_that("KGE computation with sd(obs)==0",{
  sim <- c(0.5, 0.5, 10, 15, 0.5, 20, 25, 0.1, 15, 10)
  obs <- rep(0.1, 10)
  expect_warning(rechaRge::KGE(sim, obs), regexp = "Warning: sd(obs) == 0 ; Alpha = Inf", fixed = TRUE)
})

test_that("KGE computation with no matching pair obs and sim",{
  sim <- c(0.5, NA, 10, NA, 0.5, NA, 25, NA, 15, NA)
  obs <- c(NA, -1, NA, -0.1, NA, -30, NA, -20, NA, -30)
  expect_warning(rechaRge::KGE(sim, obs), regexp = "There are no pairs of 'sim' and 'obs' without missing values !", fixed = TRUE)
})
