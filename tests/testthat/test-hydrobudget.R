test_that("Water budget computation for a single cell", {
  df <- load_example_local_datasets()
  hb <- load_hydrobudget()
  simul_period <- c(2010, 2010)

  wb <- rechaRge::compute_recharge(
    hb,
    rcn = df$rcn,
    climate = df$climate,
    rcn_climate = df$rcn_climate,
    period = simul_period,
    workers = 1
  )

  wb_expected <- data.table::fread(file.path("data", "water_budget_79402.csv"))
  expect_true(all.equal(wb, wb_expected))
})

test_that("River flow processing", {
  df <- load_example_local_datasets()
  hb <- load_hydrobudget()

  flow <- rechaRge:::process_river_flow(
    hb,
    observed_flow = df$observed_flow,
    alpha_lyne_hollick = df$alpha_lyne_hollick
  )

  flow_month_expected <- data.table::fread(file.path("data", "observed_flow_month.csv.gz"))
  expect_true(all.equal(colnames(flow$observed_flow_month), colnames(flow_month_expected)))
  expect_equal(sum(flow$observed_flow_month$`23701`, na.rm = TRUE), sum(flow_month_expected$`23701`, na.rm = TRUE))
  expect_equal(sum(flow$observed_flow_month$`23701_bf`, na.rm = TRUE), sum(flow_month_expected$`23701_bf`, na.rm = TRUE))
})

test_that("Quality assesment", {
  df <- load_example_local_datasets()
  wb <- data.table::fread(file.path("data", "water_budget_79402.csv"))
  hb <- load_hydrobudget()
  simul_period <- c(2010, 2010)

  result <- rechaRge::evaluate_simulation_quality(
    hb,
    water_budget = wb,
    rcn_gauging = df$rcn_gauging,
    observed_flow = df$observed_flow,
    alpha_lyne_hollick = df$alpha_lyne_hollick,
    period = simul_period
  )
  simul <- as.list(result$simulation_metadata)
  simul$time <- NULL
  expected_simul <- list(
    station_id = 23702,
    cal_beg = 2010,
    cal_end = 2010,
    val_beg = 2011,
    val_end = 2010,
    T_m = 2.1,
    C_m = 6.2,
    TT_F = -17.6,
    F_T = 16.4,
    t_API = 3.9,
    f_runoff = 0.63,
    sw_m = 431,
    f_inf = 0.07,
    KGE_qtot_cal = 0.43740657,
    KGE_qbase_cal = 0.46935149,
    KGE_qtot_val = 0.43740657,
    KGE_qbase_val = 0.46935149,
    qtot_sim = 530.5,
    aet_sim = 459.7,
    gwr_sim = 93.3,
    KGE_mean_cal = 0.45337903,
    KGE_mean_val = 0.45337903
  )
  expect_equal(expected_simul, simul)
})
