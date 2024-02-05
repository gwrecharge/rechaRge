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
    nb_core = 1
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
