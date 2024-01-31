test_that("Water budget computation for a single cell", {
  df <- load_example_datasets()
  hb <- load_hydrobudget()
  
  cell_id <- 79402
  input_rcn <- df$input_rcn[cell_ID == cell_id]
  simul_period <- c(2010, 2010)
  
  wb <- rechaRge::compute_recharge(
    hb,
    rcn = input_rcn,
    climate = df$input_climate,
    rcn_climate = df$input_rcn_climate,
    period = simul_period,
    nb_core = 1
  )
  
  wb_expected <- data.table::fread(file.path("data", paste0("water_budget_", cell_id,".csv")))
  expect_true(all.equal(wb, wb_expected))
})