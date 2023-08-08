test_that("Water budget computation for a single cell", {
  df <- load_example_datasets()
  param <- load_parameters()
  
  cell_id <- 79402
  input_rcn <- df$input_rcn[cell_ID == cell_id]
  input_rcn_gauging <- df$input_rcn_gauging[cell_ID == cell_id]
  simul_period <- c(2010, 2010)
  
  wb <- rechaRge::compute_hydrobudget(
    calibration = param,
    input_rcn = input_rcn,
    input_rcn_gauging = input_rcn_gauging,
    input_climate = df$input_climate,
    simul_period = simul_period,
    nb_core = 1
  )
  
  wb_expected <- data.table::fread(file.path("data", paste0("water_budget_", cell_id,".csv")))
  expect_true(all.equal(wb, wb_expected))
})