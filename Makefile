sim1=simulation_HydroBudget_20230726T10:45
sim2=simulation_HydroBudget_20230727T17:09

# basique results comparision between different computations
diff:
	diff ${sim1}/01_bilan_spat_month.csv ${sim2}/01_bilan_spat_month.csv
	diff ${sim1}/02_bilan_unspat_month.csv ${sim2}/02_bilan_unspat_month.csv
	diff ${sim1}/03_bilan_unspat_month_23702.csv ${sim2}/03_bilan_unspat_month_23702.csv
	#diff ${sim1}/04_simulation_metadata.csv ${sim2}/04_simulation_metadata.csv
	diff ${sim1}/05_interannual_runoff_NAD83.tif ${sim2}/05_interannual_runoff_NAD83.tif
	diff ${sim1}/06_interannual_aet_NAD83.tif ${sim2}/06_interannual_aet_NAD83.tif
	diff ${sim1}/07_interannual_gwr_NAD83.tif ${sim2}/07_interannual_gwr_NAD83.tif