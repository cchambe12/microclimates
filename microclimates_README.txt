Microclimates Manuscript README
Treespotters, Harvard Forest (John O'Keefe's Route), and Common Garden


Main Goals: 
	(a) To determine if using microclimate data better predicts budburst or duration of vegetative risk over weather station data
	(b) If more urban sites are more sensitive to microclimates (ie., CG or Treespotters) than less urban sites (ie., Harvard Forest)

<><<><><><><><><><>
ORDER OF OPERATIONS
<><><><><><><><><>>
1a. Clean up the hobo logger data. To do this you must appropriately label
	the .csv files and store them in the correct folder (ie., either /microclimates/arb_data or microclimates/hf_data)
	*** How to label: [site][num. code].csv  ##To note, I also have the hobo owner initials next to the num.code after an underscore
	** eg., Arb001.csv ## Need the number codes to be 3 digits so they are appropriately ordered!!

1b. To clean the logger data, you then run the two R scripts:
	** microclimates/analyses/cleanloggerdata
	/clean_hobologgerdata_arb.R
	/clean_hobologgerdata_hf.R

2. Next run the analyses/cleaning/bb_cleanmergall.R code

3. Finally, run the analyses/calculating/calc_mergeall.R code - this takes a long time if running hobo logger data!!! 
	*** Make sure to indicate the type of flags you want to use

<><><><><><><><><><
Output files to use
<><><><><><><><><><

Weather station data: analyses/output/clean_gdd_bbanddvr.csv
Hobo data: analyses/output/clean_gdd_bbanddvr_hobo.csv

