Charlotte Grasset-05/06/2024

Open with R studio the file “script-charlotte_list_of peaks”
R will ask you to load the GHG file, e.g. gga_er_2018-01-23_f0000.txt
R will then ask you to load the file with the injection times of the discrete samples, e.g. injectiontimesf000.txt
Check visually each peak in R studio and adjust the time in the file called with the injection times if needed (e.g. if the peak is cut)
R will create a file called “areas_peaks.csv” with the area of the discrete samples you injected. 
You will need to do a calibration curve (inject standards of similar volume than your injected samples, and with different gas concentrations) to convert the area of the peaks into a number of moles (conversion done in the file “area_to_ppm”)



Jake Beaulieu 09/10/2024

script-beaulieu_list_of_peaks.R
Rewrote Charlotte's code using tidyverse workflows and two different CRAN functions for calculating area under the curve. 
Charlotte's code assumes the structure of the Greenhouse Gas Analyzer data file. This likely works when only use one analyzer, but our group uses 5 analyzers and the files are not formatted identically. Updated code to select variables based on column names (which are consistent across multiple analyzers) to make it more robust to data file structure.
