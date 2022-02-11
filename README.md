# krill-proportional-recruitment
Proportional recruitment in Antarctic krill: data and calculations

This repository contains r-scripts and datasets on length-frequencies of Antarctic krill in the diets of three penguin species (gentoos, adelies, and chinstraps), U.S. AMLR summer research surveys, and observer data collected from the international krill fishery in CCAMLR Subarea 48.1. The r-scripts calculate length-based proportional recruitment (the annual means and standard deviations of these means for the proportion of krill smaller than a specified length) in each dataset. The results are illustrated in the 'plots' directory.

To reproduce these results, download the seven r-scripts and the 'data' directory onto a local computer. Identify the local working directory in line 2 of the file '1_manage_scripts.r'. Running '1_manage_scripts.r' will call the other six r-scripts and produce the plots in the 'plots' directory.

All the figures in the 'plots' directory can be reproduced by changing the input values of three lines in '1_manage_scripts.r'.

Line 4 of '1_manage_scripts.r' identifies whether the user wants to combine the data from all four spatial strata (enter '1') or consider each stratum separately (enter '4').

Line 5 identifies whether the user wants to combine January and Februrary sampling (enter '1') or consider them separately (enter '2').

Line 6 identifies the length in mm below which krill are considered to be juveniles. These lengths must be either '30', '35', or '40' for the '4_lter_adelie.r' script to work properly (krill lengths are in 5 mm bins in the dataset used by that script).
