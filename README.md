# krill-proportional-recruitment
Proportional recruitment in Antarctic krill: data and calculations

This repository contains r-scripts and datasets on length-frequencies of Antarctic krill in U.S. AMLR summer trawl surveys, the diets of three penguin species (gentoos, adelies, and chinstraps), and, once data access is granted, observer data collected from the international krill fishery in CCAMLR Subarea 48.1. The r-scripts calculate length-based proportional recruitment (the annual means and standard deviations for each multiyear time series of the proportion of krill smaller than a specified length). The results are illustrated in the 'plots' directory.

The 'plots/contours' directory shows the how the distribution of krill lengths varied with month and survey strata in the U. S. AMLR trawls. Here the '1area1leg' subdirectory illustrates the combined distribution of four survey strata and two legs (January and February). The 'measured' plot shows the sample data and the 'scaled' plot shows the sample data scaled up to total catch (a maximum of 200 krill were measured per trawl). The '1area2legs' subdirectory shows the scaled plots for all strata combined but separated by leg. The '4areas1leg' subdirectory shows the distribution separately for each stratum but combined by leg. The '4areas2legs' subdirectory shows the distribution separated by both area and leg.

The 'plots_30mm', 'plots_35mm' and 'plots_40mm' subdirectories show two features of representing the full length distributions shown in the 'contours' subdirectory by a single number and its standard deviation (proportional recruitment). First, the temporal similarity in the peaks and troughs between the different data sources is visible. Second, the differences in proportional recruitment resulting from considering krill to be juveniles when they are less <= three alternative body lengths: 30mm, 35mm, or 40mm is also visible. Other cutoff lengths can also be explored but one of the datasets for Adelie penguin diets in the Palmer LTER, 'Adelie Penguin Diet Composition, Krill.csv', is reported in 5 mm bins. Lengths in the other datasets are reported to the nearest mm.

To reproduce the results in the 'plots' directory, download the seven r-scripts and the 'data' directory onto a local computer. Identify the local working directory in line 2 of the file '1_manage_scripts.r'. Running '1_manage_scripts.r' will call the other six r-scripts and produce the plots in the 'plots' directory.

All the figures in the 'plots' directory can be reproduced by changing the input values of three lines in '1_manage_scripts.r'.

Line 4 of '1_manage_scripts.r' identifies whether the user wants to combine the data from all four spatial strata (enter '1') or consider each stratum separately (enter '4').

Line 5 identifies whether the user wants to combine January and Februrary sampling (enter '1') or consider them separately (enter '2').

Line 6 identifies the length in mm below which krill are considered to be juveniles. These lengths must be either '30', '35', or '40' for the '4_lter_adelie.r' script to work properly (krill lengths are in 5 mm bins in the dataset used by that script).

# Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
