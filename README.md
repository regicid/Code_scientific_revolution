# Data and code of the paper "Quantifying the scientific revolution"

* The `data_*` files are lists from scientists and other creative disciplines, constructed through Wikipedia categories. It includes demographic informations, metrics of their importance (size of the page, number of translations and number of reference to this page in other Wikipedia pages). These files could and should be re-used for other projects, feel free! 
* `Plots.R` is, predictably, the code used for the plots
* Same for `Regressions.R`, which includes the code for statistical analysis
* In the `Environment_variables` folder, you will find the data we used for the GDP per capita of Countries, the urbanization rate and the number of universites. The sources are in the paper.
* `Pomp.R` was used to launch the Pomp analysis on a slurm computing cluster (the computations are too long to be run on a laptop). 
* `Pomp_analysis.R` produces the statistical analysis of the Pomp models (likelihood ratio tests, scaled coefficients table, models scoring).
