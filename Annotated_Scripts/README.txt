These are the annotated scripts that I have polished for reproducibility. 

1. "NMDS.rmd" contains the script for all of the NMDS analysis that I did. This script is broken up by vegetation class and species type (vascular, lichen, nonvascular). Each section contains code for park and time overlays, as well as canopy cover and elevation gradient when relevant. It also contains the code for the correlated species loading values for each NMDS axis for all NMDS ordinations. Lastly, the end of the script contains the stepdown analysis code I used a few months ago when we were evaluating removing rare species to assist with the overall NMDS ordinations converging. But when they wouldn't converge I ended up scrapping the overall (all veg classes together) NMDS ordinations and just making them by veg class. But I kept the code in case you wanted to do this in the future. 

2. "PERMANOVA.rmd" contains the script for all of the PERMANOVA analysis I did. It is broken up by vegetation class and species type (vascular, lichen, and nonvascular). It contains the script for the creation of the summary tables, balancing the data frames, creating the variable dataframes, and running all of the PERMANOVA and beta dispersion analysis. 

3. "Species_Turnover_Analysis.R" contains the script for all of the species richness analysis I did. This script contains the turnover proportion analysis using Codyn, the number of plots appeared/disappeared for each species, average turnover across plots script, calculating species richness across plots script, the creation of the overall species richness dataframes that I used in PERMANOVA and GLM analysis, and the GLM analysis. At the end there is also the code that I used for model comparison to decide which distribution to use (poisson vs. negative binomial) in the GLM analysis. 










