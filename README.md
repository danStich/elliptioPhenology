# elliptioPhenology

Data and code for analysis of seasonal patterns in Eastern Elliptio (*Elliptio complanata*) reproductive phenology.

## Files

`eliptioData.csv` contains the current working data set for the analysis. These data are relocation events for individually marked Eastern Elliptio in Otego Creek, NY, during summer 2016.

  `tag`  = Individual tag ID
  
  `sex`  = sex of mussel (excludes apparent hermaphrodites)
  
  `status` = stage of gamete expression (not brooding, brooding[gamete] stage of females, or sperm expression for males)
  
  `gametes`  = a binary indicator of whether or not gametes were present in either sex
  
  `utm`  = UTM (NAD83) northings and eastings for individual mussels where recorded
 
  `date` = calendar date in m/d/Y format
  
<br>

`elliptioPhenology.R` is an R script that does some basic data manipulation, variable creation, and analysis of temporal trends in Eastern Elliptio gamete expression.
