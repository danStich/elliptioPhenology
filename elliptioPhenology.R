# Front-end needs -----
# . Package load -----
library(lubridate)
library(geosphere)
library(lme4)
library(dplyr)

# Data manipulation -----
# Read in the data
  ellip = read.csv('elliptioData.csv')
  head(ellip)
  
# Change the date format to something 
# we can use 
  ellip$date = as.Date(
    as.character(
      ellip$date
      ),
    format = "%m/%d/%Y"
    )

# Add column for day of year
  ellip$doy = yday(ellip$date)

# Square the doy
  ellip$doy2 = ellip$doy^2
    
# Standardize doy and doy2
  ellip$sdoy = as.vector(scale(ellip$doy))
  ellip$sdoy2 = as.vector(scale(ellip$doy2))
  
# Remove missing observations for gametes
  ellip = ellip[!is.na(ellip$gametes), ]
  
# Make a new data frame with just females
  #ellip = ellip[ellip$sex=='f', ]
  
# Data analysis -----
# Fit a model to describe changes in Elliptio gamete
# expression as a function of date with a random
# effect on the intercept for individual
  pmod = glmer(formula = gametes~sdoy+sdoy2+(1|tag),
               data=ellip,
               family = 'binomial'
               )
  summary(pmod)
  
# Model predictions -----
library(merTools)

# Make a sequence of new doy
  ndoy = seq(-3,
             15,
             by=.1)

# Make a sequence of new doy2
  ndoy2 = rep(0, length(ndoy))#seq(-3,
             # 15,
             # by=.1)  
    
# Make a new set of tags from the data
  ntag = 999#unique(ellip$tag)
  
# Make a df that contains lengths
# and a label for population
  newd = data.frame(
    tag = rep(ntag, length(ndoy)),
    sdoy = rep(ndoy, length(unique(ntag))),
    sdoy2 = rep(ndoy2, length(unique(ntag)))    
  )

# Simulate predictions from the relationship
# stored in the model fit using our new data
  PI <- predictInterval(merMod = pmod, newdata = newd, 
                        level = 0.95, n.sims = 100,
                        stat = "median", type="linear.prediction"
                        )
  
  apply(PI, 2, function(x) {exp(x)/(1+exp(x))})
  
  
# # Calculate percent gravid by month for raw data comparison
plot(x=ellip$doy, y=ellip$gametes,pch=21,bg=rgb(.1,.1,.1,.05),cex=2)
  check <- ellip %>% group_by(sdoy) %>%
         mutate(per=paste0(round(gametes/sum(gametes), 2), "%")) %>%
         ungroup
# 
# # Plot the raw data for the population of interest, i
#   plot(10,10,
#        ylim = c(0,1), xlim = c(-3,3), pch=21, bg='gray87',
#        cex=1, xlab='Day of year', ylab='Gravid'
#        )
# 
# # Predict new values for mass from lens
#   masses = effs$pop[i,1] + effs$pop[i,2]*lens
# # Add lines to the plot
#   lines(newd$length, PI$fit, lty=1, lwd=2, col='blue') # Mean
#   lines(newd$length, PI$upr, lty=1, lwd=2, col='red')  # Upper CI
#   lines(newd$length, PI$lwr, lty=1, lwd=2, col='red')  # Lower CI
#   
#   
  
  
   