# Front-end needs -----
# . Package load -----
packages <- c("dplyr", "lubridate", "geosphere", "lme4", "plyr")
lapply(packages, library, character.only = TRUE)

# Data manipulation -----
# Read in the data
  ellip = read.csv('elliptioData.csv', stringsAsFactors = FALSE)
  head(ellip)
  str(ellip)
    
# Change the date format to something 
# we can use 
  ellip$date = as.Date(
    as.character(
      ellip$date
      ),
    format = "%m/%d/%Y"
    )

# Add column for day of year
  ellip$doy = lubridate::yday(ellip$date)

# Square the doy
  ellip$doy2 = ellip$doy^2
    
# Standardize doy and doy2
  ellip$sdoy = as.vector(scale(ellip$doy))
  ellip$sdoy2 = as.vector(scale(ellip$doy2))
  
# Remove missing observations for gametes
  ellip = ellip[!is.na(ellip$gametes), ]
  
# Make a new data frame with just females
  #ellip = ellip[ellip$sex=='F', ]
  
# Data analysis -----
# Fit a model to describe changes in Elliptio gamete
# expression as a function of date with a random
# effect on the intercept for individual
  pmod = lme4::glmer(formula = gametes~sdoy+I(sdoy^2)+(1|tag),
                     data=ellip, family = 'binomial'
                     )
  summary(pmod)
  
# Model predictions -----

# Make a sequence of new doy
  ndoy = seq(-3, 3, by=.01)
  
# Make a df that contains lengths
# and a label for population
  newd = data.frame(
    tag = rep(ntag, length(ndoy)),
    sdoy = rep(ndoy, length(unique(ntag)))    
  )

# Simulate predictions from the relationship
# stored in the model fit using our new data
# Extract coefficients to an object with shorter name
  res = summary(pmod)$coefficients
  
# Get means of regression coeffs
  int = res[1,1]
  b1 = res[2,1]
  b2 = res[3,1]
  
  lint = res[1,1]-1.96*res[1,2]
  lb1 = res[2,1]-1.96*res[2,2]
  lb2 = res[3,1]-1.96*res[3,2]
  
  uint = res[1,1]+1.96*res[1,2]
  ub1 = res[2,1]+1.96*res[2,2]
  ub2 = res[3,1]+1.96*res[3,2]
  
# Make predictions  
  pred = invlogit(int+b1*newd$sdoy+b2*newd$sdoy^2)
  lpred = invlogit(lint+lb1*newd$sdoy+lb2*newd$sdoy^2)
  upred = invlogit(uint+ub1*newd$sdoy+ub2*newd$sdoy^2)  
  
# Plot raw data and predictions
  # Graphical parameters
  par(mar=c(5,5,1,1))

  # Raw data and plotting region set up
  plot(x=ellip$sdoy,
       y=ellip$gametes,
       pch=21,bg=rgb(.1,.1,.1,.05),
       cex=2, cex.lab=1.10,
       xlab='Day of year', ylab='p ( gametes | day )',
       xlim=c(-2, 2), ylim=c(0,1), xaxt='n', yaxt='n')
  
  # Lines for mean and 95% CI of predictions
  lines(newd$sdoy, lpred, col='red', lwd=2, lty=2)
  lines(newd$sdoy, upred, col='red', lwd=2, lty=2)
  lines(newd$sdoy, pred, col='blue', lwd=2, lty=1)
  
  # Add white points to cover up old points and lines
  points(x=ellip$sdoy, y=ellip$gametes, pch=21, bg='white', cex=2)

  # Add transparent points to increase opacity for
  # larger sample sizes
  points(x=ellip$sdoy,
       y=ellip$gametes,
       pch=21,bg=rgb(0.1,0.1,0.1,0.05),
       cex=2)
  
  # X-axis tick labels
  axis(side=1,
       at=seq(-2, 2, 0.5),
       labels= 
         #Get calendar date from ordinal date using
         # back-transformed values of doy from a sequence
         # of new standardized values
           chron::chron(
             round(
               seq(-2, 2, 0.5)*sd(ellip$doy)+mean(ellip$doy)
               ),
             origin.=c(month=1, day=1, year=2016),
             out.format="day mon"
             )
       )
  
  # Y-axis tick labels
  axis(side=2, las=2)

  # Add points for proportion of F brooding
    # Get proportion of brooders in each 
    # sample
    ellip$counts = 1
    pr = plyr::ddply(ellip[ellip$sex=='F',],
                     c('date', 'doy'),
                     summarize,
                     pBrooder = sum(brooding)/sum(counts)
                     )
    points(x=(pr[,2]-mean(ellip$doy))/sd(ellip$doy),
           y=pr[,3],
           pch=21,
           bg='white'
           )
    
  
  
  