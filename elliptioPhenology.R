# Front-end needs -----
# . Package load -----
# Make a list of packages to load
  packages <- c("dplyr",
                "lubridate",
                "geosphere", 
                "lme4",
                "plyr",
                "boot",
                "waterData",
                "piecewiseSEM",
                "AICcmodavg",
                "reshape"
                )
# Load them
  lapply(packages, library, character.only = TRUE)

# Data manipulation -----
# . Climate data -----
# Discharge data
  # Import discharge data from the USGS gage on
  # the Susquehanna River at Unadilla, NY
    usgs = waterData::importDVs("01500500",
                                       sdate="2016-01-01",
                                       edate="2016-12-31"
                                       )
  # Convert to cubic meters per second
    usgs$cms = usgs$val * 0.028316847
  # Change date columnn name to match the others
    names(usgs)[3] = 'date'
    
# NOAA climate data from www.ncdc.noaa.gov/cdo-web/
  # Read in the data file
    noaa = read.csv('noaaClimateData.csv')
  # Compute daily means from max and min
    noaa$tmean = apply(noaa[,c(8:9)],1,mean)
  # Change date to correct format
    noaa$date = as.Date(as.character(noaa$DATE), format="%m/%d/%Y")
  # Calculate degree day
    ddprep = noaa$tmean
    ddprep[ddprep<0] = 0
    noaa$dd = cumsum(ddprep)
  
# . Elliptio data
# Read in the data
  ellip = read.csv('elliptioData_aug.csv', stringsAsFactors = FALSE)
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

# . Data merge and covariates -----
# Merge the mussel data with the flow data
  climate = merge(usgs, noaa)      
  ellip = merge(ellip, climate)   

# Calculate photoperiod
  ellip$photo = daylength(lat=ellip$LATITUDE, doy = ellip$doy)
 
# Standardize day of year
  ellip$sdoy = as.vector(scale(ellip$doy))
         
# Standardize photoperiod
  ellip$sphoto = as.vector(scale(ellip$photo))
  
# Standardize temperature
  ellip$stmean = as.vector(scale(ellip$tmean))

# Standardize dd
  ellip$sdd = as.vector(scale(ellip$dd))

# Standardize discharge
  ellip$scms = as.vector(scale(ellip$cms))

# Remove missing observations for gametes
  # ellip = ellip[!is.na(ellip$gametes), ]
  
# Make a new data frame with just females
  # ellip = ellip[ellip$sex=='F', ]
    
# Data analysis -----
# Fit a list of models to describe changes in Elliptio gamete
# expression as a function of environmental factors with 
# a random effect on the intercept for individual

# Define an empty list
  pmods = list()
  
# Add each of the models to the list  
  pmods[[1]] = lme4::glmer(formula = gametes~stmean+I(stmean^2)+(1|tag),
                           data=ellip,
                           family = 'binomial'
                           )
  pmods[[2]] = lme4::glmer(formula = gametes~sdd+I(sdd^2)+(1|tag),
                           data=ellip,
                           family = 'binomial'
                           )
  pmods[[3]] = lme4::glmer(formula = gametes~sphoto+I(sphoto^2)+(1|tag),
                           data=ellip,
                           family = 'binomial'
                           )     
  pmods[[4]] = lme4::glmer(formula = gametes~scms+I(scms^2)+(1|tag),
                           data=ellip,
                           family = 'binomial'
                           )   
  pmods[[5]] = lme4::glmer(formula = gametes~stmean+I(stmean^2)+
                           scms+I(scms^2)+(1|tag),
                           data=ellip,
                           family = 'binomial'
                           )
  pmods[[6]] = lme4::glmer(formula = gametes~sdd+I(sdd^2)+
                           scms+I(scms^2)+(1|tag),
                           data=ellip,
                           family = 'binomial'
                           )
  pmods[[7]] = lme4::glmer(formula = gametes~sphoto+I(sphoto^2)+
                           scms+I(scms^2)+(1|tag),
                           data=ellip,
                           family = 'binomial'
                           )  
  pmods[[8]] = lme4::glmer(formula = gametes~(1|tag),
                           data=ellip,
                           family = 'binomial'
                           ) 
  

# Name the models in the list  
  # Loop through the list and name each
  # model using the formula from the fit
  for(i in 1:length(pmods)){
    names(pmods)[i] = as.character(summary(pmods[[i]])$call[2])
  }
  # Use gsub to eliminate response var from names
  names(pmods) = gsub(pattern = 'gametes ~ ',
                      replacement = '',
                      x = names(pmods)
                      )
  
# Make a model selection table for the models
  tab = aictab(pmods, names(pmods))
  tab
  
# Extract R-squared values based on:
# Nakagawa, S., and H. Schielzeth. 2013. A general
#      and simple method for obtaining R2 from
#      generalized linear mixed-effects models.
#      Methods in Ecology and Evolution 4(2):133-142.
  rtab = rsquared(pmods)
  
# Merge the AIC table with the R-sqaured values
  rtab$Modnames = rownames(rtab)
  mtab = merge(tab, rtab[,c(5,9)], by='Modnames')
  
# Write model selection stats to a file
  write.table(x=mtab,
              file='modelSelection.csv',
              row.names = FALSE,
              quote=FALSE,
              sep = ','
              )
  
# Model predictions -----

# Make a sequence of new doy
  ndd = seq(-3, 3, by=.01)
  
# Make a df that contains lengths
# and a label for population
  newd = data.frame(
    sdd = ndd    
  )

# Simulate predictions from the relationship
# stored in the model fit using our new data
  
# Extract coefficients from best model using
# row names from the model selection table
  res = summary(pmods[[as.numeric(row.names(tab))[1]]])$coefficients
  
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
  pred = boot::inv.logit(int+b1*newd$sdd+b2*newd$sdd^2)
  lpred = boot::inv.logit(lint+lb1*newd$sdd+lb2*newd$sdd^2)
  upred = boot::inv.logit(uint+ub1*newd$sdd+ub2*newd$sdd^2)  
  
jpeg(filename='Figure2.jpeg', height = 2160, 
     width = 3000, res=350)  
# Plot raw data and predictions
  # Graphical parameters
  par(mar=c(5,5,1,1))

  # Raw data and plotting region set up
  plot(x=ellip$sdd,
       y=ellip$gametes,
       pch=21,bg=rgb(.1,.1,.1, 0),
       col=rgb(.1,.1,.1, 0),
       cex=2, cex.lab=1.10,
       xlab='Accumulated thermal units (ATU)',
       ylab='p ( gametes | ATU )',
       xlim=c(-2, 2), ylim=c(0,1), xaxt='n', yaxt='n')
  
  # Add a shaded region for the CI
  xx = c((newd$sdd), rev(newd$sdd))
  yy = c(c(lpred), rev(c(upred)))
  polygon(xx, yy, col='gray87', border='gray87')
  
  # Lines for mean and 95% CI of predictions
  # lines(newd$sdd, lpred, col='red', lwd=2, lty=2)
  # lines(newd$sdd, upred, col='red', lwd=2, lty=2)
  lines(newd$sdd, pred, col='gray40', lwd=2, lty=1)
  
  # # Add white points to cover up old points and lines
  # points(x=ellip$sdd, y=ellip$gametes, pch=21, bg='white', cex=2)

  # # Add transparent points to increase opacity for
  # # larger sample sizes
  # points(x=ellip$sdd,
  #      y=ellip$gametes,
  #      pch=21,bg=rgb(0.1,0.1,0.1,0.05),
  #      cex=2)
  
  # X-axis tick labels
  axis(side=1,
       at=seq(-2, 2, 0.5),
       labels= 
         #Get calendar date from ordinal date using
         # back-transformed values of doy from a sequence
         # of new standardized values
           round(
             seq(-2, 2, 0.5)*sd(ellip$dd)+mean(ellip$dd)
             )
       )
  
  # Y-axis tick labels
  axis(side=2, las=2)

  # Add points for proportion of F brooding
    # Get proportion of brooders in each 
    # sample
    ellip$counts = 1
    pr = plyr::ddply(ellip,#[ellip$sex=='F',],
                     'dd',
                     summarize,
                     pBrooder = sum(brooding)/sum(counts),
                     pgamete = sum(gametes)/sum(counts)
                     )
    points(x=(pr[,1]-mean(ellip$dd))/sd(ellip$dd),
           y=pr[,3],
           pch=21,
           bg='black',
           cex=1.5
           )
    
  # Add the box to the outside
    box()
  dev.off()  
  
  
  
# Summary table for sampling events ----
# Get numbers of newly marked individuals,
# recaptured individuals, and total numbers
# for each sampling date.
  # Tabulate ndividual capture histories    
    caps <- reshape::cast(ellip, tag~date)
  # Create a copy and assign recaptures
    caps2 <- caps
    # For each mussel, if it was collected
    # in each time period and was previously
    # collected, then give it a one for recap
    for(i in 3:ncol(caps)){
      for(t in 1:nrow(caps)){
        if(caps[t,i]==1 & sum(caps[t, 2:(i-1)]) > 0){
          caps2[t, i] <- 1
        } else {
          caps2[t,i] <- 0
        }
      }
    }  
    # Remove captures from first time period
    caps2[,2] <- 0
    
  # Add up the capture and recapture matrices  
    caps3 <- caps[,2:ncol(caps)]+caps2[,2:ncol(caps2)]
    
  # Use the sum of caps and recaps to get 
  # numbers of marked and recaptured individuals 
  # in each time period, along with total
    cap <- c()
    recap <- c() 
    total <- c()
    for(i in 1:ncol(caps3)){
    cap[i] <- length(caps3[ ,i][caps3[,i]==1])
    recap[i] <- length(caps3[ ,i][caps3[,i]==2])
    total[i] <- sum(cap[i], recap[i])
    }
  
  # Put this all in a dataframe
    recaps <- data.frame(date=as.Date(colnames(caps)[2:ncol(caps)]),
                         cap=cap,
                         recap=recap,
                         total=total
                         )

    
# Summarize conditions on each day
  merged <- merge(ellip, recaps, by='date', all.x = T)  
    
  tempdf <- merged[,c(1,17,28,29,30,37:39)]  
  tempdf$date <- as.character(tempdf$date)
  sum.table <- ddply(tempdf,
                      .(date),
                      numcolwise(unique)
                      )
  write.csv(sum.table, 'summaryStats.csv')
  
    
  