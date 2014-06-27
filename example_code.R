##############################
##                          ##
##     USEFUL R PACKAGES    ##
##                          ##
##         DSSG 2014        ##
##         Joe Walsh        ##
##       June 3, 2014       ##
##                          ##
##############################

set.seed(1234)

#####################
##  LOAD DATA

  ## SAS, Stata, SPSS format
  
  # The 1998 Cape Verde Reproductive Health Survey provides data in SAS, Stata, and SPSS formats
  # only.  I will use this data to show how to import those file types.  Here's the URL:
  data_url <- "http://ghdx.healthdata.org/sites/default/files/record-attached-files/CPV_RHS_1998_FEMALES_DATA.zip"

  # Download the data
  temp <- tempfile()
  download.file(url=data_url, destfile=temp)

  # Unzip the data 
  unzipped_data <- unzip(zipfile=temp) 

  # unzipped_data is a list that includes the same data in different formats
  list(unzipped_data)

  # Load SAS-formatted data
  install.packages("sas7bdat")
  require(sas7bdat)
  SAS_data <- read.sas7bdat( file = unzipped_data[[7]] )
  head(SAS_data)

  # Load Stata-formatted data
  install.packages("foreign")
  require(foreign)
  Stata_data <- read.dta( file = unzipped_data[[2]] )
  head(Stata_data)  #[1:5, 1:5]

  # Load SPSS-formatted data
  SPSS_data <- read.spss( file = unzipped_data[[13]], to.data.frame = TRUE )
  head(SPSS_data)  #[1:5, 1:5]



  ## JSON, CSV from the web

  # DSSG fellows
  # I scraped the lists using Kimono (kimonolabs.com), but we could also scrape the pages 
  # using R.  See http://files.meetup.com/1625815/crug_scraper_05-01-2013.pdf.

  # JSON
    install.packages("rjson")
    require(rjson)

    # 2013 DSSG fellows
    url_dssg_2013 <- "http://www.kimonolabs.com/api/3pawko60?apikey=13ff6ad50d64d091e0a23328afd5c04e"
    dssg_fellows_2013_list <- fromJSON(file = url_dssg_2013)
    dssg_fellows_2013 <- as.data.frame(do.call(what = rbind, 
                                               args = dssg_fellows_2013_list$results$collection1))
      dssg_fellows_2013$Year <- 2013

    # 2014 DSSG fellows
    url_dssg_2014 <- "http://www.kimonolabs.com/api/d09ydb1o?apikey=13ff6ad50d64d091e0a23328afd5c04e"
    dssg_fellows_2014_list <- fromJSON(file = url_dssg_2014)
    dssg_fellows_2014 <- as.data.frame(do.call(what = rbind, 
                                               args = dssg_fellows_2014_list$results$collection1))
      dssg_fellows_2014$Year <- 2014

    # Combine 2013 and 2014 fellows
    dssg_fellows <- rbind(dssg_fellows_2013, dssg_fellows_2014)
    dssg_fellows

    # Carl Shan's record wasn't scraped correctly.  His name and field are
    # missing, and his school is in the wrong column.  Fix it.
    levels(dssg_fellows$Name)[ levels(dssg_fellows$Name)=="" ] <- "Carl Shan"
    levels(dssg_fellows$Field)[ levels(dssg_fellows$Field)=="University of California, Berkeley" ] <- "Statistics"
    levels(dssg_fellows$School)[ levels(dssg_fellows$School)=="" ] <- "University of California, Berkeley"

    dssg_fellows


  # CSV
    # 2013 DSSG fellows
    url_dssg_2013 <- "http://www.kimonolabs.com/api/csv/3pawko60?apikey=13ff6ad50d64d091e0a23328afd5c04e"
    dssg_fellows_2013 <- read.table(file = url_dssg_2013, skip = 1, header = TRUE, sep="," )
      dssg_fellows_2013$Year <- 2013

    # 2014 DSSG fellows
    url_dssg_2014 <- "http://www.kimonolabs.com/api/csv/d09ydb1o?apikey=13ff6ad50d64d091e0a23328afd5c04e"
    dssg_fellows_2014 <- read.table(file = url_dssg_2014, skip = 1, header = TRUE, sep="," )
      dssg_fellows_2014$Year <- 2014

    # Combine 2013 and 2014 fellows
    dssg_fellows <- rbind(dssg_fellows_2013, dssg_fellows_2014)
    dssg_fellows

    # Carl Shan's record wasn't scraped correctly.  His name and field are
    # missing, and his school is in the wrong column.  Fix it.
    levels(dssg_fellows$Name)[ levels(dssg_fellows$Name)=="" ] <- "Carl Shan"
    levels(dssg_fellows$Field)[ levels(dssg_fellows$Field)=="University of California, Berkeley" ] <- "Statistics"
    levels(dssg_fellows$School)[ levels(dssg_fellows$School)=="" ] <- "University of California, Berkeley"

    dssg_fellows





#####################
##  SQL

  # Install package
  install.packages("sqldf")
  require(sqldf)

  # Show the 2013 fellows
  sqldf("select * from dssg_fellows where Year==2013")

  # Show the 2014 fellows
  sqldf("select * from dssg_fellows where Year==2014")

  # Show Joseph Walsh
  sqldf("select * from dssg_fellows where Name=='Joseph Walsh'")

  Show Carnegie Mellon fellow names
  sqldf("select Name from dssg_fellows where School=='Carnegie Mellon University'")

  # Show in descending order how many fellows come from each school  
  sqldf("select School, count(*) from dssg_fellows group by School order by count(*) desc")

  # Show in descending order how many fellows come from each school each year
  sqldf("select School, Year, count(*) from dssg_fellows group by School, Year order by count(*) desc")




#####################
##  MLR

  # Load the package
  install.packages("mlr")
  require(mlr)

  # Load sample data
  install.packages("mlbench")
  library(mlbench)
  data(BreastCancer)
  head(BreastCancer)

  # Id is a unique character predictor; eliminate.
  class(BreastCancer$Id)
  BreastCancer$Id <- NULL

  # Data have missing values.  Impute using missForest.
  install.packages("missForest")
  library(missForest)
  BreastCancer[, 1 : ( ncol(BreastCancer) - 1 ) ] <- missForest( BreastCancer[, -ncol(BreastCancer)] )[[1]]

  # Try to create a supervised-classification task
  classif.task <- makeClassifTask(id = "BreastCancer", data = BreastCancer, target = "Class")
  
  # Try to create the task again
  classif.task <- makeClassifTask(id = "BreastCancer", data = BreastCancer, target = "Class")
  classif.task

  # Create the learner (identify the algorithm to use)
  lrn <- makeLearner("classif.kknn")
  lrn

  # Identify hyperparameter and its possible values
  ps <- makeParamSet( makeDiscreteParam("k", values = 1:10) )

  # Choose resampling option
  rdesc <-  makeResampleDesc("CV", iters = 3)

  # Make a tuning grid
  ctrl <- makeTuneControlGrid()

  # Tune the hyperparameter
  optimal.k <- tuneParams(learner=lrn, task=classif.task, resampling=rdesc, 
                          par.set=ps, control=ctrl, measure=mmce)[[3]]$k

  # Create new learner that uses tuned hyperparameter
  lrn.optim <- makeLearner("classif.kknn", 
                           par.vals = list(kernel = "triangular", k = optimal.k))

  # Train the learner
  mod <- train(lrn.optim, classif.task)
  mod

  # Evaluate model performance
  resample(lrn.optim, task=classif.task, resampling=rdesc, list(mmce, acc))






#####################
##  GGPLOT2

# Tom Schenk GitHub: https://github.com/coalitionforopendataeducation/ggplot2-introduction.git


# Filename:  1-introduction-to-basic-plots.R
# Title:	Introduction to Basic Plots in ggplot2
# Author:	Tom Schenk Jr. (adapted from Hadley Wickham) with modifications from Joe Walsh
# Created:	2014-06-03
# Modified:
# Libraries: ggplot2
# Notes: Heavily adpated from online tutorials provided by Hadley Wickham on http://docs.ggplot2.org. Was used in Chicago Data Visualization tutorials in 2012.


#INSTALLS ggplot2 PACKAGE. YOU WILL BE ASKED TO PICK A SERVER
install.packages("ggplot2")

#IF ALREADY INSTALLED, THIS WILL LOAD IT FOR USE
library(ggplot2)

head(movies)

# GEOMS
# http://docs.ggplot2.org/current/index.html


# SIMPLE HISTOGRAM USING QPLOT() "QUICK PLOT" FUNCTION
qplot(x=rating, data=movies, geom="histogram")

# SIMPLE DENSITY PLOT USING QPLOT()
qplot(x=rating, data=movies, geom="density")

# SIMPLE BOXPLOT USING QPLOT()
qplot(x=rating, data=movies, geom="boxplot")

# CAN CONTROL OTHER VISUAL ELEMENTS THROUGH OPTIONS
qplot(rating, data=movies, geom="histogram", binwidth=0.1)

# A WEIGHTED HISTOGRAM
qplot(rating, data=movies, weight=votes, geom="histogram", binwidth=0.1)

# NOW A SCATTER PLOT
qplot(mpaa, rating, data=movies)

# JITTER TO PREVENT OVER PLOTTING
qplot(mpaa, rating, data=movies, geom="jitter")

# BOX PLOTS
qplot(mpaa, rating, data=movies, geom=c("boxplot"))

# LAYER OTHER PLOT TYPES
qplot(mpaa, rating, data=movies, geom=c("boxplot", "jitter"))

# OOPS, OTHER WAY
qplot(mpaa, rating, data=movies, geom=c("jitter", "boxplot"))

# ADD OTHER DIMENSTIONS THROUGH COLORS
qplot(mpaa, rating, data=movies, geom=c("jitter"), color=factor(Action))


# ANOTHER DATA SET WITH CONTINUOUS DATA
head(mtcars)
qplot(wt, mpg, data=mtcars, color=factor(cyl))

# QPLOT() IS QUICK, BUT LIMITED IN OPTIONS. GGPLOT() GIVES YOU FULL CONTROL
# SAME PLOT AS BEFORE USING GGPLOT()
ggplot(mtcars, aes(x=wt, y=mpg, color=factor(cyl))) + geom_point()

# THE AES() OPTION CONTROLS THE AESTHETIC VALUES
ggplot(mtcars, aes(x=wt, y=mpg, color=factor(cyl), shape=factor(gear))) + geom_point()

# GGPLOT IS BUILT AROUND THE GRAMMAR OF GRAPHICS TO BUILD LAYERS
p <- ggplot(mtcars, aes(x=wt, y=mpg))
# THE FIRST P PRODUCES NOTHING
p

# THE USER LAYERS THE GRAPHICAL ELEMENTS
p + geom_point(aes(color=factor(cyl), shape=factor(gear)))
p + geom_point(aes(color=factor(cyl), shape=factor(gear))) + geom_jitter(position=position_jitter(height=5))

# CHANGE THE Y-AXIS
p + geom_point(aes(color=factor(cyl), shape=factor(gear))) + scale_y_log10()

# OVERLAY A LOESS ON A DOT PLOT
p + geom_point(aes(color=factor(cyl))) + stat_smooth()

# THE ORIGINAL p DOESN'T CONTAIN DATA, SO LINE WITHOUT POINTS IS:
p + stat_smooth()

# CAN ALSO USE LM OR GLM MODEL
p + stat_smooth(method="lm") + geom_point()

# MAKING THE LINE VERY UGLY
p + stat_smooth(fill="blue", size=2, alpha=1)
p + stat_smooth(fill="blue", size=2, alpha=.25)

# OVERLAY A LEAST-SQUARES REGRESSOR FOR EACH CYL
c <- ggplot(mtcars, aes(y=wt, x=mpg, color=factor(cyl)))
c + stat_smooth(method=lm) + geom_point()

# FACETING BREAKS THE GRAPH INTO GRIDS FOR ANALYSIS
p + geom_point() + facet_grid(. ~ cyl)

# CAN OVERLAY A LEAST-SQUARES REGRESSION FOR EACH FACET
p + geom_point() + stat_smooth(method=lm) + facet_grid(. ~ cyl)

# ADD SOME COLOR
c <- ggplot(mtcars, aes(y=wt, x=mpg, color=factor(cyl)))
c + geom_point() + stat_smooth(method=lm)

# EXTRAPOLATING THE LINES
c + geom_point() + stat_smooth(method=lm, fullrange=TRUE, alpha=0.1)



##########################
# SOME MEANINGFUL EXAMPLES

crime <- read.csv("/Users/User/Downloads/Crimes_-_2001_to_present.csv", header=TRUE)

# FIX DATA
str(crime) # Show data structures
crime$Date <- strptime(crime$Date, "%m/%d/%Y %H:%M") # Changes date from factor to proper POSIXlt date.
  min(crime$Date, na.rm=TRUE)
  max(crime$Date, na.rm=TRUE)
crime$Ward <- as.factor(crime$Ward) # Ward isn't a continuous variable, it's distinct sets
crime$Beat <- as.factor(crime$Beat) # Beat isn't a continuous variable, it's distinct sets

date.graph <- ggplot(crime, aes(x = Date)) # This prepares graphs with Date on the x-axis.
date.graph + geom_histogram() # Create a histogram with a date of crime on the x-axis. Shows the frequency of crime.

qplot(Date, data=crime, geom="histogram")

# THERE ARE LOTS OF GEOMETRIC SHAPES TO DISPLAY DATA
# SOME BASIC SHAPES ARE:
#
# geom_bar() a bar graph
# geom_histogram() a histogram
# geom_density() like a histogram, but shows periodic distribution
# geom_line() line graph
# geom_point() scatterplot
# geom_boxplot() Boxplot

# EXAMPLE DENSITY FUNCTION 
date.graph + geom_density() # SHOWS THE PERIODIC DISTRIBUTION OF CRIMES
date.graph + geom_density(size = 2) # SHOWS THE PERIODIC DISTRIBUTION OF CRIMES
date.graph + geom_density(adjust = 1/2) # ROUGH
date.graph + geom_density(adjust = 3) # SMOOTH
date.graph + geom_density(fill="blue") # A BLUE PLOT
date.graph + geom_density(fill="blue", alpha = .2) # BLUE AND TRANSPARENT
date.graph + geom_density(size = 2) + geom_histogram(aes(y=..density..)) # DRAW DENSITY AND HISTOGRAM, MUST ADJUST HISTOGRAM TO DENSITY
date.graph + geom_histogram(aes(y=..density..)) + geom_density(size = 2) # SAME AS PREVIOUS GRAPH, BUT THE DENSITY IS DRAWN ON TOP

# EXAMPLES OF THE BAR CHART
crime.type <- qplot(Primary.Type, data=crime, geom="bar") # COLUMN GRAPH OF CRIMES BY PRIMARY TYPE OF CRIME
crime.type + coord_flip() # Horizontal bar chart


# WE HAVE SOME TYPE-Os THAT ARE CREATING DUPLICATIONS. LET'S FIX THIS.
levels(crime$Primary.Type)
levels(crime$Primary.Type)[11] <- "INTERFERENCE WITH PUBLIC OFFICER"
levels(crime$Primary.Type)[22] <- "OTHER OFFENSE"

ggplot(crime, aes(x = Primary.Type)) + geom_bar() + coord_flip() # SAME HORIZONTAL BAR CHART AS ABOVE

# PLOT THE WARD AND ARRESTS
ward.graph <- qplot(Ward, data=crime, geom="bar") # Which ward has the most crimes?
ward.graph + coord_flip() # Graph it with horizontal bar chart
ward.arrests <- ggplot(crime, aes(x = Ward, fill = Arrest))
ward.arrests + geom_bar() + coord_flip()

# PLOT THE WARD AND CRIME TYPE
ward.crime.type <- ggplot(crime, aes(x= Ward, y = Primary.Type))
ward.crime.type + geom_point() # LOTS OF OVERPLOTTING
ward.crime.type + geom_point(position="jitter") # JITTER THE PLOT, BETTER, BUT NOT PERFECT

# PLOT THE WARD, CRIME TYPE, AND WHETHER THERE WAS AN ARREST
ward.crime.arrest <- ggplot(crime, aes(x = Ward, y = Primary.Type))
ward.crime.arrest + geom_point(aes(color = Arrest),position = "jitter") # Color shows whether there was an arrest
ward.crime.arrest + geom_point(aes(color = Arrest, shape = Domestic), position = "jitter") # Now we add a shape to determine if there was an arrest...but it's getting a bit thick.

# DO CERTAIN CRIMES HAPPEN DURING CERTAIN PERIODS?
arrest.time <- ggplot(crime, aes(x = Date))
arrest.time + geom_histogram() + facet_grid(Arrest ~ Domestic) 


# USEFUL RESOURCES
# http://docs.ggplot2.org/current/index.html
