#' ---
#' title: "Econ436 Project 2, Stationarity in Stock Prices"
#' author: "Eric Weber"
#' date: "April 10, 2019"
#' output:
#'   pdf_document:
#'     fig_width: 9
#'     fig_height: 5
#' fontsize: 12pt
#' geometry: margin=0.1in
#' ---

#Initializing Useful Functions=============================================================

#given the type of the model, returns whether significant, and at what level
check_stationarity <- function(t_val, type, linear) {
  #type: "none", "intercept", "trend" (plus intercept)
  #linear: TRUE/FALSE
  
  if (is.na(t_val)) return(NA)  #if an invalid NA type is given, return the same
  
  if (linear) {
    if (type == "none") {
      if (t_val < -2.58) return("< 1%")
      if (t_val < -1.95) return("< 5%")
      if (t_val < -1.62) return("< 10%")
    } else if (type == "intercept") {
      if (t_val < -3.43) return("< 1%")
      if (t_val < -2.86) return("< 5%")
      if (t_val < -2.57) return("< 10%")
    } else if (type == "trend") {
      if (t_val < -3.96) return("< 1%")
      if (t_val < -3.41) return("< 5%")
      if (t_val < -3.12) return("< 10%")
    }
  } else {  #USE NONLINEAR CRITICAL VALUES
    #from (Kapetanios 2003) Table 1 "asymptotic critical values for nonlinear statistic"
    #http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.499.9407&rep=rep1&type=pdf
    if (type == "none") {
      if (t_val < -2.82) return("< 1%")
      if (t_val < -2.22) return("< 5%")
      if (t_val < -1.92) return("< 10%")
    } else if (type == "intercept") {
      if (t_val < -3.48) return("< 1%")
      if (t_val < -2.93) return("< 5%")
      if (t_val < -2.66) return("< 10%")
    } else if (type == "trend") {
      if (t_val < -3.93) return("< 1%")
      if (t_val < -3.40) return("< 5%")
      if (t_val < -3.13) return("< 10%")
    }
  }
  return("...")
}


#LINEAR REGRESS, INTERCEPT ONLY, NO TREND
get_tval_linregress_intOnly <- function(rp) {
  #input: relative price, [country-baseCountry] (vector)
  if (is.na(rp[1])) return(NA)  #if an invalid NA type, return the same
  
  n <- length(rp)
  d.rp <- rp[2:n] - rp[1:n-1]
  mod <- lm(d.rp[2:n] ~ rp[1:n-1] + d.rp[1:n-1])
  
  sum <- summary(mod)$coefficients
  t_val <- sum[8] #there are 3 rows (intercept+2 vars)... 3rd column is t-values
  return(t_val)
}


#do a linear regression with a cubed term
get_tval_nonlin_regress_intOnly <- function(rp) {
  #input: relative price, [country-base_country] (vector)
  if (is.na(rp[1])) return(NA)  #if an invalid NA type, return the same
  
  n <- length(rp)
  d.rp <- rp[2:n] - rp[1:n-1]
  rpcubed <- (rp^3)
  mod <- lm(d.rp[2:n] ~ rpcubed[1:n-1] + d.rp[1:n-1])
  
  sum <- summary(mod)$coefficients
  t_val <- sum[8] #there are 3 rows (intercept+2 vars)... 3rd column is t-values
  return(t_val)
}



#1) for countries with INCOMPLETE data, find row at which entries stop being NA
get_start_date <- function(vect) {
  #find the first index in a vector that is defined, i.e. not [NA]
  for (i in 1:length(vect)) {
    if (!is.na(vect[i])) return(i)  #if entry NOT NA, it's the valid starting index, return it
  }
}


#convert a country vector to a vector of relative, logged prices
get_rel_price <- function(country, baseCountry) {
  #inputs: STRING of the country, and the base country we're comparing it to
  if (country == baseCountry) return(NA)  #this makes sure "NA" is placed in the table 
  
  start <- get_start_date(allData[,country])  #get start index for the country
  logBASE <- log(allData[start:CONST_ROWS,baseCountry])  #get logged values of base country
  logCOUNT <- log(allData[start:CONST_ROWS,country]) #get logged values of main country
  return(logCOUNT - logBASE)  #return the difference
}


#unit root test, returns t-value
get_tval_dickeyFuller <- function(rp) {
  if (is.na(rp[1])) return(NA)  #if an invalid NA type, return the same
  
  test <- ur.df(rp, type="drift", lag=1, selectlag="Fixed")
  return(attributes(test)$teststat[1])
}


#START: INITIALIZE data=====================================================================
library(tseries)  #for ADF
library(urca)     #for unit root test
library(moments)  #for skewness() and kurtosis()
library(ggplot2)

options(digits = 4)
setwd("~/R")
allData <- read.csv("MSCI_ALL.csv",header=TRUE)

CONST_ROWS <- length(allData[,1])  #number of all entries in the main data set
CONST_COLS <- length(allData)  #number of all columns in the main data set

CONST_CNTRY_CNT <- length(allData)-1  #get the number of countries, all columns except the date
CONST_CNTRY_NAMES <- names(allData)[-1]  #array of the names

CONST_UK = "UNITED.KINGDOM"  #for safety, in case we start changing names


#Plotting the Pairwise Graphs================================================================

#storing plot objects in list:
#https://stackoverflow.com/questions/1820590/storing-plot-objects-in-a-list
#https://stackoverflow.com/questions/41883409/how-to-store-plots-in-a-list-when-inside-a-loop
#https://stackoverflow.com/questions/39799886/r-assigning-ggplot-objects-to-list-in-loop

#adjusting aesthetics, theme
#https://stackoverflow.com/questions/6528180/ggplot2-plot-without-axes-legends-etc
#https://stackoverflow.com/questions/28243514/ggplot2-change-title-size
#https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
#


library(gridExtra)  #for arranging the graphs into matrix
allData$Date <- as.Date(allData$Date, format = "%d-%b-%y")  #fix the date so ggplot recognizes

#the trick:
#1) use gridExtra package to arrange graphs
#2) you must pass data as a data.frame in order to accurately store plot objects in a list
#3) convert ggplot object to a "grob" using ggplotGrob(x)
#4) store the object as a vector inside a list: plotList[[x]] <-
#5) use gridArrange(plots[[i]], nrow=X, ncol=Y) to plot 
#6) plot multiple using: gridArrange(grobs=plots[i:n], nrow=X, ncol=Y)


#ell <- ggplot(NULL,aes(x=allData[,1], y=log(allData[,3]))) + geom_line()
#ell <- ell + geom_line(aes(x=allData[,1], y=log(allData[,4])),colour = "red")
#ell <- ell + xlab("Date") + ylab("Relative Stock Price") + ggtitle("title")
#print(ell)



# theme(axis.line=element_blank(),
#       axis.text.x=element_blank(),   #text = axis numbers
#       axis.text.y=element_blank(),
#       axis.ticks=element_blank(),   #tick marks only
#       axis.title.x=element_blank(),  #title = labels
#       axis.title.y=element_blank(),
#       legend.position="none",
#       panel.background=element_blank(),  #the visible grey panel
#       panel.border=element_blank(),
#       panel.grid.major=element_blank(),
#       panel.grid.minor=element_blank(),
#       plot.background=element_blank())
# 
# iplot <- ggplot(data=data.frame(x=allData$Date, y=log(allData[,"NORWAY"])),aes(x,y)) + geom_line()
# iplot <- iplot+geom_line(data=data.frame(x=allData$Date, y=log(allData[,"USA"])),aes(x,y),colour="red")
# iplot <- iplot + xlab("Date") + ylab("Relative Stock Price") + ggtitle("NORWAY")
# iplot <- iplot + theme(plot.title = element_text(size=9, hjust=0.5),
#                        axis.ticks=element_blank(),
#                        axis.title.x=element_blank(),
#                        axis.title.y=element_blank()) + coord_cartesian(ylim = c(4, 11))
# grid.arrange(ggplotGrob(iplot), nrow=2, ncol=2)



#' ##Log of Stock Index, Countries vs. USA

plist <- list()
count <- 1
for (country in CONST_CNTRY_NAMES) {
  if (country == "USA") next
  
  iplot <- ggplot(data=data.frame(x=allData$Date, y=log(allData[,country])),aes(x,y)) + geom_line()
  iplot <- iplot+geom_line(data=data.frame(x=allData$Date, y=log(allData[,"USA"])),aes(x,y),colour="red")
  iplot <- iplot + xlab("Date") + ylab("Relative Stock Price") + ggtitle(country)
  iplot <- iplot + theme(plot.title = element_text(size=9, hjust=0.5),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank()) + coord_cartesian(ylim = c(3.7, 11.3))
  if ((count-1)%%3 != 0) { #it's not a multiple of 3
    iplot <- iplot + theme(axis.text.y=element_blank())  #don't draw the y-axis numbers
  }
  
  plist[[count]] <- ggplotGrob(iplot)
  
  count <- count+1
}

count <- count-1  #so we don't reach into an index that doesn't exist for the next loop:
i = 1
while (i < count) {
  add <- 6-1
  if (i+add > count) add <- count-i
  grid.arrange(grobs=plist[i:(i+add)], nrow=2, ncol=3)
  i <- i+6
}


#' ***

#' ##Log of Stock Index, Countries vs. United Kingdom
plist <- list()
count <- 1
for (country in CONST_CNTRY_NAMES) {
  if (country == CONST_UK) next
  
  iplot <- ggplot(data=data.frame(x=allData$Date, y=log(allData[,country])),aes(x,y)) + geom_line()
  iplot <- iplot+geom_line(data=data.frame(x=allData$Date, y=log(allData[,CONST_UK])),aes(x,y),colour="red")
  iplot <- iplot + xlab("Date") + ylab("Relative Stock Price") + ggtitle(country)
  iplot <- iplot + theme(plot.title = element_text(size=9, hjust=0.5),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank()) + coord_cartesian(ylim = c(3.7, 11.3))
  if ((count-1)%%3 != 0) { #it's not a multiple of 3
    iplot <- iplot + theme(axis.text.y=element_blank())  #don't draw the y-axis numbers
  }
  
  plist[[count]] <- ggplotGrob(iplot)
  count <- count+1
}

count <- count-1  #so we don't reach into an index that doesn't exist for the next loop:
i = 1
while (i < count) {
  add <- 6-1
  if (i+add > count) add <- count-i
  grid.arrange(grobs=plist[i:(i+add)], nrow=2, ncol=3)
  i <- i+6
}
#' ***



#Summary Statistics Tables==================================================================

createTable_standard_stats <- function(baseCountry) {
  #input: the country you're comparing to
  
  #create the table
  tableCols <- c("mean","stdDev","skewness","kurtosis")
  df <- data.frame(matrix(ncol = length(tableCols), nrow = CONST_CNTRY_CNT), 
                   row.names = CONST_CNTRY_NAMES) 
  colnames(df) <- tableCols
  
  #fill in the table
  for (country in CONST_CNTRY_NAMES) {
    if (country != baseCountry) {
      vector <- get_rel_price(country, baseCountry) #get vector of the relative prices
      
      df[country,"mean"] <- mean(vector)
      df[country,"stdDev"] <- sd(vector)
      df[country,"skewness"] <- skewness(vector)
      df[country,"kurtosis"] <- kurtosis(vector)
    }
  }
  
  return(df)
}

#' ##Summary Statistics for log relative stock prices vs. USA
tableStand_USA <- createTable_standard_stats("USA")
tableStand_USA
#' ***

#' ##Summary Statistics for log relative stock prices vs. United Kingdom
tableStand_UK <- createTable_standard_stats(CONST_UK)
tableStand_UK
#' ***


#Initialize Tables for Stationarity Tests===================================================

#creating data frames to put the stationarity test data in

#Create the Table for USA comparison
tableCols <- c("USA ADF","ADF Result","USA Nonlinear","Nonlinear Result")
dfusa <- data.frame(matrix(ncol=length(tableCols), nrow=CONST_CNTRY_CNT), row.names=CONST_CNTRY_NAMES) 
colnames(dfusa) <- tableCols

#Create the Table for UK comparison
tableCols <- c("UK ADF","ADF Result","UK Nonlinear","Nonlinear Result")
dfuk <- data.frame(matrix(ncol = length(tableCols), nrow = CONST_CNTRY_CNT), row.names = CONST_CNTRY_NAMES) 
colnames(dfuk) <- tableCols


#FOR LOOP================================================================

for (country in CONST_CNTRY_NAMES) {  #where 'country' is a string of the country's name
  
  #1) get the relative price vectors
  
  rp_usa <- get_rel_price(country, "USA")
  rp_uk <- get_rel_price(country, CONST_UK)
  
  #2) call functions to perform the unit root tests on the relative prices, get t-values
  
  usa_lin <- get_tval_linregress_intOnly(rp_usa)
  usa_nonlin <- get_tval_nonlin_regress_intOnly(rp_usa)
  
  uk_lin <- get_tval_linregress_intOnly(rp_uk)
  uk_nonlin <- get_tval_nonlin_regress_intOnly(rp_uk)
  
  #3) Fill in the data frame tables with the test results
  #   check_stationarity() checks the t-stat against the appropriate t-critical value,
  #   based on the model parameters used. 
  
  dfusa[country,"USA ADF"]       <- usa_lin
  dfusa[country,"ADF Result"]    <- check_stationarity(usa_lin, "intercept", TRUE)
  dfusa[country,"USA Nonlinear"]    <- usa_nonlin
  dfusa[country,"Nonlinear Result"] <- check_stationarity(usa_nonlin, "intercept", FALSE)
  
  dfuk[country,"UK ADF"]         <- uk_lin
  dfuk[country,"ADF Result"]     <- check_stationarity(uk_lin, "intercept", TRUE)
  dfuk[country,"UK Nonlinear"]      <- uk_nonlin
  dfuk[country,"Nonlinear Result"]  <- check_stationarity(uk_nonlin, "intercept", FALSE)

} #end of main FOR loop.


#Printing the Stationarity Table Results=====================================================

#' ##ADF and Nonlinear Test for Stationarity with USA stock prices
#' Linear ADF Test with p=1, Nonlinear test with p=1
print(dfusa)
#write.csv(dfusa, 'usa_test_results.csv')
#' ***

#' ##ADF and Nonlinear Test for Stationarity with United Kingdom stock prices
#' Linear ADF Test with p=1, Nonlinear test with p=1
print(dfuk)
#write.csv(dfuk, 'uk_test_results.csv')
#' ***

#SubSample Analysis=========================================================================

#do the sub-sample analysis (before and after the financial crisis).
#separating data into two eras: before financial crisis, after financial crisis
#check stationarity of these two eras with linear and nonlinear test
#compare t-values to 
#[before], [after]
#[linear], [nonlinear]
#[t-value], [result]

#Comparison with USA: Before and after financial crisis (Sept. 2008)========================

#Create the Tables for USA comparison
#Before and After the Crisis

tableCols <- c("ADF Before","Before Result","ADF After","After Result")
dfusa_1 <- data.frame(matrix(ncol=length(tableCols), nrow=CONST_CNTRY_CNT), row.names=CONST_CNTRY_NAMES) 
colnames(dfusa_1) <- tableCols

tableCols <- c("Nonlin Before","Before Result","Nonlin After","After Result")
dfusa_2 <- data.frame(matrix(ncol=length(tableCols), nrow=CONST_CNTRY_CNT), row.names=CONST_CNTRY_NAMES) 
colnames(dfusa_2) <- tableCols

tableCols <- c("ADF FullSample","ADF Sub-Before","ADF Sub-After")
dfusa_3 <- data.frame(matrix(ncol=length(tableCols), nrow=CONST_CNTRY_CNT), row.names=CONST_CNTRY_NAMES) 
colnames(dfusa_3) <- tableCols

tableCols <- c("Nonlin FullSample","Nonlin Sub-Before","Nonlin Sub-After")
dfusa_4 <- data.frame(matrix(ncol=length(tableCols), nrow=CONST_CNTRY_CNT), row.names=CONST_CNTRY_NAMES) 
colnames(dfusa_4) <- tableCols

#sept 2008 = row # 466
#max n = 592
#so (592-127) = 465, the last entry in the [before financial crisis] era

for (country in CONST_CNTRY_NAMES) {  #where 'country' is a string of the country's name
  if (country == "USA") next #just skip it
  
  #1) get the relative price vectors
  
  rp <- get_rel_price(country, "USA")
  n <- length(rp)
  rp_b4 <- rp[1:(n-127)]
  rp_af <- rp[(n-126):n]
  
  
  #2) call functions to perform the unit root tests on the relative prices, get t-values
  
  lin_b4 <- get_tval_linregress_intOnly(rp_b4)
  lin_af <- get_tval_linregress_intOnly(rp_af)
  
  nonlin_af <- get_tval_nonlin_regress_intOnly(rp_b4)
  nonlin_b4 <- get_tval_nonlin_regress_intOnly(rp_af)
  
  
  #3) Fill in the data frame tables with the test results
  #   check_stationarity() checks the t-stat against the appropriate t-critical value,
  #   based on the model parameters used. 
  
  dfusa_1[country,"ADF Before"]       <- lin_b4
  dfusa_1[country,"Before Result"] <- check_stationarity(lin_b4, "intercept", TRUE)
  dfusa_1[country,"ADF After"]    <- lin_af
  dfusa_1[country,"After Result"]  <- check_stationarity(lin_af, "intercept", FALSE)
  
  dfusa_2[country,"Nonlin Before"]       <- nonlin_b4
  dfusa_2[country,"Before Result"] <- check_stationarity(nonlin_b4, "intercept", TRUE)
  dfusa_2[country,"Nonlin After"]    <- nonlin_af
  dfusa_2[country,"After Result"]  <- check_stationarity(nonlin_af, "intercept", FALSE)
  
  dfusa_3[country,"ADF FullSample"] <- dfusa[country,"ADF Result"]
  dfusa_3[country,"ADF Sub-Before"] <- dfusa_1[country,"Before Result"]
  dfusa_3[country,"ADF Sub-After"]  <- dfusa_1[country,"After Result"]
  
  dfusa_4[country,"Nonlin FullSample"] <- dfusa[country,"Nonlinear Result"]
  dfusa_4[country,"Nonlin Sub-Before"] <- dfusa_2[country,"Before Result"]
  dfusa_4[country,"Nonlin Sub-After"]  <- dfusa_2[country,"After Result"]
  
} #end of main FOR loop.

#' ##Relative to USA: ADF Test Before and After Financial Crisis
print(dfusa_1)
#' ##Relative to USA: Nonlinear Test Before and After Financial Crisis
print(dfusa_2)
#' ##Relative to USA: ADF Test Results in Full-Sample and Sub-Sample Analyses
print(dfusa_3)
#' ##Relative to USA: Nonlinear Test Results in Full-Sample and Sub-Sample Analyses
print(dfusa_4)

#write.csv(dfusa_3, "subsamp_usa.csv")
#write.csv(dfusa_4, "subsamp_usa_2.csv")


#Comparison with UK: Before and After Eurozone Crisis (approximately Sept. 2009)=============

#Not sure if the sept 2009 designation is correct. it was said to be "late 2009"

#Create the Tables for UK comparison
#Before and After the EuroZone Crisis

tableCols <- c("ADF Before","Before Result","ADF After","After Result")
dfuk_1 <- data.frame(matrix(ncol=length(tableCols), nrow=CONST_CNTRY_CNT), row.names=CONST_CNTRY_NAMES) 
colnames(dfuk_1) <- tableCols

tableCols <- c("Nonlin Before","Before Result","Nonlin After","After Result")
dfuk_2 <- data.frame(matrix(ncol=length(tableCols), nrow=CONST_CNTRY_CNT), row.names=CONST_CNTRY_NAMES) 
colnames(dfuk_2) <- tableCols

tableCols <- c("ADF FullSample","ADF Sub-Before","ADF Sub-After")
dfuk_3 <- data.frame(matrix(ncol=length(tableCols), nrow=CONST_CNTRY_CNT), row.names=CONST_CNTRY_NAMES) 
colnames(dfuk_3) <- tableCols

tableCols <- c("Nonlin FullSample","Nonlin Sub-Before","Nonlin Sub-After")
dfuk_4 <- data.frame(matrix(ncol=length(tableCols), nrow=CONST_CNTRY_CNT), row.names=CONST_CNTRY_NAMES) 
colnames(dfuk_4) <- tableCols

#sept 2009 = row # 478
#max n = 592
#so (592-115) = 477, the last entry in the [before eurozone crisis] era

for (country in CONST_CNTRY_NAMES) {  #where 'country' is a string of the country's name
  if (country == CONST_UK) next #just skip it
  
  #1) get the relative price vectors
  
  rp <- get_rel_price(country, CONST_UK)
  n <- length(rp)
  rp_b4 <- rp[1:(n-115)]
  rp_af <- rp[(n-115):n]
  
  
  #2) call functions to perform the unit root tests on the relative prices, get t-values
  
  lin_b4 <- get_tval_linregress_intOnly(rp_b4)
  lin_af <- get_tval_linregress_intOnly(rp_af)
  
  nonlin_af <- get_tval_nonlin_regress_intOnly(rp_b4)
  nonlin_b4 <- get_tval_nonlin_regress_intOnly(rp_af)
  
  
  #3) Fill in the data frame tables with the test results
  #   check_stationarity() checks the t-stat against the appropriate t-critical value,
  #   based on the model parameters used. 
  
  dfuk_1[country,"ADF Before"]       <- lin_b4
  dfuk_1[country,"Before Result"] <- check_stationarity(lin_b4, "intercept", TRUE)
  dfuk_1[country,"ADF After"]    <- lin_af
  dfuk_1[country,"After Result"]  <- check_stationarity(lin_af, "intercept", FALSE)
  
  dfuk_2[country,"Nonlin Before"]       <- nonlin_b4
  dfuk_2[country,"Before Result"] <- check_stationarity(nonlin_b4, "intercept", TRUE)
  dfuk_2[country,"Nonlin After"]    <- nonlin_af
  dfuk_2[country,"After Result"]  <- check_stationarity(nonlin_af, "intercept", FALSE)
  
  dfuk_3[country,"ADF FullSample"] <- dfuk[country,"ADF Result"]
  dfuk_3[country,"ADF Sub-Before"] <- dfuk_1[country,"Before Result"]
  dfuk_3[country,"ADF Sub-After"]  <- dfuk_1[country,"After Result"]
  
  dfuk_4[country,"Nonlin FullSample"] <- dfuk[country,"Nonlinear Result"]
  dfuk_4[country,"Nonlin Sub-Before"] <- dfuk_2[country,"Before Result"]
  dfuk_4[country,"Nonlin Sub-After"]  <- dfuk_2[country,"After Result"]
  
} #end of main FOR loop.

#' ##Relative to UK: ADF Test Before and After Eurozone Crisis
print(dfuk_1)
#' ##Relative to UK: Nonlinear Test Before and After Eurozone Crisis
print(dfuk_2)
#' ##Relative to UK: ADF Test Results in Full-Sample and Sub-Sample Analyses
print(dfuk_3)
#' ##Relative to UK: Nonlinear Test Results in Full-Sample and Sub-Sample Analyses
print(dfuk_4)

#write.csv(dfuk_3, "subsamp_uk.csv")
#write.csv(dfuk_4, "subsamp_uk_2.csv")


#END======================================================================================