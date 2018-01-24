#' Generate summary statistics for a data frame
#'
#' The function is an extension of the R "summary" function in base package. This function takes a data frame
#' or a vector as input, and then segregates the data in to several data frames based on the data type
#' (i.e. numeric, integer, factor, character, date, POSIXct) and then runs statitics for each of the data frames
#' seperately. The function calculates general descriptive statistics such as total row count, number of NA's,
#' min, max, mean, median, mode, standard deviation, variance, 1st Quadrant (25th percentile),
#' 3rd Quadrant (75th percentile), IQR, Possible Outliers. Applicable statistical measures are calculated for
#' each of the data types in the data frame, and then finally aggregated in to a data frame which is returned
#' as an output of the function
#'
#'@param x A data frame, or a vector of numeric, character, date etc.
#'@return A data frame with the summary statistics
#'@author Siva Chandrasekar
#'@examples
#'
#'## Example with numeric vector
#'a <- c(1,2,3,6,7,89)
#'summaryR(a)
#'
#'## Example with factor / character vector
#'b <- as.factor(c("Rob", "Lacy", "Rob", "Sekar", "Thomas"))
#'b <- c("Rob", "Lacy", "Rob", "Prasath", "Chandrasekar")
#'summaryR(b)
#'
#'## Example with date vector
#'
#'x <- c(as.Date("2012-12-01"), as.Date("1996-04-21"),as.Date("1987-03-22"),
#'as.Date("1995-01-13"),as.Date("1900-08-09"))
#'summaryR(x)
#'
#'## Example with data frame
#'
#'df <- data.frame(Name = c("Rob", "Michelle", "Siva", "Rob", "Prasath"),
#'Age = c(25,54,12,67,182),
#'DOB = c(as.Date("1993-12-01"), as.Date("1964-12-01"), as.Date("2006-05-28"),
#'        NA, as.Date("2000-05-28")),
#'TestScore = c(72,82.5,NA,79.86,86),
#'Residence = as.factor(c(NA,NA,NA, "USA", "India")), stringsAsFactors = FALSE)
#'
#'summaryR(df)
#'
#'## Transposing the summaryR data frame can give better readability when working with large data sets
#'t(summaryR(df))
#'
#'## Note that the output from summaryR is compactible with View()
#'
#'## Example for usage with column in the data set
#'
#'summaryR(df$TestScore)
#'
#'
#'@import stats
#'@export "summaryR"



#### SUMMARY STATS FUNCTION STARTS HERE ####

summaryR <- function(x){
  df <- x
  options(scipen = 999)
  #### MODE FUNCTION STARTS HERE ####
  modeInfo <- function(y){
    modeTbl <- table(y)
    names(modeTbl[which.max(modeTbl)])
  }

  ##### SEGREGATING FACT / CHR COLUMNS AND NUM / INT COLUMNS #####

  factCols <- NULL
  numCols <- NULL
  dateCols <- NULL
  logiCols <- NULL
  if(class(x) == "data.frame"){
    for (i in 1:ncol(x)) {

      if(class(x[,i]) %in% c("factor","character")){
        factCols <- c(names(x[i]),factCols)
      }
      if(class(x[,i]) %in% c("numeric","integer")) {
        numCols <- c(names(x[i]),numCols)
      }
      if(class(x[,i]) %in% c("Date", "POSIXct")){
        dateCols <- c(names(x[i]),dateCols)
      }
      if(class(x[,i]) == "logical"){
        logiCols <- c(names(x[i]),logiCols)
      }
      if(!is.null(factCols) & length(factCols) == 1){
        fact.df <- data.frame(ColName = x[,factCols])
        names(fact.df)[1] <- factCols
      }
      else if(!is.null(factCols) & length(factCols) > 1){
        fact.df <- x[,factCols]
      }
      if(!is.null(numCols) & length(numCols) == 1){
        num.df <- data.frame(numCols = x[,numCols])
        names(num.df)[1] <- numCols
      }
      else if(!is.null(numCols) & length(numCols) > 1){
        num.df <- x[,numCols]
      }
      if(!is.null(dateCols) & length(dateCols) == 1){
        date.df <- data.frame(ColName = x[,dateCols])
        names(date.df)[1] <- dateCols
      }
      else if(!is.null(dateCols) & length(dateCols) > 1){
        date.df <- x[,dateCols]
      }
      if(!is.null(logiCols) & length(logiCols) == 1){
        logi.df <- data.frame(logiCols = x[,logiCols])
        names(logi.df)[1] <- logiCols
      }
      else if(!is.null(logiCols) & length(logiCols) > 1){
        logi.df <- x[,logiCols]
      }
    }
  }   else if(class(x) %in% c("numeric","integer")){
    numCols <- "Var"
    num.df <- data.frame(Var = x)
  }   else if(class(x) %in% c("factor","character")){
    fact.df <- data.frame(Var = x)
    factCols <- "Var"
  }   else if(class(x) %in% c("Date", "POSIXct")){
    date.df <- data.frame(Var = x)
    dateCols <- "Var"
  }

  #### COLUMN SEGREGATION ENDS HERE

  #### STATS FOR NUM COLS BEGINS HERE ####

  if(is.null(numCols)) {
    num.df <- NULL
    numStat.df <- NULL
  }  else if(ncol(num.df) == 1){
    Variable <- names(num.df[1])
    `NA's` <- length(which(is.na(num.df[1]) == T))
    `Min` <- round(min(num.df[1], na.rm = T),2)
    `Max` <- round(max(num.df[1], na.rm = T),2)
    `Mean` <- round(unlist(lapply(num.df[1], mean, na.rm=T)),2)
    `Median` <- round(unlist(lapply(num.df[1], median, na.rm=T)),2)
    `Mode` <- modeInfo(num.df[[1]])
    `SD` <- round(unlist(lapply(num.df[1], sd, na.rm=T)),2)
    `Variance` <- round(unlist(lapply(num.df[1], var, na.rm=T)),2)
    `Percentile.25` <- round(quantile(num.df[1], na.rm = T,0.25),2)
    `Percentile.75` <- round(quantile(num.df[1], na.rm = T,0.75),2)
    `IQR` <- round(quantile(num.df[1], na.rm = T,0.75) - quantile(num.df[1], na.rm = T,0.25),2)
    Possible.Outliers <- num.df[1] [(num.df[1] < (Percentile.25 - (1.5*IQR))) | (num.df[1]  > (Percentile.75 + (1.5*IQR)))]

    numStat.df <- data.frame(Variable = Variable,
                             Class = class(num.df[[1]]),
                             Count = nrow(num.df),
                             `Levels` = NA,
                             `NA's`=`NA's`,
                             `Min` = `Min`,
                             `Max` = `Max`,
                             `Mean` = `Mean`,
                             `Median` = `Median`,
                             `Mode` = `Mode`,
                             `SD` = `SD`,
                             `Variance` = `Variance`,
                             `Percentile.25` = `Percentile.25`,
                             `Percentile.75` = `Percentile.75`,
                             `IQR` = `IQR`,
                             Possible.Outliers = length(Possible.Outliers[!is.na(Possible.Outliers)]))
  }   else if(ncol(num.df > 1)){

    for(j in 1:ncol(num.df)) {

      Variable <- names(num.df[j])
      `NA's` <- length(which(is.na(num.df[j]) == T))
      `Min` <- round(min(num.df[j], na.rm = T),2)
      `Max` <- round(max(num.df[j], na.rm = T),2)
      `Mean` <- round(unlist(lapply(num.df[j], mean, na.rm=T)),2)
      `Median` <- round(unlist(lapply(num.df[j], median, na.rm=T)),2)
      `Mode` <- modeInfo(num.df[[j]])
      `SD` <- round(unlist(lapply(num.df[j], sd, na.rm=T)),2)
      `Variance` <- round(unlist(lapply(num.df[j], var, na.rm=T)),2)
      `Percentile.25` <- round(quantile(num.df[j], na.rm = T,0.25),2)
      `Percentile.75` <- round(quantile(num.df[j], na.rm = T,0.75),2)
      `IQR` <- round(quantile(num.df[j], na.rm = T,0.75) - quantile(num.df[j], na.rm = T,0.25),2)
      Possible.Outliers <- num.df[j] [(num.df[j] < (Percentile.25 - (1.5*IQR))) | (num.df[j]  > (Percentile.75 + (1.5*IQR)))]
      if(j == 1){
        numStat.df <- data.frame(Variable = Variable,
                                 Class = class(num.df[[j]]),
                                 Count = nrow(num.df),
                                 `Levels` = NA,
                                 `NA's`=`NA's`,
                                 `Min` = `Min`,
                                 `Max` = `Max`,
                                 `Mean` = `Mean`,
                                 `Median` = `Median`,
                                 `Mode` = `Mode`,
                                 `SD` = `SD`,
                                 `Variance` = `Variance`,
                                 `Percentile.25` = `Percentile.25`,
                                 `Percentile.75` = `Percentile.75`,
                                 `IQR` = `IQR`,
                                 Possible.Outliers = length(Possible.Outliers[!is.na(Possible.Outliers)]))
      }
      if(j>1){
        numStat1 <- numStat.df
        numStat.df <- data.frame(Variable = Variable,
                                 Class = class(num.df[[j]]),
                                 Count = nrow(num.df),
                                 `Levels` = NA,
                                 `NA's`=`NA's`,
                                 `Min` = `Min`,
                                 `Max` = `Max`,
                                 `Mean` = `Mean`,
                                 `Median` = `Median`,
                                 `Mode` = `Mode`,
                                 `SD` = `SD`,
                                 `Variance` = `Variance`,
                                 `Percentile.25` = `Percentile.25`,
                                 `Percentile.75` = `Percentile.75`,
                                 `IQR` = `IQR`,
                                 Possible.Outliers = length(Possible.Outliers[!is.na(Possible.Outliers)]))
        numStat.df <- rbind(numStat.df,numStat1)
      }
    }

  }# STATS FOR NUM COLS ENDS HERE

  #### STATS FOR FACT COLS BEGINS HERE ####

  if(is.null(factCols)){ # STATS FOR FACT COLS BEGINS HERE
    fact.df <- NULL
    factStat.df <- NULL
  }   else if(ncol(fact.df) == 1) {
    `NA's` <- length(which(is.na(fact.df[1]) == T))
    Mode <- modeInfo(fact.df[[1]])
    `Levels` <- length(unique(fact.df[[1]]))
    factStat.df <- data.frame(Variable = names(fact.df[1]),
                              Class = class(fact.df[[1]]),
                              Count = nrow(fact.df),
                              `Levels` = `Levels`,
                              `NA's`=`NA's`,
                              `Min` = NA,
                              `Max` = NA,
                              `Mean` = NA,
                              `Median` = NA,
                              `Mode` = `Mode`,
                              `SD` = NA,
                              `Variance` = NA,
                              `Percentile.25` = NA,
                              `Percentile.75` = NA,
                              `IQR` = NA,
                              Possible.Outliers = NA)
  }   else if(ncol(fact.df) > 1){
    for(k in 1:ncol(fact.df)) {
      `NA's` <- length(which(is.na(fact.df[k]) == T))
      Mode <- modeInfo(fact.df[[k]])
      `Levels` <- length(unique(fact.df[[k]]))

      if(k == 1){
        factStat.df <- data.frame(Variable = names(fact.df[k]),
                                  Class = class(fact.df[[k]]),
                                  Count = nrow(fact.df),
                                  `Levels` = `Levels`,
                                  `NA's`=`NA's`,
                                  `Min` = NA,
                                  `Max` = NA,
                                  `Mean` = NA,
                                  `Median` = NA,
                                  `Mode` = `Mode`,
                                  `SD` = NA,
                                  `Variance` = NA,
                                  `Percentile.25` = NA,
                                  `Percentile.75` = NA,
                                  `IQR` = NA,
                                  Possible.Outliers = NA)
      }
      if(k>1){
        factStat1 <- factStat.df
        factStat.df <- data.frame(Variable = names(fact.df[k]),
                                  Class = class(fact.df[[k]]),
                                  Count = nrow(fact.df),
                                  `Levels` = `Levels`,
                                  `NA's`=`NA's`,
                                  `Min` = NA,
                                  `Max` = NA,
                                  `Mean` = NA,
                                  `Median` = NA,
                                  `Mode` = `Mode`,
                                  `SD` = NA,
                                  `Variance` = NA,
                                  `Percentile.25` = NA,
                                  `Percentile.75` = NA,
                                  `IQR` = NA,
                                  Possible.Outliers = NA)
        factStat.df <- rbind(factStat1,factStat.df)
      }

    } # STATS FOR FACT COLS ENDS HERE
  }

  #### STATS FOR DATE COLS BEGINS HERE ####

  if(is.null(dateCols)) {
    date.df <- NULL
    dateStat.df <- NULL
  }   else if(ncol(date.df) == 1){
    Variable <- names(date.df[1])
    `NA's` <- length(which(is.na(date.df[[1]]) == T))
    `Min` <- round(min(date.df[[1]], na.rm = T),2)
    `Max` <- round(max(date.df[[1]], na.rm = T),2)
    `Mean` <- NA
    `Median` <- NA
    `Mode` <- modeInfo(date.df[[1]])
    `SD` <- NA
    `Variance` <- NA
    `Percentile.25` <- NA
    `Percentile.75` <- NA
    `IQR` <- NA
    Possible.Outliers <- NA

    dateStat.df <- data.frame(Variable = Variable,
                              Class = class(date.df[[1]]),
                              Count = nrow(date.df),
                              `Levels` = NA,
                              `NA's`=`NA's`,
                              `Min` = `Min`,
                              `Max` = `Max`,
                              `Mean` = `Mean`,
                              `Median` = `Median`,
                              `Mode` = `Mode`,
                              `SD` = `SD`,
                              `Variance` = `Variance`,
                              `Percentile.25` = `Percentile.25`,
                              `Percentile.75` = `Percentile.75`,
                              `IQR` = `IQR`,
                              Possible.Outliers = NA)
  } else if(ncol(date.df > 1)){
    for(j in 1:ncol(date.df)) {

      Variable <- names(date.df[j])
      `NA's` <- length(which(is.na(date.df[[j]]) == T))
      `Min` <- round(min(date.df[[j]], na.rm = T),2)
      `Max` <- round(max(date.df[[j]], na.rm = T),2)
      `Mean` <- NA
      `Median` <- NA
      `Mode` <- modeInfo(date.df[[j]])
      `SD` <- NA
      `Variance` <- NA
      `Percentile.25` <- NA
      `Percentile.75` <- NA
      `IQR` <- NA
      Possible.Outliers <- NA
      if(j == 1){
        dateStat.df <- data.frame(Variable = Variable,
                                  Class = class(date.df[[j]]),
                                  Count = nrow(date.df),
                                  `Levels` = NA,
                                  `NA's`=`NA's`,
                                  `Min` = `Min`,
                                  `Max` = `Max`,
                                  `Mean` = `Mean`,
                                  `Median` = `Median`,
                                  `Mode` = `Mode`,
                                  `SD` = `SD`,
                                  `Variance` = `Variance`,
                                  `Percentile.25` = `Percentile.25`,
                                  `Percentile.75` = `Percentile.75`,
                                  `IQR` = `IQR`,
                                  Possible.Outliers = NA)
      }
      if(j>1){
        dateStat1.df <- dateStat.df
        dateStat.df <- data.frame(Variable = Variable,
                                  Class = class(date.df[[j]]),
                                  Count = nrow(date.df),
                                  `Levels` = NA,
                                  `NA's`=`NA's`,
                                  `Min` = `Min`,
                                  `Max` = `Max`,
                                  `Mean` = `Mean`,
                                  `Median` = `Median`,
                                  `Mode` = `Mode`,
                                  `SD` = `SD`,
                                  `Variance` = `Variance`,
                                  `Percentile.25` = `Percentile.25`,
                                  `Percentile.75` = `Percentile.75`,
                                  `IQR` = `IQR`,
                                  Possible.Outliers = NA)
        dateStat.df <- rbind(dateStat.df,dateStat1.df)
      }
    }

  }# STATS FOR DATE COLS ENDS HERE

  if(!is.null(numStat.df)){
    numStat.df <- data.frame(lapply(numStat.df ,as.character), stringsAsFactors = F)
    numStat.df[is.na(numStat.df)] <- "-"
  }
  if(!is.null(factStat.df)){
    factStat.df <- data.frame(lapply(factStat.df ,as.character), stringsAsFactors = F)
    factStat.df[is.na(factStat.df)] <- "-"
  }
  if(!is.null(dateStat.df)){
    dateStat.df <- data.frame(lapply(dateStat.df ,as.character), stringsAsFactors = F)
    dateStat.df[is.na(dateStat.df)] <- "-"
  }
  summaryStats.df <- rbind(numStat.df, factStat.df, dateStat.df)
  if(class(df) == "data.frame"){
    allStatCols <- c(numCols, dateCols, factCols)
    dfCols <- names(df)
    filteredCols <- dfCols[which(dfCols %in% allStatCols)]
    summaryStats.df <- summaryStats.df[match(filteredCols, summaryStats.df$Variable),]
  }
  row.names(summaryStats.df) <- NULL
  return(summaryStats.df)

} # SUMMARY STATS FUNCTION ENDS HERE
