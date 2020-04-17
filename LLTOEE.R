#                  Title: Lab to Envelope (LTOEE)
#------------------------------------------------------
# Date: 10.2.2019
# Purpose: User can input test data and vizualize
#          compressor performance over an envelope
#          and specified run points as well as
#          calculate number of hours ran inside
#          and outside specified tolerance zones.
#------------------------------------------------------

#######################################################
# Instructions, Notes
#######################################################

# Instructions____________________________________________________________________________________

#   1. Ctrl+Enter to run code and select the data file in the popup window.
#   2. Change inputs as needed.
#__________________________________________________________________________________________________

# Notes____________________________________________________________________________________________

#   1. The plots created using plotly can be viewed in the "Viewer" tab of the lower right window.
#      To see all traces, click the arrow/window icon next to the broom in the Viewer tab.
#
#      The plots created using ggplot2 can be viewed in the "Plots" tab and can be enlarged by
#      clicking the "Zoom" option.
#__________________________________________________________________________________________________

#install refprop package
#install.packages("EmersonDataScience", repos = "C:/Users/E377433/Desktop/EmersonDataScience_0.1.0.tar.gz/", type="source", configure.args = "-no-multiarch")

#Select Packages
{
  library(readr)
  library(lubridate)
  library(plyr) #for bar plot labels
  library(plotly)
  library(reshape2)
  library(EmersonDataScience)
  library(parallel) #for detectCores()
  library(tidyverse) #for ggplot2 and dplyr
  library(EnvStats)
}

#############################################################################
#Load in Data and Plot to find Date/Time for Conditions
#############################################################################
{
  dataReceived <- read_csv(file.choose()) # Load in data to be manipulated.
  dataReceived$Time <- lubridate::mdy_hms(dataReceived$Time)
  
  dataReceived <- dataReceived[complete.cases(dataReceived), ]
  
  # INPUT SECTION BELOW
  dateTimeCol <- 1 # USER NEEDS TO SPECIFY THE COLUMN THAT HAS DATE TIME DATA
  skipEvery <- 2 # Describes the number of rows to skip for downsampling and creating a faster plot.
  
  # CALC SECTION BELOW
  index <- seq(1, nrow(dataReceived), by = skipEvery)
  dataDownSampled <- dataReceived[index,]
  
  #dataDownSampled[[dateTimeCol]] <- mdy_hms(dataDownSampled[[dateTimeCol]])
  
  skipVars <- colnames(dataDownSampled[1:dateTimeCol])
  plotVars <- melt(data = dataDownSampled, id.vars = skipVars, variable.name = 'Selected_Variables')
  
  plotAll <- plot_ly(data = plotVars, x = ~Time, y = ~as.numeric(value), color = ~Selected_Variables, type = "scattergl", mode = "lines")
  #plotAll
}

#############################################################################
#USER INPUTS
#############################################################################
{
  #Below inputs define the envelope
  envelopeNumPts <- 6 #Define number of points in envelope (repeat the first point at the end to get an enclosed envelope)
  envelope <- data.frame(EvapTemp = numeric(envelopeNumPts),
                         CondTemp = numeric(envelopeNumPts)
  )
  envelope$EvapTemp = c(-20,-20,35,40,40,-20) #Specify the Evap Condition Points
  envelope$CondTemp = c(80,95,145,145,80,80)
  
  #Input refrigerant name
  refrigerantName <- "R410A.mix"
  
  #Set to TRUE if pressure is in psig, FALSE if psia
  pressure_PSIG = TRUE
  
  #Specify the column numbers that contain the Evap/Cond Pressure data.
  disPressColNum <- 2
  sucPressColNum <- 3
  
  #Specify column number for amps and amp filter settings
  ampColNum <- 5
  ampFilter = TRUE #Turns filter on/off
  ampThreshold = 18.6 #Set Amp Threshold
  
  #Balance pressure filter settings
  balPressFilter = TRUE
  balPress.threshold = 10
  
  #Define if pressure is in gauge or atmospheric.
  pressureState = "gauge" #input either "gauge" or "atm"
  
  # Below sets the bin size for the 2D Histogram
  binSize = 25
  
  # Below defines the tolerance band inputs
  toleranceBandNum <- 1
  toleranceBands <- data.frame(percRange = numeric(toleranceBandNum),
                               evapPt = numeric(toleranceBandNum),
                               condPt = numeric(toleranceBandNum),
                               startTime = character(toleranceBandNum),
                               endTime = character(toleranceBandNum)
  )
  standardValues = TRUE #set to TRUE if using the values set by Reliability, set to FALSE if using user-defined tolerance values
  
  toleranceBands$percRange <- c(0.05)
  toleranceBands$evapPt <- c(40)
  toleranceBands$condPt <- c(160)
  
  # Below are start/end times for the C221 Compressor
  toleranceBands$startTime <- mdy_hms("01/16/2020 05:05:02")
  toleranceBands$endTime <-   mdy_hms("02/18/2020 03:56:50")
  
  #specifications for polynomial curve
  plotPolynomial1 = F
  if(plotPolynomial1 == TRUE){
    polynomialDegree1 = 2
    polynomialName1 = "93%"
    numPts1 = 19
    polynomialCoords1 <- data.frame(X1 = numeric(numPts1),
                                    Y1 = numeric(numPts1))
    polynomialCoords1$X1 <- c(-40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
    polynomialCoords1$Y1 <- c(107, 108.75, 110.5, 112.5, 114.5, 116.75, 119, 121.8, 124.6, 127.3, 130, 133.5, 137, 140.5, 144, 147.5,151, 155, 159)
    
    fit1 <- lm(Y1 ~ poly(X1,polynomialDegree1), data = polynomialCoords1)
  }
  
  plotPolynomial2 = F
  if(plotPolynomial2 == TRUE){
    polynomialDegree2 = 2
    polynomialName2 = "10%"
    numPts2 = 18
    polynomialCoords2 <- data.frame(X2 = numeric(numPts2),
                                    Y2 = numeric(numPts2))
    polynomialCoords2$X2 <- c(-40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45)
    polynomialCoords2$Y2 <- c(113, 115, 117, 119, 121, 123, 125, 127.5, 130, 132.75, 135.5, 138.5, 141.5, 144.75, 148, 151.7, 155, 159)
    
    fit2 <- lm(Y2 ~ poly(X2,polynomialDegree2), data = polynomialCoords2)
  }
}

#############################################################################
#DEVELOPER INPUTS
#############################################################################
{
  #conversion constant
  pressureAtm = 14.7 #psi
  
  #input the +/- psig value for reliability suction pressure
  suction_PSIG_RelValue <- 7
  #input the +/- psig value for reliability condenser pressure
  discharge_PSIG_RelValue <- 20
  #input the % value for reliability condenser pressure
  discharge_Perc_RelValue <- 0.05
}
#############################################################################
#PRESSURE/TEMP CONVERSION CALCULATIONS
#############################################################################
{
  #Initialize temperature columns
  dataReceived$`Evap Temp F` = ""
  dataReceived$`Cond Temp F` = ""
  
  #Finds number of cores available for use
  maxNumCore <- as.numeric(detectCores(logical=FALSE))
  nRow <- nrow(dataReceived)
  #Loop sets number of cores to use for parallel computing
  i = 1
  numCore = 1
  while(numCore < maxNumCore) { # while loop ensures we never set the function below to use a
    if(nRow > i^4) {            # number of cores that exceeds the max num cores we have available
      numCore <- i
      i = i + 1
    } else {
      break
    }
  }
  #Transforms data frame column lists into vectors for parameter requirements
  pressureValue_SUC <- unlist(dataReceived[,sucPressColNum])
  pressureValue_DIS <- unlist(dataReceived[,disPressColNum])
  
  #Pressure -> temperature conversion for Evap Temp F column
  #if(pressure_PSIG == TRUE) { #If gauge pressure, adds 14.7 to pressures
  dataReceived$`Evap Temp F` <- Par_refprope(prop_req='T', spec1='P', value1=pressureValue_SUC + 14.7, spec2='Q',
                                             value2=1, substance=refrigerantName, NCore=numCore, Round=2)
  dataReceived$`Cond Temp F` <- Par_refprope(prop_req='T', spec1='P', value1=pressureValue_DIS + 14.7, spec2='Q',
                                             value2=1, substance=refrigerantName, NCore=numCore, Round=2)
  
  #} else { #If atmospheric pressure, no changes necessary
  #  dataReceived$`Evap Temp F` <- Par_refprope(prop_req='T', spec1='P', value1=pressureValue_SUC, spec2='Q',
  #                                             value2=1, substance=refrigerantName, NCore=numCore, Round=2)
  #  dataReceived$`Cond Temp F` <- Par_refprope(prop_req='T', spec1='P', value1=pressureValue_DIS, spec2='Q',
  #                                             value2=1, substance=refrigerantName, NCore=numCore, Round=2)
  # }
  
  {
    #develop curve to extrapolate temps from pressures outside of conversion range for refprop
    #use 4th polynomial order
    dfModel <- data.frame(pressure = seq(from = 40, to = 665, by = 5), #pressures in PSIG
                       temperature = numeric(126))
    dfModel$temperature <- as.vector(Par_refprope(prop_req='T', spec1='P', value1=as.matrix(dfModel$pressure + 14.7), spec2='Q',
                                               value2=1, substance=refrigerantName, NCore=numCore, Round=2))
    
    # fit <- lm(temperature ~ poly(pressure, 4, raw = T), data = dfModel)
    fit <- lm(formula = dfModel$pressure ~ poly(x = dfModel$temperature, degree = 4, raw = TRUE))
    fit1 <- fit$fitted.values
    
    # newstuff <- data.frame(pressure = c(668,670,672,674,676,680,688))
    # newstuff[,2] <- predict.lm(fit, newstuff)
    
    plot(pressure ~ temperature, data = dfModel) 
    lines(fit ~ dfModel$temperature, col = 'red')
    
    coef <- fit$coefficients
  dataReceivedAll <- dataReceived
    listCond <- dataReceivedAll$`Cond Temp F`
  
    for(i in 1:length(dataReceivedAll$Time)){
      if(is.na(listCond[i])){
        # dataReceivedAll[i,7] <- predict(fit, dataReceivedAll[i,2])
        
         listCond[i] <- coef[1] + coef[2]*dataReceivedAll[i,2] +
           coef[3]*dataReceivedAll[i,2]^2 + coef[4]*dataReceivedAll[i,2]^3
      }
    }
    
    dataReceivedAll[,7] <- listCond
  }
}

#############################################################################
#Calculation Section for setting up the dataframes for the scatter plot and
#the 2D Histogram Plot
#############################################################################
{
  #sampleRate <- second(dataReceived$Time[2]) - second(dataReceived$Time[1])
  sampleRate <- 3
  # Below scans through the data and finds the date/times that are closest to the specified start/end time specified in the input section.
  # startTimeLocBands <- 0
  # endTimeLocBands <- 0
  # dateTimeFilteredDF_Length <- 0
  # for(i in 1:toleranceBandNum){
  #   startTimeLocBands[i] <- which.min(abs(as_datetime(toleranceBands$startTime[i]) - as_datetime(dataReceived$Time)))
  #   endTimeLocBands[i] <-  which.min(abs(as_datetime(toleranceBands$endTime[i])   - as_datetime(dataReceived$Time)))
  # }
  # # Below creates a new dataframe with just the data that is wanted to be inspected.
  # dateTimeFilteredDF <- dataReceived[startTimeLocBands[1]:endTimeLocBands[1],]
  # dateTimeFilteredDF$Condition <- paste0(toleranceBands$evapPt[1], "/", toleranceBands$condPt[1], " Scatter")
  # dataReceived$Condition = "" # This must be equal sign and not arrow.
  # dataReceived$Condition[startTimeLocBands[1]:endTimeLocBands[1]] <- paste0(toleranceBands$evapPt[1], "/", toleranceBands$condPt[1], " Scatter")
  # for (i in 2:toleranceBandNum){
  #   dataReceived$Condition[startTimeLocBands[i]:endTimeLocBands[i]] <- paste0(toleranceBands$evapPt[i], "/", toleranceBands$condPt[i], " Scatter")
  #   dateTimeFilteredDF <- rbind(dateTimeFilteredDF,  dataReceived[startTimeLocBands[i]:endTimeLocBands[i],])
  # }
  
  evapColNum <- grep("Evap Temp F", colnames(dataReceived))
  condColNum <- grep("Cond Temp F", colnames(dataReceived))
  
  # Creates new filtered data frame
  if(ampFilter == TRUE) {
    dataFiltered.1 <- dataReceived %>%
      filter(dataReceived[,ampColNum] > ampThreshold)
  } else {
    dataFiltered.1 <- dataReceived
  }
  if(balPressFilter == TRUE) {
    dataFiltered <- dataFiltered.1 %>%
      filter(abs(dataFiltered.1[,sucPressColNum] - dataFiltered.1[,disPressColNum]) > balPress.threshold)
  } else {
    dataFiltered <- dataFiltered.1
  }
  nRow_filtered <- nrow(dataFiltered)
}

#############################################################################
#TOLERANCE BAND CALCULATIONS
#############################################################################
{
  #Conversion of user-defined condition from temperature to pressure
  toPressure <- function(tolBandTemps){
    refprope(prop_req='P', spec1='T', value1=tolBandTemps, spec2='Q', value2=1, substance=refrigerantName)
  }
  
  toleranceBands$evapPressure <- lapply(toleranceBands$evapPt, toPressure)
  toleranceBands$condPressure <- lapply(toleranceBands$condPt, toPressure)
  
  #######This section runs if we want reliability values tolerance bands
  if(standardValues == TRUE) {
    relyTolBandVals <- data.frame(condOrig = numeric(toleranceBandNum),
                                  condPSIGMax = numeric(toleranceBandNum),
                                  condPercMax = numeric(toleranceBandNum),
                                  condPSIGMin = numeric(toleranceBandNum),
                                  condPercMin = numeric(toleranceBandNum),
                                  evapOrig = numeric(toleranceBandNum),
                                  evapMax = numeric(toleranceBandNum),
                                  evapMin = numeric(toleranceBandNum))
    
    relyTolBandVals$evapOrig <- toleranceBands$evapPressure #this is in psig already
    relyTolBandVals$condOrig <- toleranceBands$condPressure
    
    #get values for discharge/condenser pressure
    addPSIGcond <- function(inputCol) {
      inputCol + discharge_PSIG_RelValue
    }
    relyTolBandVals$condPSIGMax <- lapply(relyTolBandVals$condOrig, addPSIGcond)
    
    subtractPSIGcond <- function(inputCol) {
      inputCol - discharge_PSIG_RelValue
    }
    relyTolBandVals$condPSIGMin <- lapply(relyTolBandVals$condOrig, subtractPSIGcond)
    
    addPercent <- function(inputCol) {
      inputCol + (discharge_Perc_RelValue*inputCol)
    }
    relyTolBandVals$condPercMax <- lapply(relyTolBandVals$condOrig, addPercent)
    
    subtractPercent <- function(inputCol) {
      inputCol - (discharge_Perc_RelValue*inputCol)
    }
    relyTolBandVals$condPercMin <- lapply(relyTolBandVals$condOrig, subtractPercent)
    
    #get values for suction/evaporator pressure
    addPSIGevap <- function(inputCol) {
      inputCol + suction_PSIG_RelValue
    }
    relyTolBandVals$evapMax <- lapply(relyTolBandVals$evapOrig, addPSIGevap)
    
    subtractPSIGevap <- function(inputCol) {
      inputCol - suction_PSIG_RelValue
    }
    relyTolBandVals$evapMin <- lapply(relyTolBandVals$evapOrig, subtractPSIGevap)
    
    #choose lesser of two from set point (discharge pressures only)
    for(i in 1:toleranceBandNum) {
      relyTolBandVals$condMin[[i]] <- max(c(relyTolBandVals$condPSIGMin[[i]], relyTolBandVals$condPercMin[[i]]))
      relyTolBandVals$condMax[[i]] <- min(c(relyTolBandVals$condPSIGMax[[i]], relyTolBandVals$condPercMax[[i]]))
    }
    
    pEvapMaxTol <- relyTolBandVals$evapMax
    pCondMaxTOl <- relyTolBandVals$condMax
    pEvapMinTol <- relyTolBandVals$evapMin
    pCondMinTol <- relyTolBandVals$condMin
  }
  
  #######This section runs if we want user defined tolerance bands
  if(standardValues == FALSE) {
    #Find the pressure value that is tolband % away
    pEvapMaxTol <- (as.numeric(toleranceBands$percRange) * as.numeric(toleranceBands$evapPressure)) + as.numeric(toleranceBands$evapPressure)
    pCondMaxTOl <- (as.numeric(toleranceBands$percRange) * as.numeric(toleranceBands$condPressure)) + as.numeric(toleranceBands$condPressure)
    pEvapMinTol <- -(as.numeric(toleranceBands$percRange) * as.numeric(toleranceBands$evapPressure)) + as.numeric(toleranceBands$evapPressure)
    pCondMinTol <- -(as.numeric(toleranceBands$percRange) * as.numeric(toleranceBands$condPressure)) + as.numeric(toleranceBands$condPressure)
  }
  
  #Convert tol values to temp
  backToTemp <- function(tolBandPressures){
    refprope(prop_req='T', spec1='P', value1=tolBandPressures, spec2='Q', value2=1, substance=refrigerantName)
  }
  
  #Apply function and save to new lists
  tEvapMax <- lapply(pEvapMaxTol, backToTemp)
  tCondMax <- lapply(pCondMaxTOl, backToTemp)
  tEvapMin <- lapply(pEvapMinTol, backToTemp)
  tCondMin <- lapply(pCondMinTol, backToTemp)
  rm(pEvapMaxTol, pCondMaxTOl, pEvapMinTol, pCondMinTol)
  
  #Round values for displaying on plot later
  tEvapMax <- lapply(tEvapMax,round,2)
  tCondMax <- lapply(tCondMax,round,2)
  tEvapMin <- lapply(tEvapMin,round,2)
  tCondMin <- lapply(tCondMin,round,2)
  
  finalTolValues <- do.call(rbind, Map(data.frame, "TolBandNum"=1:toleranceBandNum, "CondMax"=tCondMax, "CondMin"=tCondMin,
                                       "EvapMax"=tEvapMax, "EvapMin"=tEvapMin))
  rm(tCondMax, tEvapMax, tEvapMin, tCondMin)
  
  #creates object for saving Condition input order
  namesList <- c()
  for(i in 1:toleranceBandNum){
    namesList[i] <- paste0(toleranceBands$evapPt[i], "/", toleranceBands$condPt[i])
  }
  
  #reorders columns for drawing tolerance bands on plot
  evapOrder <- finalTolValues[,c(4,4,5,5,4)]
  condOrder <- finalTolValues[,c(2,3,3,2,2)]
  XCoords <- as.data.frame(t(evapOrder))
  YCoords <- as.data.frame(t(condOrder))
  names(YCoords) <- c(namesList)
  names(XCoords) <- c(namesList)
  
  rm(evapOrder, condOrder)
}

#############################################################################
#2D Histogram for only desired conditions
#############################################################################

{#Below sets up the color options for the 2D Histogram plots.
  colorOptions <- data.frame(Invisible = 1:20, Visible = 1:20)
  colorOptions$Invisible[1] <- "rgba(73, 112, 46, 0)" # Dark Green
  colorOptions$Visible[1] <- "rgba(73, 112, 46, 1)"
  colorOptions$Invisible[2] <- "rgba(50, 0, 192, 0)" # Indigo
  colorOptions$Visible[2] <- "rgba(50, 0, 192, 1)"
  colorOptions$Invisible[3] <- "rgba(192, 14, 0, 0)" # Dark Red
  colorOptions$Visible[3] <- "rgba(192, 14, 0, 1)"
  colorOptions$Invisible[4] <- "rgba(192, 123, 0, 0)" # Gold
  colorOptions$Visible[4] <- "rgba(192, 123, 0, 1)"
  colorOptions$Invisible[5] <- "rgba(0, 191, 209, 0)" # Aqua
  colorOptions$Visible[5] <- "rgba(0, 191, 209, 1)"
  colorOptions$Invisible[6] <- "rgba(123, 0, 192, 0)" # Dark Purple
  colorOptions$Visible[6] <- "rgba(123, 0, 192, 1)"
  colorOptions$Invisible[7] <- "rgba(224, 0, 179, 0)" # Fuchsia
  colorOptions$Visible[7] <- "rgba(224, 0, 179, 1)"
  colorOptions$Invisible[8] <- "rgba(0, 192, 73, 0)" # Green
  colorOptions$Visible[8] <- "rgba(0, 192, 73, 1)"
  colorOptions$Invisible[9] <- "rgba(1, 183, 122, 0)" # Dark Teal
  colorOptions$Visible[9] <- "rgba(1, 183, 122, 1)"
  colorOptions$Invisible[10] <- "rgba(184, 184, 0, 0)" # Dark Yellow
  colorOptions$Visible[10] <- "rgba(184, 184, 0, 1)"
  colorOptions$Invisible[11] <- "rgba(8, 109, 43, 0)" # Blue
  colorOptions$Visible[11] <- "rgba(8, 109, 43, 1)"
  colorOptions$Invisible[12] <- "rgba(252, 132, 12, 0)" # Orange
  colorOptions$Visible[12] <- "rgba(252, 132, 12, 1)"
  colorOptions$Invisible[13] <- "rgba(1, 1, 121, 0)" # Navy
  colorOptions$Visible[13] <- "rgba(1, 1, 121, 1)"
  colorOptions$Invisible[14] <- "rgba(128, 75, 14, 0)" # Brown
  colorOptions$Visible[14] <- "rgba(128, 75, 14, 1)"
  colorOptions$Invisible[15] <- "rgba(255, 56, 241, 0)" # Pink
  colorOptions$Visible[15] <- "rgba(255, 56, 241, 1)"
  colorOptions$Invisible[16] <- "rgba(255, 81, 81, 0)" # Light Red
  colorOptions$Visible[16] <- "rgba(255, 81, 81, 1)"
  colorOptions$Invisible[17] <- "rgba(244, 165, 87, 0)" # Pale Orange
  colorOptions$Visible[17] <- "rgba(244, 165, 87, 1)"
  colorOptions$Invisible[18] <- "rgba(198, 151, 246, 0)" # Light Purple
  colorOptions$Visible[18] <- "rgba(198, 151, 246, 1)"
  colorOptions$Invisible[19] <- "rgba(140, 152, 170, 0)" # Grey
  colorOptions$Visible[19] <- "rgba(140, 152, 170, 1)"
  colorOptions$Invisible[20] <- "rgba(246, 228, 39, 0)" # Yellow
  colorOptions$Visible[20] <- "rgba(246, 228, 39, 1)"
}

{
  densityPlot <- plot_ly()
  #Below Creates 2D Histogram Plots for the desired conditions.
  for(i in 1:toleranceBandNum){
    densityPlot <- add_trace(p = densityPlot,
                             x = dataReceived$`Evap Temp F`,
                             #drop = TRUE is needed so Plotly can read the data.
                             y = dataReceived$`Cond Temp F`,
                             type = 'histogram2d',
                             histnorm = "percent",
                             nbinsx = binSize, nbinsy = binSize,
                             zsmooth = FALSE,
                             hoverlabel = list(bgcolor = colorOptions$Visible[i]),
                             colorscale = list(c(0, colorOptions$Invisible[i]), list(1, colorOptions$Visible[i])),
                             colorbar=list(title= paste('Percent Time Ran', toleranceBands$evapPt[i], "/", toleranceBands$condPt[i])),
                             opacity = 1,
                             name = paste0(toleranceBands$evapPt[i], "/", toleranceBands$condPt[i]))
  }
  #Below Adds Trace to draw the envelope.
  densityPlot <- add_trace(p = densityPlot,
                           x = envelope$EvapTemp, y = envelope$CondTemp, type = 'scatter', mode = 'lines',
                           name = "Envelope Pt", legendgroup = 'misc')
  #Below Adds Trace for data points within specified time span.
  densityPlot <- add_trace(p = densityPlot,
                           x = dataReceived$`Evap Temp F`, y = dataReceived$`Cond Temp F`,
                           type = 'scattergl', mode = 'markers', opacity = 1,
                           marker = list(size = 2, color = 'red'), legendgroup = 'scatter')
  #Below Adds Trace for desired running condition.
  densityPlot <- add_trace(p = densityPlot,
                           x = toleranceBands$evapPt, y = toleranceBands$condPt,
                           type = 'scatter', mode = 'markers', opacity = .75, name = "Desired Run Points",
                           marker = list(size = 10, color = "Blue"), legendgroup = 'misc') %>%
    #Below defines certain layout attributes of the plot.
    layout(
      title = 'Full Test',
      xaxis = list(title = 'Evap Temp, F'),
      yaxis = list(title = 'Cond Temp, F')
      )
  
  #Adds tolerance bands
  for(i in 1:toleranceBandNum){
    densityPlot <- add_trace(p = densityPlot, x = XCoords[[i]], y = YCoords[[i]],
                             type = 'scatter', mode = 'lines', name = paste0(namesList[[i]], " Tol Band"), legendgroup = "g")
  }
  
  if(plotPolynomial1 == TRUE){
    #add trace for polynomial curve 1
    densityPlot <- add_trace(p = densityPlot, x = polynomialCoords1$X1, y = polynomialCoords1$Y1,
                             type = 'scatter', mode = 'markers', marker = list(size = 6, color = "green"),
                             name = paste0(polynomialName1, " User Defined Points"), legendgroup = "curve 1") %>%
      add_trace(x = ~polynomialCoords1$X1, y = fitted(fit1), type = 'scatter', mode = 'lines', line = list(color = "green"),
                name = paste0(polynomialName1, " Reliability Strength Curve"), legendgroup = "curve 1")
  } #legendgroup = "somevariable" -> all traces with same "somevariable" will be linked together in the legend
  if(plotPolynomial2 == TRUE){
    #add trace for polynomial curve 2
    densityPlot <- add_trace(p = densityPlot, x = polynomialCoords2$X2, y = polynomialCoords2$Y2,
                             type = 'scatter', mode = 'markers', marker = list(size = 6, color = "purple"),
                             name = paste0(polynomialName2, " User Defined Points"), legendgroup = "curve 2") %>%
      add_trace(x = ~polynomialCoords2$X2, y = fitted(fit2), type = 'scatter', mode = 'lines', line = list(color = "purple"),
                name = paste0(polynomialName2, " Reliability Strength Curve"), legendgroup = "curve 2")
  }
  
  #Below sinds the Plot to R for viewing.
  densityPlot
}

#############################################################################
#2D Histogram for full filtered temperatures
#############################################################################
{
  {
    allTempsPlot <- plot_ly()
    allTempsPlot <- add_trace(p = allTempsPlot,
                              x = dataFiltered$`Evap Temp F`,
                              y = dataFiltered$`Cond Temp F`,
                              type = 'histogram2d',
                              histnorm = "percent",
                              nbinsx = binSize, nbinsy = binSize,
                              zsmooth = FALSE,
                              hoverlabel = list(bgcolor = colorOptions$Visible[19]),
                              colorscale = list(c(0, colorOptions$Invisible[19]), list(1, colorOptions$Visible[19])),
                              colorbar=list(title= "Concentration Gradient"),
                              opacity = 1,
                              name = "Concentration")
    #Below Adds Trace to draw the envelope.
    allTempsPlot <- add_trace(p = allTempsPlot,
                              x = envelope$EvapTemp, y = envelope$CondTemp, type = 'scatter',
                              mode = 'lines', name = "Envelope Pt", legendgroup = 'misc') %>%
      #Below Adds Trace for data points
      add_trace(x = dataFiltered$'Evap Temp F', y = dataFiltered$'Cond Temp F',
                type = 'scattergl', mode = 'markers', opacity = .45, name = "Temperature",
                marker = list(size = 2, color = "red")) %>%
      #Below Adds Trace for desired running condition.
      add_trace(x = toleranceBands$evapPt, y = toleranceBands$condPt,
                type = 'scatter', mode = 'markers', opacity = 1, name = "Desired Run Points",
                marker = list(size = 11, color = "blue"), legendgroup = 'misc')
    #Adds tolerance bands
    for(i in 1:toleranceBandNum){
      allTempsPlot <- add_trace(p = allTempsPlot, x = XCoords[[i]], y = YCoords[[i]],
                                type = 'scatter', mode = 'lines', name = paste0(namesList[[i]], " Tol Band"), legendgroup = 'bands')
    }
    if(plotPolynomial1 == TRUE){
      #add trace for polynomial curve 1
      allTempsPlot <- add_trace(p = allTempsPlot, x = polynomialCoords1$X1, y = polynomialCoords1$Y1,
                                type = 'scatter', mode = 'markers', marker = list(size = 6, color = "green"),
                                name = paste0(polynomialName1, " User Defined Points"), legendgroup = "curve 1") %>%
        add_trace(x = ~polynomialCoords1$X1, y = fitted(fit1), type = 'scatter', mode = 'lines', line = list(color = "green"),
                  name = paste0(polynomialName1, " Reliability Strength Curve"), legendgroup = "curve 1")
    } #legendgroup = "somevariable" -> all traces with same "somevariable" will be linked together in legend
    if(plotPolynomial2 == TRUE){
      #add trace for polynomial curve 2
      allTempsPlot <- add_trace(p = allTempsPlot, x = polynomialCoords2$X2, y = polynomialCoords2$Y2,
                                type = 'scatter', mode = 'markers', marker = list(size = 6, color = "purple"),
                                name = paste0(polynomialName2, " User Defined Points"), legendgroup = "curve 2") %>%
        add_trace(x = ~polynomialCoords2$X2, y = fitted(fit2), type = 'scatter', mode = 'lines', line = list(color = "purple"),
                  name = paste0(polynomialName2, " Reliability Strength Curve"), legendgroup = "curve 2")
    }
    allTempsPlot <- layout(allTempsPlot, title = 'Amp Filter Applied', xaxis = list(title = 'Evap Temp, F'),
                           yaxis = list(title = 'Cond Temp, F'))
  }
  allTempsPlot
}

#############################################################################
#TOLERANCE ZONE CALCULATIONS AND BAR PLOT
#############################################################################
#Calculate the number of points inside each tolerance zone
{
  {
    insideValue <- 0
    ptsInsideTol <- c()
    #finds the number of points inside the tolerance zones, saving the values to a list then resetting the value
    # evapmaxList <- as.list(finalTolValues$EvapMax)
    # evapminList <- as.list(finalTolValues$EvapMin)
    # condmaxList <- as.list(finalTolValues$CondMax)
    # condminList <- as.list(finalTolValues$CondMin)
    # evaptempList <- as.list(dateTimeFilteredDF$`Evap Temp F`)
    # condtempList <- as.list(dateTimeFilteredDF$`Cond Temp F`)
    
    evapmaxList <- 62.37
    evapminList <- 57.57
    condmaxList <- 162
    condminList <- 157.59
    evaptempList <- dataFiltered$`Evap Temp F`
    condtempList <- dataFiltered$`Cond Temp F`
    
    #counts pts inside for one condition
    dataNew <- dataFiltered %>%
      filter(dataFiltered$`Evap Temp F` <= evapmaxList &
               dataFiltered$`Evap Temp F` >= evapminList &
               dataFiltered$`Cond Temp F` <= condmaxList &
               dataFiltered$`Cond Temp F` >= condminList)
    
    ptsInsideTol <- nrow(dataNew)
    ptsTotal <- nrow(dataFiltered)
    ptsOutsideTol <- ptsTotal - ptsInsideTol
    
    # #for(j in 1:toleranceBandNum){
    # for(i in 1:nrow(dateTimeFilteredDF)){
    #   if(dateTimeFilteredDF$`Evap Temp F`[[i]] <= evapmaxList &
    #      dateTimeFilteredDF$`Evap Temp F`[[i]] >= evapminList &
    #      dateTimeFilteredDF$`Cond Temp F`[[i]] <= condmaxList &
    #      dateTimeFilteredDF$`Cond Temp F`[[i]] >= condminList){
    #     insideValue = insideValue + 1
    #   }
    # }
    # #ptsInsideTol <- insideValue
    # #insideValue <- 0
    # #}
    # rm(i, evapmaxList, evapminList, condmaxList, condminList, evaptempList, condtempList)
    
    # dateTimeFilteredDF <- dataReceived[startTimeLocBands[1]:endTimeLocBands[1],]
    # dateTimeFilteredDF$Condition <- paste0(toleranceBands$evapPt[1], "/", toleranceBands$condPt[1])
    # for (i in 2:toleranceBandNum){
    #   dataReceived$Condition[startTimeLocBands[i]:endTimeLocBands[i]] <- paste0(toleranceBands$evapPt[i], "/", toleranceBands$condPt[i])
    #   dateTimeFilteredDF <- rbind(dateTimeFilteredDF,  dataReceived[startTimeLocBands[i]:endTimeLocBands[i],])
    # }
    
    #forms data frame from matrix
    # output <- data.frame(namesList)
    # names(output) <- c("Condition")
    # #counts number of total points per condition for later calculations
    # output <- dateTimeFilteredDF %>%
    #   count(Condition)       ########## Sometimes this errors with "" and sometimes it works with ""
    # #arranges conditions in input order
    # output <- output %>%
    #   arrange(factor(Condition, levels=as.list(namesList)))
    # colnames(output)[2] <- "Total Points"
    # #adds columns to namesList dataframe
    # output <- cbind(output, "Points Within" = ptsInsideTol)
    # ptsOutsideTol <- c()
    # ptsOutsideTol <- output$`Total Points` - output$`Points Within`
    # output <- cbind(output, "Points Outside" = ptsOutsideTol)
    # #calculates num hours from num points
    
    oneCondDF <- data.frame(Pts_Inside = ptsInsideTol,
                            Pts_Total = ptsTotal,
                            Pts_Outside = ptsTotal - ptsInsideTol,
                            Hours_Inside = numeric(1),
                            Hours_Outside = numeric(1))
  
    oneCondDF$Hours_Inside <- ptsInsideTol * sampleRate / 3600
    oneCondDF$Hours_Outside <- ptsOutsideTol * sampleRate / 3600
    
    #hoursOutside <- ptsOutsideTol * sampleRate / 3600
    
    oneCondDF$Hours_Inside <- round(oneCondDF$Hours_Inside, 2)
    oneCondDF$Hours_Outside <- round(oneCondDF$Hours_Outside, 2)
    
    #output <- cbind(output, "Hours Within" = hoursInside)
    #output <- cbind(output, "Hours Outside" = hoursOutside)
    #selects column for printed namesList
    namesList_final <- oneCondDF[,c(4,5)]
    Condition <- "60/160"
    namesList_final <- cbind(Condition, namesList_final)
  }
  #print(namesList_final)
}

#############################################################################
#Bar Plot of time within/outside tolerance zones
#############################################################################
{
  {
    dataMelt <- melt(namesList_final,id.vars="Condition")
    
    dataMelt <- dataMelt %>%
      arrange(factor(Condition, levels=as.factor(namesList)))
    Hours <- dataMelt$value
    
    
    barPlot <- ggplot(data = dataMelt, aes(x=Condition, y=Hours, fill=variable)) +
      geom_bar(stat="identity", position = "dodge", width = 1) +
      geom_text(aes(label=value), vjust=1.6, color="black", position = position_dodge(0.9), size=3) +
      scale_fill_manual(values = c("forestgreen", "firebrick3"), name = "", labels = c("Hours Within Tolerance", "Hours Outside Tolerance")) +
      theme_minimal() + scale_x_discrete(limits=dataMelt$`Condition`)
    barPlot
  }
}

#############################################################################
#Pie Chart of time within/outside tolerance zones
#############################################################################
{
  #converts hours to percentages
  list <- as.data.frame(rowsum(dataMelt[,3], as.integer(gl(nrow(dataMelt), 2, nrow(dataMelt))))) #sums every two rows
  list <- cbind(list, "freq" = 2)
  list <- list %>% uncount(freq)
  dataMelt$percent = ""
  for(i in 1:nrow(list)){
    dataMelt[[i,"percent"]] <- dataMelt[i,"value"] / list[i,1] * 100
  }
  dataMelt$percent <- round(as.numeric(dataMelt[,"percent"]), digits = 1)
  
  #creates pie charts
  ggplot(dataMelt, aes(x = 1, y = percent, fill = variable)) +
    geom_bar(stat = "identity", width = 1) +
    facet_wrap(~Condition, ncol = 3) +
    coord_polar(theta = "y") +
    scale_fill_manual(values=c('forestgreen', 'firebrick3'), name = "", labels = c("Hours Within Tolerance", "Hours Outside Tolerance")) +
    theme_void() +
    geom_text(aes(y = percent, label = paste0(percent, "%")), color = "white", position = position_stack(vjust = 0.5))
}

#############################################################################
#NEW STUFF
#############################################################################

# objective:
# analyze the compressor's performance over time, use trend line to see pos or neg changes

#plot number of points inside tolerance zones over time
#decide time intervals to achieve both downsampling and showing trends
#calculate percent increase or decrease from previous point

################################
#inputs
numPts <- #num of desired points
  
  new <- data.frame(colName = numeric(numRows),
                    colName2 = numeric(numRows2))






