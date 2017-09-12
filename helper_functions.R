# Helper functions to generalize charts and tables for LaTeX

## ---- get_fcv_comparators ---
get_fcv_comparators <- function(couName, countries){
  
  cou <- .getCountryCode(couName)
  # get country's FCV class
  fcv_class <- countries[countries$name == couName, 'FCVclass']
  
  # Get FCV comparators using logic provided by WBG IC-FCS team (Kunxiang Dao)
  if (fcv_class == "FCV"){
    
    # Check if country is SIDS or not
    if (countries[countries$name == couName, 'sids']){
      # If Country A is FCS also Small Island Developing States, then Comparators = SIDS FCS (top4)
      neighbors <- filter(countries, sids_long == "Yes", FCVclass == "FCV")$iso3
    } else {
      # If Country A is FCS but not Small Island Developing States, then Comparators= FCS in same region, and same income group (top4)
      couRegion <- as.character(countries[countries$iso3==cou,]$region)
      couIncomeLevel <- as.character(countries[countries$iso3==cou,]$incomeLevel)
      neighbors <- filter(countries, FCVclass == "FCV", region == couRegion, incomeLevel == couIncomeLevel)$iso3
    }
    
  } else if (fcv_class == "Past-FCV"){
    # If Country A = past-FCS, then Comparators=past-FCS in same income group (top4)
    couIncomeLevel <- as.character(countries[countries$iso3==cou,]$incomeLevel)
    neighbors <- filter(countries, FCVclass == "Past-FCV", incomeLevel == couIncomeLevel)$iso3
    
  } else if (fcv_class == "Non-FCV"){
    # If Country A = Non-FCS, then Comparators = 4 Non-FCS with least nominal GDP gap (countries with similar market size)
    couNominalGDP <- as.character(countries[countries$iso3==cou,]$latestNominalGDP)
    neighbors <- filter(countries, FCVclass == "Non-FCV") %>%
      mutate(GDPgap = abs(latestNominalGDP - as.numeric(couNominalGDP))) %>%
      arrange(-desc(GDPgap))
    neighbors <- head(neighbors, 5)$iso3
  }
  
  neighbors <- as.character(neighbors[!(neighbors==cou)]) # exclude the selected country
  return(neighbors)
}


## ---- figure_number_rank_only ----
figure_number_rank_only <- function(Report_data,reportConfig,couName,table, str_wrap_size=30,useRank=TRUE){      
  
  cou <- .getCountryCode(couName)
  ## Examples like Edward Tufte's sparklines:
  #table <- "combo1"
  data <- Report_data %>%
    filter(CountryCode==cou, Subsection2==table, !is.na(Observation)) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear)-1),Period),
           Observation = Observation/ifelse(is.na(Scale),1,Scale))
  
  data <- filter(data,!is.na(Observation))
  dataLast <- filter(data, Period == max(Period,na.rm=TRUE))
  # data
  
  dataPoint <- format(dataLast$Observation, digits=2, decimal.mark=".",
                      big.mark=",",small.mark=".", small.interval=3)
  # period
  dataPeriod <- dataLast$Period
  
  dataWorld <- filter(Report_data, Subsection2==table)
  dataWorld <- filter(dataWorld,!is.na(Observation))
  dataWorld <- dataWorld %>%
    group_by(iso2) %>%
    mutate(Period = max(Period,na.rm=TRUE)) %>%
    distinct(Period, .keep_all = TRUE) %>%
    as.data.frame()
  
  dataWorld <- arrange(dataWorld, desc(Observation))
  # rank in the world
  rank <- which(dataWorld$CountryCode == cou)
  rankedTotal <- nrow(dataWorld)
  
  indicator <- data$IndicatorShort[1]
  indicator <- str_wrap(paste0(indicator), width = str_wrap_size)
  unit <- data$Unit[1]
  
  if (nrow(data)>0){
    
    # Print the combo -----------------------------------------------
    par(family = 'serif',mfrow=c(5,1), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,2), #sets margin size for the figures
        oma=c(0,1,0,1)) #sets outer margin
    
    # print indicator name
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.0,indicator, col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=7)
    
    # print data point and rank
    if (useRank){
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1.0,paste0("Rank (",dataPeriod,")"), col="#818181", cex=5)
      # print data point and rank
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1.0,paste0(rank,"/",rankedTotal), col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=18)
    } else {
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1.0,paste0(unit, " (",dataPeriod,")"), col="#818181", cex=5)
      # print data point and rank
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1.0,dataPoint, col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=18)
    }
    
  } else {
    
    # Print the combo -----------------------------------------------
    par(family = 'serif',mfrow=c(5,1), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,2), #sets margin size for the figures
        oma=c(0,1,0,1)) #sets outer margin
    
    # print indicator name
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1,indicator, col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=10)
    graphics::text(1.5, 0.7,unit, col="#818181", cex=5)
    # print data point and rank
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 0.95,"No data available", col="#818181", cex=10)
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    # graphics::text(1.5, 1.1,paste0("(Rank: /",rankedTotal,")"), col="#818181", cex=7)
    # plot sparkline  
    # par(family = 'serif',#sets number of rows in space to number of cols in data frame x
    #     mar=c(0,5,0,5))#sets margin size for the figures
    #oma=c(0,4,0,4)) #sets outer margin
    
  } 
  
}

## ---- figure_sparkline ----
figure_sparkline <- function(Report_data,reportConfig,couName,table,rankBig=FALSE){      
  
  cou <- .getCountryCode(couName)
  ## Examples like Edward Tufte's sparklines:
  #table <- "combo1"
  data <- Report_data %>%
    filter(CountryCode==cou, Subsection2==table, !is.na(Observation)) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear)-1),Period),
           Observation = Observation/ifelse(is.na(Scale),1,Scale))
  
  if (table == "figureFin2"){
    data <- filter(data,Observation > 0)
    dataLast <- filter(data, Period == max(Period,na.rm=TRUE))
    #dataLast$Observation <- ifelse(dataLast$Observation>1000000,dataLast$Observation/1000000,dataLast$Observation)
  # } else if (table == "figure3" | table =="figure6"){
  #   data <- filter(data,!is.na(Observation))
  #   dataLast <- filter(data, Period == max(data[data$Period!=max(data$Period,na.rm=TRUE), "Period"]))
  } else {
    data <- filter(data,!is.na(Observation))
    dataLast <- filter(data, Period == max(Period,na.rm=TRUE))
  }
  # data
  
  dataPoint <- format(dataLast$Observation, digits=2, decimal.mark=".",
                      big.mark=",",small.mark=".", small.interval=3)
  # period
  dataPeriod <- dataLast$Period
  
  dataWorld <- filter(Report_data, Subsection2==table)
  dataWorld <- filter(dataWorld,!is.na(Observation))
  dataWorld <- dataWorld %>%
    group_by(iso2) %>%
    mutate(Period = max(Period,na.rm=TRUE)) %>%
    distinct(Period, .keep_all = TRUE) %>%
    as.data.frame()
  
  dataWorld <- arrange(dataWorld, desc(Observation))
  # rank in the world
  rank <- which(dataWorld$CountryCode == cou)
  rankedTotal <- nrow(dataWorld)

  indicator <- data$IndicatorShort[1]
  unit <- data$Unit[1]
    
  if (nrow(data)>0){
    
    minPeriod <- min(data$Period, na.rm=TRUE)
    maxPeriod <- max(data$Period, na.rm=TRUE)
    # sparkline
    spark <- data %>%
      arrange(Period) %>%
      select(Observation)
    # impute NAs and standardize so all sparklines are scaled
    spark[is.na(spark),1] <- mean(spark[,1],na.rm = TRUE)  #impute NAs to the mean of the column
    if (sum(spark[,1],na.rm = TRUE)==0){ 
      spark[,1] <- 0
      #x[1,i] <- -10
      spark[nrow(spark),1] <- 10
    }
    
    # Print the combo -----------------------------------------------
    par(family = 'serif',mfrow=c(5,1), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,2), #sets margin size for the figures
        oma=c(0,1,0,1)) #sets outer margin
    
    # print indicator name
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1,indicator, col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=10)
    graphics::text(1.5, 0.7,paste0(unit, " (",dataPeriod,")"), col="#818181", cex=5)
    # print data point and rank
    if (!rankBig){
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 0.95,dataPoint, col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=18)
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1.1,paste0("(Rank: ",rank,"/",rankedTotal,")"), col="#818181", cex=10)
    } else {
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 0.95,paste0(rank,"/",rankedTotal), col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=18)
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1.1,paste0("Value: ",dataPoint), col="#818181", cex=10)
    }
    
    # plot sparkline  
    par(family = 'serif',#sets number of rows in space to number of cols in data frame x
        mar=c(0,5,0,5))#sets margin size for the figures
        #oma=c(0,4,0,4)) #sets outer margin
    if (sum(spark[1:(nrow(spark)-1),1])==0){ # paint in white empty rows
      plot(spark[,1], #use col data, not rows from data frame x
           col="white",lwd=4, #color the line and adjust width
           axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
      
      axis(2,yaxp=c(min(spark[,1],na.rm = TRUE),max(spark[,1],na.rm = TRUE),2),col="white",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
      ymin<-min(spark[,1],na.rm = TRUE); tmin<-which.min(spark[,1]);ymax<-max(spark[,1], na.rm = TRUE);tmax<-which.max(spark[,1]);
      points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("white","white"),cex=7) # add coloured points at max and min# 
    } else {
      plot(spark[,1], #use col data, not rows from data frame x
           col="darkgrey",lwd=10, #color the line and adjust width
           axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
      
      axis(2,yaxp=c(min(spark[,1],na.rm = TRUE),max(spark[,1],na.rm = TRUE),2),col="white",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
      ymin<-min(spark[,1],na.rm = TRUE); tmin<-which.min(spark[,1]);ymax<-max(spark[,1], na.rm = TRUE);tmax<-which.max(spark[,1]); # 
      points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("red","green"),cex=7) # add coloured points at max and min
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      if (minPeriod==maxPeriod){
        graphics::text(1.5, 1.2,minPeriod, col="#818181", cex=5)
      } else{
        graphics::text(1.05, 1.2,minPeriod, col="#818181", cex=5)
        graphics::text(1.95, 1.2,maxPeriod, col="#818181", cex=5)
      }
    }
    
  } else {
    
    # Print the combo -----------------------------------------------
    par(family = 'serif',mfrow=c(5,1), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,2), #sets margin size for the figures
        oma=c(0,1,0,1)) #sets outer margin
    
    # print indicator name
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1,indicator, col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=10)
    graphics::text(1.5, 0.7,unit, col="#818181", cex=5)
    # print data point and rank
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 0.95,"No data available", col="#818181", cex=10)
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1,paste0("(Rank: /",rankedTotal,")"), col="#818181", cex=7)
    # plot sparkline  
    par(family = 'serif',#sets number of rows in space to number of cols in data frame x
      mar=c(0,5,0,5))#sets margin size for the figures
    #oma=c(0,4,0,4)) #sets outer margin
    
  } 
  
}

## ---- numberBig ----
numberBig <- function(Report_data,reportConfig,couName,section,table,rankBig=FALSE){      
  
  cou <- .getCountryCode(couName)
  ## Examples like Edward Tufte's sparklines:
  #table <- "combo1"
  data <- Report_data %>%
    filter(CountryCode==cou, Section == section, Subsection==table, !is.na(Observation)) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear)-1),Period),
           Observation = Observation/ifelse(is.na(Scale),1,Scale))
  
  data <- filter(data,!is.na(Observation))
  dataLast <- filter(data, Period == max(Period,na.rm=TRUE))

  # data
  dataPoint <- format(dataLast$Observation, digits=2, decimal.mark=".",
                      big.mark=",",small.mark=".", small.interval=3)
  # period
  dataPeriod <- dataLast$Period
  
  dataWorld <- filter(Report_data, Section == section, Subsection==table)
  dataWorld <- filter(dataWorld,!is.na(Observation))
  dataWorld <- dataWorld %>%
    group_by(iso2) %>%
    mutate(Period = max(Period,na.rm=TRUE)) %>%
    distinct(Period, .keep_all = TRUE) %>%
    as.data.frame()
  
  dataWorld <- arrange(dataWorld, desc(Observation))
  # rank in the world
  rank <- which(dataWorld$CountryCode == cou)
  rankedTotal <- nrow(dataWorld)
  
  indicator <- data$IndicatorShort[1]
  unit <- data$Unit[1]
  # add the right scale
  if (data$Scale[1] == 1000000){
    unit <- paste0(unit, ", million")
  }
  
  if (nrow(data)>0){
    
    # Print the combo -----------------------------------------------
    par(family = 'serif',mfrow=c(3,1), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,2), #sets margin size for the figures
        oma=c(0,1,0,1)) #sets outer margin
    
    # print indicator name
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1,indicator, col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=10)
    graphics::text(1.5, 0.7,paste0(unit, " (",dataPeriod,")"), col="#818181", cex=5)
    # print data point and rank
    if (!rankBig){ # rank bigger than actual value
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 0.95,dataPoint, col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=18)
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1.1,paste0("(Rank: ",rank,"/",rankedTotal,")"), col="grey", cex=7)
    } else {
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 0.95,paste0(rank,"/",rankedTotal), col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=18)
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1.1,paste0("Value: ",dataPoint), col="grey", cex=7)
    }
    
  } else {
    
    # Print the combo -----------------------------------------------
    par(family = 'serif',mfrow=c(5,1), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,2), #sets margin size for the figures
        oma=c(0,1,0,1)) #sets outer margin
    
    # print indicator name
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1,indicator, col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=10)
    graphics::text(1.5, 0.7,unit, col="#818181", cex=5)
    # print data point and rank
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 0.95,"No data available", col="grey", cex=10)
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1,paste0("(Rank: /",rankedTotal,")"), col="grey", cex=7)
    
  } 
  
}

## ---- table_time ----
# table_time <- function(Report_data,reportConfig,couName,section, table){      
#   
#   cou <- .getCountryCode(couName)
#   #table <- "table1"
#   tableKeys <- unique(filter(Report_data, Section == section, Subsection==table)[,c("Key","IndicatorShort")])
#   data <- filter(Report_data, CountryCode==cou, Section == section, Subsection==table)
#   data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
#   # keep the latest period (excluding projections further than 2 years)
#   data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period))
#   data <- filter(data, Period <= (as.numeric(thisYear) + 1))
#   
#   #keep only periods of interest in data
#   data <- filter(data, Period > (as.numeric(thisYear) - 7))
#   # Scale Observations
#   data <- mutate(data, ObsScaled = ifelse(grepl("current US",Unit),Observation/1000000000,Observation),
#                  Unit = ifelse(grepl("current US",Unit),"USD billions",Unit),
#                  IndicatorShort = paste0(IndicatorShort, ", ",Unit))
#   
#   data <- arrange(data, Key)
#   data <- select(data, Key, IndicatorShort, Period, ObsScaled)
#   # restrict to 2 decimal places
#   data$ObsScaled <- round(data$ObsScaled,2)
#   data[is.na(data)] <- "..."
#   # format numbers
#   data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
#                            big.mark=",",small.mark=".", small.interval=3)
#   
# #   for (i in 1:nrow(data)){
# #     
# #     data$IndicatorShort[i] <- ifelse(!is.na(merge(data,TCMN_indic[TCMN_indic$Subsection==table,], by="Key")$Note[i]),
# #                                      paste0(data$IndicatorShort[i]," \\large{[", merge(data,TCMN_indic[TCMN_indic$Subsection==table,], by="Key")$Note[i],"]}"),
# #                                      data$IndicatorShort[i])  
# #   }
#   # escape reserved characters
#   data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
#   data$IndicatorShort <- gsub("&", "\\&", data$IndicatorShort, fixed=TRUE)
#   
#   # final table format
#   data <- spread(data, Period, ObsScaled)
#   data <- data[,-1] #drop the Key column
#   
#   # remove columns with all NAs
#   data <- data[,!(colSums(data == "...   ")==nrow(data))]
#   
#   # dummy columns in to keep the pdf layout fixed to 6 columns
#   if (ncol(data)<=5){
#     for (j in (ncol(data)+1):7){
#       data[,j] <- "---"
#       names(data)[j] <- as.character(as.numeric(thisYear)-6+j)
#     }
#   }
#   # I have to add a dummy column so the alignment works (align)
#   data$dummy <- rep("",nrow(data))
#   # modify column names
#   names(data) <- c("",names(data)[2:(ncol(data)-1)],"")
#   
#   # substitute NAs for "---" em-dash
#   data[is.na(data)] <- "---"
#   rowsSelect <- seq(1,nrow(data)-1,2)
#   col <- rep("\\rowcolor[gray]{0.95}", length(rowsSelect))
#   data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
#   align(data.table) <- c('l','>{\\raggedright}p{6in}','r',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-3),'l')
#   print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
#         size="\\Large",add.to.row = list(pos = as.list(rowsSelect), command = col),
#         booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
#         sanitize.text.function = function(x){x}) # include sanitize to control formats
#   
# }

## ---- line_chart_avg ----
line_chart_avg <- function(Report_data,reportConfig,couName, section, table, minTime="1900",neighbor="region",max_neighbors=4, gender=FALSE){
  # if max_neighbors = 0, plot only selected country
  # if max_neighbors = 1, plot selected country vs average of all countries
  # if max_neighbors > 1, plot selected country vs the rest of the individual countries
  cou <- .getCountryCode(couName)
  if (neighbor=="region"){ # region level
    # get region mapping excluding HIC (if available)
    if (!is.null(as.character(countries[countries$iso3==cou,]$adminRegion))){
      couRegion <- as.character(countries[countries$iso3==cou,]$adminRegion)  # obtain the region for the selected country
      data <- filter(Report_data, adminRegion==couRegion, Section == section, Subsection == table, !(is.na(Observation)), Period >= minTime)
    } else {
      couRegion <- as.character(countries[countries$iso3==cou,]$region)  # obtain the region for the selected country
      data <- filter(Report_data, region==couRegion, Section == section, Subsection == table, !(is.na(Observation)), Period >= minTime) #select country, region and world
    }
  } else if (neighbor == "fcv"){
    couRegion <- as.character(countries[countries$iso3==cou,]$FCVclass)  # obtain the region for the selected country
    FCV_classmates <- countries[countries$FCVclass == couRegion, 'iso2']
    data <- filter(Report_data, iso2 %in% FCV_classmates, Section == section, Subsection == table, !(is.na(Observation)), Period >= minTime)
  } else { # income level 
    couRegion <- as.character(countries[countries$iso3==cou,]$incomeLevel)  # obtain the region for the selected country
    data <- filter(Report_data, incomeLevel==couRegion, Section == section, Subsection == table, !(is.na(Observation)), Period >= minTime) #select country, region and world
  }
  
  if (max_neighbors == 1){ # use the average of all neighbors
    
    region_avg <- dplyr::group_by(data, Key,Period) %>%
      dplyr::mutate(Observation = mean(Observation/ifelse(is.na(Scale),1,Scale),na.rm=TRUE)) %>%
      distinct(Key,Period,.keep_all=TRUE) %>%
      dplyr::mutate(Country = couRegion, CountryCode = couRegion) %>%
      #select(Key,Period,CountryCode) %>%
      as.data.frame()
    topNeighbors <- region_avg$CountryCode
    data <- filter(data, CountryCode == cou) %>%
      mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
      arrange(CountryCode,Period) %>%
      bind_rows(region_avg)
    
  } else { # compare against top incomes wihtin region 
    # select top neighbors according to income
    if (!is.null(as.character(countries[countries$iso3==cou,]$adminRegion))){
      income <- filter(Report_data, adminRegion==couRegion & Section=="aux_income")
    } else {income <- filter(Report_data, region==couRegion & Section=="aux_income")}
    
    income <- income %>%
      group_by(CountryCode) %>%
      filter(!is.na(Observation), Period < thisYear, !(CountryCode==cou)) %>%
      filter(Period == max(Period,na.rm=TRUE))
    
    topNeighbors <- head(arrange(as.data.frame(income), desc(Observation)),15)$CountryCode
    data <- filter(data, CountryCode %in% c(cou,topNeighbors)) %>%
      mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
      arrange(CountryCode,Period)
  }
  
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  par(mar=c(1,1,1,1))
  # order lines in chart and hide elements in legend
  if (nrow(filter(data,CountryCode==cou))>0){
    
    if (length(unique(data$Key))>1){ # plot several indicators for 1 country
      
      order_legend <- c(couName,as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country)))
      # catch errors for multiple indicators
      temp_unique_order <- c(couName,order_legend[2:length(order_legend)])
      temp_unique_order <- unique(temp_unique_order[!is.na(temp_unique_order)])
      country_order <- factor(order_legend, levels = temp_unique_order)
      
      my_order <- data.frame(Country = country_order, order = seq(1,length(order_legend),1))
      data <- merge(data,my_order, by="Country") %>%
        filter(order <= (max_neighbors+1)) %>% # keep some countries
        arrange(order,Period)

      if(!gender){
      ggplot(data, aes(x=Period, y=Observation)) +
        geom_line(stat="identity",aes(group=factor(Key), colour=factor(Key), size=factor(Key), alpha=factor(Key))) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              legend.text = element_text(family="Times", size = 10, colour = "#818181"),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
              axis.line = element_line(size=0.1, colour = "lightgrey"),
              axis.text.x = element_text(family="Times", color="#818181",hjust = 1),
              axis.text.y = element_text(family="Times", color="#818181")) +
        labs(x="",y=""#,title="Goods Export and Import volume growth, 2012-2015"
        ) + 
        scale_color_manual(labels = unique(data$IndicatorShort), values = c("darkgray",paste0("#",filter(reportConfig, Section_Level == 10)$Color),"lightblue","lightgreen","pink")) +
        scale_alpha_manual(labels = unique(data$IndicatorShort),values = c(1, rep(0.6,4))) + 
        scale_size_manual(labels = unique(data$IndicatorShort),values = c(2, rep(1,4))) +
        geom_point(size = 3, aes(colour = factor(Key))) +
        geom_label(aes(Period, Observation, colour = factor(Key), label = sprintf('%0.1f', Observation)),
                   data = rbind(tail(filter(data, Key==1), 1), tail(filter(data, Key==2), 1)),                 
                   show.legend = FALSE)
        } else{
          
          toMatch <- c("female", "girl")
          matches <- unique (grep(paste(toMatch,collapse="|"), data$IndicatorShort, value=TRUE))
          data[data$IndicatorShort %in% matches, "gender"] <- "Female"
          data[!data$IndicatorShort %in% matches, "gender"] <- "Male"
          data$IndicatorShort <- gsub("\nfemale| female|, female|,female|\nmale| male|,male|, male", "", data$IndicatorShort)
          
          ggplot(data, aes(x=Period, y=Observation)) +
            geom_line(stat="identity",aes(group=factor(gender), colour=factor(gender))) +
            labs(x="",y="",title=data$IndicatorShort[1]) +
            theme(legend.key=element_blank(),
                  legend.title=element_blank(),
                  legend.position="top",
                  legend.text = element_text(family="Times", size = 10, colour = "#818181"),
                  panel.border = element_blank(),
                  panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
                  axis.line = element_line(size=0.1, colour = "lightgrey"),
                  axis.text.x = element_text(family="Times", color="#818181",hjust = 1),
                  axis.text.y = element_text(family="Times", color="#818181")) +
            geom_point(size = 3, aes(colour = factor(gender))) +
            geom_label(aes(Period, Observation, colour = factor(gender), label = sprintf('%0.1f', Observation)),
                       data = rbind(tail(filter(data, gender=='Female'), 1), tail(filter(data, gender=='Male'), 1)),                 
                       show.legend = FALSE)
      }
      
    } else { # plot 1 indicator for 1 country and perhaps region or other countries
      order_legend <- c(couName,as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country)))
      # catch errors for multiple indicators
      temp_unique_order <- c(couName,order_legend[2:length(order_legend)])
      temp_unique_order <- unique(temp_unique_order[!is.na(temp_unique_order)])
      country_order <- factor(order_legend, levels = temp_unique_order)
      
      my_order <- data.frame(Country = country_order, order = seq(1,length(order_legend),1))
      data <- merge(data,my_order, by="Country") %>%
        filter(order <= (max_neighbors+1)) %>% # keep some countries
        arrange(order,Period)
      
      order_legend[2] <- paste0(order_legend[2], " (average)", collapse=NULL)
      
      ggplot(data, aes(x=Period, y=Observation)) +
        geom_line(stat="identity",aes(group=factor(order), colour=factor(order), size=factor(order), alpha=factor(order))) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              legend.text = element_text(family="Times", size = 10, colour = "#818181"),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
              axis.line = element_line(size=0.1, colour = "lightgrey"),
              axis.text.x = element_text(family="Times", color="#818181",hjust = 1),
              axis.text.y = element_text(family="Times", color="#818181")) +
        labs(x="",y=""#,title="Goods Export and Import volume growth, 2012-2015"
        ) + 
        scale_color_manual(labels = order_legend, values = c("darkgray",paste0("#",filter(reportConfig, Section_Level == 10)$Color),"lightblue","lightgreen","pink")) +
        scale_alpha_manual(labels = order_legend,values = c(1, rep(0.6,4))) + 
        scale_size_manual(labels = order_legend,values = c(2, rep(1,4))) +
        geom_point(size = 3, aes(colour = factor(order))) +
        geom_label(aes(Period, Observation, colour = factor(order), label = sprintf('%0.1f', Observation)),
                   data = rbind(tail(filter(data, order==1), 1), tail(filter(data, order==2), 1)),                 
                        show.legend = FALSE)
      # + scale_x_discrete(breaks = unique(arrange(data,Period)$Period)[seq(1,length(unique(data$Period)),4)])
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}
## ---- line_chart ----
line_chart <- function(Report_data,reportConfig,couName, section, table, minTime="1900",neighbor="region",max_neighbors=4){
  # if max_neighbors = 0, plot only selected country
  # if max_neighbors = 1, plot selected country vs average of all countries
  # if max_neighbors > 1, plot selected country vs the rest of the individual countries
  cou <- .getCountryCode(couName)
  if (neighbor=="region"){ # region level
    couRegion <- as.character(countries[countries$iso3==cou,]$region)  # obtain the region for the selected country
    data <- filter(Report_data, region==couRegion, Section == section, Subsection == table, !(is.na(Observation)), Period >= minTime) #select country, region and world
  } else { # income level 
    couRegion <- as.character(countries[countries$iso3==cou,]$incomeLevel)  # obtain the region for the selected country
    data <- filter(Report_data, incomeLevel==couRegion, Section == section, Subsection == table, !(is.na(Observation)), Period >= minTime) #select country, region and world
  }
  
  if (max_neighbors == 1){ # use the average of all neighbors
    
    region_avg <- dplyr::group_by(data, Key,Period) %>%
      dplyr::mutate(Observation = mean(Observation/ifelse(is.na(Scale),1,Scale),na.rm=TRUE)) %>%
      distinct(Key,Period,.keep_all=TRUE) %>%
      dplyr::mutate(Country = couRegion, CountryCode = couRegion) %>%
      #select(Key,Period,CountryCode) %>%
      as.data.frame()
    topNeighbors <- region_avg$CountryCode
    data <- filter(data, CountryCode == cou) %>%
      mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
      arrange(CountryCode,Period) %>%
      bind_rows(region_avg)
    
  } else { # compare against top incomes wihtin region 
    # select top neighbors according to income
    income <- filter(Report_data, region==couRegion & Section=="aux_income")
    income <- income %>%
      group_by(CountryCode) %>%
      filter(!is.na(Observation), Period < thisYear, !(CountryCode==cou)) %>%
      filter(Period == max(Period,na.rm=TRUE))
    
    topNeighbors <- head(arrange(as.data.frame(income), desc(Observation)),15)$CountryCode
    data <- filter(data, CountryCode %in% c(cou,topNeighbors)) %>%
      mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
      arrange(CountryCode,Period)
  }
  
  
  # order lines in chart and hide elements in legend
  if (nrow(filter(data,CountryCode==cou))>0){
    
    if (length(unique(data$Key))>1){ # plot several indicators for 1 country
      
      order_legend <- c(couName,as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country)))
      # catch errors for multiple indicators
      temp_unique_order <- c(couName,order_legend[2:length(order_legend)])
      temp_unique_order <- unique(temp_unique_order[!is.na(temp_unique_order)])
      country_order <- factor(order_legend, levels = temp_unique_order)

      my_order <- data.frame(Country = country_order, order = seq(1,length(order_legend),1))
      data <- merge(data,my_order, by="Country") %>%
        filter(order <= (max_neighbors+1)) %>% # keep some countries
        arrange(order,Period)
      
      ggplot(data, aes(x=Period, y=Observation)) +
        geom_line(stat="identity",aes(group=factor(Key), colour=factor(Key), size=factor(Key), alpha=factor(Key))) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              legend.text = element_text(family="Times", size = 10, colour = "#818181"),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
              axis.line = element_line(size=0.1, colour = "lightgrey"),
              axis.text.x = element_text(family="Times", color="#818181",hjust = 1),
              axis.text.y = element_text(family="Times", color="#818181")) +
        labs(x="",y=""#,title="Goods Export and Import volume growth, 2012-2015"
        ) + 
        scale_color_manual(labels = unique(data$IndicatorShort), values = c("darkgreen","green","lightblue","lightgreen","pink")) +
        scale_alpha_manual(labels = unique(data$IndicatorShort),values = c(0.6, rep(0.6,4))) + 
        scale_size_manual(labels = unique(data$IndicatorShort),values = c(2, rep(2,4))) + 
        scale_x_discrete(breaks = unique(arrange(data,Period)$Period)[seq(1,length(unique(data$Period)),4)])
      
    } else { # plot 1 indicator for 1 country and perhaps region or other countries
      order_legend <- c(couName,as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country)))
      # catch errors for multiple indicators
      temp_unique_order <- c(couName,order_legend[2:length(order_legend)])
      temp_unique_order <- unique(temp_unique_order[!is.na(temp_unique_order)])
      country_order <- factor(order_legend, levels = temp_unique_order)
      
      my_order <- data.frame(Country = country_order, order = seq(1,length(order_legend),1))
      data <- merge(data,my_order, by="Country") %>%
        filter(order <= (max_neighbors+1)) %>% # keep some countries
        arrange(order,Period)
      
      ggplot(data, aes(x=Period, y=Observation)) +
        geom_line(stat="identity",aes(group=factor(order), colour=factor(order), size=factor(order), alpha=factor(order))) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              legend.text = element_text(family="Times", size = 10, colour = "#818181"),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
              axis.line = element_line(size=0.1, colour = "lightgrey"),
              axis.text.x = element_text(family="Times", color="#818181",hjust = 1),
              axis.text.y = element_text(family="Times", color="#818181")) +
        labs(x="",y=""#,title="Goods Export and Import volume growth, 2012-2015"
        ) + 
        scale_color_manual(labels = order_legend, values = c("orange",paste0("#",filter(reportConfig, Section_Level == 10)$Color),"lightblue","lightgreen","pink")) +
        scale_alpha_manual(labels = order_legend,values = c(1, rep(0.6,4))) + 
        scale_size_manual(labels = order_legend,values = c(2, rep(1,4))) + 
        scale_x_discrete(breaks = unique(arrange(data,Period)$Period)[seq(1,length(unique(data$Period)),4)])
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}

## ---- table_time_avg ----
table_time_avg <- function(Report_data,reportConfig,couName,section,table, GDPgrowthrate=FALSE){      
  
  cou <- .getCountryCode(couName)
  #table <- "table1"
  tableKeys <- unique(filter(Report_data, Section == section, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(Report_data, CountryCode==cou, Section == section, Subsection==table)
  data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
  
  if (sum(data$Observation,na.rm=TRUE)==0){ # in case this country has no data
    data$Observation <- 0
    data$Period <- as.numeric(thisYear)-1
    # To create table's reference points in the LaTeX output
    data_initial <- data
    for (per in (as.numeric(thisYear)-7):(as.numeric(thisYear)-2)){
      data_plus <- mutate(data_initial,Period = per)
      data <- bind_rows(data, data_plus)
    }
    data$Period <- as.character(data$Period)
  }
    # keep the latest period (excluding projections further than 2 years)
    data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period)) %>%
    filter(Period <= (as.numeric(thisYear))) %>%
    # remove NAs rows
    # calculate average for 1st column
    mutate(Unit = ifelse(grepl("Active population",Unit),"% of TEA",Unit),
                   IndicatorShort = paste0(IndicatorShort,", ",Unit))
    data$IndicatorShort <- gsub("Entrepreneurial","Entrepr.", data$IndicatorShort)
    data$IndicatorShort <- gsub("auditors","audit.", data$IndicatorShort)
    
    if (GDPgrowthrate & 28107 %in% data$Key){
      gdp_growth <- filter(data, Key == 28107) %>% arrange(Period) %>%
        mutate(GDPgrowth=Observation/lag(Observation,1))
      gdp_growth$Observation <- gdp_growth$GDPgrowth
      gdp_growth$IndicatorShort <- "GDP Growth Rate %"
      gdp_growth$Key <- 'gdp_growth'
      gdp_growth$Scale <- 1
      gdp_growth$Unit <- NA
      gdp_growth <- subset(gdp_growth, select = -c(GDPgrowth) )
      data <- rbind(data, gdp_growth)
    }
    
    data_avg <- data %>%
      group_by(Key) %>%
      filter(Period < (as.numeric(thisYear)-5) & Period > (as.numeric(thisYear)-15)) %>%
      mutate(historical_avg = mean(Observation,na.rm=TRUE))
    # add average as one of the time periods
    min_year <- min(data_avg$Period,na.rm=TRUE)
    max_year <- max(data_avg$Period,na.rm=TRUE)
    data_avg <- mutate(data_avg, Period = paste("Avg ",min_year,"-",max_year,sep=""),
                       Observation = historical_avg, ObsScaled = historical_avg)
    
    data_avg <- data_avg[!duplicated(data_avg$Key),] # remove duplicates
    data_avg <- select(data_avg, -historical_avg, -ObsScaled) # remove some variables
    
    #keep only periods of interest in data
    data <- mutate(data, Period = ifelse(Period==thisYear & is.na(CountryCode),as.numeric(thisYear)-1,Period)) %>%
      filter(Period > (as.numeric(thisYear) - 7) & Period < (as.numeric(thisYear)))
    data <- bind_rows(data, data_avg) %>% # add rows to data
    #data <- as.data.frame(data)
    # Scale Observations
    mutate(ObsScaled = Observation/ifelse(is.na(Scale),1,Scale)) %>%
    arrange(Key) %>%
    select(Key, IndicatorShort, Period, ObsScaled)
    # restrict to 2 decimal places
    data$ObsScaled <- round(data$ObsScaled,2)
    
    # format numbers
    data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
    
    # escape reserved characters
    data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
    data$IndicatorShort <- gsub("&", "\\&", data$IndicatorShort, fixed=TRUE)
    
    data <- distinct(data, Key,Period, .keep_all = TRUE)
    # final table format
    data <- spread(data, Period, ObsScaled)
    data <- data[,-1] #drop the Key column
    if (ncol(data)>2){
      data <- data[,c(1,ncol(data),2:(ncol(data)-1))] # reorder columns
      # rid of characters in numeric columns
      data[,ncol(data)] <- gsub("NA", "---", data[,ncol(data)], fixed=TRUE)
    } 
    
    # dummy columns in to keep the pdf layout
    if (ncol(data)<=6){
      for (j in (ncol(data)+1):7){
        data[,j] <- "---"
        names(data)[j] <- as.character(as.numeric(thisYear)-6+j)
      }
    }
    # I have to add a dummy column so the alignment works (align)
    data$dummy <- rep("",nrow(data))
    # modify column names
    names(data) <- c("",names(data)[2:(ncol(data)-1)],"")
    
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
    #if (round(nrow(data)/2,0)>nrow(data)/2){ # odd number
    rowsSelect <- seq(1,nrow(data)-1,2)
    #} else{ # even
    #  rowsSelect <- seq(1,nrow(data)-1,2)
    #}
    if (section %in% c("Culture","Supports")){
      col <- rep("\\rowcolor{white}", length(rowsSelect))
    } else {
      col <- rep("\\rowcolor[gray]{0.95}", length(rowsSelect))
    }
    
    data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
    align(data.table) <- c('l','>{\\raggedright}p{6in}','r',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-3),'l')
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\Large",add.to.row = list(pos = as.list(rowsSelect), command = col),
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x}) # include sanitize to control formats
  
}

## ---- sparklines ----
sparklines <- function(Report_data,reportConfig,couName,section,table, num_period=5, GDPgrowthrate=FALSE){      
  
  cou <- .getCountryCode(couName)
  #table <- "table1"
  tableKeys <- unique(filter(Report_data, Section == section, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(Report_data, CountryCode==cou, Section == section, Subsection==table)
  data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
  
  if (sum(data$Observation,na.rm=TRUE)==0){ # in case this country has no data
    data$Observation <- 0
    data$Period <- as.numeric(thisYear)-1
    # To create table's reference points in the LaTeX output
    data_initial <- data
    for (per in (as.numeric(thisYear)-2-num_period):(as.numeric(thisYear)-2)){
      data_plus <- mutate(data_initial,Period = per)
      data <- bind_rows(data, data_plus)
    }
    data$Period <- as.character(data$Period)
  }
  
  if (nrow(data)>0){
    data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
    data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period)) %>%
      filter(Period <= (as.numeric(thisYear))) %>%
      mutate(Period = ifelse(Period < 1900, 1900, Period))
    
    if (GDPgrowthrate & 28107 %in% data$Key){
      gdp_growth <- filter(data, Key == 28107) %>% arrange(Period) %>%
        mutate(GDPgrowth=Observation/lag(Observation,1))
      gdp_growth$Observation <- gdp_growth$GDPgrowth
      gdp_growth$IndicatorShort <- "GDP Growth Rate %"
      gdp_growth$Key <- 'gdp_growth'
      gdp_growth$Scale <- 1
      gdp_growth$Unit <- NA
      gdp_growth <- subset(gdp_growth, select = -c(GDPgrowth) )
      data <- rbind(data, gdp_growth)
    }
    
    # keep the latest period (excluding projections further than 2 years)
    data <- data %>%
      mutate(Period = ifelse(Period==thisYear & is.na(CountryCode),as.numeric(thisYear)-1,Period)) %>%
      filter(Period > (as.numeric(thisYear) - 2-num_period) & Period < (as.numeric(thisYear))) %>%
      mutate(Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period)) %>%
      filter(Period <= (as.numeric(thisYear) - 1), Period > (as.numeric(thisYear) - 15)) %>%
      select(Key, Period, Observation) %>%
      arrange(Key, Period) %>%
      distinct(Key,Period, .keep_all = TRUE)
    
    minPeriod <- min(data$Period,na.rm = TRUE)
    maxPeriod <- max(data$Period,na.rm = TRUE)
    
    x <- spread(data, Key, Observation)
    x <- x[,-1] # don't need Period column anymore
    
    # impute NAs and standardize so all sparklines are scales
    for (i in 1:ncol(x)){ # setup for statement to loop over all elements in a list or vector
      
      x[is.na(x[,i]),i] <- mean(x[,i],na.rm = TRUE)  #impute NAs to the mean of the column
      if (sum(x[,i],na.rm = TRUE)==0){ 
        x[,i] <- 0
        #x[1,i] <- -10
        x[nrow(x),i] <- 10
      }
    }
    #x <- scale(x) # standardize x
    
    par(family = 'serif',mfrow=c(ncol(x)+3,1), #sets number of rows in space to number of cols in data frame x
        mar=c(1,0,0,0), #sets margin size for the figures
        oma=c(1,2,1,1)) #sets outer margin
    
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.05, 1,minPeriod, col="darkgrey", cex=3)
    graphics::text(1.95, 1,maxPeriod, col="darkgrey", cex=3)
    
    for (i in 1:ncol(x)){ # setup for statement to loop over all elements in a list or vector
      
      if (sum(x[1:(nrow(x)-1),i])==0){ # paint in white empty rows
        plot(x[,i], #use col data, not rows from data frame x
             col="lightgrey",lwd=4, #color the line and adjust width
             axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
        
        axis(2,yaxp=c(min(x[,i],na.rm = TRUE),max(x[,i],na.rm = TRUE),2),col="lightgrey",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
        ymin<-min(x[,i],na.rm = TRUE); tmin<-which.min(x[,i]);ymax<-max(x[,i], na.rm = TRUE);tmax<-which.max(x[,i]);
        points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("lightgrey","lightgrey"),cex=5) # add coloured points at max and min# 
      } else {
        plot(x[,i], #use col data, not rows from data frame x
             col="darkgrey",lwd=4, #color the line and adjust width
             axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
        
        axis(2,yaxp=c(min(x[,i],na.rm = TRUE),max(x[,i],na.rm = TRUE),2),col="lightgrey",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
        ymin<-min(x[,i],na.rm = TRUE); tmin<-which.min(x[,i]);ymax<-max(x[,i], na.rm = TRUE);tmax<-which.max(x[,i]); # 
        points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("red","green"),cex=5) # add coloured points at max and min
      }
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1)
  } 
  
}

## ---- bar_chart_gender ----
bar_chart_gender <- function(Report_data,reportConfig,couName,section,table,paste_unit, kind="normal"){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Report_data, CountryCode==cou, Section %in% section, Subsection %in% table)
  data <- data %>%
    filter(!(is.na(Observation))) %>%
    mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
    distinct(Key,Period,.keep_all=TRUE)
  maxPeriod <- filter(data, Period == max(Period,na.rm=TRUE))$Period[1]
  
  if (nrow(data)>0){
    # order the factors
    if (!any(is.na(data$Period))){
      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period))
    }
    
    require(stringr) # to wrap label text
    require(scales) # to use thousands separator
    
    if (paste_unit){ # should unit be included in indicator name
      data <- mutate(data, Unit = ifelse(grepl("0-100",Unit),"100=full ownership allowed",Unit))
      data <- mutate(data, IndicatorShort = str_wrap(paste0(IndicatorShort,", ",Unit," (",Period,")"), width = 30))
    } else {
      data <- mutate(data, IndicatorShort = str_wrap(paste0(IndicatorShort," (",Period,")"), width = 30))
    }
    # store max value to better place figures in or out the bars
    max_value <- max(data$Observation)

      if (nrow(filter(data, !(Key %in% c(949,1177))))>0){
      
      data <- filter(data, !(Key %in% c(949,1177))) # make sure gdp and employ don't show up here
      
      
      # remapping indicator names based on "female"
      toMatch <- c("female", "girl")
      matches <- unique (grep(paste(toMatch,collapse="|"), data$IndicatorShort, value=TRUE))
      data[data$IndicatorShort %in% matches, "gender"] <- "Female"
      data[!data$IndicatorShort %in% matches, "gender"] <- "Male"
      
      data$IndicatorShort <- gsub("\nfemale| female|, female|,female|\nmale| male|,male|, male", "", data$IndicatorShort)
      
      if (kind == "pie"){
        obs <- data[1,"Observation"]$Observation/24.0
        gender <- data[1,"gender"]$gender
        
        # make dataframe
        df <- data.frame(
          group = c("Female", ""),
          value = c(obs, 1-obs)
        )
        
        p1 <- ggplot(df, aes(x="", y=value, fill=group))+
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y", start=0)+
          ggtitle("Female") +
          scale_fill_manual(values=c("#818181", "pink")) +
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                plot.title = element_text(family="Times", lineheight=.8, size = 20, colour = "darkgrey"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                legend.position="none") + 
          labs(x="",y="") +
        geom_text(aes(label=percent(obs),y=0),
                  size=12,color="white")

        obs2 <- data[2,"Observation"]$Observation/24.0
        gender <- data[2,"gender"]$gender

        # make dataframe
        df <- data.frame(
          group = c("Male", ""),
          value = c(obs2, 1-obs2)
        )
        
        p2 <- ggplot(df, aes(x="", y=value, fill=group))+
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y", start=0)+
          ggtitle("Male") +
          scale_fill_manual(values=c("#818181", "blue")) +
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                plot.title = element_text(family="Times", lineheight=.8, size = 20, colour = "darkgrey"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                legend.position="none") + 
          labs(x="",y="") +
          geom_text(aes(label=percent(obs2),y=0),
                    size=12,color="white")
        
        grid.arrange(p1,p2,ncol=2)
        
        
      } else if (kind == "stackedbar"){
        ggplot(data, aes(x = gender, y = Observation, fill = IndicatorShort)) + 
          geom_bar(stat = "identity") + coord_flip() + labs(y = "", x="", fill="") +
          theme(panel.border = element_blank(),
                panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
                axis.text.x = element_text(family="Times", color = "#818181", size = 7),
                axis.text.y = element_text(family="Times", color = "#818181", size = 7),
                legend.text=element_text(family="Times", color = "#818181", size = 10))}
      else if(kind=="single_indicator"){
        ggplot(data, aes(x=IndicatorShort, y=Observation, group=gender, fill=gender)) +
          geom_bar(stat="identity",position="dodge", colour="black") +
          coord_flip() +
          labs(y = "", x="", fill="Gender") +
          theme(panel.border = element_blank(),
                panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
                axis.text.x = element_text(family="Times", color = "#818181", size = 7),
                axis.text.y = element_blank(),
                legend.text=element_text(family="Times", color = "#818181", size = 10))+
          geom_text(data=data, aes(label=sprintf('%0.1f', Observation)), position = position_dodge(width=1), hjust=-0.25)
      } else if (kind=="normal") {
        ggplot(data, aes(x=IndicatorShort, y=Observation, group=gender, fill=gender)) +
          geom_bar(stat="identity",position="dodge", colour="black") +
          coord_flip() +
          labs(y = "", x="", fill="Gender") +
          theme(panel.border = element_blank(),
                      panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
                      axis.text.x = element_text(family="Times", color = "#818181", size = 7),
                      axis.text.y = element_text(family="Times", color = "#818181", size = 7),
                legend.text=element_text(family="Times", color = "#818181", size = 10))+
                geom_text(data=data, aes(label=sprintf('%0.1f', Observation)), position = position_dodge(width=1), hjust=-0.1)
        
       }
      
    } else {
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}

## ---- bar_chart ----
bar_chart <- function(Report_data,reportConfig,couName,section,table,paste_unit,percentBar = FALSE){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Report_data, CountryCode==cou, Section %in% section, Subsection %in% table)
  data <- data %>%
    filter(!(is.na(Observation))) %>%
    mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
    distinct(Key,Period,.keep_all=TRUE)
  maxPeriod <- filter(data, Period == max(Period,na.rm=TRUE))$Period[1]
  
  if (nrow(data)>0){
    # order the factors
    if (!any(is.na(data$Period))){
      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period))
    }
    
    require(stringr) # to wrap label text
    require(scales) # to use thousands separator
    
    if (paste_unit){ # should unit be included in indicator name
      data <- mutate(data, Unit = ifelse(grepl("0-100",Unit),"100=full ownership allowed",Unit))
      data <- mutate(data, IndicatorShort = str_wrap(paste0(IndicatorShort,", ",Unit," (",Period,")"), width = 30))
    } else {
      data <- mutate(data, IndicatorShort = str_wrap(paste0(IndicatorShort," (",Period,")"), width = 30))
    }
    # store max value to better place figures in or out the bars
    max_value <- max(data$Observation)
    
    if (percentBar & nrow(filter(data, Key %in% c(949,1177)))>1 & nrow(filter(data, !(Key %in% c(949,1177))))>1){
      
      if (data$Section[1]=="TOURISM ECONOMIC INDICATORS"){
        
        gdp <- as.numeric(filter(data, Key == 949)$Observation)
        employ <- as.numeric(filter(data, Key == 1177)$Observation)
       
        require(stringr) 
        data <- filter(data, !(Key %in% c(949,1177))) %>%
          mutate(ObservationPerc = ifelse(Key %in% c(24695,24650), Observation/(10*gdp), Observation/(10*employ))) %>%
          mutate(IndicatorShort = ifelse(Key==24695, "Total contribution to GDP", 
                                         ifelse(Key==24650,"Direct contribution to GDP",
                                                ifelse(Key==24643,"Direct contribution to employment","Total contribution to employment")))) %>%
          #mutate(Observation = ifelse(Key == 24644, Observation*ObservationPerc, Observation),
          #       Unit = ifelse(Key == 24644, "employed, in millions", Unit)) %>%
          mutate(IndicatorShort = str_wrap(paste0(IndicatorShort,", ",Unit, " (", Period,")"), width = 25)) %>%
          as.data.frame()
        
        max_valuePerc <- max(data$ObservationPerc,rm.na = TRUE)
      }
      
      data_grey <- data.frame(IndicatorShort=data$IndicatorShort,ObservationPerc=rep(100,nrow(data)))
      #data <- mutate(data, id = seq(1,nrow(data),1))
      ggplot(NULL,aes(x=IndicatorShort,y=ObservationPerc)) +
        geom_bar(data=data_grey,color="#DCDCDC",fill = "#DCDCDC",stat="identity") +
        geom_bar(data=data,color=paste0("#",filter(reportConfig, Section_Level == 10)$Color),fill=paste0("#",filter(reportConfig, Section_Level == 10)$Color),stat="identity") +
        geom_text(data=data, aes(label=paste0(round(ObservationPerc,1),"%"),y=ifelse(ObservationPerc<70,90,ifelse(ObservationPerc > 80, 70, ObservationPerc*1.15))),
                  size=10,color="darkblue") + 
        geom_text(data=data, aes(label=format(round(Observation,1),big.mark = ","),y=ifelse(ObservationPerc > 40, 15, ObservationPerc + 15)),
                  size=8,color=ifelse(data$ObservationPerc > 40, "white", paste0("#",filter(reportConfig, Section_Level == 10)$Color))) + 
        coord_flip()+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position='none',
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(family="Times", size = 20),
              axis.text = element_text(family="Times", color = "#818181")) + 
        labs(x="",y=""#,title="Top 5 constraints according to 2013 Enterprise Survey (in percent)"
        ) 
      
    } else if (nrow(filter(data, !(Key %in% c(949,1177))))>0){
      
      data <- filter(data, !(Key %in% c(949,1177))) # make sure gdp and employ don't show up here
      
      ggplot(NULL,aes(x=IndicatorShort,y=Observation)) +
        geom_bar(data=data,color=paste0("#",filter(reportConfig, Section_Level == 10)$Color),fill=paste0("#",filter(reportConfig, Section_Level == 10)$Color),stat="identity") +
        geom_text(data=data, aes(label=format(round(Observation,1),big.mark = ","),y=ifelse(Observation<max_value*.15,Observation + max(Observation)*.1,Observation - max(Observation)*.1)),
                  size=8,color=ifelse(data$Observation<max_value*.15,paste0("#",filter(reportConfig, Section_Level == 10)$Color),"white")) + 
        coord_flip()+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position='none',
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(family="Times", color = "#818181", size = 20)) + 
        labs(x="",y=""#,title="Top 5 constraints according to 2013 Enterprise Survey (in percent)"
        )
    } else {
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}

## ---- bar_chart_fcv ----
bar_chart_fcv <- function(Report_data,reportConfig,couName,section,table){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Report_data, CountryCode==cou, Section %in% section, Subsection %in% table)
  data <- data %>%
    filter(!(is.na(Observation))) %>%
    mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
    distinct(Key,Period,.keep_all=TRUE)
  maxPeriod <- filter(data, Period == max(Period,na.rm=TRUE))$Period[1]
  
  if (nrow(data)>0){
    # order the factors
    if (!any(is.na(data$Period))){
      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period))
    }
    
    #get top 5 obstacles
    data <- head(arrange(data, desc(Observation)),5)
    
    require(stringr) # to wrap label text
    require(scales) # to use thousands separator
    
    data <- mutate(data, IndicatorShort = str_wrap(paste0(IndicatorShort," (",Period,")"), width = 30))
    # store max value to better place figures in or out the bars
    max_value <- max(data$Observation)
    
    ggplot(data,aes(x=reorder(IndicatorShort, Observation),y=Observation)) +
      geom_bar(data=data,color=paste0("#",filter(reportConfig, Section_Level == 10)$Color),fill=paste0("#",filter(reportConfig, Section_Level == 10)$Color),stat="identity") +
      geom_text(data=data, aes(label=format(round(Observation,1),big.mark = ","),y=ifelse(Observation<max_value*.15,Observation + max(Observation)*.1,Observation - max(Observation)*.1)),
                size=8,color=ifelse(data$Observation<max_value*.15,paste0("#",filter(reportConfig, Section_Level == 10)$Color),"white")) + 
      coord_flip()+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.position='none',
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(family="Times", color = "#818181", size = 20)) + 
      labs(x="",y=""#,title="Top 5 constraints according to 2013 Enterprise Survey (in percent)"
      )
    } else {
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
    }
}

## ---- bar_chart_fcv_class ----
bar_chart_fcv_class <- function(Report_data,reportConfig,couName,section,table){      
  
  cou <- .getCountryCode(couName)
  # data <- filter(Report_data, CountryCode==cou, Section %in% section, Subsection %in% table)
  couFCV <- as.character(countries[countries$iso3==cou,]$FCVclass)  # obtain the region for the selected country
  FCV_classmates <- countries[countries$FCVclass == couFCV, 'iso2']
  data <- filter(Report_data, iso2 %in% FCV_classmates, Section == section, Subsection == table)
  
  data <- data %>%
    filter(!(is.na(Observation))) %>%
    mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
    distinct(Key,Period,.keep_all=TRUE)
  maxPeriod <- filter(data, Period == max(Period,na.rm=TRUE))$Period[1]
  
  region_avg <- dplyr::group_by(data, Key,Period) %>%
    dplyr::mutate(Observation = mean(Observation/ifelse(is.na(Scale),1,Scale),na.rm=TRUE)) %>%
    distinct(Key,Period,.keep_all=TRUE) %>%
    dplyr::mutate(Country = couRegion, CountryCode = couRegion) %>%
    #select(Key,Period,CountryCode) %>%
    as.data.frame()
  topNeighbors <- region_avg$CountryCode
  data <- filter(data, CountryCode == cou) %>%
    mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
    arrange(CountryCode,Period) %>%
    bind_rows(region_avg)
  
  if (nrow(data)>0){
    # order the factors
    if (!any(is.na(data$Period))){
      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period))
    }
    
    #get top 5 obstacles
    data <- head(arrange(data, desc(Observation)),5)
    
    require(stringr) # to wrap label text
    require(scales) # to use thousands separator
    
    data <- mutate(data, IndicatorShort = str_wrap(paste0(IndicatorShort," (",Period,")"), width = 30))
    # store max value to better place figures in or out the bars
    max_value <- max(data$Observation)
    
    ggplot(data,aes(x=reorder(IndicatorShort, Observation),y=Observation)) +
      geom_bar(data=data,color=paste0("#",filter(reportConfig, Section_Level == 10)$Color),fill=paste0("#",filter(reportConfig, Section_Level == 10)$Color),stat="identity") +
      geom_text(data=data, aes(label=format(round(Observation,1),big.mark = ","),y=ifelse(Observation<max_value*.15,Observation + max(Observation)*.1,Observation - max(Observation)*.1)),
                size=8,color=ifelse(data$Observation<max_value*.15,paste0("#",filter(reportConfig, Section_Level == 10)$Color),"white")) + 
      coord_flip()+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.position='none',
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(family="Times", color = "#818181", size = 20)) + 
      labs(x="",y=""#,title="Top 5 constraints according to 2013 Enterprise Survey (in percent)"
      )
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
}

## ---- number_chart ----
number_chart <- function(Report_data,reportConfig,couName,section,table,str_wrap_size,rankBig=FALSE,includeUnit=TRUE){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Report_data, CountryCode==cou,  Subsection %in% table)
  data <- data %>%
    filter(!(is.na(Observation))) %>%
    mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
    distinct(Key,Period,.keep_all=TRUE)
  
  dataWorld <- filter(Report_data,  Subsection %in% table)
  dataWorld <- filter(dataWorld,!is.na(Observation))
  dataWorld <- dataWorld %>%
    group_by(Country,Key) %>%
    filter( Period == max(Period,na.rm=TRUE))%>%     # mutate(Period = max(Period,na.rm=TRUE)) %>%
    distinct(Key, Period, .keep_all = TRUE) %>%
    as.data.frame()
  
  dataWorld <- dataWorld %>%
    group_by(Key) %>%
    mutate(Observation = round(Observation,1)) %>%
    arrange(desc(Observation)) %>%
    as.data.frame()
  
  if (nrow(data)>0){
    
    require(stringr) # to wrap label text
    # Print the combo -----------------------------------------------
    par(family = 'serif',mfrow=c(length(unique(dataWorld$Key)),2), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,2), #sets margin size for the figures
        oma=c(0,1,0,1)) #sets outer margin
    
    i <- 1
    rankedTotal <- c()
    rank <- c()
    for (ind in unique(dataWorld$Key)){
      thisKey <- filter(dataWorld, Key == ind)
      if (!includeUnit) { # Remove unit from final output
        thisKey <- mutate(thisKey, Unit = "") %>% as.data.frame()
      }
      
      thisKey <- mutate(thisKey, Unit = ifelse(grepl("0-100",Unit),"100=full ownership allowed",Unit))
      thisKey <- mutate(thisKey, IndicatorShort = str_wrap(paste0(IndicatorShort), width = str_wrap_size))
      rankedTotal[i] <- nrow(thisKey)
      
      if (nrow(filter(thisKey, CountryCode == cou))>0){# country has data for this indicator
        
        rank[i] <- which(thisKey$CountryCode == cou)
        # print indicator name
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1, 1.1,thisKey$IndicatorShort[1], col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=3, adj=0)
        graphics::text(1, 0.75,str_wrap(paste0(thisKey$Unit[1], " (",thisKey$Period[1],")"), width = str_wrap_size+8), col="#818181", cex=2, adj = 0)
        # print data point and rank
        if (!rankBig){ # rank bigger than actual value
          plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
          graphics::text(1.17, 1,filter(thisKey,CountryCode==cou)$Observation , col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=5)
          graphics::text(1.42, 0.95,paste0("(Rank: ",rank[i],"/",rankedTotal[i],")"), col="#818181", cex=3, adj=0)
        } else {
          plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
          graphics::text(1.2, 1,paste0(rank[i],"/",rankedTotal[i]) , col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=5)
          graphics::text(1.5, 1,paste0("Value: ",filter(thisKey,CountryCode==cou)$Observation), col="#818181", cex=3, adj=0)
        }
        
      
      } else { # no data for this indicator
        
        # print indicator name
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1, 1.1,thisKey$IndicatorShort[1], col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=3, adj=0)
        graphics::text(1, 0.75,paste0(thisKey$Unit[1], " (",thisKey$Period[1],")"), col="#818181", cex=2, adj = 0)
        # print data point and rank
        if (!rankBig){ # rank bigger than actual value
          plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
          graphics::text(1.17, 1," " , col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=8)
          graphics::text(1.42, 0.95,paste0("(Rank: /",rankedTotal[i],")"), col="#818181", cex=3, adj=0)
        } else {
          plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
          graphics::text(1.17, 1,paste0("NA/",rankedTotal[i]), col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=5)
          graphics::text(1.5, 1," ", col="#818181", cex=4, adj=0)
        }
      }
      i <- i + 1
    }

  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}

## ---- number_chart_yesno ----
number_chart_yesno <- function(Report_data,reportConfig,couName,section,table,str_wrap_size){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Report_data, CountryCode==cou,  Subsection %in% table)
  data <- data %>%
    filter(!(is.na(Observation))) %>%
    mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
    distinct(Key,Period,.keep_all=TRUE)
  
  dataWorld <- filter(Report_data,  Subsection %in% table)
  dataWorld <- filter(dataWorld,!is.na(Observation))
  dataWorld <- dataWorld %>%
    group_by(Country,Key) %>%
    filter( Period == max(Period,na.rm=TRUE))%>%     # mutate(Period = max(Period,na.rm=TRUE)) %>%
    distinct(Key, Period, .keep_all = TRUE) %>%
    as.data.frame()
  
  dataWorld <- dataWorld %>%
    group_by(Key) %>%
    mutate(Observation = round(Observation,1)) %>%
    arrange(desc(Observation)) %>%
    as.data.frame()
  
  if (nrow(data)>0){
    
    require(stringr) # to wrap label text
    # Print the combo -----------------------------------------------
    par(family = 'serif',mfrow=c(length(unique(dataWorld$Key)),2), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,0), #sets margin size for the figures
        oma=c(0,1,0,0)) #sets outer margin
    
    i <- 1
    rankedTotal <- c()
    rank <- c()
    for (ind in unique(dataWorld$Key)){
      thisKey <- filter(dataWorld, Key == ind)
      thisKey <- mutate(thisKey, Unit = ifelse(grepl("0-100",Unit),"100=full ownership allowed",Unit))
      thisKey <- mutate(thisKey, IndicatorShort = str_wrap(paste0(IndicatorShort), width = str_wrap_size))
      rankedTotal[i] <- nrow(thisKey)
      
      if (nrow(filter(thisKey, CountryCode == cou))>0){# country has data for this indicator
        
        rank[i] <- which(thisKey$CountryCode == cou)
        # print indicator name
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1, 1.1,thisKey$IndicatorShort[1], col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=3, adj=0)
        # print data point and rank
          #remap 1/0 back to Yes/No
          temp_val <- filter(thisKey,CountryCode==cou)$Observation
          if (temp_val == 1){
            temp_val <- "Yes"
          } else if (temp_val == 0){
            temp_val <- "No"
          }
          plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
          if (temp_val == 'Yes'){
            temp_col="darkgreen"
          } else {temp_col="red"}
          graphics::text(1, 1,paste0(temp_val), col=temp_col, cex=3, adj=0)
        
        } else { # no data for this indicator
        
        # print indicator name
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1, 1.1,thisKey$IndicatorShort[1], col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=3, adj=0)
        graphics::text(1, 0.75,paste0(thisKey$Unit[1], " (",thisKey$Period[1],")"), col="#818181", cex=2, adj = 0)
        # print data point and rank
        if (!rankBig){ # rank bigger than actual value
          plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
          graphics::text(1.17, 1," " , col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=8)
          graphics::text(1.42, 0.95,paste0("(Rank: /",rankedTotal[i],")"), col="#818181", cex=3, adj=0)
        } else {
          plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
          graphics::text(1.17, 1,paste0("NA/",rankedTotal[i]), col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=5)
          graphics::text(1.5, 1," ", col="#818181", cex=4, adj=0)
        }
      }
      i <- i + 1
    } 
    } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}

## ---- bar_facewrap_chart_fcv ----
bar_facewrap_chart_fcv <- function(Report_data,reportConfig,couName, section, table, append_unit = TRUE, max_num_comparators = 4, dataset, str_wrap_size = 20, range_vals){      
  
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  country <- as.character(countries[countries$iso3==cou,]$Country)
  
  # Get all possible neighbors
  neighbors <- get_fcv_comparators(couName, countries)
  
  # Get top neighbors based on identified dataset
  dataNeighbor <- filter(Report_data, CountryCode %in% c(neighbors), Subsection2 %in% dataset, !is.na(Observation))
  
  if (nrow(filter(dataNeighbor))>0){ 
    # maxPeriod <- max(dataNeighbor[complete.cases(dataNeighbor$Observation),]$Period)
    maxPeriod <- max(filter(Report_data, CountryCode==cou, Subsection2 %in% dataset, !is.na(Observation))$Period)
    dataNeighbor <- filter(dataNeighbor, Period==maxPeriod)
    dataset_ave <- aggregate(dataNeighbor$Observation, list(dataNeighbor$CountryCode), mean) %>%
      arrange(desc(x))
    topNeighbors <- head(dataset_ave, max_num_comparators)$Group.1
    
    # Get data for bar chart
    data <- filter(Report_data, CountryCode %in% c(cou,topNeighbors), Subsection %in% table) %>%
      mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear)-1),Period),
             Observation = Observation/ifelse(is.na(Scale),1,Scale))
    
    if (nrow(filter(data, CountryCode==cou))>0){
      
      data <- data %>%
        filter(!is.na(Observation)) %>%
        group_by(Key,Country) %>%
        filter(Period == maxPeriod) %>%
        distinct(Key,CountryCode, .keep_all = TRUE)
      
      if (nrow(filter(data, CountryCode==cou))>0){
        order_legend <- c(couName,as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country)))
        country_order <- factor(order_legend, levels = c(couName,order_legend[2:length(order_legend)]))
        my_order <- data.frame(Country = country_order, order = seq(1,length(order_legend),1))
        data <- merge(data,my_order, by="Country") %>%
          arrange(order) 
            
        require(stringr) # to wrap label text
        
        if (append_unit) {
          data$Unit <- paste0(", ",data$Unit)
        } else data$Unit <- ""
          
        maxPeriod_thisCou <- filter(data, CountryCode==cou)$Period[1]
        data <- data %>%
          group_by(Key) %>%
          filter(Period == max(Period,na.rm=TRUE)) %>%
          mutate(IndicatorShort = str_wrap(paste0(IndicatorShort, Unit," (",Period,")"), width = str_wrap_size)) %>%
          filter(order < 6) %>%
          as.data.frame()
        
        ggplot(data, aes(x=reorder(Country,order),y=Observation,fill=reorder(Country,order),alpha=reorder(Country,order))) +
          geom_bar(position="dodge",stat="identity") +
          scale_y_discrete(limits=range_vals) +
          coord_flip(ylim = range_vals) +
          facet_wrap(~IndicatorShort,scales="fixed", ncol=length(unique(data$IndicatorShort))) +
          theme(strip.text.x = element_text(family="Times", size = 12, colour = "white"),
                strip.background = element_rect(colour = paste0("#",filter(reportConfig, Section_Level == 10)$Color), fill = paste0("#",filter(reportConfig, Section_Level == 10)$Color)),
                legend.key=element_blank(),
                legend.title=element_blank(),
                legend.text = element_text(family="Times", size = 10, colour = "#818181"),
                panel.border = element_blank(),
                panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
                #axis.ticks.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(family="Times", color="#818181")) + 
          labs(x="",y="")+#,title="World Governance Indicators")+
          scale_fill_manual(breaks=order_legend,values = c("orange","brown","lightblue","lightgreen","pink")) +
          scale_alpha_manual(labels = order_legend,values = c(1, rep(1,4)),guide=FALSE)
        } else {
          plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
          graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)}
      } else {
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)}
    } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}

## ---- bar_facewrap_chart ----
bar_facewrap_chart <- function(Report_data,reportConfig,couName, section, table, vertical_bars = TRUE, append_unit = TRUE, str_wrap_size = 20){      
  
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  
  couRegion <- countries[countries$iso3==cou,]$region  # obtain the region for the selected country
  neighbors <- countries[countries$region==couRegion,]$iso3 # retrieve all countries in that region
  neighbors <- as.character(neighbors[!(neighbors==cou)]) # exclude the selected country
  
  data <- Report_data %>%
    filter(CountryCode %in% c(cou,neighbors), Section==section, Subsection==table) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear)-1),Period),
           Observation = Observation/ifelse(is.na(Scale),1,Scale))
  
  if (nrow(filter(data, CountryCode==cou))>0){
    
    data <- data %>%
      filter(!is.na(Observation)) %>%
      group_by(Key,Country) %>%
      filter(Period == max(Period,na.rm=TRUE)) %>%
      distinct(Key,CountryCode, .keep_all = TRUE)
    # select top 4 countries from the neighborhood based on their income level
    income <- filter(Report_data, CountryCode %in% neighbors & Section=="aux_income")
    income <- income %>%
      group_by(CountryCode) %>%
      filter(!is.na(Observation), Period < thisYear) %>%
      filter(Period == max(Period,na.rm=TRUE))
    
    topNeighbors <- head(arrange(as.data.frame(income), desc(Observation)),15)$CountryCode
    data <- filter(data, CountryCode %in% c(cou,topNeighbors))
    data$IndicatorShort <- gsub(" Index","",data$IndicatorShort)
    #data <- group_by(Key,Country) %>%
    #  filter(Period == max(Period))
    
    order_legend <- c(couName,as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country)))
    country_order <- factor(order_legend, levels = c(couName,order_legend[2:length(order_legend)]))
    my_order <- data.frame(Country = country_order, order = seq(1,length(order_legend),1))
    data <- merge(data,my_order, by="Country") %>%
      arrange(order) 
#     
    require(stringr) # to wrap label text
    
    if (append_unit) {
      data$Unit <- paste0(", ",data$Unit)
    } else data$Unit <- ""
    
    if (vertical_bars == TRUE){

      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period,na.rm=TRUE)) %>%
        mutate(IndicatorShort = str_wrap(paste0(IndicatorShort, Unit," (",Period,")"), width = str_wrap_size)) %>%
        filter(order < 6) %>%
        as.data.frame()
      
      ggplot(data, aes(x=reorder(Country,order),y=Observation,fill=reorder(Country,order),alpha=reorder(Country,order))) +
        geom_bar(position="dodge",stat="identity") +
        #coord_flip()+
        facet_wrap(~IndicatorShort,scales="free_y") +
        theme(strip.text.x = element_text(family="Times", size = 12, colour = "white"),
              strip.background = element_rect(colour = paste0("#",filter(reportConfig, Section_Level == 10)$Color), fill = paste0("#",filter(reportConfig, Section_Level == 10)$Color)),
              legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_text(family="Times", size = 10, colour = "#818181"),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
              #axis.ticks.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(family="Times", color="#818181")
              ) + 
        labs(x="",y="")+#,title="World Governance Indicators")+
        scale_fill_manual(breaks = order_legend,values = c("orange","brown","lightblue","lightgreen","pink")) +
        scale_alpha_manual(labels = order_legend,values = c(1, rep(1,4)),guide=FALSE)
      
    } else{
      
      maxPeriod_thisCou <- filter(data, CountryCode==cou)$Period[1]
      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period,na.rm=TRUE)) %>%
        mutate(IndicatorShort = str_wrap(paste0(IndicatorShort, Unit," (",Period,")"), width = str_wrap_size)) %>%
        filter(order < 6) %>%
        as.data.frame()
      
      ggplot(data, aes(x=reorder(Country,order),y=Observation,fill=reorder(Country,order),alpha=reorder(Country,order))) +
        geom_bar(position="dodge",stat="identity") +
        coord_flip() +
        facet_wrap(~IndicatorShort,scales="free_x") +
        theme(strip.text.x = element_text(family="Times", size = 12, colour = "white"),
              strip.background = element_rect(colour = paste0("#",filter(reportConfig, Section_Level == 10)$Color), fill = paste0("#",filter(reportConfig, Section_Level == 10)$Color)),
              legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_text(family="Times", size = 10, colour = "#818181"),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
              #axis.ticks.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(family="Times", color="#818181")) + 
        labs(x="",y="")+#,title="World Governance Indicators")+
        scale_fill_manual(breaks=order_legend,values = c("orange","brown","lightblue","lightgreen","pink")) +
        scale_alpha_manual(labels = order_legend,values = c(1, rep(1,4)),guide=FALSE)
    }
    
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}

## --- radar_chart_fcv ---
radar_chart_fcv <- function(Report_data,reportConfig,couName,section,table,max_num_comparators = 4, shortlist_tcdata360_id, radar_fontsize=1.1){      
  
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  country <- as.character(countries[countries$iso3==cou,]$Country)
  
  # Get all possible neighbors
  neighbors <- get_fcv_comparators(couName, countries)
  
  # Get top neighbors based on indicator identified in shortlist_tcdata360_id
  dataNeighbor <- filter(Report_data, CountryCode %in% c(neighbors), Key %in% shortlist_tcdata360_id, !is.na(Observation))
  maxPeriod <- max(filter(Report_data, CountryCode==cou, Key %in% shortlist_tcdata360_id, !is.na(Observation))$Period)
  dataNeighbor <- filter(dataNeighbor, Period==maxPeriod) %>%
    arrange(desc(Observation))
  topNeighbors <- head(dataNeighbor, max_num_comparators)$CountryCode
  
  # Get data for radar chart
  data <- filter(Report_data, CountryCode %in% c(cou,topNeighbors), Subsection %in% table) %>%
    mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale))
  
  if (nrow(filter(data, CountryCode==cou))>0){  

    # add the max and min columns to make it work:
    obs_allCountries <- filter(Report_data, Subsection %in% table) %>%
      mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale))
    max <- ceiling(max(obs_allCountries$Observation, na.rm = TRUE))
    min <- floor(min(obs_allCountries$Observation, na.rm = TRUE))
    data <- as.data.frame(data)
    data <- cbind(data,max,min)
    data <- filter(data, Period == maxPeriod)
    
    thisPeriod <- data$Period[1]
    ind_name <- data[data$Country == couName, 'IndicatorShort']
    dataTrans <- reshape(data[c('Country', 'Key', 'Observation')], idvar = "Country", timevar = "Key", direction = "wide")
    new_col_names <- c(dataTrans$Country, "max", "min")
    order_legend <- c(couName,as.character(dataTrans$Country[!(dataTrans$Country==couName)]))
    dataTrans <- rbind(dataTrans, max, min)
    dataTrans$Country  <- new_col_names
    
    #reorder dataTrans based on order_legend
    dataTrans <- dataTrans[match(c("max", "min", order_legend), dataTrans$Country),]
    rownames(dataTrans) <- dataTrans$Country
    dataTrans$Country <- NULL
    
    color_list <- c(paste0("#",filter(reportConfig, Section_Level == 10)$Color), "red", "orange", "darkgreen", "darkgray")
    num_countries <- nrow(dataTrans)-2
    
    # catch error if number of variables is less than 3 (radarchart requires 3 or more non-NULL variables).
    if (ncol(dataTrans) > 2){
      layout(matrix(c(2,1), ncol=2), widths = c(7,2))
      
      # plot legend
      par(family = 'serif',mar=c(0,0,0,0))
      plot(c(1.75,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      legend(1,1.5, legend=unique(order_legend), seg.len=0.5, pch=19, inset=50, cex=1.5,
             bty="n" ,lwd=3, x.intersp=0.5, horiz=FALSE, col=color_list[1:num_countries])
      
      par(mar=c(1,1,1,1),family="serif")
      
      radarchart(dataTrans, axistype=1, centerzero = FALSE,seg=4, caxislabels=c(min,"",(min+max)/2,"",max),
                 pcol=color_list[1:num_countries], cglwd=2,axislabcol="lightgrey",
                 plty=c(1,rep(1,as.numeric(num_countries-1))),plwd=c(8,rep(3,as.numeric(num_countries-1))),
                 pdensity=rep(0,num_countries),
                 vlabels=ind_name, cex.main=1,cex=2.5,vlcex = radar_fontsize)
      
    } else{
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }  
  
}

## ---- radar_chart ----
radar_chart <- function(Report_data,reportConfig,couName,section,table,neighbor = "region"){      
  
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  country <- as.character(countries[countries$iso3==cou,]$Country)
  
  if (neighbor=="region"){ # region level
    couRegion <- as.character(countries[countries$iso3==cou,]$region)  # obtain the region for the selected country
    neighbors <- countries[countries$region==couRegion,]$iso3 # retrieve all countries in that region
    neighbors <- as.character(neighbors[!(neighbors==cou)]) # exclude the selected country
    region <- as.character(countries[countries$iso3==cou,]$region) 
    data <- filter(Report_data, CountryCode %in% c(cou,neighbors), Subsection %in% table) %>%
      mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale))
  } else { # income level 
    couRegion <- as.character(countries[countries$iso3==cou,]$incomeLevel)  # obtain the region for the selected country
    neighbors <- countries[countries$incomeLevel==couRegion,]$iso3 # retrieve all countries in that region
    neighbors <- as.character(neighbors[!(neighbors==cou)]) # exclude the selected country
    region <- as.character(countries[countries$iso3==cou,]$incomeLevel) 
    data <- filter(Report_data, CountryCode %in% c(cou,neighbors), Subsection %in% table) %>%
      mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale))
  }
  
  if (nrow(filter(data, CountryCode==cou))>0){  
    
    # calculate the average for the region
    data <- data %>%
      filter(!is.na(Observation)) %>%
      #group_by(Key) %>%
      #mutate(Observation = ifelse(Observation > 0 & Observation < 1, Observation*100,Observation)) %>%
      filter(Period == max(Period)) %>%
      dplyr::group_by(IndicatorShort) %>%
      dplyr::mutate(regionAvg = mean(Observation)) %>%
      filter(region==couRegion) %>%
      #filter(CountryCode==cou) %>%
      as.data.frame()
  
    # I must add the max and min columns to make it work:
    obs_allCountries <- filter(Report_data, Subsection %in% table) %>%
      mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale))
    max <- ceiling(max(obs_allCountries$Observation, na.rm = TRUE))
    min <- floor(min(obs_allCountries$Observation, na.rm = TRUE))
    data <- as.data.frame(data)
    data <- cbind(data,max,min)
    
    dataWorld <- obs_allCountries %>%
      filter(!is.na(Observation)) %>%
      filter(Period == max(Period)) %>%
      dplyr::group_by(IndicatorShort) %>%
      dplyr::mutate(worldAvg = mean(Observation)) %>%
      #filter(CountryCode==cou) %>%
      as.data.frame()
    # add the world average to data
    data <- merge(data, dataWorld[,c("Key","worldAvg")], by="Key")
    # order labels ad-hoc:
    #order <- c(1,3,4,6,2,5)
    #data <- cbind(data,order)
    
    #data <- arrange(data,order) %>%
    thisPeriod <- data$Period[1]
    data <- data%>%filter(CountryCode==cou)
    data <- unique(data)
    data <- select(data,IndicatorShort, max, min, Observation, regionAvg, worldAvg)
    # transpose the data for radarchart to read
    dataTrans <- as.data.frame(t(data[,2:ncol(data)]))
    
    # catch error if number of variables is less than 3 (radarchart requires 3 or more non-NULL variables).
    if (ncol(dataTrans) > 2){
      layout(matrix(c(2,1), ncol=2), widths = c(7,2))
      #       col.axis="red",col.lab=c("red","red"),col.main="red",col.sub="red",family="serif")
      
      par(family = 'serif',mar=c(0,0,0,0))
      plot(c(1.75,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      # legend(1,1.5, legend=c(paste0(couName," (",thisPeriod,")"),region,"World"), seg.len=0.5, pch=19, inset=50, 
      #        bty="n" ,lwd=3, cex = 1.5, x.intersp=0.5, horiz=TRUE, col=c("orange",paste0("#",filter(reportConfig, Section_Level == 10)$Color),"darkgreen"))
      legend(1,1.5, legend=c(couName,region,"World"), seg.len=0.5, pch=19, inset=50, cex=1.5,
             bty="n" ,lwd=3, x.intersp=0.5, horiz=FALSE, col=c("orange",paste0("#",filter(reportConfig, Section_Level == 10)$Color),"darkgreen"))
      
      par(mar=c(1,1,1,1),family="serif")
      
      radarchart(dataTrans, axistype=1, centerzero = FALSE,seg=4, caxislabels=c(min,"",(min+max)/2,"",max),
                       plty=c(1,2,4),plwd=c(8,4,4),pcol=c("orange",paste0("#",filter(reportConfig, Section_Level == 10)$Color),"darkgreen"),pdensity=c(0, 0, 0),
                       cglwd=2,axislabcol="lightgrey", vlabels=data$IndicatorShort, cex.main=1,cex=2.5,vlcex = 1.1)
            
      # title="WEF Competitiveness Indicators, stage of development (1-7)",
      
    } else{
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }  
  
}

## ---- radar_chart_widget ----
radar_chart_widget <- function(Report_data,reportConfig,couName,section,table){      
  
  library(radarchart)
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  
  couRegion <- countries[countries$iso3==cou,]$region  # obtain the region for the selected country
  neighbors <- countries[countries$region==couRegion,]$iso3 # retrieve all countries in that region
  neighbors <- as.character(neighbors[!(neighbors==cou)]) # exclude the selected country
  
  # country and Region descriptors
  country <- as.character(countries[countries$iso3==cou,]$Country)
  region <- as.character(countries[countries$iso3==cou,]$region) 
  # filter the data
  data <- filter(Report_data, CountryCode %in% c(cou,neighbors), Section == section, Subsection==table) %>%
    mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale))
  
  if (nrow(filter(data, CountryCode==cou))>0){  
    # calculate the average for the region
    data <- data %>%
      filter(!is.na(Observation)) %>%
      group_by(Key) %>%
      mutate(Observation = ifelse(Observation > 0 & Observation < 1, Observation*100,Observation)) %>%
      filter(Period == max(Period)) %>%
      mutate(regionAvg = mean(Observation, na.rm=TRUE)) %>%
      filter(CountryCode==cou) %>%
      as.data.frame()
    
    # I must add the max and min columns to make it work:
    obs_allCountries <- filter(Report_data, Section == section, Subsection==table) %>%
      mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale))
    max <- ceiling(max(obs_allCountries$Observation, na.rm = TRUE))
    min <- floor(min(obs_allCountries$Observation, na.rm = TRUE))
    data <- as.data.frame(data)
    data <- cbind(data,max,min)
    
    # order labels ad-hoc:
    order <- c(1,3,4,6,2,5)
    data <- cbind(data,order)
    
    data <- arrange(data,order) %>%
      select(IndicatorShort, max, min, Observation, regionAvg)
    
    # plot radarchart
    labs <- c("Communicator", "Data Wangler", "Programmer",
              "Technologist",  "Modeller", "Visualizer")
    
    scores <- list(
      "Rich" = c(9, 7, 4, 5, 3, 7),
      "Andy" = c(7, 6, 6, 2, 6, 9),
      "Aimee" = c(6, 5, 8, 4, 7, 6)
    )
    
    chartJSRadar(scores = scores, labs = labs, maxScale = 10)
    
    # transpose the data for radarchart to read
    # dataTrans <- as.data.frame(t(data[,2:ncol(data)]))
    # layout(matrix(c(1,2),ncol=1), heights =c(4,1))
    # #       col.axis="red",col.lab=c("red","red"),col.main="red",col.sub="red",family="serif")
    # par(mar=c(0,1,3,1),family="serif")
    
    # radarchart(dataTrans, axistype=1, centerzero = FALSE,seg=4, caxislabels=c(min,"",(min+max)/2,"",max),
    #            plty=c(1,1),plwd=c(6,3),pcol=c("orange","lightblue"),pdensity=c(0, 0),
    #            cglwd=2,axislabcol="lightgrey", vlabels=data$IndicatorShort, cex.main=1,cex=2.5)
    
    #title="WEF Competitiveness Indicators, stage of development (1-7)",
    # par(family = 'serif',mar=c(0,1,1,1))
    # plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    # legend(1,1.5, legend=c(couName,region), seg.len=0.5, pch=19, inset=50, 
    #        bty="n" ,lwd=3, x.intersp=0.5, horiz=TRUE, col=c("orange","lightblue"))
    # 
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }  
  
}

## ---- combo_percent ----
combo_percent <- function(Report_data,reportConfig,couName,section,table){      
  
  cou <- .getCountryCode(couName)
  
  data <- filter(Report_data, CountryCode==cou, Section == section, Subsection==table)
  if (is.na(data$Period)) data$Period <- as.character(as.numeric(thisYear) - 1)
  
  if (nrow(filter(data, CountryCode==cou))>0){
    data <- filter(data, Period==max(Period))
    data <- select(data, IndicatorShort, Observation)
    pickColor <- ifelse(data$Observation > 50,"darkgreen","red")
    pickMirror <- ifelse(data$Observation > 50,FALSE,TRUE)
    data <- rbind(data, c(" ",0)) # add "Other" category
    data$Observation <- round(as.numeric(data$Observation),2)
    data$color <- c(pickColor,"lightgrey") # add the color
    data[data$IndicatorShort==" ",]$Observation <- 100 - sum(data$Observation)
    
    # format numbers
    data$Observation <- format(data$Observation, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    data$Observation <- as.numeric(data$Observation)
    data <- data %>%
      mutate(ObsLabel = paste0(Observation,"%")) %>%
      arrange(desc(IndicatorShort))
    
    data$ObsLabel[2] <- ""
    
    treemap(data,
            index=c("IndicatorShort","ObsLabel"),
            vSize="Observation",
            fontsize.labels=c(24, 64), 
            align.labels=list(c("left", "top"), c("center","center")),
            lowerbound.cex.labels=0.5,
            vColor="color",
            type="color",
            mirror.x = pickMirror,
            title="")
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }  
  
}


## ---- combo_rate ----
combo_rate <- function(Report_data,reportConfig,couName){      
  
  cou <- .getCountryCode(couName)
  
  data <- filter(Report_data, CountryCode==cou, Section == "Human capital", Subsection=="combo1")
  if (is.na(data$Period)) data$Period <- as.character(as.numeric(thisYear) - 1)
  
  if (nrow(filter(data, CountryCode==cou))>0){
    data <- filter(data, Period==max(Period))
    data <- select(data, IndicatorShort, Observation)
    pickColor <- ifelse(data$Observation >= 3.5,"green","red")
    pickMirror <- ifelse(data$Observation >= 3.5,FALSE,TRUE)
    data <- rbind(data, c(" ",0)) # add "Other" category
    data$Observation <- round(as.numeric(data$Observation),2)
    data$color <- c(pickColor,"lightgrey") # add the color
    data[data$IndicatorShort==" ",]$Observation <- 7 - sum(data$Observation)
    
    # format numbers
    data$Observation <- format(data$Observation, digits=2, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    data$Observation <- as.numeric(data$Observation)
    data <- data %>%
      mutate(ObsLabel = as.character(Observation)) %>%
      arrange(desc(IndicatorShort))
    
    data$ObsLabel[2] <- ""
    
    treemap(data,
            index=c("IndicatorShort","ObsLabel"),
            vSize="Observation",
            fontsize.labels=c(24, 64), 
            align.labels=list(c("left", "top"), c("center","center")),
            lowerbound.cex.labels=0.5,
            vColor="color",
            type="color",
            mirror.x = pickMirror,
            title="")
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }  
  
}

## ---- table_region ----
table_region <- function(Report_data,reportConfig,couName,section,table){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Report_data, CountryCode==cou, Section == section, Subsection==table) #select country, region and world
  if (nrow(data[data$CountryCode==cou,])>0){
    
    couRegion <- as.character(countries[countries$CountryCodeISO3==cou,]$RegionCodeES)  # obtain the region for the selected country
    data <- filter(Report_data, CountryCode %in% c(cou,couRegion, "RWe"), Section == section, Subsection==table) #select country, region and world
    
    # country, Region, World descriptors
    country <- as.character(countries[countries$CountryCodeISO3==cou,]$Country)
    region <- as.character(countries[countries$CountryCodeISO3==cou,]$Region) 
    world <- "All Countries"
    
    neighbors <- data.frame(CountryCode=c(cou,couRegion,"RWe"),colName=c(country,region,world), stringsAsFactors = FALSE)
    
    # remove NAs rows
    #data <- filter(data, !is.na(Observation))
    # keep the latest period (excluding projections further than 2 years)
    data <- data %>%
      group_by(Key,CountryCode) %>%
      mutate(Period = max(Period,na.rm = TRUE))
    
    # Scale Observations
    data <- mutate(data, ObsScaled = Observation)
    data <- arrange(data, Key)
    #data <- select(data, Key, IndicatorShort, Period, ObsScaled)
    # restrict to 2 decimal places
    data$ObsScaled <- round(data$ObsScaled,2)
    # format numbers
    data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
    
    data <- distinct(data, Key, .keep_all = TRUE)
    #data <- filter(data, Period <= (as.numeric(thisYear) + 1))
    
    # prepare for table
    data <- merge(data, neighbors, by="CountryCode")
    data <- mutate(data, Unit = ifelse(grepl("population",Unit),"per 100 pop.",Unit),
                   IndicatorShort = paste0(IndicatorShort," (",Unit,")"))
    data <- select(data, IndicatorShort, Observation, colName)
    data$Observation <- format(data$Observation, digits=2, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    
    data <- spread(data, colName, Observation)
    if (ncol(data)==4){
      data <- data[,c(1,4,3,2)]  
    }
    names(data)[1] <-""
    #names(data) <- str_wrap(names(data),width=12)
    
    # I have to add a dummy column so the alignment works (align)
    data$dummy <- rep("",nrow(data))
    names(data)[ncol(data)] <-""
    
    # make sure there are always 3 rows on the table to avoid black stripes on following table
    if (nrow(data)<3){
      for (r in 1:(3-nrow(data))){
        data <- rbind(data,c("",rep("",ncol(data))))
      }
    }
    
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
    if (nrow(data)>1){
      rowsSelect <- seq(1,nrow(data)-1,2)
    } else{
      rowsSelect <- c(1)
    }
    col <- rep("\\rowcolor[gray]{0.95}", length(rowsSelect))
    data.table <- xtable(data)
    align(data.table) <- c('l','l',rep('>{\\raggedleft}p{1.2in}',(ncol(data)-2)),'l')
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\Large",add.to.row = list(pos = as.list(rowsSelect), command = col),
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center")
    
  } else{
    
    data[!is.na(data)] <- ""
    #data <- select(data, Key)
    names(data) <- c(" ",rep(" ",ncol(data)-1))
    data.table <- xtable(data)
    align(data.table) <- rep('l',ncol(data)+1)
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\tiny",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center")
  } 
  
}

## ---- table_countries ----
table_countries <- function(Report_data,reportConfig,couName,section,table,compareCountries = 3,appendUnits = FALSE){      
  
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  
  couRegion <- countries[countries$iso3==cou,]$region  # obtain the region for the selected country
  neighbors <- countries[countries$region==couRegion,]$iso3 # retrieve all countries in that region
  neighbors <- as.character(neighbors[!(neighbors==cou)]) # exclude the selected country
  
  data <- Report_data %>%
    filter(CountryCode %in% c(cou,neighbors), Section==section, Subsection==table) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear)-1),Period),
           Observation = Observation/ifelse(is.na(Scale),1,Scale))
  
  if (nrow(filter(data, CountryCode==cou))>0){
    
    data <- data %>%
      filter(!is.na(Observation)) %>%
      group_by(Key,Country) %>%
      filter(Period == max(Period,na.rm=TRUE)) %>%
      distinct(Key,CountryCode, .keep_all = TRUE)
    # select top 4 countries from the neighborhood based on their income level
    income <- filter(Report_data, CountryCode %in% neighbors & Section=="aux_income")
    income <- income %>%
      group_by(CountryCode) %>%
      filter(!is.na(Observation), Period < thisYear) %>%
      filter(Period == max(Period,na.rm=TRUE))
    
    topNeighbors <- head(arrange(as.data.frame(income), desc(Observation)),15)$CountryCode
    data <- filter(data, CountryCode %in% c(cou,topNeighbors))
    data$IndicatorShort <- gsub(" Index","",data$IndicatorShort)
    #data <- group_by(Key,Country) %>%
    #  filter(Period == max(Period))
    
    order_legend <- c(couName,as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country)))
    country_order <- factor(order_legend, levels = c(couName,order_legend[2:length(order_legend)]))
    my_order <- data.frame(Country = country_order, order = seq(1,length(order_legend),1))
    data <- merge(data,my_order, by="Country") %>%
      arrange(order) 
    
    thisPeriod <- data$Period[1]
    # rearrange data to create the table and append or not units to indicators    
    if (appendUnits){
      data <- select(data, Country, Observation, IndicatorShort, Unit) %>%
        spread(Country, Observation) %>%
        mutate(IndicatorShort = paste0(IndicatorShort, ",", Unit)) %>%
        select(IndicatorShort, get(couName), everything(), -Unit)
    } else {
      data <- select(data, Country, Observation, IndicatorShort, Unit) %>%
        spread(Country, Observation) %>%
        select(IndicatorShort, get(couName), everything(), -Unit)
    }
    # keep compareCountries neighbour countries at most
    if (ncol(data) > (compareCountries + 2)) data <- data[,c(1:(compareCountries + 2))]
    #
    require(stringr) # to wrap label text
    
    # I have to add a dummy column so the alignment works (align)
    data$dummy <- rep("",nrow(data))
    names(data)[ncol(data)] <-""
    # remove the column name for the indicators
    names(data)[1] <- ""
    # make sure there are always 3 rows on the table to avoid black stripes on following table
    if (nrow(data)<3){
      for (r in 1:(3-nrow(data))){
        data <- rbind(data,c("",rep("",ncol(data))))
      }
    }
    
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
    if (nrow(data)>1){
      rowsSelect <- seq(1,nrow(data)-1,2)
    } else{
      rowsSelect <- c(1)
    }
    col <- rep("\\rowcolor[gray]{0.95}", length(rowsSelect))
    data.table <- xtable(data)
    align(data.table) <- c('l','>{\\raggedright}p{3in}',rep('>{\\raggedleft}p{0.8in}',(ncol(data)-2)),'l')
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\Large",add.to.row = list(pos = as.list(rowsSelect), command = col),
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center")
    
  } else{
    
    data[!is.na(data)] <- ""
    #data <- select(data, Key)
    names(data) <- c(" ",rep(" ",ncol(data)-1))
    data.table <- xtable(data)
    align(data.table) <- rep('l',ncol(data)+1)
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\tiny",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center")
  } 
  
}

## ---- doing_business_table ----
doing_business_table <- function(Report_data,reportConfig,couName){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Report_data, CountryCode == cou, grepl("dbtable",Subsection), !is.na(Observation)) #select country, region and world
  
  if (nrow(data[data$CountryCode==cou,])>0){
    
    # prepare for table
    data <- select(data, Subsection, IndicatorShort, Period, Observation) %>%
      arrange(Subsection, Period, IndicatorShort)
    # format numbers
    #data$Observation <- format(data$Observation, digits=0, decimal.mark=".",
    #                                                       big.mark=",",small.mark=".", small.interval=3)
    
    data$Observation <- as.numeric(data$Observation)
    dataR <- data %>%
      filter(grepl("R",Subsection)) %>%
      select(-Subsection)
    
    # adjusted thisYear by 2 years (instead of 1) since TCdata360 Doing Business data displays year when data was collected
    # instead of year Doing Business report was released
    dataDTF <- data %>%
      filter(grepl("DTF",Subsection), Period >= as.numeric(thisYear)-2) %>%
      select(-Subsection)
    
    dataR <- spread(dataR, Period, Observation)
    dataDTF <- spread(dataDTF, Period, Observation)
    
    # calculate difference in Rank
    dataR$ChangeRank <- dataR[,2] - dataR[,3]
    dataDTF$ChangeDTF <- round(dataDTF[,3] - dataDTF[,2],2)
    
    # red for negative, green for positive changes
    dataR <- mutate(dataR, ChangeRank = ifelse(ChangeRank<0, paste0("\\color{red}{",ChangeRank,"}"),
                                               ifelse(ChangeRank>0, paste0("\\color{green}{",ChangeRank,"}"),paste0("\\color{gray}{",ChangeRank,"}"))))
    dataDTF <- mutate(dataDTF, ChangeDTF = ifelse(ChangeDTF<0, paste0("\\color{red}{",ChangeDTF,"}"),
                                                  ifelse(ChangeDTF>0, paste0("\\color{green}{",ChangeDTF,"}"),paste0("\\color{gray}{",ChangeDTF,"}"))))
    
    names(dataR) <- c("",paste("Rank",names(dataR)[2]),paste("Rank",names(dataR)[3]),"Rank Change")
    names(dataDTF) <- c("",paste("DTF",names(dataDTF)[2]),paste("DTF",names(dataDTF)[3]),"DTF Change")
    # put them together in 1 table
    data <- cbind(dataDTF,dataR[,-c(1)])
    # reorder rows. Want overall indicator on top
    order <- c(2,1,seq(3,nrow(data),1))
    data <- cbind(data,order)
    data <- arrange(data, order)
    data <- select(data, -order)
    # I have to add a dummy column so the alignment works (align)
    data$dummy <- rep("",nrow(data))
    names(data)[1] <- "" 
    names(data)[ncol(data)] <-""
    # highlight top row
    data[1,c(1:(ncol(data)-1))] <- paste0("\\textbf{",data[1,c(1:(ncol(data)-1))],"}")
    # add an extra header. Push current header to row1
    data_aux <- data
    data_aux[1,] <- names(data)
    for (i in 1:nrow(data)){
      data_aux[i+1,] <- data[i,]
    }
    data <- data_aux
    data[1,] <- gsub("Rank |DTF","",data[1,])
    names(data) <- c(rep("",2),"DTF",rep("",2),"Rank",rep("",2))
    
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
  } else{
    
    data[!is.na(data)] <- ""
  }   
  
  
  #align(data.table) <- c('l','l',rep('>{\\raggedleft}p{0.6in}',2),'>{\\raggedleft}p{0.8in}',"|",rep('>{\\raggedleft}p{0.6in}',2),'>{\\raggedleft}p{0.8in}','r')
  if (nrow(data)>0){
    rowsSelect <- seq(2,nrow(data)-1,2)
    col <- rep("\\rowcolor[gray]{0.95}", length(rowsSelect))
    data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
    align(data.table) <- c('l','l',rep('r',2),'r',"|",rep('r',2),'r','r')
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\normalsize", add.to.row = list(pos = as.list(rowsSelect), command = col),
          booktabs = FALSE, table.placement="", hline.after = c(1) ,latex.environments = "right",
          sanitize.text.function = function(x){x}) # include sanitize to control format like colors
  } else {
    data[1,] <- c("No data",rep("",ncol(data)-1))
    names(data) <- c(rep(" ",2),"DTF",rep(" ",2),"Rank",rep(" ",2))
    data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
    #align(data.table) <- c('l','l',rep('r',2),'r',"|",rep('r',2),'r','r')
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\normalsize", 
          booktabs = FALSE, table.placement="" ,latex.environments = "right",
          sanitize.text.function = function(x){x}) # include sanitize to control format like colors
  }
}

## ---- macroInd ----
macroInd <- function(Report_data,reportConfig,couName){      
  
  cou <- .getCountryCode(couName)
  
  tableKeys <- unique(filter(TCMN_data, Subsection=="table2head")[,c("Key","IndicatorShort")])
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="table2head")
  if (nrow(data)>0){
    #data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
    # keep the latest period (excluding projections further than 2 years)
    data <- filter(data, Period <= (as.numeric(thisYear) + 1))
    
    data <- data %>%
      group_by(Key) %>%
      filter(Period == max(Period))
    # add Period to Indicator name
    data$IndicatorShort <- paste(data$IndicatorShort, " (",data$Period,")", sep="")
    # Scale Observations
    data <- mutate(data, ObsScaled = Scale*Observation)
    # format numbers
    data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
    for (i in 1:nrow(data)){
      
      data$IndicatorShort[i] <- ifelse(!is.na(merge(data,TCMN_indic[TCMN_indic$Subsection=="table2head",], by="Key")$Note[i]),
                                       paste0(data$IndicatorShort[i]," \\large{[", merge(data,TCMN_indic[TCMN_indic$Subsection=="table2head",], by="Key")$Note[i],"]}"),
                                       data$IndicatorShort[i])  
    }
    data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
    data$IndicatorShort <- gsub("&", "\\&", data$IndicatorShort, fixed=TRUE)
    data$IndicatorShort <- gsub("$", "\\$", data$IndicatorShort, fixed=TRUE)
    
    data <- arrange(data, Key)
    data <- data[,c("IndicatorShort", "ObsScaled")] # short indicator name and scaled data
    data <- as.data.frame(t(data)) # transpose the data
    # I have to add a dummy column so the alignment works (align)
    j <- ncol(data)+1
    while (j <= 7){
      data[,j] <- ""
      names(data)[j] <- ""
      j <- j + 1
    }
    data$dummy <- rep("",nrow(data))
    
    data.table <- xtable(data)
    align(data.table) <- c('l',rep('>{\\centering}p{1.5in}',ncol(data.table)-1),'l')
    print(data.table, include.rownames=FALSE,include.colnames=FALSE, floating=FALSE, 
          size="\\LARGE", #sanitize.text.function=bold,
          booktabs = FALSE, table.placement="", hline.after = NULL ,latex.environments = "center",
          sanitize.text.function = function(x){x})
    
  }
  
}

## ---- pie_chart_double ----
pie_chart_double <- function(Report_data,reportConfig,couName,section,table){      
  
  cou <- .getCountryCode(couName)
  
  data <- Report_data %>%
    filter(CountryCode==cou & Section == section & Subsection==table) %>%
    filter(!is.na(Observation)) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear) - 1),Period),
           Observation = Observation/ifelse(is.na(Scale),1,Scale))

  couRegion <- countries[countries$iso3==cou,]$region  # obtain the region for the selected country
  # country and Region descriptors
  country <- as.character(countries[countries$iso3==cou,]$Country)
  region <- as.character(countries[countries$iso3==cou,]$region) 
  # filter the data
  dataRegion <- Report_data %>%
    filter(region==couRegion & Section == section & Subsection==table) %>%
    filter(!is.na(Observation)) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear) - 1),Period))
  
  if (nrow(filter(data, CountryCode==cou))>0){
    data <- filter(data, Period==max(Period))
    data <- select(data, IndicatorShort, Observation)
    pickColor <- ifelse(data$Observation > 50,"green","red")
    data <- rbind(data, c(" ",0)) # add "Other" category
    data$Observation <- round(as.numeric(data$Observation),2)
    data$color <- c(pickColor,"lightgrey") # add the color
    data[data$IndicatorShort==" ",]$Observation <- 100 - sum(data$Observation)
    
    # format numbers
    data$Observation <- format(data$Observation, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    data$Observation <- as.numeric(data$Observation)
    data <- data %>%
      mutate(ObsLabel = paste0(Observation,"%")) %>%
      arrange(desc(IndicatorShort))
    
    data$ObsLabel[2] <- ""
    
    if (nrow(dataRegion)>0){ # make sure dataRegion is not empty, so I show only 1 pie
      dataRegion <- filter(dataRegion, Period==max(Period))
      dataRegion <- select(dataRegion, IndicatorShort, Observation)
      pickColor <- ifelse(dataRegion$Observation > 50,"green","red")
      dataRegion <- rbind(dataRegion, c(" ",0)) # add "Other" category
      dataRegion$Observation <- round(as.numeric(dataRegion$Observation),2)
      dataRegion$color <- c(pickColor,"lightgrey") # add the color
      dataRegion[dataRegion$IndicatorShort==" ",]$Observation <- 100 - sum(dataRegion$Observation)
      
      # format numbers
      dataRegion$Observation <- format(dataRegion$Observation, digits=0, decimal.mark=".",
                                       big.mark=",",small.mark=".", small.interval=3)
      dataRegion$Observation <- as.numeric(dataRegion$Observation)
      dataRegion <- dataRegion %>%
        mutate(ObsLabel = paste0(Observation,"%")) %>%
        arrange(desc(IndicatorShort))
      
      dataRegion$ObsLabel[2] <- ""
    
      p1 <- ggplot(data, aes("",Observation,fill=IndicatorShort)) +
        geom_bar(width=1,stat="identity") +
        scale_fill_manual(values = c("lightgrey","orange"),guide=FALSE) +
        coord_polar("x",start = 0) +
        geom_text(aes(label=ObsLabel,y=0),
                  size=12,color="white") + 
        ggtitle(country) + 
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(family="Times", lineheight=.8, size = 20, colour = "darkgrey"),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()) + 
        labs(x="",y="")
      
      p2 <- ggplot(dataRegion, aes("",Observation,fill=IndicatorShort)) +
        geom_bar(width=1,stat="identity") +
        scale_fill_manual(values = c("lightgrey","darkgreen"),guide=FALSE) +
        coord_polar("x",start = 0) +
        geom_text(aes(label=ObsLabel,y=0),
                  size=12,color="white") + 
        ggtitle(paste0(region," (simple average)")) + 
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(family="Times", lineheight=.8, size = 20, colour = "darkgrey"),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()) + 
        labs(x="",y="")
      
      grid.arrange(p1,p2,ncol=2)
    
    } else{ # region is empty
      
      ggplot(data, aes("",Observation,fill=IndicatorShort)) +
        geom_bar(width=1,stat="identity") +
        scale_fill_manual(values = c("lightgrey","blue"),guide=FALSE) +
        coord_polar("x",start = 0) +
        geom_text(aes(label=ObsLabel,y=0),
                  size=12,color="white") + 
        ggtitle(country) + 
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(family="Times", lineheight=.8, size = 20, colour = "darkgrey"),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()) + 
        labs(x="",y="")
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }  
  
}

## ---- pie_chart_region ----
pie_chart_region <- function(Report_data,reportConfig,couName,section,table,neighbor="region",region=TRUE){      
  
  cou <- .getCountryCode(couName)
  
  data <- Report_data %>%
    filter(CountryCode==cou & Section == section & Subsection %in% table) %>%
    filter(!is.na(Observation)) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear) - 1),Period)) %>%
    mutate(Observation = Observation/ifelse(is.na(Scale),1,Scale))
  
  country <- as.character(countries[countries$iso3==cou,]$Country)
  
  if (neighbor=="region"){
    couRegion <- countries[countries$iso3==cou,]$region  # obtain the region for the selected country
    # filter the data
    dataRegion <- Report_data %>%
      filter(region==couRegion & Section == section & Subsection %in% table) %>%
      filter(!is.na(Observation)) %>%
      mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear) - 1),Period))
  } else {
    couRegion <- countries[countries$iso3==cou,]$incomeLevel  # obtain the region for the selected country
    # filter the data
    dataRegion <- Report_data %>%
      filter(incomeLevel==couRegion & Section == section & Subsection %in% table) %>%
      filter(!is.na(Observation)) %>%
      mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear) - 1),Period))
  }
  
  if (nrow(filter(data, CountryCode==cou))>0){
    data <- filter(data, Period==max(Period))
    maxPeriod <- data$Period
    data <- select(data, IndicatorShort, Observation)
    pickColor <- ifelse(data$Observation > 50,"green","red")
    data <- rbind(data, c(" ",0)) # add "Other" category
    data$Observation <- round(as.numeric(data$Observation),2)
    data$color <- c(pickColor,"#DCDCDC") # add the color
    data[data$IndicatorShort==" ",]$Observation <- 100 - sum(data$Observation)
    
    # format numbers
    data$Observation <- format(data$Observation, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    data$Observation <- as.numeric(data$Observation)
    data <- data %>%
      mutate(ObsLabel = paste0(Observation,"%")) %>%
      arrange(desc(IndicatorShort))
    
    data$ObsLabel[2] <- ""
    
    if (nrow(dataRegion)>0 && region == TRUE){ # make sure dataRegion is not empty, so I show only 1 pie
      
      dataRegion <- filter(dataRegion, Period==maxPeriod) %>% #max period for the selected country
        mutate(Observation = mean(Observation, is.na=TRUE)) %>%
        select(IndicatorShort, Observation) %>%
        distinct(IndicatorShort, Observation)
      pickColor <- ifelse(dataRegion$Observation > 50,"green","red")
      dataRegion <- rbind(dataRegion, c(" ",0)) # add "Other" category
      dataRegion$Observation <- round(as.numeric(dataRegion$Observation),2)
      dataRegion$color <- c(pickColor,"#DCDCDC") # add the color
      dataRegion[dataRegion$IndicatorShort==" ",]$Observation <- 100 - sum(dataRegion$Observation)
      
      # format numbers
      dataRegion$Observation <- format(dataRegion$Observation, digits=0, decimal.mark=".",
                                       big.mark=",",small.mark=".", small.interval=3)
      dataRegion$Observation <- as.numeric(dataRegion$Observation)
      dataRegion <- dataRegion %>%
        mutate(ObsLabel = paste0(Observation,"%")) %>%
        arrange(desc(IndicatorShort))
      
      dataRegion$ObsLabel[2] <- ""
        
      p1 <- ggplot(data, aes("",Observation,fill=IndicatorShort)) +
        geom_bar(width=1,stat="identity") +
        scale_fill_manual(values = c("#DCDCDC","orange"),guide=FALSE) +
        coord_polar("y",start = 0) +
        geom_text(aes(label=ObsLabel,y=15),
                  size=12,color=ifelse(data$Observation[1] > 15,"darkblue","darkblue")) + 
        ggtitle(paste0(couName," (",maxPeriod,")")) + 
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(family="Times", lineheight=.8, size = 20, colour = "#818181"),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()) + 
        labs(x="",y="")
      
      p2 <- ggplot(dataRegion, aes("",Observation,fill=IndicatorShort)) +
        geom_bar(width=1,stat="identity") +
        scale_fill_manual(values = c("#DCDCDC",paste0("#",filter(reportConfig, Section_Level == 10)$Color)),guide=FALSE) +
        coord_polar("y",start = 0) +
        geom_text(aes(label=ObsLabel,y=15),
                  size=12,color=ifelse(dataRegion$Observation[1] > 15,"darkblue","darkblue")) + 
        ggtitle(paste0(couRegion," (average, ",maxPeriod,")")) + 
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(family="Times", lineheight=.8, size = 20, colour = "#818181"),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()) + 
        labs(x="",y="")
      
        #grid.arrange(p1,p2,p1,p2,ncol=2,nrow=2)
      grid.arrange(p1,p2,ncol=2)
    
    } else {
      
      ggplot(data, aes("",Observation,fill=IndicatorShort)) +
        geom_bar(width=1,stat="identity") +
        scale_fill_manual(values = c("#DCDCDC","orange"),guide=FALSE) +
        coord_polar("y",start = 0) +
        geom_text(aes(label=ObsLabel,y=10),
                  size=12,color=ifelse(data$Observation[1] > 15,"darkblue","darkblue")) + 
        ggtitle(country) + 
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(family="Times", lineheight=.8, size = 20, colour = "#818181"),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              plot.margin=unit(c(0,0,0,0), "cm"),
              panel.spacing=unit(c(0,0,0,0), "cm")) + 
        labs(x="",y="")
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }  
  
}

## ---- pie_chart_regular ----
pie_chart_regular <- function(Report_data,reportConfig,couName,section,table){      
  
  cou <- .getCountryCode(couName)
  
  data <- Report_data %>%
    filter(CountryCode==cou & Section == section & Subsection %in% table)
  data <- filter(data, !(is.na(Observation)))
  data <- mutate(data, Period = ifelse(is.na(Period),as.character(as.numeric(thisYear) - 1),Period))
  
  # country and Region descriptors
  #country <- as.character(countries[countries$CountryCodeISO3==cou,]$Country)
  # filter the data
  if (nrow(filter(data, CountryCode==cou))>0){
    data <- filter(data, Period==max(Period))
    data$IndicatorShort <- gsub(" (%)","",data$IndicatorShort,fixed=TRUE)
    data$IndicatorShort <- gsub("with","having",data$IndicatorShort,fixed=TRUE)
    require(stringr) # to wrap label text
    data <- mutate(data, IndicatorShort = str_wrap(IndicatorShort, width = 29))
    
    data1 <- data[1,]
    data1 <- select(data1, IndicatorShort, Observation)
    pickColor <- ifelse(data1$Observation > 50,"green","red")
    data1 <- rbind(data1, c(" ",0)) # add "Other" category
    data1$Observation <- round(as.numeric(data1$Observation),2)
    data1$color <- c(pickColor,"#f1f3f3") # add the color
    data1[data1$IndicatorShort==" ",]$Observation <- 100 - sum(data1$Observation)
    
    # format numbers
    data1$Observation <- format(data1$Observation, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    data1$Observation <- as.numeric(data1$Observation)
    data1 <- data1 %>%
      mutate(ObsLabel = paste0(Observation,"%")) %>%
      arrange(desc(IndicatorShort))
    
    data1$ObsLabel[2] <- ""
    
    data2 <- data[2,]
    data2 <- select(data2, IndicatorShort, Observation)
    pickColor <- ifelse(data2$Observation > 50,"green","red")
    data2 <- rbind(data2, c(" ",0)) # add "Other" category
    data2$Observation <- round(as.numeric(data2$Observation),2)
    data2$color <- c(pickColor,"#f1f3f3") # add the color
    data2[data2$IndicatorShort==" ",]$Observation <- 100 - sum(data2$Observation)
    
    # format numbers
    data2$Observation <- format(data2$Observation, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
    data2$Observation <- as.numeric(data2$Observation)
    data2 <- data2 %>%
      mutate(ObsLabel = paste0(Observation,"%")) %>%
      arrange(desc(IndicatorShort))
    
    data2$ObsLabel[2] <- ""
    
    #if (section=="Markets")  thisColor = "blue"
    #else thisColor = "darkgreen"
    #par(family = 'serif',#mar=c(0,5,5,5),
    #    oma=c(0,5,5,5))
    
    p1 <- ggplot(data1, aes("",Observation,fill=IndicatorShort)) +
      geom_bar(width=1,stat="identity") +
      scale_fill_manual(values = c("#f1f3f3",paste0("#",filter(reportConfig, Section_Level == 10)$Color)),guide=FALSE) +
      coord_polar("y",start = 0) +
      geom_text(aes(label=ObsLabel,y=10),
                size=5,color="white") + 
      ggtitle(data1$IndicatorShort) + 
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(family="Times", lineheight=.8, size = 12, colour = "#818181"),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()) + 
      labs(x="",y="")
    
    p2 <- ggplot(data2, aes("",Observation,fill=IndicatorShort)) +
      geom_bar(width=1,stat="identity") +
      scale_fill_manual(values = c("#f1f3f3",paste0("#",filter(reportConfig, Section_Level == 10)$Color)),guide=FALSE) +
      coord_polar("y",start = 0) +
      geom_text(aes(label=ObsLabel,y=10),
                size=5,color="white") + 
      ggtitle(data2$IndicatorShort) + 
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(family="Times", lineheight=.8, size = 12, colour = "#818181"),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()) + 
      labs(x="",y="")
    
    grid.arrange(p1,p2,ncol=2)
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }  
  
}

## ---- table_time ----
table_time <- function(Report_data,reportConfig,couName,section,table){      
  
  cou <- .getCountryCode(couName)
  #table <- "table1"
  tableKeys <- unique(filter(Report_data, Section == section, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(Report_data, CountryCode==cou, Section == section, Subsection==table)
  data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
  
  if (sum(data$Observation,na.rm=TRUE)==0){ # in case this country has no data
    data$Observation <- 0
    data$Period <- as.numeric(thisYear)-1
    # To create table's reference points in the LaTeX output
    data_initial <- data
    for (per in (as.numeric(thisYear)-7):(as.numeric(thisYear)-2)){
      data_plus <- mutate(data_initial,Period = per)
      data <- bind_rows(data, data_plus)
    }
    data$Period <- as.character(data$Period)
  }
  # keep the latest period (excluding projections further than 2 years)
  data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period),
                 Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
    filter(Period <= (as.numeric(thisYear))) %>%
    # remove NAs rows
    # calculate average for 1st column
    mutate(Unit = ifelse(grepl("Active population",Unit),"% of TEA",Unit),
           IndicatorShort = paste0(IndicatorShort,", ",Unit))
  data$IndicatorShort <- gsub("Entrepreneurial","Entrepr.", data$IndicatorShort)
  data$IndicatorShort <- gsub("auditors","audit.", data$IndicatorShort)
  
  #keep only periods of interest in data
  data <- mutate(data, Period = ifelse(Period==thisYear & is.na(CountryCode),as.numeric(thisYear)-1,Period)) %>%
    filter(Period > (as.numeric(thisYear) - 7) & Period < (as.numeric(thisYear)))
  data <- mutate(data, ObsScaled = Observation) %>%
    arrange(Key) %>%
    select(Key, IndicatorShort, Period, ObsScaled)
  # restrict to 2 decimal places
  data$ObsScaled <- round(data$ObsScaled,2)
  # format numbers
  data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  
  # escape reserved characters
  data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
  data$IndicatorShort <- gsub("&", "\\&", data$IndicatorShort, fixed=TRUE)
  
  data <- distinct(data, Key,Period, .keep_all = TRUE)
  # final table format
  data <- spread(data, Period, ObsScaled)
  data <- data[,-1] #drop the Key column
  if (ncol(data)>2){ # rid of characters in numeric columns
    data[,ncol(data)] <- gsub("NA", "---", data[,ncol(data)], fixed=TRUE)
  } 
  
  # dummy columns in to keep the pdf layout
  if (ncol(data)<=4){
    for (j in (ncol(data)+1):5){
      data[,j] <- "---"
      names(data)[j] <- as.character(as.numeric(thisYear)-4+j)
    }
  }
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  # modify column names
  names(data) <- c("",names(data)[2:(ncol(data)-1)],"")
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  rowsSelect <- seq(1,nrow(data)-1,2)
  
  if (section %in% c("Culture","Supports")){
    col <- rep("\\rowcolor{white}", length(rowsSelect))
  } else {
    col <- rep("\\rowcolor[gray]{0.95}", length(rowsSelect))
  }
  
  data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','>{\\raggedright}p{4.5in}','r',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-3),'l')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large",add.to.row = list(pos = as.list(rowsSelect), command = col),
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
        sanitize.text.function = function(x){x}) # include sanitize to control formats
  
}

## ---- fcvtable ----
fcvtable <- function(Report_data,reportConfig,couName,section,table, minTime='2006'){      
  
  cou <- .getCountryCode(couName)
  tableKeys <- unique(filter(Report_data, Section == section, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(Report_data, CountryCode==cou, Section == section, Subsection==table)
  data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
  
  if (sum(data$Observation,na.rm=TRUE)==0){ # in case this country has no data
    data$Observation <- 0
    data$Period <- as.numeric(thisYear)-1
    # To create table's reference points in the LaTeX output
    data_initial <- data
    for (per in (as.numeric(minTime)):(as.numeric(thisYear)-2)){
      data_plus <- mutate(data_initial,Period = per)
      data <- bind_rows(data, data_plus)
    }
    data$Period <- as.character(data$Period)
  }
  
  data$Observation[data$Observation %in% 0] <- "\\textcolor[HTML]{818181}{N}"
  data$Observation[data$Observation %in% 1] <- "\\textbf{\\textcolor[HTML]{722FF5}{Y}}"
  
  # keep the latest period (excluding projections further than 2 years)
  data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period),
                 Observation = Observation) %>%
    filter(Period <= (as.numeric(thisYear))) %>%
    # remove NAs rows
    # calculate average for 1st column
    mutate(Unit = ifelse(grepl("Active population",Unit),"% of TEA",Unit),
           IndicatorShort = paste0(IndicatorShort,", ",Unit))
  data$IndicatorShort <- gsub("Entrepreneurial","Entrepr.", data$IndicatorShort)
  data$IndicatorShort <- gsub("auditors","audit.", data$IndicatorShort)
  
  #keep only periods of interest in data
  data <- mutate(data, Period = ifelse(Period==thisYear & is.na(CountryCode),as.numeric(thisYear)-1,Period)) %>%
    filter(Period >= (as.numeric(minTime)) & Period <= (as.numeric(thisYear)))
  data <- mutate(data, ObsScaled = Observation) %>%
    arrange(Key) %>%
    select(Key, IndicatorShort, Period, ObsScaled)
  
  # escape reserved characters
  data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
  data$IndicatorShort <- gsub("&", "\\&", data$IndicatorShort, fixed=TRUE)
  
  data <- distinct(data, Key,Period, .keep_all = TRUE)
  # final table format
  data <- spread(data, Period, ObsScaled)
  data <- data[,-1] #drop the Key column
  if (ncol(data)>2){ # rid of characters in numeric columns
    data[,ncol(data)] <- gsub("NA", "---", data[,ncol(data)], fixed=TRUE)
  } 
  
  # dummy columns in to keep the pdf layout
  if (ncol(data)<=4){
    for (j in (ncol(data)+1):5){
      data[,j] <- "---"
      names(data)[j] <- as.character(as.numeric(thisYear)-4+j)
    }
  }
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  # modify column names
  names(data) <- c("",names(data)[2:(ncol(data)-1)],"")
  
  if (section %in% c("Culture","Supports")){
    col <- rep("\\rowcolor{white}", length(rowsSelect))
  } else {
    col <- rep("\\rowcolor[gray]{0.95}", length(rowsSelect))
  }
  
  data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','>{\\raggedright}p{4.5in}','r',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-3),'l')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large",
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
        sanitize.text.function = function(x){x}) # include sanitize to control formats
  
}

## ---- fcvtext ----
fcvtext <- function(countries, couName){      
  
  title <- c("Income Group", "Resource Rich", "Fragility Class", "Land Locked",  "Small Island Developing States", "Region")
  cols <- c("incomeLevel_long", "ResourceRich", "FCVclass", "landlocked_long", "sids_long")
  body <- as.character(as.vector(countries[countries$name == couName,][cols]))
  
  if (is.na(countries[countries$name == couName, 'adminRegion'])){
    region <- countries[countries$name == couName, 'region']
  } else{
    region <- countries[countries$name == couName, 'adminRegion']
  }
  
  body <- append(body, region)
  
  # Reads title and text vectors and prints them iteratively
  if (length(title) > 0){
    par(family = 'serif',mfrow=c(length(title),1), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,2), #sets margin size for the figures
        oma=c(0,1,0,1)) #sets outer margin
    
    # print text
    for (i in 1:length(title)){
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1, 1, paste0(title[i],": ", body[i]), col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), adj = c(0,0), cex=2)
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1, 1," ", col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=2)
  }
}

## ---- fcvtext_cpia ----
fcvtext_cpia <- function(countries, couName, dataset="cpia"){      
  
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  country <- as.character(countries[countries$iso3==cou,]$Country)
  fcv_class <- as.character(countries[countries$iso3==cou,]$FCVclass)
  
  # Get all possible neighbors
  FCV_classmates <- filter(countries, FCVclass == fcv_class)$iso3
  
  # Get top neighbors based on identified dataset
  dataNeighbor <- filter(Report_data, CountryCode %in% c(FCV_classmates), Subsection2 %in% dataset)
  maxPeriod <- max(filter(Report_data, CountryCode==cou, Subsection2 %in% dataset, !is.na(Observation))$Period)
  dataNeighbor <- filter(dataNeighbor, Period==maxPeriod)
  dataset_ave <- mean(aggregate(dataNeighbor$Observation, list(dataNeighbor$CountryCode), mean)$x, na.rm=TRUE)
  country_ave <- mean(filter(Report_data, CountryCode == cou, Subsection2 %in% dataset, Period == maxPeriod)$Observation, na.rm=TRUE)
  
  # Print text
  par(family = 'serif')
  plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
  graphics::text(1, 1, sprintf("Country Overall CPIA: %0.2f ; Class Average Overall CPIA: %0.2f", country_ave, dataset_ave), col="#818181", adj = c(0,0), cex=2)
}

## ---- text_box ----
text_box <- function(reportConfig,title, body, str_wrap_size=75){      
  
  title <- str_wrap(title, width = str_wrap_size)
  body <- str_wrap(body, width = str_wrap_size)
  # Reads title and text vectors and prints them iteratively
  if (length(title) > 0){
    par(family = 'serif',mfrow=c(length(title),1), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,2), #sets margin size for the figures
        oma=c(0,1,0,1)) #sets outer margin
    
    # print text
    for (i in 1:length(title)){
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1, 1.2, title[i], col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), adj = c(0,0), cex=2)
      graphics::text(1, 0.8, body[i], col="#818181",  adj = c(0,0.3), cex=1.7)
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1," ", col=paste0("#",filter(reportConfig, Section_Level == 10)$Color), cex=2)
  }
  
}
##########################
# Shiny specific functions -----------------------------------------
##########################

# Generate report. Store it in www in order to be rendered in a browser
.reportGenerator <- function(couName, input_reportID){
  
  ### Read data and configurations ---------------
  
  # # Read template report configuration
  # reportConfig <- read.csv(paste0("templates/",input_reportID, "_reportConfiguration.csv"), stringsAsFactors = FALSE)
  # 
  # # Add source links to reportConfig ------------------------
  # reportConfig <- select(dataDesc, Source_Name, Source_Link) %>% 
  #   distinct(Source_Name, Source_Link) %>%
  #   right_join(reportConfig, by = c("Source_Name" = "Section_Description")) %>%
  #   select(everything(), Section_Description = Source_Name) %>%
  #   arrange(Section_Level, Order)
  # Load data. This doesn't work, I'm afraid
  
  ### Run the report ---------------
  
    iso3 <- .getCountryCode(couName)
    knit2pdf('PDF_LaTeX.Rnw', clean = TRUE,
             encoding = "UTF-8",
             output = paste0(input_reportID,"_",iso3,".tex"))
    # copy file to pdf directory
    file.copy(paste0(input_reportID,"_",iso3,".pdf"), paste0("templates/",input_reportID,"_final_pdf/"),overwrite=TRUE)
    #file.copy(paste0(input_reportID,"_",iso3,".pdf"), "www/",overwrite=TRUE)
    file.remove(paste0(input_reportID,"_",iso3,".pdf"))
    file.remove(paste0(input_reportID,"_",iso3,".tex"))
}

