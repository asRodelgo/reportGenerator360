# Helper functions to generalize charts and tables for LaTeX
#
figure_sparkline <- function(couName,table){      
  
  cou <- .getCountryCode(couName)
  ## Examples like Edward Tufte's sparklines:
  #table <- "combo1"
  data <- Entrepr_data %>%
    filter(CountryCode==cou, Subsection2==table, !is.na(Observation)) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear)-1),Period))
  
  if (table == "figureFin2"){
    data <- filter(data,Observation > 0)
    dataLast <- filter(data, Period == max(Period,na.rm=TRUE))
    dataLast$Observation <- ifelse(dataLast$Observation>1000000,dataLast$Observation/1000000,dataLast$Observation)
  } else {
    data <- filter(data,!is.na(Observation))
    dataLast <- filter(data, Period == max(Period,na.rm=TRUE))
  }
  # data
  dataPoint <- format(dataLast$Observation, digits=2, decimal.mark=".",
                      big.mark=",",small.mark=".", small.interval=3)
  # period
  dataPeriod <- dataLast$Period
  
  dataWorld <- filter(Entrepr_data, Subsection2==table)
  dataWorld <- filter(dataWorld,!is.na(Observation))
  dataWorld <- dataWorld %>%
    group_by(iso2) %>%
    mutate(Period = max(Period,na.rm=TRUE)) %>%
    distinct(Period, .keep_all = TRUE) %>%
    as.data.frame()
#     dataWorld <- merge(dataWorld,countries[,c("CountryCodeISO2","CountryAlternat")],by.x="iso2c",by.y="CountryCodeISO2",all.x = TRUE)
#     dataWorld <- filter(dataWorld, !(CountryAlternat==""))
  dataWorld <- arrange(dataWorld, desc(Observation))
  # rank in the world
  rank <- which(dataWorld$CountryCode == cou)
  rankedTotal <- nrow(dataWorld)
  
  # Ad-hoc shorten some indicatores and units names:
  if (table == "figure1"){
    indicator <- "Tech Startups"
    unit <- "number per million pop"
  }
  if (table == "figure2"){
    indicator <- "Doing Business"
    unit <- "1=most business-friendly regulat."
  }
  if (table == "figure3"){
    indicator <- "Broadband Internet"
    unit <- "Subscriptions per 100 pop."
  }
  if (table == "figure4"){
    indicator <- "Scientists, Engineers"
    unit <- "Availability 1-7, 7=best"
  }
  if (table == "figure5"){
    indicator <- "Tertiary Education"
    unit <- "Enrollments in percent of pop."
  }
  if (table == "figure6"){
    indicator <- "Venture Capital"
    unit <- "Availability 1-7, 7=best"
  }
  if (table == "figureFin1"){
    indicator <- "FDI, net inflows"
    unit <- "BoP, current US$, as % GDP"
  }
  if (table == "figureFin2"){
    indicator <- "Investment in Telecoms w/ Private Part."
    unit <- "Millions, $US"
  }
  if (table == "figureFin3"){
    indicator <- "Market Capitaliz. of Listed Companies"
    unit <- "% of GDP"
  }
    
  if (nrow(data)>0){
    
    minPeriod <- min(data$Period, na.rm=TRUE)
    maxPeriod <- max(data$Period, na.rm=TRUE)
    # sparkline
    spark <- data %>%
      arrange(Period) %>%
      select(Observation)
    
    # text
    #indicator <- dataLast$IndicatorShort
    #unit <- dataLast$Unit
    
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
    graphics::text(1.5, 1.1,indicator, col="#22a6f5", cex=10)
    graphics::text(1.5, 0.7,paste0(unit, " (",dataPeriod,")"), col="#818181", cex=5)
    # print data point and rank
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 0.95,dataPoint, col="#22a6f5", cex=18)
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1,paste0("(Rank: ",rank,"/",rankedTotal,")"), col="grey", cex=7)
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
        graphics::text(1.5, 1.2,minPeriod, col="grey", cex=5)
      } else{
        graphics::text(1.05, 1.2,minPeriod, col="grey", cex=5)
        graphics::text(1.95, 1.2,maxPeriod, col="grey", cex=5)
      }
    }
    
  } else {
    
    # Print the combo -----------------------------------------------
    par(family = 'serif',mfrow=c(5,1), #sets number of rows in space to number of cols in data frame x
        mar=c(0,2,0,2), #sets margin size for the figures
        oma=c(0,1,0,1)) #sets outer margin
    
    # print indicator name
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1,indicator, col="#22a6f5", cex=10)
    graphics::text(1.5, 0.7,unit, col="#818181", cex=5)
    # print data point and rank
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 0.95,"No data available", col="grey", cex=10)
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1.1,paste0("(Rank: /",rankedTotal,")"), col="grey", cex=7)
    # plot sparkline  
    par(family = 'serif',#sets number of rows in space to number of cols in data frame x
      mar=c(0,5,0,5))#sets margin size for the figures
    #oma=c(0,4,0,4)) #sets outer margin
    
  } 
  
}

## ---- table_time ----
table_time <- function(couName,section, table){      
  
  cou <- .getCountryCode(couName)
  #table <- "table1"
  tableKeys <- unique(filter(Entrepr_data, Section == section, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(Entrepr_data, CountryCode==cou, Section == section, Subsection==table)
  data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
  # keep the latest period (excluding projections further than 2 years)
  data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period))
  data <- filter(data, Period <= (as.numeric(thisYear) + 1))
  
  #keep only periods of interest in data
  data <- filter(data, Period > (as.numeric(thisYear) - 7))
  # Scale Observations
  data <- mutate(data, ObsScaled = ifelse(grepl("current US",Unit),Observation/1000000000,Observation),
                 Unit = ifelse(grepl("current US",Unit),"USD billions",Unit),
                 IndicatorShort = paste0(IndicatorShort, ", ",Unit))
  
  data <- arrange(data, Key)
  data <- select(data, Key, IndicatorShort, Period, ObsScaled)
  # restrict to 2 decimal places
  data$ObsScaled <- round(data$ObsScaled,2)
  data[is.na(data)] <- "..."
  # format numbers
  data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  
#   for (i in 1:nrow(data)){
#     
#     data$IndicatorShort[i] <- ifelse(!is.na(merge(data,TCMN_indic[TCMN_indic$Subsection==table,], by="Key")$Note[i]),
#                                      paste0(data$IndicatorShort[i]," \\large{[", merge(data,TCMN_indic[TCMN_indic$Subsection==table,], by="Key")$Note[i],"]}"),
#                                      data$IndicatorShort[i])  
#   }
  # escape reserved characters
  data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
  data$IndicatorShort <- gsub("&", "\\&", data$IndicatorShort, fixed=TRUE)
  
  # final table format
  data <- spread(data, Period, ObsScaled)
  data <- data[,-1] #drop the Key column
  
  # remove columns with all NAs
  data <- data[,!(colSums(data == "...   ")==nrow(data))]
  
  # dummy columns in to keep the pdf layout fixed to 6 columns
  if (ncol(data)<=5){
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
  rowsSelect <- seq(1,nrow(data)-1,2)
  col <- rep("\\rowcolor[gray]{0.95}", length(rowsSelect))
  data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','>{\\raggedright}p{6in}','r',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-3),'l')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large",add.to.row = list(pos = as.list(rowsSelect), command = col),
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
        sanitize.text.function = function(x){x}) # include sanitize to control formats
  
}

## ---- line_chart ----
line_chart <- function(couName, section, table){
  
  cou <- .getCountryCode(couName)
  couRegion <- as.character(countries[countries$iso3==cou,]$region)  # obtain the region for the selected country
  
  data <- filter(Entrepr_data, region==couRegion, Section == section, Subsection == table, !(is.na(Observation))) #select country, region and world
  #data <- merge(data, countries[,c("CountryCodeISO3","Country")],by.x = "CountryCode", by.y="CountryCodeISO3")
  #data$Country <- gsub("(ES) ","",data$Country,fixed=TRUE)
  income <- filter(Entrepr_data, region==couRegion & Section=="aux_income")
  income <- income %>%
    group_by(CountryCode) %>%
    filter(!is.na(Observation), Period < thisYear, !(CountryCode==cou)) %>%
    filter(Period == max(Period,na.rm=TRUE))
  
  topNeighbors <- head(arrange(as.data.frame(income), desc(Observation)),15)$CountryCode
  data <- filter(data, CountryCode %in% c(cou,topNeighbors)) %>%
    arrange(CountryCode,Period)
  
  # order lines in chart and hide elements in legend
  if (nrow(filter(data,CountryCode==cou))>0){

    order_legend <- c(couName,as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country)))
    country_order <- factor(order_legend, levels = c(couName,order_legend[2:length(order_legend)]))
    my_order <- data.frame(Country = country_order, order = seq(1,length(order_legend),1))
    data <- merge(data,my_order, by="Country") %>%
      filter(order < 6) %>% # keep 5 countries
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
      scale_color_manual(labels = order_legend, values = c("orange","brown","lightblue","lightgreen","pink")) +
      scale_alpha_manual(labels = order_legend,values = c(1, rep(0.6,4))) + 
      scale_size_manual(labels = order_legend,values = c(2, rep(1,4))) + 
      scale_x_discrete(breaks = unique(arrange(data,Period)$Period)[seq(1,length(unique(data$Period)),4)])
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}

## ---- table_time_avg ----
table_time_avg <- function(couName,section,table){      
  
  cou <- .getCountryCode(couName)
  #table <- "table1"
  tableKeys <- unique(filter(Entrepr_data, Section == section, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(Entrepr_data, CountryCode==cou, Section == section, Subsection==table)
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
    mutate(ObsScaled = Observation) %>%
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
sparklines <- function(couName,section,table){      
  
  cou <- .getCountryCode(couName)
  #table <- "table1"
  tableKeys <- unique(filter(Entrepr_data, Section == section, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(Entrepr_data, CountryCode==cou, Section == section, Subsection==table)
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
  
  if (nrow(data)>0){
    data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
    data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period)) %>%
      filter(Period <= (as.numeric(thisYear))) %>%
      mutate(Period = ifelse(Period < 1900, 1900, Period))
    
    # keep the latest period (excluding projections further than 2 years)
    data <- data %>%
      mutate(Period = ifelse(Period==thisYear & is.na(CountryCode),as.numeric(thisYear)-1,Period)) %>%
      filter(Period > (as.numeric(thisYear) - 7) & Period < (as.numeric(thisYear))) %>%
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

## ---- bar_chart ----
bar_chart <- function(couName,section,table,paste_unit){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Entrepr_data, CountryCode==cou, Section==section, Subsection %in% table)
  data <- data %>%
    filter(!(is.na(Observation))) %>%
    distinct(Key,Period,.keep_all=TRUE)
  maxPeriod <- filter(data, Period == max(Period,na.rm=TRUE))$Period[1]
  #couRegion <- as.character(countries[countries$CountryCodeISO3==cou,]$RegionCodeES)  # obtain the region for the selected country
  #data <- filter(Entrepr_data, CountryCode %in% c(cou,couRegion, "RWe"), Category=="Policy", Subsection=="bar1") #select country, region and world
  
  # country, Region, World descriptors
  #country <- as.character(countries[countries$CountryCodeISO3==cou,]$Country)
  #region <- as.character(countries[countries$CountryCodeISO3==cou,]$Region) 
  #world <- "All Countries"
  
  if (nrow(data)>0){
    # order the factors
    #data$IndicatorShort = factor(as.character(data$IndicatorShort), 
    #                             levels = data$IndicatorShort[order(data$Observation)])
    if (!any(is.na(data$Period))){
      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period))
    }
    
    require(stringr) # to wrap label text
    
    if (!paste_unit){ # should unit be included in indicator name
      data <- mutate(data, Unit = ifelse(grepl("0-100",Unit),"100=full ownership allowed",Unit))
      data <- mutate(data, IndicatorShort = str_wrap(paste0(IndicatorShort,", ",Unit," (",Period,")"), width = 30))
    } else {
      data <- mutate(data, IndicatorShort = str_wrap(paste0(IndicatorShort," (",Period,")"), width = 30))
    }
    
    if (section == "Markets"){
      
      data_grey <- data.frame(IndicatorShort=data$IndicatorShort,Observation=rep(100,length(table)))
      #data <- mutate(data, id = seq(1,nrow(data),1))
      ggplot(NULL,aes(x=IndicatorShort,y=Observation)) +
        geom_bar(data=data_grey,color="#f1f3f3",fill = "#f1f3f3",stat="identity") +
        geom_bar(data=data,color="#22A6F5",fill="#22A6F5",stat="identity") +
        geom_text(data=data, aes(label=round(Observation,1),y=ifelse(Observation<21,Observation + max(Observation)*.1,Observation - max(Observation)*.1)),
                  size=12,color=ifelse(data$Observation<21,"#22a6f5","white")) + 
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
    } else {
      ggplot(NULL,aes(x=IndicatorShort,y=Observation)) +
        geom_bar(data=data,color="#22A6F5",fill="#22A6F5",stat="identity") +
        geom_text(data=data, aes(label=round(Observation,1),y=ifelse(Observation<14,Observation + max(Observation)*.1,Observation - max(Observation)*.1)),
                  size=6,color=ifelse(data$Observation<14,"#22a6f5","white")) + 
        coord_flip()+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position='none',
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(family="Times", lineheight=.5),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(family="Times", size = 15)) + 
        labs(x="",y=""#,title="Top 5 constraints according to 2013 Enterprise Survey (in percent)"
        )
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}

## ---- number_chart ----
number_chart <- function(couName,section,table,str_wrap_size){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Entrepr_data, CountryCode==cou, Section==section, Subsection %in% table)
  data <- data %>%
    filter(!(is.na(Observation))) %>%
    distinct(Key,Period,.keep_all=TRUE)
  
  # data
  #dataPoints <- format(data$Observation, digits=2, decimal.mark=".",
  #                    big.mark=",",small.mark=".", small.interval=3)
  # period
  #dataPeriods <- data$Period
  
  dataWorld <- filter(Entrepr_data, Section==section, Subsection %in% table)
  dataWorld <- filter(dataWorld,!is.na(Observation))
  dataWorld <- dataWorld %>%
    group_by(Country,Key) %>%
    mutate(Period = max(Period,na.rm=TRUE)) %>%
    distinct(Key, Period, .keep_all = TRUE) %>%
    as.data.frame()
  #dataWorld <- merge(dataWorld,countries[,c("CountryCodeISO2","CountryAlternat")],by.x="iso2c",by.y="CountryCodeISO2",all.x = TRUE)
  #dataWorld <- filter(dataWorld, !(CountryAlternat==""))
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
      
      thisKey <- mutate(thisKey, Unit = ifelse(grepl("0-100",Unit),"100=full ownership allowed",Unit))
      thisKey <- mutate(thisKey, IndicatorShort = str_wrap(paste0(IndicatorShort), width = str_wrap_size))
      rankedTotal[i] <- nrow(thisKey)
      
      if (nrow(filter(thisKey, CountryCode == cou))>0){# country has data for this indicator
        
        rank[i] <- which(thisKey$CountryCode == cou)
        # print indicator name
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1, 1.1,thisKey$IndicatorShort[1], col="#22a6f5", cex=3, adj=0)
        graphics::text(1, 0.75,paste0(thisKey$Unit[1], " (",thisKey$Period[1],")"), col="#818181", cex=2, adj = 0)
        # print data point and rank
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1.17, 1,filter(thisKey,CountryCode==cou)$Observation , col="#22a6f5", cex=8)
        graphics::text(1.42, 0.95,paste0("(Rank: ",rank[i],"/",rankedTotal[i],")"), col="grey", cex=3, adj=0)
      
      } else { # no data for this indicator
        
        # print indicator name
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1, 1.1,thisKey$IndicatorShort[1], col="#22a6f5", cex=3, adj=0)
        graphics::text(1, 0.75,paste0(thisKey$Unit[1], " (",thisKey$Period[1],")"), col="#818181", cex=2, adj = 0)
        # print data point and rank
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1.17, 1," " , col="#22a6f5", cex=8)
        graphics::text(1.42, 0.95,paste0("(Rank: /",rankedTotal[i],")"), col="grey", cex=3, adj=0)
      }
      i <- i + 1
    }

  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}


## ---- bar_facewrap_chart ----
bar_facewrap_chart <- function(couName, section, table){      
  
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  
  couRegion <- countries[countries$iso3==cou,]$region  # obtain the region for the selected country
  neighbors <- countries[countries$region==couRegion,]$iso3 # retrieve all countries in that region
  neighbors <- as.character(neighbors[!(neighbors==cou)]) # exclude the selected country
  
  # hardcode for now
  #neighbors <- c("DZA","JOR","MAR","EGY","TUN")
  
  data <- Entrepr_data %>%
    filter(CountryCode %in% c(cou,neighbors), Section==section, Subsection==table) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear)-1),Period))
  
  if (nrow(filter(data, CountryCode==cou))>0){
    
    #data <- merge(data, countries[,c("Country","CountryCodeISO3")], by.x="CountryCode", by.y="CountryCodeISO3") # add country name
    data <- data %>%
      filter(!is.na(Observation)) %>%
      group_by(Key,Country) %>%
      filter(Period == max(Period,na.rm=TRUE)) %>%
      distinct(Key,CountryCode, .keep_all = TRUE)
    # select top 4 countries from the neighborhood based on their income level
    income <- filter(Entrepr_data, CountryCode %in% neighbors & Section=="aux_income")
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
      #filter(order < 10) #keep 5 countries to compare
    # order the factors
#     data$Country = factor(as.character(data$Country), 
#                           levels = c(as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country)),
#                                      unique(as.character(data[data$CountryCode==cou,]$Country))))
#     order_legend <- c(couName,as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country)))
#     
    require(stringr) # to wrap label text
    #data <- mutate(data, Country = str_wrap(Country, width = 15))
    if (section == "Policy"){

      #maxPeriod_thisCou <- filter(data, CountryCode==cou)$Period[1]
      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period,na.rm=TRUE)) %>%
        mutate(IndicatorShort = str_wrap(paste0(IndicatorShort," (",Period,")"), width = 20)) %>%
        filter(order < 6) %>%
        as.data.frame()
      
      ggplot(data, aes(x=reorder(Country,order),y=Observation,fill=reorder(Country,order),alpha=reorder(Country,order))) +
        geom_bar(position="dodge",stat="identity") +
        #coord_flip()+
        facet_wrap(~IndicatorShort,scales="free_y") +
        theme(strip.text.x = element_text(family="Times", size = 12, colour = "white"),
              strip.background = element_rect(colour = "#22A6F5", fill = "#22A6F5"),
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
        scale_alpha_manual(labels = order_legend,values = c(1, rep(0.4,4)),guide=FALSE)
      
    } else{
      
      maxPeriod_thisCou <- filter(data, CountryCode==cou)$Period[1]
      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period,na.rm=TRUE)) %>%
        mutate(IndicatorShort = str_wrap(paste0(IndicatorShort," (",Period,")"), width = 20)) %>%
        filter(order < 6) %>%
        as.data.frame()
      
      ggplot(data, aes(x=reorder(Country,order),y=Observation,fill=reorder(Country,order),alpha=reorder(Country,order))) +
        geom_bar(position="dodge",stat="identity") +
        coord_flip() +
        facet_wrap(~IndicatorShort) +
        theme(strip.text.x = element_text(family="Times", size = 12, colour = "white"),
              strip.background = element_rect(colour = "#22A6F5", fill = "#22A6F5"),
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
        scale_alpha_manual(labels = order_legend,values = c(1, rep(0.4,4)),guide=FALSE)
    }
    
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }
  
}


## ---- radar_chart ----
radar_chart <- function(couName,section,table){      
  
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  
  couRegion <- countries[countries$iso3==cou,]$region  # obtain the region for the selected country
  neighbors <- countries[countries$region==couRegion,]$iso3 # retrieve all countries in that region
  neighbors <- as.character(neighbors[!(neighbors==cou)]) # exclude the selected country
  
  # country and Region descriptors
  country <- as.character(countries[countries$iso3==cou,]$Country)
  region <- as.character(countries[countries$iso3==cou,]$region) 
  #country <- paste0(country," (Rank: ",round(filter(TCMN_data, CountryCode==cou, Key=="P00b")$Observation,1),")")
  #region <- paste0(region," (Avg Rank: ",round(filter(TCMN_data, CountryCode==couRegion, Key=="P00b")$Observation,1),")")
  # filter the data
  data <- filter(Entrepr_data, CountryCode %in% c(cou,neighbors), Section == section, Subsection==table)
  
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
    max<-100
    min <-1
    data <- as.data.frame(data)
    data <- cbind(data,max,min)
    
    # order labels ad-hoc:
    order <- c(1,3,4,6,2,5)
    data <- cbind(data,order)
    
    data <- arrange(data,order) %>%
    select(IndicatorShort, max, min, Observation, regionAvg)
    # transpose the data for radarchart to read
    dataTrans <- as.data.frame(t(data[,2:ncol(data)]))
    layout(matrix(c(1,2),ncol=1), heights =c(4,1))
    #       col.axis="red",col.lab=c("red","red"),col.main="red",col.sub="red",family="serif")
    par(mar=c(0,1,3,1),family="serif")
    
    radarchart(dataTrans, axistype=1, centerzero = FALSE,seg=4, caxislabels=c(" ","","50%","","100%"),
                     plty=c(1,1),plwd=c(6,3),pcol=c("orange","lightblue"),pdensity=c(0, 0),
                     cglwd=2,axislabcol="lightgrey", vlabels=data$IndicatorShort, cex.main=1,cex=2.5)
          
    #title="WEF Competitiveness Indicators, stage of development (1-7)",
    par(family = 'serif',mar=c(0,1,1,1))
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    legend(1,1.5, legend=c(couName,region), seg.len=0.5, pch=19, inset=50, 
           bty="n" ,lwd=3, x.intersp=0.5, horiz=TRUE, col=c("orange","lightblue"))
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }  
  
}


## ---- combo_percent ----
combo_percent <- function(couName,section,table){      
  
  cou <- .getCountryCode(couName)
  
  data <- filter(Entrepr_data, CountryCode==cou, Section == section, Subsection==table)
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
combo_rate <- function(couName){      
  
  cou <- .getCountryCode(couName)
  
  data <- filter(Entrepr_data, CountryCode==cou, Section == "Human capital", Subsection=="combo1")
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
table_region <- function(couName,section,table){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Entrepr_data, CountryCode==cou, Section == section, Subsection==table) #select country, region and world
  if (nrow(data[data$CountryCode==cou,])>0){
    
    couRegion <- as.character(countries[countries$CountryCodeISO3==cou,]$RegionCodeES)  # obtain the region for the selected country
    data <- filter(Entrepr_data, CountryCode %in% c(cou,couRegion, "RWe"), Section == section, Subsection==table) #select country, region and world
    
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

## ---- doing_business_table ----
doing_business_table <- function(couName){      
  
  cou <- .getCountryCode(couName)
  data <- filter(Entrepr_data, CountryCode == cou, grepl("dbtable",Subsection), !is.na(Observation)) #select country, region and world
  
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
    
    dataDTF <- data %>%
      filter(grepl("DTF",Subsection), Period >= as.numeric(thisYear)-1) %>%
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
macroInd <- function(couName){      
  
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
pie_chart_double <- function(couName,section,table){      
  
  cou <- .getCountryCode(couName)
  
  data <- Entrepr_data %>%
    filter(CountryCode==cou & Section == section & Subsection==table) %>%
    filter(!is.na(Observation)) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear) - 1),Period))

  couRegion <- countries[countries$iso3==cou,]$region  # obtain the region for the selected country
  # country and Region descriptors
  country <- as.character(countries[countries$iso3==cou,]$Country)
  region <- as.character(countries[countries$iso3==cou,]$region) 
  # filter the data
  dataRegion <- Entrepr_data %>%
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
pie_chart_region <- function(couName,section,table){      
  
  cou <- .getCountryCode(couName)
  
  data <- Entrepr_data %>%
    filter(CountryCode==cou & Section == section & Subsection==table) %>%
    filter(!is.na(Observation)) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear) - 1),Period))
  
  couRegion <- countries[countries$iso3==cou,]$region  # obtain the region for the selected country
  # country and Region descriptors
  country <- as.character(countries[countries$iso3==cou,]$Country)
  region <- as.character(countries[countries$iso3==cou,]$region) 
  # filter the data
  dataRegion <- Entrepr_data %>%
    filter(region==couRegion & Section == section & Subsection==table) %>%
    filter(!is.na(Observation)) %>%
    mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear) - 1),Period))
  
  if (nrow(filter(data, CountryCode==cou))>0){
    data <- filter(data, Period==max(Period))
    maxPeriod <- data$Period
    data <- select(data, IndicatorShort, Observation)
    pickColor <- ifelse(data$Observation > 50,"green","red")
    data <- rbind(data, c(" ",0)) # add "Other" category
    data$Observation <- round(as.numeric(data$Observation),2)
    data$color <- c(pickColor,"#f1f3f3") # add the color
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
      
      dataRegion <- filter(dataRegion, Period==maxPeriod) %>% #max period for the selected country
        mutate(Observation = mean(Observation, is.na=TRUE)) %>%
        select(IndicatorShort, Observation) %>%
        distinct(IndicatorShort, Observation)
      pickColor <- ifelse(dataRegion$Observation > 50,"green","red")
      dataRegion <- rbind(dataRegion, c(" ",0)) # add "Other" category
      dataRegion$Observation <- round(as.numeric(dataRegion$Observation),2)
      dataRegion$color <- c(pickColor,"#f1f3f3") # add the color
      dataRegion[dataRegion$IndicatorShort==" ",]$Observation <- 100 - sum(dataRegion$Observation)
      
      # format numbers
      dataRegion$Observation <- format(dataRegion$Observation, digits=0, decimal.mark=".",
                                       big.mark=",",small.mark=".", small.interval=3)
      dataRegion$Observation <- as.numeric(dataRegion$Observation)
      dataRegion <- dataRegion %>%
        mutate(ObsLabel = paste0(Observation,"%")) %>%
        arrange(desc(IndicatorShort))
      
      dataRegion$ObsLabel[2] <- ""
        
    #  if (section=="Markets")  thisColor = "blue"
    #  else thisColor = "darkgreen"
      
      p1 <- ggplot(data, aes("",Observation,fill=IndicatorShort)) +
        geom_bar(width=1,stat="identity") +
        scale_fill_manual(values = c("#f1f3f3","orange"),guide=FALSE) +
        coord_polar("y",start = 0) +
        geom_text(aes(label=ObsLabel,y=15),
                  size=12,color="white") + 
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
        scale_fill_manual(values = c("#f1f3f3","#22a6f5"),guide=FALSE) +
        coord_polar("y",start = 0) +
        geom_text(aes(label=ObsLabel,y=15),
                  size=12,color="white") + 
        ggtitle(paste0(region," (simple average, ",maxPeriod,")")) + 
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
      
      grid.arrange(p1,p2,ncol=2)
    
    } else {
      
      ggplot(data, aes("",Observation,fill=IndicatorShort)) +
        geom_bar(width=1,stat="identity") +
        scale_fill_manual(values = c("#f1f3f3","#22a6f5"),guide=FALSE) +
        coord_polar("y",start = 0) +
        geom_text(aes(label=ObsLabel,y=15),
                  size=12,color="white") + 
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
              panel.margin=unit(c(0,0,0,0), "cm")) + 
        labs(x="",y="")
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="lightgrey", cex=1.5)
  }  
  
}

## ---- pie_chart_regular ----
pie_chart_regular <- function(couName,section,table){      
  
  cou <- .getCountryCode(couName)
  
  data <- Entrepr_data %>%
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
      scale_fill_manual(values = c("#f1f3f3","#22a6f5"),guide=FALSE) +
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
      scale_fill_manual(values = c("#f1f3f3","#22a6f5"),guide=FALSE) +
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
