library(tidyverse)
library(ggplot2)

####### 3-bar plot
mydata <- mutate(mtcars, car = rownames(mtcars)) %>%
  filter(grepl("Merc 2",car)) %>%
  gather(variable, value, -car) %>%
  filter(variable %in% c("wt","gear","drat")) %>%
  mutate(value = ifelse(variable == "gear", -value, value))

ggplot(data = mydata, aes(car,value,fill=variable)) +
  geom_bar(data = filter(mydata, variable == "drat"),stat = 'identity', position = 'dodge') +
  geom_bar(data = filter(mydata, variable == "gear"), stat = 'identity', alpha = .5) +
  geom_bin2d(data = filter(mydata, variable == "wt"), position = 'dodge')

####### faceted bar-line chart
mydata2 <- mutate(mydata, chart_type = ifelse(variable == "wt","line","bar"))

secondFacet <- FALSE # see below
ggplot(data = mydata2, aes(car,value,fill=variable)) +
  facet_grid(chart_type~., scale = "free") +
  #geom_bar(data = filter(mydata, variable == "drat"),stat = 'identity', position = 'dodge') +
  #geom_bar(data = filter(mydata, variable == "gear"), stat = 'identity', alpha = .5) +
  geom_bar(data = filter(mydata, variable %in% c("drat","gear")), aes(fill = variable),stat = 'identity') +
  geom_bin2d(data = filter(mydata, variable == "wt")) +
  scale_y_continuous(name = NULL, labels = function(b) {
    if(!secondFacet) {
      secondFacet <<- TRUE # this is a little cray (and relies on dtF seq = facet seq; works though)
      return(paste0(round(b * 100, 0), "%"))
    }else{
      return(b)
    }
  })

##### faceted double axis
# define dummy dataset
mydata3 <- data.frame(Period=c(2014,2015, 2016, 2017), 
                 Country = c("Ethiopia","Ethiopia","Ethiopia","Ethiopia","Ethiopia","Ethiopia","Ethiopia","Ethiopia"),
                 Indicator=c("Current Account","Current Account","Current Account","Current Account",
                             "Current Account","Current Account","Current Account","Current Account",
                             "Trade Balance","Trade Balance","Trade Balance","Trade Balance",
                             "Trade Balance","Trade Balance","Trade Balance","Trade Balance"), 
                 Unit = c("USD","USD","USD","USD","%GDP","%GDP","%GDP","%GDP",
                          "USD","USD","USD","USD","%GDP","%GDP","%GDP","%GDP"),
                 Observation=c(-100, -110, -125, -96, -2.45, -3.6, -5.1, -4.3, 
                               -120, -137, -145, -161, -8.45, -6.6, -7.1, -10.3))

secondFacet <- FALSE # see below
ggplot(data = mydata3, mapping = aes(x = Period, y = Observation)) +
  facet_grid(Unit~., scale = "free") +
  geom_bar(data = filter(mydata3, Unit == "USD"), aes(fill = Indicator),stat = 'identity') +
  geom_line(data = filter(mydata3, Unit == "%GDP"), aes(colour = Indicator)) +
  scale_y_continuous(name = NULL, labels = function(b) {
    if(!secondFacet) {
      secondFacet <<- TRUE # this is a little cray (and relies on dtF seq = facet seq; works though)
      return(paste0(round(b * 100, 0), "%"))
    }else{
      return(b)
    }
  })

####### double y-axis
# scale the data
mydata4 <- group_by(mydata3, Unit) %>%
  mutate(maxObs = max(Observation), minObs = min(Observation)) %>%
  #mutate(Scaled_Observation = sign(Observation)*(Observation-min(Observation))/(max(Observation)-min(Observation))) %>%
  #ungroup() %>%
  mutate(Scaled_Observation = ifelse(maxObs < 0,-.1-(Observation-maxObs)/(minObs-maxObs),
                                     .1+(Observation-minObs)/(maxObs-minObs)))

maxObs <- max(filter(mydata4, Unit == "USD")$Observation)
minObs <- min(filter(mydata4, Unit == "USD")$Observation)
maxObs2 <- max(filter(mydata4, !(Unit == "USD"))$Observation)
minObs2 <- min(filter(mydata4, !(Unit == "USD"))$Observation)

ggplot(data = mydata4, mapping = aes(x = Period, y = Scaled_Observation)) +
  geom_bar(data = filter(mydata4, Unit == "USD"), aes(fill = Indicator), alpha = .5,stat = 'identity') +
  geom_line(data = filter(mydata4, Unit == "%GDP"), aes(colour = Indicator), size = 1) +
  geom_point(data = filter(mydata4, Unit == "%GDP"), aes(colour = Indicator), size = 4) +
  scale_y_continuous(name = "USD", labels = function(a) { paste0(round((a + .1)* (maxObs-minObs) + maxObs, 0), "$")},
                     sec.axis = sec_axis(~., name = "%GDP", 
                                         labels = function(b) { paste0(round((b+.1) * (maxObs2-minObs2) + maxObs2, 1), "%")}))



