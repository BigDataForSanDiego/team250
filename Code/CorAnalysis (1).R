library(readxl)
fulldata<- read_excel("dataCrashes.xlsx", sheet="Crashes",col_names = TRUE) #3121 obs, 80 variables
fulldata<- fulldata[, c(1, 2, 6, 9, 11, 15, 23, 24, 37:41, 43, 46:48, 50, 51, 53, 61, 75, 76, 78:80)]

library(tidyverse)
library(reshape2)
library(ggplot2)
library(psych)
library(ggcorrplot)

attach(fulldata)

#Correlation Analysis
#Remove more columns that will not be used 
fulldata[, c("CASE_ID","POPULATION", "ACCIDENT_YEAR","LATITUDE", "LONGITUDE", "POINT_X", "POINT_Y")]<- NULL
fulldata$ALCOHOL_INVOLVED<- as.numeric(as.factor(ALCOHOL_INVOLVED))
fulldata[is.na(fulldata$ALCOHOL_INVOLVED), ]$ALCOHOL_INVOLVED<- 0
fulldata <- fulldata[complete.cases(fulldata),]
#Change all char to factor to numeric
char_col<- c("CHP_BEAT_TYPE", "INTERSECTION", "WEATHER_1", "PRIMARY_COLL_FACTOR",
             "PCF_VIOL_CATEGORY", "HIT_AND_RUN", "TYPE_OF_COLLISION", "MVIW", "ROAD_SURFACE",
             "ROAD_COND_1", "LIGHTING", "CITY")

fulldata[, char_col] <- fulldata %>% 
  select("CHP_BEAT_TYPE", "INTERSECTION", "WEATHER_1", "PRIMARY_COLL_FACTOR",
         "PCF_VIOL_CATEGORY", "HIT_AND_RUN", "TYPE_OF_COLLISION", "MVIW", "ROAD_SURFACE",
         "ROAD_COND_1", "LIGHTING", "CITY") %>% 
  lapply(as.factor)

fulldata[, char_col] <- fulldata %>% 
  select("CHP_BEAT_TYPE", "INTERSECTION", "WEATHER_1", "PRIMARY_COLL_FACTOR",
         "PCF_VIOL_CATEGORY", "HIT_AND_RUN", "TYPE_OF_COLLISION", "MVIW", "ROAD_SURFACE",
         "ROAD_COND_1", "LIGHTING", "CITY") %>% 
  lapply(as.numeric)


str(fulldata)

fulldata<- data.frame(fulldata)
rmindex1<- as.numeric(rownames(fulldata[fulldata[, 5]==1, ]))
fulldata<- fulldata[-rmindex1, ]


#compute correlation

cor <- cor(fulldata, use = "complete.obs")
# sdfck the correlations
cor <- melt(cor)
# rename the columns appropriately
cor <- rename(cor, c("Target" = "Var1",
                     "Variable" = "Var2",
                     "Correlation" = "value"))

#obtain the set of highly correlated variables to severity.
C <- cor[which(cor$Target == "COLLISION_SEVERITY"),]
# sort by descending absolute correlation
C <- C[order(- abs(C$Correlation)), ]
# select variable with greater than 10% correlation to Target
C <- subset(C, abs(C$Correlation) > 0.05)
# now we have our highly correlated variables

df <- fulldata %>% select("COLLISION_SEVERITY","NUMBER_KILLED","TYPE_OF_COLLISION",
                          "LIGHTING","ALCOHOL_INVOLVED","CITY","PARTY_COUNT","DAY_OF_WEEK", "PCF_VIOL_CATEGORY",
                          "INTERSECTION","CHP_BEAT_TYPE")

#Correlation Plot
options(repr.plot.width=12, repr.plot.height=12)
pairs.panels(df, hist.col = 'yellow', sdfrs = T, stars = T)

options(repr.plot.width=12, repr.plot.height=12)
corr <- round(cor(df, use="complete.obs"), 2)
ggcorrplot(corr, lab = TRUE,colors = c("aquamarine", "white", "dodgerblue"), 
           show.legend = F, outline.color = "gray", type = "upper",  
           tl.cex = 10, lab_size = 5, sig.level = .2) +
  labs(fill = "Correlation")


