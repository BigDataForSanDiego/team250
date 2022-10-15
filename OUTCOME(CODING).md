library(readxl)
fulldata<- read_excel("dataCrashes.xlsx", sheet="Crashes",col_names = TRUE) #3121 obs, 80 variables
fulldata<- fulldata[, c(1, 2, 6, 9, 11, 15, 23, 24, 37:41, 43, 46:48, 50, 51, 53, 61, 75, 76, 78:80)]


library(tidyverse)
library(ggplot2)
attach(fulldata)
uni<-unique(CITY)
data_by_city<-NULL

for (i in 1:length(uni)){
  data_by_city[i] <- length(filter(fulldata, CITY==uni[i])$CASE_ID) 
}

uni
data_by_city



barplot_data<-cbind(uni,data_by_city)
barplot_data_frame<-data.frame(barplot_data)
barplot_data_frame

set_plot_dimensions <- function(width_choice, height_choice) {
  options(repr.plot.width=width_choice, repr.plot.height=height_choice)
}

set_plot_dimensions(50,25)
par(bg = "white")
City<-reorder(uni,-data_by_city)
Accidents<-data_by_city

ggplot(barplot_data_frame,aes(x=City, y=Accidents,fill=Accidents))+
  geom_bar(stat = 'identity')+
  scale_fill_gradient(low="blue", high="red")+
  theme(axis.text.x = element_text(colour = "red", size = 15, angle=90),
        axis.text.y = element_text(colour = "blue", size = 20),
        axis.title = element_text(size = 17),
        legend.title = element_text( size = 17),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
  )
  
 


#another barplot
#tab<-table(CITY)
#ordered_data<-sort(tab, decreasing = T)
#barplot(ordered_data,col=rainbow(7),border = "black",font.axis=15,col.axis="red",cex.axis=1,cex.names=0.5,
#        las=2)

#Severity Anlysis
tab_sev<-table(CITY,as.factor(COLLISION_SEVERITY))
tab_sev
tab_sev_df<-data.frame(tab_sev)
colnames(tab_sev_df)<-c("CITY","Severity","Accidents")
tab_sev_df


set_plot_dimensions(50,25)

tab_sev_df_ordered <- transform(tab_sev_df, CITY = reorder(CITY, -Accidents))
p<-ggplot(data=tab_sev_df_ordered,aes(x=CITY,y=Accidents,fill=Severity))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(colour = "red", size = 15, angle=90),
        axis.text.y = element_text(colour = "blue", size = 15))+
  scale_fill_manual(values=c("#bbbcfc", "#7779fc","#3336ff", "#080187"), 
                    labels=c("1" = "Fatal", "2" = "Severe", "3" = "Other Visible",
                             "4" = "Complaint of Pain"))

p+theme(
  
  axis.title = element_text(size = 17),
  legend.title = element_text( size = 17),
  legend.text = element_text(color = "red", size = 15),
  legend.key.size = unit(2, "cm"),
  legend.key.width = unit(1,"cm"),
  #   legend.background = element_rect(fill = "darkgray"),
  #   legend.key = element_rect(fill = "lightblue", color = NA),
)

#Weather condition
tab_wea<-table(WEATHER_1,COLLISION_SEVERITY)
tab_wea
tab_wea_df<-data.frame(tab_wea)
colnames(tab_wea_df)<-c("Weather_Condition","Severity","Accidents")
tab_wea_df
set_plot_dimensions(50,25)

tab_wea_df_ordered <- transform(tab_wea_df, Weather_Condition = reorder(Weather_Condition, -Accidents))
p<-ggplot(data=tab_wea_df_ordered,aes(x=Weather_Condition,y=Accidents,fill=Severity))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(colour = "red", size = 15, angle = 90))+
  scale_fill_manual(values=c("#bbbcfc", "#7779fc","#3336ff", "#080187"), 
                    labels=c("1" = "Fatal", "2" = "Severe", "3" = "Other Visible",
                             "4" = "Complaint of Pain"))+
  scale_x_discrete(labels=c("A" = "Clear", "B" = "Cloudy", "C" = "Raining",
                            "G" = "Windy", "E" = "Fog",
                            "F" = "Other", "-" = "Not Stated"))

p+theme(
  axis.title = element_text(size = 17),
  legend.title = element_text( size = 17),
  legend.text = element_text(color = "red", size = 15),
  legend.key.size = unit(2, "cm"),
  legend.key.width = unit(1,"cm"),
)


# Accidents by weekday & Severity.
tab_dow<-table(DAY_OF_WEEK,as.factor(COLLISION_SEVERITY))
tab_dow
tab_dow_df<-data.frame(tab_dow)
colnames(tab_dow_df)<-c("DAY_OF_WEEK","Severity","Accidents")
tab_dow_df

set_plot_dimensions(50,25)

tab_dow_df_ordered <- transform(tab_dow_df)
p<-ggplot(data=tab_dow_df_ordered,aes(x=DAY_OF_WEEK,y=Accidents,fill=Severity))+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values=c("#bbbcfc", "#7779fc","#3336ff", "#080187"),
                    labels=c("1" = "Fatal", "2" = "Severe", "3" = "Other Visible",
                             "4" = "Complaint of Pain"))+
  scale_x_discrete(labels=c("1" = "Monday", "2" = "Tuesday",
                            "3" = "Wednesday", "4" = "Thursday", "5" = "Friday", "6" = "Saturday", "7" = "Sunday"))+
  theme(axis.text.x = element_text(colour = "red", size = 15, angle=0),
        axis.text.y = element_text(colour = "blue", size = 15))
p+theme(
  axis.title = element_text(size = 17),
  legend.title = element_text( size = 17),
  legend.text = element_text(color = "red", size = 15),
  legend.key.size = unit(2, "cm"),
  legend.key.width = unit(1,"cm"),
  
) 


#Accidents by Hour of the Day & Severity

COLLISION_SETTIME<- c()
for (i in 1:3121){
  if (COLLISION_TIME[i] <= 600){
    COLLISION_SETTIME[i] = 1
  }
  if (COLLISION_TIME[i] > 600 && COLLISION_TIME[i] <= 1200){
    COLLISION_SETTIME[i] = 2
  }
  if (COLLISION_TIME[i] > 1200 && COLLISION_TIME[i] <= 1800){
    COLLISION_SETTIME[i] = 3
  }
  if (COLLISION_TIME[i] > 1800){
    COLLISION_SETTIME[i] = 4
  }
}
COLLISION_TIME
COLLISION_SETTIME

tab_hod<- table(COLLISION_SETTIME,as.factor(COLLISION_SEVERITY))
tab_hod
tab_hod_df<-data.frame(tab_hod)
colnames(tab_hod_df)<-c("COLLISION_SETTIME","Severity","Accidents")
tab_hod_df

set_plot_dimensions(50,25)

tab_hod_df_ordered <- transform(tab_hod_df)
tab_hod_df_ordered
p<-ggplot(data=tab_hod_df_ordered,aes(x=COLLISION_SETTIME,y=Accidents,fill=Severity))+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values=c("#bbbcfc", "#7779fc","#3336ff", "#080187"),
                    labels=c("1" = "Fatal", "2" = "Severe", "3" = "Other Visible",
                             "4" = "Complaint of Pain"))+
  scale_x_discrete(labels=c("1" = "Dawn", "2" = "Morning",
                            "3" = "Evening", "4" = "Night"))+
  theme(axis.text.x = element_text(colour = "red", size = 15),
        axis.text.y = element_text(colour = "blue", size = 15))
p+theme(
  axis.title = element_text(size = 17),
  legend.title = element_text( size = 17),
  legend.text = element_text(color = "red", size = 15),
  legend.key.size = unit(2, "cm"),
  legend.key.width = unit(1,"cm"),
)


#Correlation Anlysis

#Machine Learning models
    
