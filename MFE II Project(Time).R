#import the librarires
library(magrittr)
library(dplyr)

#import the data
path1 <-'D:/Downloads/archive/only_road_accidents_data3.csv'
  
#creating a dataframe for the data
road_accidents_time<- read.csv(path1)
    
#creating a dataframe for grouping the states 
Total_acidents_for_states_14Y_time<-road_accidents_time %>% group_by(STATE.UT)%>%summarise(across(c(X0.3.hrs...Night.,X3.6.hrs...Night.,X6.9.hrs..Day.,X9.12.hrs..Day.,X12.15.hrs..Day.,X15.18.hrs..Day.,X18.21.hrs..Night.,X21.24.hrs..Night.), sum))
#sorting the data into Morning, Afternoon,Evening and Night time
Data_time_wise<-Total_acidents_for_states_14Y_time |> group_by(X0.3.hrs...Night.,X3.6.hrs...Night.,X21.24.hrs..Night.)|>mutate(Night=sum(X21.24.hrs..Night.,X0.3.hrs...Night.,X3.6.hrs...Night.))
Data_time_wise<-Data_time_wise|> group_by(X18.21.hrs..Night.)|>mutate(Evening=sum(X18.21.hrs..Night.))
Data_time_wise<-Data_time_wise|> group_by(X6.9.hrs..Day.,X9.12.hrs..Day.)|>mutate(Morning=sum(X6.9.hrs..Day.,X9.12.hrs..Day.))
Data_time_wise<-Data_time_wise|> group_by(X12.15.hrs..Day.,X15.18.hrs..Day.)|>mutate(Afternoon=sum(X12.15.hrs..Day.,X15.18.hrs..Day.))
    
#creating a vector for total accidents
Data_time_wise<-Data_time_wise |> group_by(Night,Evening,Morning)|>mutate(Total=sum(Morning,Night,Evening,Afternoon))
    
#creating vectors for Morning, Afternoon, Evening and Night as Percentage
Data_time_wise$`Morning_P` <- Data_time_wise$Morning / Data_time_wise$Total *100
Data_time_wise$`Afternoon_P` <- Data_time_wise$Afternoon / Data_time_wise$Total *100
Data_time_wise$`Evening_P` <- Data_time_wise$Evening / Data_time_wise$Total *100
Data_time_wise$`Night_P` <- Data_time_wise$Night / Data_time_wise$Total *100
  
#plotting a boxplot to check for median and compare all the various times of day
boxplot(Data_time_wise$Morning_P, Data_time_wise$Night_P,Data_time_wise$Afternoon_P,Data_time_wise$Evening_P)
  
#creating a pie graph for composition according to time of day
pie(colSums(Data_time_wise[, c("Evening_P","Afternoon_P","Morning_P","Night_P")]), labels=c("Evening", "Afternoon", "Morning", "Night"), main="Distribution of all accidents in India according to time(2001-14)")
    
#sorting the data according to number of accidents
morning_sorted <- Data_time_wise[order(Data_time_wise$Morning, decreasing = TRUE),]
afternoon_sorted <- Data_time_wise[order(Data_time_wise$Afternoon, decreasing = TRUE),]
evening_sorted <- Data_time_wise[order(Data_time_wise$Evening, decreasing = TRUE),]
night_sorted <- Data_time_wise[order(Data_time_wise$Night, decreasing = TRUE),]
  
#plotting the sorted data as bar plot to see the top states 
barplot(sort(Data_time_wise$Morning, decreasing = TRUE)[1:5], names.arg =(morning_sorted$STATE.UT[1:5]),main = "Highest Morning-Time Accidents")
barplot(sort(Data_time_wise$Afternoon, decreasing = TRUE)[1:5],names.arg =(afternoon_sorted$STATE.UT[1:5]), main = "Highest Afternoon-Time Accidents")
barplot(sort(Data_time_wise$Evening, decreasing = TRUE)[1:5], names.arg =(evening_sorted$STATE.UT[1:5]),main = "Highest Evening-time Accidents")
barplot(sort(Data_time_wise$Night, decreasing = TRUE)[1:5], names.arg =(night_sorted$STATE.UT[1:5]),main = "Highest Night-Time Accidents")
  
#creating a dataframe for sorting the highest number of total accidents 
highest_accident_states_time <- Data_time_wise[order(Data_time_wise$Total, decreasing = TRUE),]
  
#getting the names of the top 5 states with highest accidents 
high_states_time <- highest_accident_states_time$STATE.UT[1:5]
df4_time<- road_accidents_time %>%  filter(STATE.UT %in% high_states_time) %>%   select(STATE.UT, YEAR,Total)
  
#plotting a line graph to see progression over the years for each of the top states 
library(ggplot2)
ggplot(df4_time, aes(x = YEAR, y = Total, group = STATE.UT, color = STATE.UT)) +  geom_line() +  theme_bw()

#plotting the line graph for total accidents per year 
Total_over_years<-road_accidents_month %>% group_by(YEAR)%>%summarise(across(c(TOTAL), sum))
plot(Total_over_years, type = "o")

