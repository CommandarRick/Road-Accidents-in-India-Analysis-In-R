#import the librarires
library(magrittr)
library(dplyr)
library(ggplot2)

#import the data
path <-'D:/Downloads/archive/only_road_accidents_data_month2.csv'

#creating a dataframe for the data
road_accidents_month<- read.csv(path)

#creating a dataframe for grouping the states
Total_acidents_for_states_14Y<-road_accidents_month %>% group_by(STATE.UT)%>%summarise(across(c(JANUARY,FEBRUARY,MARCH,APRIL,MAY,JUNE,JULY,AUGUST,SEPTEMBER,OCTOBER,NOVEMBER, DECEMBER), sum))

#sorting the data into Summer, Spring,Autumn and Winter
Data_season_wise<-Total_acidents_for_states_14Y |> group_by(SEPTEMBER,OCTOBER,NOVEMBER)|>mutate(Autumn=sum(SEPTEMBER,OCTOBER,NOVEMBER))
Data_season_wise<-Data_season_wise |> group_by(AUGUST,JULY,JUNE)|>mutate(Summer=sum(AUGUST,JULY,JUNE))
Data_season_wise<-Data_season_wise |> group_by(MARCH,APRIL,MAY)|>mutate(Spring=sum(MARCH,APRIL,MAY))
Data_season_wise<-Data_season_wise |> group_by(DECEMBER,JANUARY,FEBRUARY)|>mutate(Winter=sum(DECEMBER,JANUARY,FEBRUARY))
Data_season_wise<-Data_season_wise |> group_by(Summer,Winter,Autumn,Spring)|>mutate(TOTAL=sum(Summer,Spring,Autumn,Winter))

#creating vectors for Summer, Spring,Autumn and Winter as Percentage
Data_season_wise$`SUMMER_P` <- Data_season_wise$Summer / Data_season_wise$TOTAL *100
Data_season_wise$`SPRING_P` <- Data_season_wise$Spring / Data_season_wise$TOTAL *100
Data_season_wise$`WINTER_P` <- Data_season_wise$Winter / Data_season_wise$TOTAL *100
Data_season_wise$`Autumn_P` <- Data_season_wise$Autumn / Data_season_wise$TOTAL *100

#plotting a boxplot to check for median and compare all the various seasons
boxplot(Data_season_wise$SUMMER_P, Data_season_wise$SPRING_P,Data_season_wise$WINTER_P,Data_season_wise$Autumn_P, ylab="Percenatge of acciddents", labels=c("SUMMER_P", "SPRING_P","WINTER_P", "AUTUMN_P" ))

#creating a pie graph for composition according to seasons
pie(colSums(Data_season_wise[, c("Summer", "Winter", "Autumn", "Spring")]), labels=c("SUMMER", "WINTER", "AUTUMN", "SPRING"), main="Seasonal distribution of all accidents in India(2001-14)")

#sorting the data according to number of accidents
summer_sorted <- Data_season_wise[order(Data_season_wise$`Summer`, decreasing = TRUE),]
Autumn_sorted <- Data_season_wise[order(Data_season_wise$`Autumn`, decreasing = TRUE),]
winter_sorted <- Data_season_wise[order(Data_season_wise$`Spring`, decreasing = TRUE),]
spring_sorted <- Data_season_wise[order(Data_season_wise$`Winter`, decreasing = TRUE),]

#plotting bar graphs for the sorted data for the top 5 states 
barplot(sort(Data_season_wise$`Summer`, decreasing = TRUE)[1:5], names.arg =(summer_sorted$STATE.UT[1:5]),main = "Highest Summer Accidents")
barplot(sort(Data_season_wise$`Winter`, decreasing = TRUE)[1:5],names.arg =(winter_sorted$STATE.UT[1:5]), main = "Highest Winter Accidents")
barplot(sort(Data_season_wise$Autumn, decreasing = TRUE)[1:5], names.arg =(Autumn_sorted$STATE.UT[1:5]),main = "Highest Autumn Accidents")
barplot(sort(Data_season_wise$`Spring`, decreasing = TRUE)[1:5], names.arg =(spring_sorted$STATE.UT[1:5]),main = "Highest Spring Accidents")

#sorting and plotting a line graph for the top 5 states to see increase in total accidents  
highest_accident_states <- Data_season_wise[order(Data_season_wise$TOTAL, decreasing = TRUE),]
high_states <- highest_accident_states$STATE.UT[1:5]
df4 <- road_accidents_month %>%  filter(STATE.UT %in% high_states) %>%   select(STATE.UT, YEAR, TOTAL)
ggplot(df4, aes(x = YEAR, y = TOTAL, group = STATE.UT, color = STATE.UT)) +  geom_line() +  theme_bw()

#making a linear regression model to predict total number of accidents for the next few years
model_for_total<-lm(TOTAL ~ YEAR ,data = Total_over_years)
new_data <- tibble(YEAR= 2015:2022)
prediction<-predict(model_for_total, newdata = new_data)
plot(prediction, type = "o")
print(prediction)

#making a prediction model for top 5 states

#Tamil Nadu
TN_over_years<-road_accidents_month[road_accidents_month$STATE.UT %in% c('Tamil Nadu'),]
model_for_TN<-lm(TOTAL ~ YEAR ,data = TN_over_years)
predict(model_for_TN, newdata=new_data)
plot(predict(model_for_TN, newdata=new_data), type='o')

#Maharashtra
MH_over_years<-road_accidents_month[road_accidents_month$STATE.UT %in% 'Maharashtra',]
model_for_MH<-lm(TOTAL ~ YEAR ,data = MH_over_years)
predict(model_for_MH, newdata=new_data)
plot(predict(model_for_MH, newdata=new_data), type='o')

#Karnataka
KN_over_years<-road_accidents_month[road_accidents_month$STATE.UT %in% 'Karnataka',]
model_for_KN<-lm(TOTAL ~ YEAR ,data = KN_over_years)
predict(model_for_KN, newdata=new_data)
plot(predict(model_for_KN, newdata=new_data), type='o')

#Andhra Pradesh
AP_over_years<-road_accidents_month[road_accidents_month$STATE.UT %in% 'Andhra Pradesh',]
model_for_AP<-lm(TOTAL ~ YEAR ,data = AP_over_years)
predict(model_for_AP, newdata=new_data)
plot(predict(model_for_AP, newdata=new_data), type='o')

#Kerala
KL_over_years<-road_accidents_month[road_accidents_month$STATE.UT %in% 'Kerala',]
model_for_KL<-lm(TOTAL ~ YEAR ,data = KL_over_years)
predict(model_for_KL, newdata=new_data)
plot(predict(model_for_KL, newdata=new_data), type='o')

# Perform one-way ANOVA test
fit <- aov(TOTAL ~ Summer + Spring + Autumn + Winter, data = Data_season_wise)
summary(fit)
