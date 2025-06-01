
library(tidyverse)
library(readxl)
library(data.table)
library(treemap)

MainData<-read.csv("ProjectTycho_Level2_v1.1.0.csv", header=TRUE)

#checking type of variables

glimpse(MainData)

#class changing for "event"

class(MainData$event)
unique(MainData$event)

MainData$event<-as.factor(MainData$event)
class(MainData$event)

#class chnaging for "from_date" & "to_date"
cleanedata$from_date<-as.Date(cleanedata$from_date)
cleanedata$to_date<-as.Date(cleanedata$to_date)


#removing unnecessory columns

cleanedata<-MainData %>% select(-c(country, url))

#removing raws with missing values
##first checking the values

cleanedata %>% select(state) %>% filter(!complete.cases(.))

##for all columns (there's no missing values)

#there's no requiremnets to cleaning for dupplicates

#remove raw having 0 value in number column
cleanedata<-cleanedata[cleanedata$number!=0, ]

#treemap for all state with top 3 disease

##grouping data by state and disease

data_state<-cleanedata %>% group_by(state, disease) %>% summarize(total_cases=sum(number))

##ranking disease
rank <- data_state %>%
  +     group_by(state) %>%
  +     mutate(rank = dense_rank(desc(total_cases)))

##taking top 3 state
rank<-rank %>% filter(rank<=3)

##treemap code

treemap(rank, index = c("state","disease"),
          +         vSize = "total_cases", vColor = "disease", type = "index", bg.labels = 0,
          +         title = "  Treemap for all states with top 3 disease", border.col = c("black", "white"),
          +         border.lwds = c(0,0), palette = ("Set3"),
          +         align.labels = list(c("left","top"),c("right","bottom"))) 

#line graph for top 3 disease of USA by year wise

##finding top 3 disease in USa

data_usa<-cleanedata %>% group_by(disease) %>% summarize(total_cases=sum(number))

###here, we can find top 3 disease (measals, influenza, scarlet fever)

##grouping data by these 3 disease year wise

###triming epi_week to get year column
data_usa$year<-substr(data_usa$epi_week, 1, nchar(data_usa$epi_week) - 2)

###removing unneccesry columns
data_usa<-data_usa %>% select(-c(epi_week, state, loc, loc_type, from_date, to_date))

###keeping data of only 3 diseases

dsc<-c("MEASLES", "SCARLET FEVER", "INFLUENZA")
data_usa<-subset(data_usa, disease %in% dsc)

###grouping data
data_usa<-data_usa %>% group_by(year, disease, event) %>% summarize(total_cases=sum(number))

###first we need to make the different table for event deaths and cases

##plotting line graph for deaths

xscale <- seq(1885, 2015, by = 5)
yscale <- seq(1, 15001, by = 1000)
ggplot(data_usa1, aes(x = year, y = total_cases, color = disease)) +
geom_line(size=1) +
scale_x_continuous(breaks = xscale) +
scale_y_continuous(breaks = yscale, limits = c(0, 15001)) +
labs(Title="Line graph of Death caused by Most effected Disease in USA", x = "Effected Year", y = "Number of deaths", color = "Name of Disease") +
theme_minimal()

##plotting line graph for cases

ggplot(data_usa2, aes(x = year, y = total_cases, color = disease))
  +     geom_line(size=1)
  +     scale_x_continuous(breaks = xscale)
  +     scale_y_continuous(breaks = yscale, limits = c(0, 1100000))
  +     labs(Title="Line graph of Cases caused by Most effected Disease in USA", x = "Effected Year", y = "Number of Cases", color = "Name of Disease")
  +     theme_minimal()


#Cases of Measles in all state between 1906-2001

