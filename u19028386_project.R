#Name : Sinenhlanhla Dlamini
#Studednt number : u19028386
#WST Project
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)

#data
provincesdata=read.csv("ProvincePopulation.csv")
crimedata=read.csv("SouthAfricaCrimeStats_v2.csv")

#question
#q1 
#comparing density with population in plot
q1a <- ggplot(data=provincesdata, 
              mapping =aes( x=Density, 
                            y = Population, 
                            color = Province, 
                            fill = Province)) + 
       labs(title =  "Population VS Density for different provinces in SA") + 
       geom_col() +
      theme(plot.title = element_text(hjust = 0.5)) 
plot(q1a)

#comparing area with population in plot
q1b <- ggplot(data=provincesdata, 
              mapping =aes(x=Area, 
                          y = Population,
                          fill = Province,
                          color = Province,
                         )) + 
       labs( title = "Population VS Area for different provinces in SA") + 
       scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
       geom_col() + 
       theme(plot.title = element_text(hjust = 0.5)) 
plot(q1b)

#q2
#crime in the provinces from 2015 to 2016 
q2a <- crimedata %>%
      select(Province,X2015.2016) %>% 
      group_by(Province) %>% 
      summarise(TotalCrime = sum(X2015.2016)) %>%
      arrange(desc(TotalCrime))

#crime in the provinces from 2005 to 2006      
q2b <- crimedata %>%
  select(Province,X2005.2006) %>% 
  group_by(Province) %>% 
  summarise(TotalCrime = sum(X2005.2006)) %>%
  arrange(desc(TotalCrime))

#q3
#calculating total crime for each Station
q3 <- crimedata %>%
      filter(Province == "Gauteng") 

#adding a column of totalcrime
q3$TotalCrime <- q3$X2005.2006 + q3$X2006.2007 + 
                 q3$X2007.2008 + q3$X2008.2009 +
                 q3$X2009.2010 + q3$X2010.2011 +
                 q3$X2011.2012 + q3$X2012.2013 +
                 q3$X2013.2014 + q3$X2014.2015 +
                 q3$X2015.2016

#adding if crime too high or low comparing to the mean 
q3$High_Crime <- ifelse(q3$TotalCrime > mean(q3$TotalCrime),"Yes","No")

#getting Station with maximum Crime
q3a <- q3 %>% 
       filter(TotalCrime == max(TotalCrime)) %>%
       select(Station,TotalCrime)

#Stations with a high crime rate
q3b <- q3 %>%
       filter(High_Crime == "Yes") %>%
       select(Station,High_Crime)

#Stations with a low crime rate
q3c <- q3 %>%
  filter(High_Crime == "No") %>%
  select(Station,High_Crime)

#q4
#getting the crime category that is the highest from JHB central
q4a <- q3 %>% 
  filter(TotalCrime == max(TotalCrime)) %>%
  select(Station,TotalCrime,Category) 

#q5
#getting all JHB central crime categories
q5 <- q3 %>%
  filter(Station == "Jhb Central") %>%
  group_by(Category) %>%
  select(Station,Category,TotalCrime)

#plotting the different categories
q5a <- ggplot(data=q5, 
              mapping =aes(y=Category, 
                           x=TotalCrime
                           )) +
              labs(title = "Category VS Total Crime in Jhb Central",
                   x= "Total Crime") + 
  geom_col(fill = "Purple") +
  theme(plot.title = element_text(hjust = 0.5)) 
plot(q5a)

#q6
#trend of the most highest crime gategory with a plot over the years
q6 <- q3 %>%
      filter(Station == "Jhb Central" & Category == "All theft not mentioned elsewhere")%>%
      select(-c(Province,TotalCrime))

#creating a dataframe for the crime cases along with the years
q6a <- data.frame(mYears = c("2005","2006","2007","2008","2009","2010",
                             "2011","2012","2013","2014","2015"),
                  mCrime = c(6093,4602,3761,3610,3267,3037,
                             2886,2638,2809,3050,2434))

#plotting data from dataframe created above
q6b <- ggplot(data=q6a, 
              mapping =aes(x=mYears,y=mCrime)) +
      labs(title = "All theft not mentioned elsewhere VS Years in Jhb Central",
           x = "Years",
           y = "All theft not mentioned elsewhere") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) 
  
plot(q6b)




     