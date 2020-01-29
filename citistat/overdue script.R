library(readxl)
library(ggplot2)
library(ggiteam)
library(stringr)

##Load Data from Excel and create weekly date table
df <- read_excel("C:/Users/brendan.hellweg/Desktop/DOTSRS.xlsx",
                 sheet = "DOTSRS")
dates <- seq.Date(from = as.Date('2014-01-01'), to = as.Date('2020-01-01'), by = '7 days')

##Format the dates properly
df %>%
  mutate(as.Date(`Due Date`)) %>%
  mutate(as.Date(`Status Date`)) %>%
  mutate(as.Date(`Created Date`))


##Filter to desired features, just remove the hashtags to make any row live
df<-df%>%
  filter(str_detect(df$`SR Type`,"TRM-Potholes"))
  #filter(  `SR Type`=="TEC-Footways Complaint"
           #|`SR Type`=="TRM-Street and Crosswalk Markings"
           #|`SR Type`=="TRM-Curb Repair"
           #|`SR Type`=="TEC-Street Repair (Misc)"
           #|`SR Type`=="TRT-New Traffic Signal"
           #|`SR Type`=="TRM-Alleys"
           #|`SR Type`=="TRT-Traffic Calming"
           #|`SR Type`=="TRT-Sign New"
           #|`SR Type`=="TRT-Crosswalks"
           #|`SR Type`=="TRT-Sign Faded/Missing/Remove/Change"
           #|`SR Type`=="TRT-Signal Timing"
           #`SR Type`=="TRT-All Way Stop"
           #)
## %>% filter(df,df$Agency = _____)
## %>% filter(df,df$Neighborhood = _____)
## %>% filter(df,df$`Created Date` < _____)
## %>% filter(df,df$`Created Date` > _____)

##Create function that shows the overdue SRs over time
oover <- function(arg1){
                  NROW(df$`SR ID`[which(
                            df$`Due Date` < arg1&
                          ((df$`SR Status` == "Closed" &
                            df$`Status Date` > arg1)|
                            df$`SR Status` == "Open"))])
                        }

##Create a function for the total number of open SRs at a given date
oopen <- function(arg1){
                  NROW(df$`SR ID`[which(
                        df$`Created Date` < arg1&
                        ((df$`SR Status` == "Closed" &
                            df$`Status Date` > arg1)|
                          df$`SR Status` == "Open"))])
                        }

##Create a function for the percent of open SRs that were overdue at a given date
poover <- function(arg1){oover(arg1)/oopen(arg1)}

##Create a Data Frame for the dates and values for each of the above functions
od <- data.frame("Date" = dates, "Overdue" = sapply(dates,oover))
op <- data.frame("Date" = dates, "Number Open" = sapply(dates,oopen))
ov <- data.frame("Date" = dates, "Percent Open" = sapply(dates,poover))

##Plot the number of overdue SRs over time
ggplot(od,aes(Date,Overdue))+
  geom_line(colour = "#f4d57f")+
  geom_point(colour = "#fdb924")+
  theme_iteam_presentations()+
  expand_limits(y = 0)+
  labs(title = "All Overdue DOT SRs",
       subtitle = "Produced by the Mayor's Office of Performance & Innovation")

##Plot the number of open SRs over time
ggplot(op,aes(Date,Number.Open))+
  geom_line(colour = "#f4d57f")+
  geom_point(colour = "#fdb924")+
  theme_iteam_presentations()+
  expand_limits(y = 0)+
  labs(y = "Number Open",
       title = "Count of Open SRs Over Time",
       subtitle = "Produced by the Mayor's Office of Performance & Innovation")

##Plot the percent of open SRs that were overdue at a given date
ggplot(ov,aes(Date,Percent.Open))+
  scale_y_continuous(labels = scales::percent)+
  geom_line(colour = "#f4d57f")+
  geom_point(colour = "#fdb924")+
  theme_iteam_presentations()+
  expand_limits(y = 0)+
  labs(y = "Percent Overdue",
       title = "Percent of Open SRs Overdue Over Time",
       subtitle = "Produced by the Mayor's Office of Performance & Innovation")
