library(readxl)
library(ggplot2)
library(ggiteam)
library(stringr)
library(plyr)
library(anchors)
library(lubridate)
library(zoo)
library(sparkline)

##Load Data from Excel and create weekly date table
df <- read_excel("~/2.3.2020 DOT.xlsx")
dates <- seq.Date(from = as.Date('2018-01-01'), to = as.Date('2020-01-01'), by = 'days')


##Format the dates properly
df %>%
  mutate(as.Date(`Due Date`)) %>%
  mutate(as.Date(`Status Date`)) %>%
  mutate(as.Date(`Created Date`))

View(df)

##Filter to desired features, just remove the hashtags to make any row live
# df1<-df %>%
  #filter(SR.Type=="TRT-New Traffic Sign")
 # filter(  `SR Type`=="TRT-Traffic Signal Repairs"
  #         |`SR Type`=="TRT-Traffic Sign Request"
   #        |`SR Type`=="TRT-New Traffic Sign"
    #       |`SR Type`=="TRT-Traffic Calming"
     #      |`SR Type`=="TRT-Signal Timing"
      #     |`SR Type`=="TRT-Crosswalks"
       #    |`SR Type`=="TRT-New Stop Sign"
        #   |`SR Type`=="TRT-Angled Parking Request"
         #  )
 View(df)
## %>% filter(df,df$Agency = _____)
## %>% filter(df,df$Neighborhood = _____)
## %>% filter(df,df$`Created Date` < _____)
# filter(df,df$Created.Date > '2016/01/01')

##Create function that shows the overdue SRs over time
oover <- function(arg1){
                  NROW(df1$`Service Request Number`[which(
                            df1$`Due Date` < arg1&
                          ((df1$`SR Status` == "Closed" &
                            df1$`Status Date` > arg1)|
                            df1$`SR Status` == "Open"))])}

##Create a function for the total number of open SRs at a given date
oopen <- function(arg1){
                  NROW(df1$`Service Request Number`[which(
                        df1$`Created Date` < arg1&
                        ((df1$`SR Status` == "Closed" &
                            df1$`Status Date` > arg1)|
                          df1$`SR Status` == "Open"))])
                        }

##Create a function for the percent of open SRs that were overdue at a given date
poover <- function(arg1){oover(arg1)/oopen(arg1)}

##Create a Data Frame for the dates and values for each of the above functions
od <- data.frame("Date" = dates, "Overdue" = sapply(dates,oover))
op <- data.frame("Date" = dates, "Number.Open" = sapply(dates,oopen))
ov <- data.frame("Date" = dates, "Percent.Open" = sapply(dates,poover))

##Plot the number of overdue SRs over time
ggplot(od,aes(Date,Overdue))+
  geom_line(colour = "#f4d57f")+
  geom_point(colour = "#fdb924")+
  theme_iteam_presentations()+
  expand_limits(y = 0)+
  labs(title = "All Crosswalks SRs",
       subtitle = "Produced for TrafficStat")

##Plot the number of open SRs over time
ggplot(op,aes(Date,`Number Open`))+
  geom_line(colour = "#f4d57f")+
  geom_point(colour = "#fdb924")+
  theme_iteam_presentations()+
  expand_limits(y = 0)+
  labs(y = "Number Open",
       title = "Count of Open SRs Over Time",
       subtitle = "Produced by the Mayor's Office of Performance & Innovation")

##Plot the percent of open SRs that were overdue at a given date
ggplot(ov,aes(Date,`Percent Open`))+
  scale_y_continuous(labels = scales::percent)+
  geom_line(colour = "#f4d57f")+
  geom_point(colour = "#fdb924")+
  theme_iteam_presentations()+
  expand_limits(y = 0)+
  labs(y = "Percent Overdue",
       title = "Percent of Open SRs Overdue Over Time",
       subtitle = "Produced by the Mayor's Office of Performance & Innovation")


#Quick Access
df1 <- df # reset button

df1<-df1%>%
  filter(`SR Type`=="TRM-Footways Complaint")

  ggplot(data.frame("Date" = dates, "Overdue" = sapply(dates,oover)),aes(Date,Overdue))+
  geom_line(colour = "#f4d57f")+
  geom_point(colour = "#fdb924")+
  theme_iteam_presentations()+
  expand_limits(y = 0)+
  labs(title = "All DOT Service Requests",
       subtitle = "Produced for DOTStat")

# create functions and manipulate table
srtype <- function(arg1){
           df_1 <- df1
            filter(df_1,df_1$`SR Type` == arg1)
}

    oover <- function(arg1,arg2){
    oover1 <- function(p1, p2){
      NROW(p2$`Service Request Number`[which(
        p2$`Due Date` < p1 &
          ((p2$`SR Status` == "Closed" &
              p2$`Status Date` > p1)|
             p2$`SR Status` == "Open"))])}
    oover1(arg1,srtype(arg2))}

    spark = data.frame("Type" = c("bar", "line", "bullet", "pie", "tristate", "discrete"),
                       Sparkline = c(as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "bar"))),
                                     as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "line"))),
                                     as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "bullet"))),
                                     as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "pie"))),
                                     as.character(htmltools::as.tags(sparkline(c(-1,0,1,1,1,-1,0,2), type = "tristate"))),
                                     as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "discrete")))))
    out = as.htmlwidget(formattable(spark))
    out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
    out

#Create table for overdues
df1$Quarter <- df1$`Created Date` %>% floor_date(unit = "quarter") #create quarter column

dftable <- df1 %>%
  dplyr::count(sr = `SR Type`,q = as.Date(Quarter)) %>% #create summary table
  mutate(overdue = mapply(FUN = oover, arg1 = q, arg2 = sr)) %>% #apply overdue function
  dplyr::mutate(`SR Type` = sr,
                Quarter = q,
                Backlog = overdue) %>%
  dplyr::select(-c(sr,q,n,overdue)) %>%
  dplyr::filter(Quarter > '2018-12-31') %>%
  dplyr::mutate(Quarter = as.yearqtr(Quarter,format = "%Y-%m-%d")) %>%

  spread(Quarter,Backlog)

dftable <- dftable[order(-dftable[6]),]

dftable$Trend <- apply(dftable[,2:6],1,FUN = function(x)
  as.character(htmltools::as.tags(sparkline(as.numeric(x), type = "line"))))
  out = as.htmlwidget(formattable(dftable,
      align = c("l",rep("r", NCOL(dftable) - 1)),
      list(`SR Type` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")))))
out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out
