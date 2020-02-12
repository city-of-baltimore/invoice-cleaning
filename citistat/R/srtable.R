library(tidyverse)
library(stringr)
library(anchors)
library(ggplot2)
library(readxl)
library(lubridate)
library(formattable)

dot2020 <- read_excel("~/2.3.2020 DOT.xlsx")
typology <- read_excel("~/HMT2017.xlsx")

dot2020 <- filter(dot2020,dot2020$`Created Date`>as.Date('2017-01-01'))
dot2020 <- dot2020 %>% left_join(typology, copy = T)
View(dot2020)

srtable <- dot2020 %>%
  mutate(ontime = ifelse((dot2020$`SR Status` == 'Closed' &
                            dot2020$`Status Date` > dot2020$`Due Date`)|
                           (dot2020$`SR Status` == 'Open' &
                              as.Date('2/3/2020') > dot2020$`Due Date`), 0, 1)) %>%
  dplyr::filter(geo_transportation_sector != 'NULL') %>%
  dplyr::count(`SR Type`, sector = geo_transportation_sector, ontime) %>%
  ungroup() %>%
  dplyr::group_by(`SR Type`, sector) %>%
  dplyr::mutate(pct = round(100* n / sum(n),0),
                n = sum(n)) %>%
  dplyr::filter(ontime==1)

comtable<-srtable %>%
  dplyr::select(-pct) %>%
  spread(sector, n) %>%
bind_cols(srtable %>%
            dplyr::select(-c(n,`SR Type`)) %>%
            spread(sector,pct))


comtable$`Sector 1 % On Time` <- comtable$`Sector 11`
comtable$`Sector 2 % On Time` <- comtable$`Sector 21`
comtable$`Sector 3 % On Time` <- comtable$`Sector 31`
comtable$`Sector 4 % On Time` <- comtable$`Sector 41`
comtable$`Sector 1` <- as.numeric(comtable$`Sector 1`)
comtable$`Sector 2` <- as.numeric(comtable$`Sector 2`)
comtable$`Sector 3` <- as.numeric(comtable$`Sector 3`)
comtable$`Sector 4` <- as.numeric(comtable$`Sector 4`)

comtable <- dplyr::select(comtable,-c(ontime,ontime1,`SR Type1`,`Sector 11`,
                                      `Sector 21`,`Sector 31`, `Sector 41`))

comtable$Total    <- apply(comtable[,2:5],1,sum)
comtable$Variance <- comtable %>%
                      apply(1,function(row) var(as.vector(row[6:9])))%>%
                      round(1)

comtable <- comtable %>% filter(Total > 100)
comtable <- comtable[order(-comtable$Variance),]

formattable(comtable, align = c("l",rep("r", NCOL(comtable) - 1)),
            list(
  `SR Type` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
  area(col = 6:9) ~ color_tile("#FA614B", "#71CA97")
                ) )

library('geojsonsf')
library('sf')
library('rgdal')


get_neighborhood_boundaries <- function(){
  suppressPackageStartupMessages(library(geojsonsf))
  suppressPackageStartupMessages(library(sf))
  suppressPackageStartupMessages(library(rgdal))


  # load Baltimore neighborhood boundaries
  # neighborhood boundaries
  hoods.url <- "https://data.baltimorecity.gov/resource/h3fx-54q3.geojson"

  message(paste0(format(Sys.time(), format="%H:%M:%S"), ": ",
                 "Getting neighborhood boundaries from Open Baltimore @ \n", hoods.url))


  # surely there is a better way, and i tried, but couldn't get anything else to work.
  # from geojson to sf to spatial df. geojson_sp didn't work for me.
  hoods <- geojson_sf(hoods.url)
  hoods <- as_Spatial(hoods)
  hoods <- spTransform(hoods, CRS("+init=epsg:4326"))

  return(hoods)
}
get_neighborhood_boundaries()






srtable <- dot2020 %>%
  mutate(ontime = ifelse((dot2020$`SR Status` == 'Closed' &
                            dot2020$`Status Date` > dot2020$`Due Date`)|
                           (dot2020$`SR Status` == 'Open' &
                              as.Date('2/3/2020') > dot2020$`Due Date`), 0, 1)) %>%
  dplyr::filter(`Group` != 'NULL') %>%
  dplyr::count(`SR Type`, type = `Group`, ontime) %>%
  ungroup() %>%
  dplyr::group_by(`SR Type`, type) %>%
  dplyr::mutate(pct = round(100* n / sum(n),2),
                n = sum(n)) %>%
  dplyr::filter(ontime==1)

View(srtable)

comtable <- srtable %>%
  dplyr::select(-c(pct)) %>%
  tidyr::spread(type, n) %>%
  dplyr::bind_cols(srtable %>%
              dplyr::select(-c(n,`SR Type`)) %>%
              tidyr::spread(type,pct))

View(comtable)

comtable$`Typology A % On Time` <- comtable$A1
comtable$`Typology B/C % On Time` <- comtable$`B/C1`
comtable$`Typology D/E % On Time` <- comtable$`D/E1`
comtable$`Typology F/G/H % On Time` <- comtable$`F/G/H1`
comtable$`Typology I/J % On Time` <- comtable$`I/J1`

comtable$`Typology A Count` <- comtable$A
comtable$`Typology B/C Count` <- comtable$`B/C`
comtable$`Typology D/E Count` <- comtable$`D/E`
comtable$`Typology F/G/H Count` <- comtable$`F/G/H`
comtable$`Typology I/J Count` <- comtable$`I/J`

comtable <- dplyr::select(comtable,-c(ontime,ontime1,
                                      A1,A,
                                      `B/C1`,`B/C`,
                                      `D/E1`,`D/E`,
                                      `F/G/H1`,`F/G/H`,
                                      `I/J1`,`I/J`))

comtable$Total    <- apply(comtable[,7:11],1,sum)
comtable$Variance <- comtable %>%
  apply(1,function(row) var(as.vector(row[2:6])))%>%
  round(1)

comtable <- comtable %>% filter(Total > 200)
comtable <- comtable[order(-comtable$Variance),]

comtable <- dplyr::select(comtable,-c(
`Typology A Count`,
`Typology B Count`,
`Typology C Count`,
`Typology D Count`,
`Typology E Count`,
`Typology F Count`,
`Typology G Count`,
`Typology H Count`,
`Typology I Count`,
`Typology J Count`))

formattable(comtable, align = c("l",rep("r", NCOL(comtable) - 1)),
            list(
              `SR Type` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              area(col = 2:6) ~ function(x) percent(x / 100, digits = 0),
              area(col = 2:6) ~ color_tile("#FA614B", "#71CA97")
            ) )
