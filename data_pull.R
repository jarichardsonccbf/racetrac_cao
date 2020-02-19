library(tidyverse)
library(RJDBC)
library(keyring)
library(lubridate)
library(plotly)
library(readxl)

# import and establish hana connection ----

# options(java.parameters = "-Xmx8048m")
# # memory.limit(size=10000000000024)
#
# # classPath="C:/Program Files/sap/hdbclient/ngdbc.jar"
# # For ngdbc.jar use        # jdbcDriver <- JDBC(driverClass="com.sap.db.jdbc.Driver",
# # For HANA Studio jar use  # jdbcDriver <- JDBC(driverClass="com.sap.ndb.studio.jdbc.JDBCConnection",
#
# jdbcDriver <- JDBC(driverClass="com.sap.db.jdbc.Driver",
#                    classPath="C:/Program Files (x86)/sap/hdbclient/ngdbc.jar")
#
# jdbcConnection <- dbConnect(jdbcDriver,
#                             "jdbc:sap://vlpbid001.cokeonena.com:30015/",
#                             "fl014036",
#                             "jasoN3#w")
#
#
# # Fetch all results
#
# sql <- 'SELECT VISITTYPE,
#                       ACCOUNT,
#                       EXECUTIONSTART,
#                       EXECUTIONEND,
#                       STATUS,
#                       SOURCE,
#                       ACCOUNT_NAME
#          FROM "_SYS_BIC"."cona-reporting.field-sales/Q_CA_R_SpringVisit"
#          WHERE
#          STATUS = ? and
#          SOURCE = ? and
#          PLANNEDSTART >= ? and
#          PLANNEDEND <= ?'
#
#
# # VISITTYPE
# param2 <- 'FINAL'
# param3 <- 'INT'
# param4 <- '2019-03-12 00:00:00'
# param5 <- '2020-02-15 00:00:00'
#
# spring.visit1 <- dbGetQuery(jdbcConnection, sql, param2, param3, param4, param5)
#
# dbDisconnect(jdbcConnection)
#
# rm(jdbcConnection, jdbcDriver, param2, param3, param4, param5, sql)
#
# saveRDS(spring.visit1, "data/spring.visit1.RDS")

spring.visit <- readRDS("data/spring.visit1.RDS")

# 10/21 go live ----

tpa.golive <- read_excel("data/Tampa - Racetrac.xlsx") %>%
  select(Customer) %>%
  mutate(DC = "TAMPA",
         Go.Live = "10/21/2019")

spring.tpa <- spring.visit %>%
  rename(Customer = ACCOUNT) %>%
  mutate(Customer = as.character(Customer),
         Customer = substring(Customer, 2)) %>%
  semi_join(tpa.golive, by = "Customer") %>%
  left_join(tpa.golive, by = "Customer") %>%
  mutate(date = date(EXECUTIONSTART)) %>%
  filter(VISITTYPE == "ZR") %>%
  mutate(Actual.Time..Minutes. = ((ymd_hms(EXECUTIONEND) - ymd_hms(EXECUTIONSTART)) / 60)) %>%
  filter(Actual.Time..Minutes. <= 120 &
           Actual.Time..Minutes. >= 10) %>%
  select(-c(STATUS, SOURCE, EXECUTIONSTART, EXECUTIONEND)) %>%
  mutate(Go.Live = as.Date(Go.Live, format = "%m/%d/%Y"),
         date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(CAO = as.numeric(date - Go.Live)) %>%
  filter(CAO > 10 | CAO < 0) %>%
  filter(CAO > -106) %>% # decrease to get more PRE dates
  mutate(CAO = case_when(
    date < Go.Live ~ "PRE",
    date >= Go.Live ~ "POST"))

counts.tpa <- spring.tpa %>%
  count(DC, CAO)

counts.tpa


# 11/18 go live ----

north.golive <- read_excel("data/RaceTrac 11-18-19.xlsx") %>%
  select(Customer, DC_Name) %>%
  rename(DC = DC_Name) %>%
  filter(DC == "JACKSONVILLE" |
           DC == "ORLANDO" |
           DC == "DAYTONA" |
           DC == "BREVARD" |
           DC == "FT PIERCE" |
           DC == "LAKELAND" |
           DC == "GAINESVILLE" |
           DC == "SEBRING" |
           DC == "OCALA") %>%
  mutate(Go.Live = "11/18/2019")

spring.nrt <- spring.visit %>%
  rename(Customer = ACCOUNT) %>%
  mutate(Customer = as.character(Customer),
         Customer = substring(Customer, 2)) %>%
  semi_join(north.golive, by = "Customer") %>%
  left_join(north.golive, by = "Customer") %>%
  mutate(date = date(EXECUTIONSTART)) %>%
  filter(VISITTYPE == "ZR") %>%
  mutate(Actual.Time..Minutes. = ((ymd_hms(EXECUTIONEND) - ymd_hms(EXECUTIONSTART)) / 60)) %>%
  filter(Actual.Time..Minutes. <= 120 &
           Actual.Time..Minutes. >= 10) %>%
  select(-c(STATUS, SOURCE, EXECUTIONSTART, EXECUTIONEND)) %>%
  mutate(Go.Live = as.Date(Go.Live, format = "%m/%d/%Y"),
         date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(CAO = as.numeric(date - Go.Live)) %>%
  group_split(DC)

spring_selector <- function(tib, PRE) {

  dc.rbinder <- tib %>%
  filter(CAO > 10 | CAO < 0) %>%
  filter(CAO > PRE) %>% # decrease to get more PRE dates
  mutate(CAO = case_when(
    date < Go.Live ~ "PRE",
    date >= Go.Live ~ "POST"))

  dc.counts <- dc.rbinder %>%
    count(DC, CAO)

  newlist <- list(dc.rbinder, dc.counts)

  return(newlist)

}

BREVARD <- spring_selector(spring.nrt[[1]], -85)

DAYTONA <- spring_selector(spring.nrt[[2]], -74)

FT.PIERCE <- spring_selector(spring.nrt[[3]], -71)

GAINESVILLE <- spring_selector(spring.nrt[[4]], -115)

JACKSONVILLE <- spring_selector(spring.nrt[[5]], -89)

LAKELAND <- spring_selector(spring.nrt[[6]], -77)

ORLANDO <- spring_selector(spring.nrt[[7]], -84)

# bind locations

spring <- rbind(spring.tpa, BREVARD[[1]], DAYTONA[[1]], FT.PIERCE[[1]], GAINESVILLE[[1]], JACKSONVILLE[[1]], LAKELAND[[1]], ORLANDO[[1]])

counts <- rbind(counts.tpa, BREVARD[[2]], DAYTONA[[2]], FT.PIERCE[[2]], GAINESVILLE[[2]], JACKSONVILLE[[2]], LAKELAND[[2]], ORLANDO[[2]])

rm(counts.tpa, BROWARD, BREVARD, DAYTONA, FT.MYERS, FT.PIERCE, GAINESVILLE, JACKSONVILLE, LAKELAND, MIAMI.DADE, north.golive, ORLANDO, PALM.BEACH, SARASOTA, spring.nrt, spring.tpa, spring.visit, tpa.golive)

# Summaries ----

spring.avg <- spring %>%
  group_by(Customer, CAO, DC) %>%
  summarise(`Average minutes in outlet` = mean(Actual.Time..Minutes., na.rm = T),
            SD = sd(Actual.Time..Minutes., na.rm = T))

# write.csv(spring.visit.avg, "deliverables/store_averages.csv", row.names = FALSE)

# use for a cutoff point

a <- spring %>%
  arrange(as.numeric(Actual.Time..Minutes.)) %>%
  mutate(order = 1:n()) %>%
  ggplot(aes(x = order, y = as.numeric(Actual.Time..Minutes.))) +
  geom_point()

ggplotly(a)

spring <- spring %>%
  mutate(Actual.Time..Minutes. = as.numeric(Actual.Time..Minutes.)) %>%
  filter(Actual.Time..Minutes. <= 120 &
           Actual.Time..Minutes. >= 10)

rm(a)
