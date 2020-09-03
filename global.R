library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinydashboard)
library(shinyjs)
library(DBI)
library(shinyFeedback)
library(plotly)
library(gt)
library(data.table)
library(httr)

source(file = "functions.R", local = TRUE)

options(shiny.maxRequestSize = 32*1024^2, stringsAsFactors = F)
# mydb <- dbConnect(RSQLite::SQLite(), "data/indexData.sqlite")

trip_volume<-readData(table_name ="tripsIndexData",key = "sYp4cTwEKYhuZV5bAJxxefFuoLVqXL6p")
domestic<-readData(table_name ="domesticTravellersIndexData",key = "sYp4cTwEKYhuZV5bAJxxefFuoLVqXL6p")
international<-readData(table_name ="internationalTravellersIndexData",key = "sYp4cTwEKYhuZV5bAJxxefFuoLVqXL6p")
stay_metric<-readData(table_name ="staymetricsIndex",key = "sYp4cTwEKYhuZV5bAJxxefFuoLVqXL6p")


# cbd_workers <- dbGetQuery(mydb,"Select * From cbdWorkersIndexData")
# domestic <- dbGetQuery(mydb,"Select * From domesticTravellersIndexData")
# international <- dbGetQuery(mydb,"Select * From internationalTravellersIndexData")
# link_volume <- dbGetQuery(mydb,"Select * From linkVolumeIndexData")
# trip_volume <-dbGetQuery(mydb,"Select * From tripsIndexData")

app_data <- list(
  "Number of Total Daily Trips"   = trip_volume %>% filter(location_type == "gcc", trip_type == "allTrips", metric_type == "Total Daily Trips"),
  "Total Distance Travelled (km)" = trip_volume %>% filter(location_type == "gcc", trip_type == "allTrips", metric_type == "Distance Travelled (km)"),
  "Domestic Interstate Visitors"  = domestic %>% filter(location_type == "gcc"),
  "International Visitors"        = international %>% filter(location_type == "gcc_level"),
  "Duration Travelled (hrs)"      = trip_volume %>% filter(location_type == "gcc", trip_type == "allTrips", metric_type == "Duration Travelled (hrs)"),
  "Average Time Spent at Home"    = stay_metric %>% filter(location_type == "gcc", metric_type == "avg_hrs")
) %>%
  dplyr::bind_rows(.id = "metric") %>%
  dplyr::select(metric, date, timestamp, location_value, value) %>% 
  mutate(value = as.numeric(value))
  # dplyr::select(metric, datedate, timestamp, agg_value, value)


gccmapping <- c(
  "Greater Sydney"                   = "1GSYD",
  "Rest of NSW"                      = "1RNSW",
  "Rest of Vic."                     = "2RVIC",
  "Greater Melbourne"                = "2GMEL",
  "Greater Brisbane"                 = "3GBRI",
  "Rest of Qld"                      = "3RQLD",
  "Greater Adelaide"                 = "4GADE",
  "Rest of SA"                       = "4RSAU",
  "Rest of WA"                       = "5RWAU",
  "Greater Perth"                    = "5GPER",
  "Greater Hobart"                   = "6GHOB",
  "Rest of Tas."                     = "6RTAS",
  "Greater Darwin"                   = "7GDAR",
  "Rest of NT"                       = "7RNTE",
  "Australian Capital Territory"     = "8ACTE"  
)

metric_colours <- list(
  "Number of Total Daily Trips"   = "#de12bf",
  "Total Distance Travelled (km)" = "#12a8de",
  "Domestic Interstate Visitors"  = "#1531bf",
  "International Visitors"        = "#9123cc",
  "Duration Travelled (hrs)"      = "#b7eb34",
  "Average Time Spent at Home"    = "#f20014"
) 
