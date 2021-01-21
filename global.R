##-- Pacotes ----
library(dplyr)
library(dbplyr)
library(rvest)
library(data.table)
library(reshape2)
library(tidyr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(rgeos)
library(shinycssloaders)
library(ggthemes)
library(RColorBrewer)
library(sf)
library(sp)
library(scales)
library(leaflet)
library(plotly)
library(highcharter)
library(DT)
library(mapview)
library(dygraphs)
library(xts)
library(geojsonR)
library(waiter)

##--utilidades  ----
source("functions/utils.R")
colores <- c("#098ebb", "#fdc23a", "#e96449", "#818286")

##-- Componentes de header shiny ----
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
tab_files <- tab_files[-grep(x = tab_files, pattern = "server")]

suppressMessages(lapply(tab_files, source))


