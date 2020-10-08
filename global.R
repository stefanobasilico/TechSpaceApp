library(maptools)
library(sp)
library(RColorBrewer)
library(dplyr)
library(readxl)
library(rgdal)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(sf)
library(leaflet)
library(plotly)
library(htmlwidgets)
library(data.table)

#setwd("C:/Users/w0grho/Nextcloud2/TechSpace/Shiny app/TechSpace_app/")
#setwd("C:\\Users\\ri62qih\\Nextcloud\\TechSpace\\Shiny app\\TechSpace_app\\")

#myShapeInR<-readOGR(dsn = "S:\\My Libraries\\General\\for Stefano from Colin", layer = "AMR_141")
geom <- st_read("data\\AMR_141.shp", quiet = TRUE)


#save(file = "C:\\Users\\ri62qih\\Nextcloud\\TechSpace\\Shiny app\\TechSpace_app\\data\\RBA_tech_adv1.RData", RBA_tech_adv1)

load("data\\RBA_tech_adv1.RData")

load("data\\total_DE.RData")

variables <- colnames(RBA_tech_adv1) [4:39]


convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}
