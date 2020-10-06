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

#setwd("C:/Users/w0grho/Nextcloud2/TechSpace/Shiny app/TechSpace_app/")
setwd("C:\\Users\\ri62qih\\Nextcloud\\TechSpace\\Shiny app\\TechSpace_app\\")

df_arbeitsmarktregionen <- read_xlsx("data\\Arbeitsmarktregionen_Diss.xlsx") 

#myShapeInR<-readOGR(dsn = "S:\\My Libraries\\General\\for Stefano from Colin", layer = "AMR_141")
geom <- st_read("data\\AMR_141.shp", quiet = TRUE)

#save(file = "C:\\Users\\ri62qih\\Nextcloud\\TechSpace\\Shiny app\\TechSpace_app\\data\\RBA_tech_adv1.RData", RBA_tech_adv1)

load("data\\RBA_tech_adv1.RData")

load("data\\total_DE.RData")

load("data\\final_RBA.RData")

load("data\\schmoch.RData")

final_RTA <- merge(final_RTA, unique(schmoch[,c("Field_number", "Field_en")]), all.x = TRUE)

final_RTA$Field_en[is.na(final_RTA$Field_en)] <- "Miscellaneous"

final_RTA$region <- as.numeric(final_RTA$region)

variables <- colnames(RBA_tech_adv1) [4:39]


convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

# User interface ----
ui <- fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 13px; line-height: 13px; }"),
  dashboardPage(
header <-    dashboardHeader(title = "TechSpace"),
sidebar <-    dashboardSidebar(
      sidebarMenu(
        convertMenuItem(menuItem("Regional RBA", tabName = "Regional_RBA", icon = icon("map"), startExpanded = TRUE, sliderInput("year", 
                                                                                               label = "Year of interest:", value = 1994,
                                                                                               min = 1994, max = 2015, sep = ""),
                 
                selectInput("variable", label = "Variable:", choices = 
                                   list("Number of RBA > 1" = variables[1],
                                        "RBA on a single technology" = variables[2:36]))), tabName = "Regional_RBA"),
      convertMenuItem(menuItem("Technological RBA", icon = icon("rocket"), tabName = "Technological_RBA", menuSubItem("German Tech Landscape", tabName = "TL"), sliderInput(inputId = "year1",label = "Year of interest:", 
                                                                                                                  min = 1995, max = 2015, value = 1995, sep = ""), menuSubItem("Single Tech Evolution", tabName = "TE"), selectInput(inputId = "variable1",label = "Technology:", 
                                                                                                                                                                               choices = unique(total_DE$Field_en))
                               ), tabName = "Technological_RBA")
              )
            )
      ,
body <-    dashboardBody(
  tabItems(
 
    tabItem(tabName = "Regional_RBA",
            h2(
              #fluidRow(
             # box(sliderInput("year", 
             #                           label = "Year of interest:", value = 1994,
             #                           min = 1994, max = 2015, sep = ""), width = 3, status = "warning"),
            
             # box(selectInput("variable", label = "Variable", choices = 
             #                   list("Number of RBA > 1" = variables[1],
             #                        "Patent Shares" = variables[2:35])), width = 3)),
              fluidRow(
                        box(leafletOutput("map", height = 745), width = 12, status = "primary"), p("Click on a region to display additional statistics", style = "color:black;font-size:10pt;"),

        conditionalPanel("input.myEvent == 'open'",absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 100, left = "auto", right = 45, bottom = "auto",
                                width = "28vw", height = "35vh", style = "background-color: white;
                        opacity: 0.85;
                        padding: 20px 20px 20px 20px;
                        margin: auto;
                       border-radius: 5pt;
                        box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                        padding-bottom: 2mm;
                         padding-top: 1mm;",          
plotlyOutput("plot2", height = 300), p("Pass over the bars to display the results", style = "color:black;font-size:10pt;")))
                   
))),
    tabItem(tabName = "Technological_RBA",
            h2(
              #fluidRow(box(sliderInput(inputId = "year1",label = "Year of interest:", 
               #            min = 1995, max = 2015, value = 1995, sep = ""), width = 3, status = "warning"), 
              fluidRow(column(6,box(plotlyOutput("plot", height = 350), width = 12, status = "primary")),column(6,
               box(plotlyOutput("plot1", height = 350), width = 12, status = "primary")))
               ))
    
  )

  
  )
)
)



# Server logic ----
server <- function(input, output, session) {
  

  
 data <- reactive({
   RBA_tech_adv1 %>%
     filter(year == as.numeric(input$year)) %>%
     select(region_number, input$variable)
 })  
   
 output$map <- renderLeaflet({
   
   data <- data()
  
  if(colnames(data)[2] == variables[1]) {
  
  colnames(data) [2] <- "shares_n"
   
  data$region_number <- as.numeric(as.character(data$region_number))
   
   missing <- setdiff(max(data$region_number):min(data$region_number),data$region_number)
   
  df2 <- rbind(data,data.frame(region_number=missing, shares_n=0))
   
   df2 <- df2[order(df2$region_number),]
   
   df2$region_number <- as.factor((sort(unique(df_arbeitsmarktregionen$AMR_Nr))))

   
   geom <- merge(geom[,c("AMR", "AMR_NAME")], df2, by.x = "AMR", by.y = "region_number")
   
  
  geom_WGS84 <- st_transform(geom, 4326)
  

  #pal <- colorNumeric(
  #  palette = "YlOrRd",
  #  domain=c(min(df2$shares_n), max(df2$shares_n)))
  
  pal <- colorQuantile("YlOrRd", unique(df2$shares_n), n = 7)
  
  # the extra code building off the existing pal
  qpal_colors <- unique(pal(sort(unique(df2$shares_n)))) # hex codes
  qpal_labs <- quantile(unique(df2$shares_n), seq(0, 1, .1428571)) # depends on n from pal
  qpal_labs <- round(qpal_labs, digits = 0)
  qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA

  p_popup <- paste0("<strong>", geom_WGS84$AMR_NAME, ": </strong>", geom_WGS84$shares_n)
  
  leaflet(geom_WGS84) %>%
   addPolygons(
      stroke = TRUE, weight = 1, color = "black", # remove polygon borders
     fillColor = ~pal(shares_n), # set fill color with function from above and value
      fillOpacity = 0.4, smoothFactor = 0.5, # make it nicer
     layerId = ~AMR,
      popup = p_popup)  %>%
    onRender("
                     function(el, x) {
                         this.on('popupopen', function(e) {
                             Shiny.onInputChange('myEvent', 'open');
                         });

                         this.on('popupclose', function(e) {
                             Shiny.onInputChange('myEvent', 'close');
                         });
                     }") %>%
    # addTiles( urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    #           attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    addTiles() %>%
    addLegend("bottomright",  # location
              colors= qpal_colors,
              labels = qpal_labs)
              #pal=pal,    # palette function
             #values=~shares_n,  # value to be passed to palette function
              #title = "")# %>%
   # setView(long, lat, zoom = 5)
  
  
  
  }else {
    colnames(data) [2] <- "shares_n"
    
    data$region_number <- as.numeric(as.character(data$region_number))
    
    missing <- setdiff(max(data$region_number):min(data$region_number),data$region_number)
    
    df2 <- rbind(data,data.frame(region_number=missing, shares_n=0))
    
    df2 <- df2[order(df2$region_number),]
    
    df2$region_number <- as.factor((sort(unique(df_arbeitsmarktregionen$AMR_Nr))))
    
    
    geom <- merge(geom[,c("AMR", "AMR_NAME")], df2, by.x = "AMR", by.y = "region_number")
    
    
    geom_WGS84 <- st_transform(geom, 4326)
    
    
    #pal <- colorNumeric(
    #  palette = "YlOrRd",
    #  domain=c(min(df2$shares_n), max(df2$shares_n)))
    
    pal <- colorQuantile("YlOrRd", unique(df2$shares_n), n = 7)
    
    # the extra code building off the existing pal
    qpal_colors <- unique(pal(sort(unique(df2$shares_n)))) # hex codes
    qpal_labs <- quantile(unique(df2$shares_n), seq(0, 1, .1428571)) # depends on n from pal
    qpal_labs <- round(qpal_labs, digits = 2)
    qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
    
    p_popup <- paste0("<strong>", geom_WGS84$AMR_NAME, ": </strong>", geom_WGS84$shares_n)
    
    leaflet(geom_WGS84) %>%
      addPolygons(
        stroke = TRUE, weight = 1, color = "black", # remove polygon borders
        fillColor = ~pal(shares_n),
        layerId = ~AMR,# set fill color with function from above and value
        fillOpacity = 0.4, smoothFactor = 0.5, # make it nicer
        popup = p_popup)  %>% onRender("
                     function(el, x) {
                         this.on('popupopen', function(e) {
                             Shiny.onInputChange('myEvent', 'open');
                         });

                         this.on('popupclose', function(e) {
                             Shiny.onInputChange('myEvent', 'close');
                         });
                     }")  %>%
      # addTiles( urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #           attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      addTiles() %>%
      addLegend("bottomright",  # location
                colors= qpal_colors,
                labels = qpal_labs)
    #pal=pal,    # palette function
    #values=~shares_n,  # value to be passed to palette function
    #title = "")# %>%
    # setView(long, lat, zoom = 5)
    
    
  }
  
 })
 
 

 
 ggplot_data <- reactive({
   region_click <- input$map_shape_click$id
   final_RTA[final_RTA$region == region_click & final_RTA$year == input$year, c("Field_en", "RTA")]
 })
 
 region_name <- reactive({
   name_click <- input$map_shape_click$id
   unique(geom$AMR_NAME[geom$AMR == name_click])
 })
 
 
 output$plot2 <- renderPlotly({
   
   data <- data()
   
   if(is.null(input$map_shape_click$id)) {}
   
    else if(colnames(data)[2] == variables[1]) {
   
   mydf <- transform(ggplot_data(), Field_en = reorder(Field_en, -RTA))
   
   fig2 <- plot_ly(y = mydf$RTA, x = mydf$Field_en, type = "bar", marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5)),
                   hovertemplate = paste('<b></b><extra></extra>',
                                         '<b>Technology:</b> %{x}<br>',
                                         '<b> RBA:</b> %{y}<br>'),showlegend = FALSE )
   
   fig2 <- fig2 %>% layout(yaxis=list(type='linear'), shapes = list(type = "line", fillcolor = "black",
                                                                    line = list(color = "black"),
                                                                    opacity = 1,
                                                                    x0 = -0.5, x1 =4.5, xref = 'paper', 
                                                                    y0 = 1, y1 = 1, yref = 'y'), xaxis= list(showticklabels = FALSE),
                           title = region_name())
   
   fig2
   
   } else {
     
     mydf <- transform(ggplot_data(), Field_en = reorder(Field_en, -RTA))
     
     col <- ifelse(mydf$Field_en == input$variable, 'red', 'rgb(158,202,225)')
     
     fig2 <- plot_ly(y = mydf$RTA, x = mydf$Field_en, type = "bar", marker = list(color = col, line = list(color = 'rgb(8,48,107)', width = 1.5)),
                    hovertemplate = paste('<b></b><extra></extra>',
                                           '<b>Technology:</b> %{x}<br>',
                                          '<b> RBA:</b> %{y}<br>'),showlegend = FALSE ) 
     
    
     fig2 <- fig2 %>% layout(yaxis=list(type='linear'), shapes = list(type = "line", fillcolor = "black",
                                                                      line = list(color = "black"),
                                                                      opacity = 1,
                                                                      x0 = -0.5, x1 =4.5, xref = 'paper', 
                                                                      y0 = 1, y1 = 1, yref = 'y'), xaxis= list(showticklabels = FALSE),
                             title = region_name())
     
     fig2
   }
   
 })
 
 
 
 
# observe({
   
 # leafletProxy("map", data())  %>%
#  clearShapes() %>%
 #  addPolygons(
 #   stroke = TRUE, weight = 1, color = "black", # remove polygon borders
 #    fillColor = ~pal(shares_n), # set fill color with function from above and value
 #     fillOpacity = 0.4, smoothFactor = 0.5, # make it nicer
 #     popup = p_popup)
 # })
   
   selectedData1 <- reactive({
     total_DE[total_DE$year == input$year1,]
   })
   
   
   # Fill in the spot we created for a plot
   output$plot <- renderPlotly({
     
     x <- list(
       title = "National Betweenness Centrality",
       range = c(0, 27000),
       zeroline = FALSE,
       tickformat = "digit"
     )
     y <- list(
       title = "Number of Regions with RBA > 1",
       range = c(10, 65),
       zeroline = FALSE,
       tickformat = "digit"
     )
     
    #Alternative using plot_ly it is nice but still work to do
    
     fig <- plot_ly(selectedData1()) 
    
    fig <- fig %>%
      add_trace(
        type = 'scatter',
        mode = 'markers',
        x = ~agg_BC,
        y = ~sum_RBA,
        text = selectedData1()$Field_en,
        hovertemplate = paste('<b>%{text}</b><extra></extra> <br>',
          '<i>Number of Regions with RBA > 1</i>: %{y}<br>',
                              '<i>National Betweenness Centrality</i>: %{x}<br>') 
        ,
        marker = list(size = 10,
                      color = 'rgba(255, 182, 193, .9)',
                      line = list(color = 'rgba(152, 0, 0, .8)',
                                  width = 2)),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = x,
        yaxis = y)
    
    fig
     
   })
   
   
   
   output$plot1 <- renderPlotly({
     data1 <- total_DE  %>%
       filter(Field_en == input$variable1) %>%
       select(year, sum_RBA, agg_BC) 
     
     data1$year <-as.character(data1$year)
     #v <- colnames(data1)[2]
     
     #colnames(data1)[2] <- "v"
     #data1$v <- as.character(data1$v)
     
     
     
     # fig1 <- plot_ly(y = data1$v, x = data1$year, 
     #                 type = "scatter", 
     #                 marker = list(color = 'rgb(158,202,225)', 
     #                               line = list(color = 'rgb(8,48,107)', width = 1.5)),
     #                 hovertemplate = paste('<b></b><extra></extra>',
     #                                       '<b>Year:</b> %{x}<br>','<b>',v,': </b> %{y}<br>'
     #                                       ),showlegend = FALSE )
     # 
     # fig1 <- fig1 %>% layout(yaxis=list(type='linear'))
     # 
     ay <- list(
       tickfont = list(color = "black"),
       overlaying = "y",
       side = "right",
       title = "Betweenness",
       tickformat = "digits"
     )
     m <- list(
       l = 70,
       r = 70,
       b = 0,
       t = 50,
       pad = 5
     )
     fig1 <- plot_ly(hovertemplate = paste('(%{x}, %{y})'))
     fig1 <- fig1 %>% add_lines(x = data1$year, y = data1$sum_RBA, name = "Number of regions with an RBA", line = list(color = 'rgb(205, 12, 24)', width = 4))
     fig1 <- fig1 %>% add_lines(x = data1$year, y = data1$agg_BC, name = "Betweenness centrality in German KS", yaxis = "y2", line = list(color = 'rgb(22, 96, 167)', width = 4))
     fig1 <- fig1 %>% layout(title = paste("Bridging of",input$variable1, "in German Regions"), yaxis = list(title="Number of regions with an RBA"),
                             yaxis2 = ay,
                             xaxis = list(title="Year"),legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.4), margin = m, hovermode = "x unified")

     
     
     fig1
   })


 
}

# Run app ----
shinyApp(ui, server)

