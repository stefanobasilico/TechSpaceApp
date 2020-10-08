# Server logic ----
shinyServer(function(input, output, session) {
  
  
  
  data <- reactive({
    dat <- RBA_tech_adv1 %>%
      filter(year == as.numeric(input$year)) %>%
      select(region_number, input$variable)
    colnames(dat)[2] <- "shares_n"
    dat$region_number <- as.numeric(as.character(dat$region_number))
    
    missing <- setdiff(max(dat$region_number):min(dat$region_number),dat$region_number)
    
    df2 <- rbind(dat,data.frame(region_number=missing, shares_n=0))
    
    df2 <- df2[order(df2$region_number),]
    df2 <- merge(geom, df2,  by.x = "AMR",by.y = "region_number")
    geom_WGS84 <- st_transform(df2, 4326)
    geom_WGS84
  })  
  
  pal <- reactive({
    colorQuantile("YlOrRd", unique(data()$shares_n), n = 7)
  })
  
  p_popup <- reactive({
    paste0("<strong>", data()$AMR_NAME, ": </strong>", data()$shares_n)
  })
  
  qpal_colors <- reactive({
    dat <- data()
    pal <- pal()
    qpal_colors <- unique(pal(sort(unique(dat$shares_n))))
    qpal_colors
  })
  
  qpal_labs <- reactive({ 
    qpal_labs <-  quantile(unique(data()$shares_n), seq(0, 1, .1428571)) # depends on n from pal
    if(input$variable == variables[1]) {
      
      
      qpal_labs <- round(qpal_labs, digits = 0)
      paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1]
    } else {
      qpal_labs <- round(qpal_labs, digits = 2)
      paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1]
    }
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
      ) %>%
      fitBounds(lat1 =  47.27, lat2 =  55.05, lng1 =  5.87, lng2 =  15.03) %>% 
      
      onRender("
                     function(el, x) {
                         this.on('popupopen', function(e) {
                             Shiny.onInputChange('myEvent', 'open');
                         });

                         this.on('popupclose', function(e) {
                             Shiny.onInputChange('myEvent', 'close');
                         });
                     }")
  })
  
  observe({
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      addPolygons(
        stroke = TRUE, weight = 1, color = "black", # remove polygon borders
        fillColor = ~pal()(shares_n), # set fill color with function from above and value
        fillOpacity = 0.4, smoothFactor = 0.5, # make it nicer
        layerId = ~AMR,
        popup = p_popup(),
        highlight = highlightOptions(
          weight = 3,
          color = "black",
          opacity = 0.7,
          bringToFront = TRUE,
          sendToBack = TRUE, fillColor = ~pal()(shares_n))
      ) 
    
    
  })
  
  
  observeEvent(input$map_shape_click,{
    # if(input$variable!=""){
    proxy <- leafletProxy("map")
    
    #get the selected polygon and extract the label point 
    selected_polygon <- subset(data(),data()$AMR==input$map_shape_click$id)
    
    #remove any previously highlighted polygon
    proxy %>% removeShape("selected")
    
    #center the view on the polygon 
    #proxy %>% setView(lng=polygon_labelPt[1],lat=polygon_labelPt[2],zoom=7)
    
    #add a slightly thicker red polygon on top of the selected one
    proxy %>% addPolygons(stroke=TRUE, weight = 3,color="black", fillOpacity = 0.4,data=selected_polygon, layerId = "selected")
    #  }
  })
  
  
  observe({
    proxy <- leafletProxy("map", data = data())
    proxy %>% clearControls()
    pal <- qpal_colors()
    qpal_labs <- qpal_labs()
    proxy %>% addLegend(position = "bottomright",
                        colors = pal, labels = qpal_labs
    )
  })
  
  
  
  
  output$condPanel <- renderUI({
    
    conditionalPanel("input.myEvent == 'open'", 
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = TRUE, top = 100, left = "auto", right = 45, bottom = "auto",
                                   width = "28vw", height = "35vh", style = "background-color: white;
           opacity: 0.85;
           padding: 20px 20px 20px 20px;
           margin: auto;
          border-radius: 5pt;
           box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
           padding-bottom: 2mm;
            padding-top: 1mm;",          
                                   plotlyOutput("plot2", height = 300), p("Pass over the bars to display the results", style = "color:black;font-size:10pt;"))
    )
    
  })
  
  
  
  observeEvent(input$map_shape_click, { 
    p <- input$map_shape_click
    
    output$plot2 <- renderPlotly({
      
      data <- data()
      
      if(colnames(data)[2] == variables[1]) {
        
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
    print(p)
  })
  
  
  
  ggplot_data <- reactive({
    region_click <- input$map_shape_click$id
    dat <- RBA_tech_adv1[RBA_tech_adv1$region_number == region_click & RBA_tech_adv1$year == input$year,]
    
    dat[,c("n_RTA")] <- NULL
    
    melt(setDT(dat), id.vars = c("region_number", "year", "region_name"), variable.name = "Field_en", value.name = "RTA")
  })
  
  region_name <- reactive({
    name_click <- input$map_shape_click$id
    unique(geom$AMR_NAME[geom$AMR == name_click])
  })
  
  
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
  
  selectedData2 <- reactive({
    total_DE[total_DE$Field_en == input$Tech1 | total_DE$Field_en == input$Tech2,]
  })
  
  output$plot3 <- renderPlotly({
    data2 <- selectedData2()
    
    data2$year <-as.character(data2$year)
    
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
    fig2 <- plot_ly(hovertemplate = paste('(%{x}, %{y})'))
    fig2 <- fig2 %>% add_lines(x = data2$year[data2$Field_en == input$Tech1], y = data2$agg_BC[data2$Field_en == input$Tech1], name = paste(input$Tech1), line = list(color = 'rgb(194, 129, 0)', width = 4))
    fig2 <- fig2 %>% add_lines(x = data2$year[data2$Field_en == input$Tech2], y = data2$agg_BC[data2$Field_en == input$Tech2], name = paste(input$Tech2), #yaxis = "y2", 
                               line = list(color = 'rgb(109, 2, 171)', width = 4))
    fig2 <- fig2 %>% layout(title = paste("Bridging of",input$Tech1, "vs", input$Tech2, "in German Regions"), #yaxis = list(title="Number of regions with an RBA"),
                            # yaxis2 = ay,
                            xaxis = list(title="Year"),
                            legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.4), margin = m, hovermode = "x unified")
    
    
    
    fig2
    
    
  })
  
  
  
})