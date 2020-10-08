shinyUI(fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 13px; line-height: 13px; }"),
  dashboardPage(
    header <-    dashboardHeader(title = "TechSpace", titleWidth = 250),
    sidebar <-    dashboardSidebar(width = 250,
                                   sidebarMenu(
                                     convertMenuItem(menuItem("Regional RBA", tabName = "Regional_RBA", icon = icon("map"), startExpanded = TRUE, sliderInput("year", 
                                                                                                                                                              label = "Year of interest:", value = 1994,
                                                                                                                                                              min = 1994, max = 2015, sep = ""),
                                                              
                                                              selectInput("variable", label = "Variable:", choices = 
                                                                            list("Number of RBA > 1" = variables[1],
                                                                                 "RBA on a single technology" = variables[2:36]))), tabName = "Regional_RBA"),
                                     convertMenuItem(menuItem("Technological RBA", icon = icon("rocket"), tabName = "Technological_RBA", menuSubItem("German Tech Landscape", tabName = "TL"), sliderInput(inputId = "year1",label = "Year of interest:", 
                                                                                                                                                                                                           min = 1995, max = 2015, value = 1995, sep = ""), menuSubItem("Single Tech Evolution", tabName = "TE"), selectInput(inputId = "variable1",label = "Technology:", 
                                                                                                                                                                                                                                                                                                                              choices = unique(total_DE$Field_en)), 
                                                              menuSubItem("Technological Comparisons", tabName = "TC"), selectInput(inputId = "Tech1", label = "First Technology:", choices = unique(total_DE$Field_en), selected = "Optics"), selectInput(inputId = "Tech2", label = "Second Technology:", choices = unique(total_DE$Field_en), selected = "Organic fine chemistry")
                                     ), tabName = "Technological_RBA")
                                   )
    )
    ,
    body <-    dashboardBody(
      tabItems(
        
        tabItem(tabName = "Regional_RBA",
                h2(
                  
                  fluidRow(
                    box(leafletOutput("map", height = 745), width = 12, status = "primary"), p("Click on a region to display additional statistics", style = "color:black;font-size:10pt;"),useShinyjs(), uiOutput("condPanel")
                    
                    
                  ))),
        tabItem(tabName = "Technological_RBA",
                h2(
                  fluidRow(column(6,box(plotlyOutput("plot", height = 350), width = 12, status = "primary")),column(6,
                                                                                                                    box(plotlyOutput("plot1", height = 350), width = 12, status = "primary")), 
                           box(plotlyOutput("plot3", height = 350), width = 12, status = "primary"))
                ))
        
      )
      
      
    )
  )
)
)