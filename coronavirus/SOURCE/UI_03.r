ui <- fluidPage(titlePanel("Coronavirus Statistics"),
 navbarPage('',
  tabPanel('World statistics',
   sidebarLayout(sidebarPanel(
    selectInput("X_FILL_TYPE",
     label = "Choose type of data to display on map",
     choices = X_STAT_TYPES,
     selected = "DEATHS"
    ),
##########
    selectInput("X_REGION_TYPE",
     label = "Choose map of region",
     choices = XV_REGIONS,
     selected = "World"
    ),
##########
    
  width = 3), 
    mainPanel(
      plotOutput("REGION_MAP"),br(),
      dataTableOutput("REGION_TABLE")
      )
     )),
  ##############################
  tabPanel('Statistics by country',
   sidebarLayout(sidebarPanel(
    selectInput("X_WYBRANY_KRAJ",
     label = "Select country/region",
     choices = X_COUNTRY,
     selected = "Poland"
    ),
    checkboxInput("X_Linia_Trendu",
     "Do You want to show the line trends?",
     value = FALSE
    ) 
    ), 
    mainPanel(tabsetPanel(
     tabPanel("Chart",
      p("Coronavirus statistic for selected region"),
      plotOutput("X_TREND"),br(),
      dataTableOutput("OT_COUNTRY")
      #br(),p("X_P_TEXT_01"),br(),
     ),
     tabPanel("Deaths Model",
     p("The result of the linear match for the Death variable"),
     verbatimTextOutput("model_d")
     ),
     tabPanel("Recovered",
     p("The result of the linear match for the Recovered variable"),
     verbatimTextOutput("model_r")
     ),
     tabPanel("Validated",
     p("The result of the linear match for the Validated variable"),
     verbatimTextOutput("model_v")
     )
    ))
   ))
 ))