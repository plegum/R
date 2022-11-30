################################################################################
UI <- fluidPage(titlePanel("Property statistics for Poland"),
navbarPage('',
################################################################################
################################################################################
###########################___<HOUSE>___########################################
tabPanel("HOUSE",icon=icon("home"),
 tabsetPanel(
  tabPanel("STATE",icon=icon("cube"),
   sidebarLayout(
    ################################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_HOUSE_STATE_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_HOUSE,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_HOUSE_STATE_FILL != 'volume'"),
      selectInput("X_HOUSE_STATE_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),
     ################################################################
     conditionalPanel(condition = ("input.X_HOUSE_STATE_STAT == 'quantile' & input.X_HOUSE_STATE_FILL != 'volume'"),
      sliderInput("X_HOUSE_STATE_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_HOUSE_STATE_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_HOUSE_STATE_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_HOUSE_STATE_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_HOUSE_STATE_TEXT",
      "Show value on the map?",
      value = FALSE
     )
    ################################################################
    ),   ################################################################################
     mainPanel(
       htmlOutput( "MAIN_TEXT_HOUSE_STATE" ),
       plotOutput("HOUSE_STATE_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_HOUSE_STATE" ),
       dataTableOutput("HOUSE_STATE_TAB")
       )
     )
  ),
  
################################################################################
########################___<HOUSE_COUNTY>___####################################
  tabPanel("COUNTY",icon=icon("cubes"),
     sidebarLayout(
    ############################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_HOUSE_COUNTY_REGION_TYPE",
      label = "Choose region",
      choices = XV_REGION_TYPE_FOR_COUNTY ,
      selected = "Poland"),
     ################################################################    
     selectInput("X_HOUSE_COUNTY_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_HOUSE,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_HOUSE_COUNTY_FILL != 'volume'"),
      selectInput("X_HOUSE_COUNTY_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),     
     ################################################################
     conditionalPanel(condition = ("input.X_HOUSE_COUNTY_STAT == 'quantile' & input.X_HOUSE_COUNTY_FILL != 'volume'"),
      sliderInput("X_HOUSE_COUNTY_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_HOUSE_COUNTY_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_HOUSE_COUNTY_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_HOUSE_COUNTY_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_HOUSE_COUNTY_TEXT",
      "Show value on the map?",
      value = FALSE
     ),
    ################################################################
     conditionalPanel(condition = ("input.X_HOUSE_COUNTY_TEXT"),
      sliderInput("Y_HOUSE_COUNTY_TEXT_NUM", "Number of top values to be shown on map:",
      min = 1, max = 10, value = 5))
    ################################################################
    ),
    ################################################################################ 
     mainPanel(
       htmlOutput( "MAIN_TEXT_HOUSE_COUNTY" ),
       plotOutput("HOUSE_COUNTY_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_HOUSE_COUNTY" ),
       dataTableOutput("HOUSE_COUNTY_TAB")
       )
     )
  
  )
)),
###########################___</HOUSE>___#######################################
################################################################################
################################################################################



################################################################################
################################################################################
###########################___<APARTMENT>___########################################
tabPanel("APARTMENT",icon=icon("building"),
 tabsetPanel(
  tabPanel("STATE",icon=icon("cube"),
   sidebarLayout(
    ################################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_APARTMENT_STATE_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_APARTMENT,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_APARTMENT_STATE_FILL != 'volume'"),
      selectInput("X_APARTMENT_STATE_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),
     ################################################################
     conditionalPanel(condition = ("input.X_APARTMENT_STATE_STAT == 'quantile' & input.X_APARTMENT_STATE_FILL != 'volume'"),
      sliderInput("X_APARTMENT_STATE_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_APARTMENT_STATE_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_APARTMENT_STATE_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_APARTMENT_STATE_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_APARTMENT_STATE_TEXT",
      "Show value on the map?",
      value = FALSE
     )
    ################################################################
    ),   ################################################################################
     mainPanel(
       htmlOutput( "MAIN_TEXT_APARTMENT_STATE" ),
       plotOutput("APARTMENT_STATE_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_APARTMENT_STATE" ),
       dataTableOutput("APARTMENT_STATE_TAB")
       )
     )
  ),
  
################################################################################
########################___<APARTMENT_COUNTY>___####################################
  tabPanel("COUNTY",icon=icon("cubes"),
     sidebarLayout(
    ############################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_APARTMENT_COUNTY_REGION_TYPE",
      label = "Choose region",
      choices = XV_REGION_TYPE_FOR_COUNTY ,
      selected = "Poland"),
     ################################################################    
     selectInput("X_APARTMENT_COUNTY_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_APARTMENT,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_APARTMENT_COUNTY_FILL != 'volume'"),
      selectInput("X_APARTMENT_COUNTY_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),     
     ################################################################
     conditionalPanel(condition = ("input.X_APARTMENT_COUNTY_STAT == 'quantile' & input.X_APARTMENT_COUNTY_FILL != 'volume'"),
      sliderInput("X_APARTMENT_COUNTY_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_APARTMENT_COUNTY_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_APARTMENT_COUNTY_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_APARTMENT_COUNTY_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_APARTMENT_COUNTY_TEXT",
      "Show value on the map?",
      value = FALSE
     ),
    ################################################################
     conditionalPanel(condition = ("input.X_APARTMENT_COUNTY_TEXT"),
      sliderInput("Y_APARTMENT_COUNTY_TEXT_NUM", "Number of top values to be shown on map:",
      min = 1, max = 10, value = 5))
    ################################################################
    ),
    ################################################################################ 
     mainPanel(
       htmlOutput( "MAIN_TEXT_APARTMENT_COUNTY" ),
       plotOutput("APARTMENT_COUNTY_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_APARTMENT_COUNTY" ),
       dataTableOutput("APARTMENT_COUNTY_TAB")
       )
     )
  
  )
)),
###########################___</APARTMENT>___#######################################
################################################################################
################################################################################




################################################################################
################################################################################
###########################___<LAND>___########################################
tabPanel("LAND",icon=icon("tree"),
 tabsetPanel(
  tabPanel("STATE",icon=icon("cube"),
   sidebarLayout(
    ################################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_LAND_STATE_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_LAND,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_LAND_STATE_FILL != 'volume'"),
      selectInput("X_LAND_STATE_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),
     ################################################################
     conditionalPanel(condition = ("input.X_LAND_STATE_STAT == 'quantile' & input.X_LAND_STATE_FILL != 'volume'"),
      sliderInput("X_LAND_STATE_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_LAND_STATE_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_LAND_STATE_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_LAND_STATE_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_LAND_STATE_TEXT",
      "Show value on the map?",
      value = FALSE
     )
    ################################################################
    ),   ################################################################################
     mainPanel(
       htmlOutput( "MAIN_TEXT_LAND_STATE" ),
       plotOutput("LAND_STATE_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_LAND_STATE" ),
       dataTableOutput("LAND_STATE_TAB")
       )
     )
  ),
  
################################################################################
########################___<LAND_COUNTY>___####################################
  tabPanel("COUNTY",icon=icon("cubes"),
     sidebarLayout(
    ############################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_LAND_COUNTY_REGION_TYPE",
      label = "Choose region",
      choices = XV_REGION_TYPE_FOR_COUNTY ,
      selected = "Poland"),
     ################################################################    
     selectInput("X_LAND_COUNTY_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_LAND,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_LAND_COUNTY_FILL != 'volume'"),
      selectInput("X_LAND_COUNTY_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),     
     ################################################################
     conditionalPanel(condition = ("input.X_LAND_COUNTY_STAT == 'quantile' & input.X_LAND_COUNTY_FILL != 'volume'"),
      sliderInput("X_LAND_COUNTY_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_LAND_COUNTY_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_LAND_COUNTY_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_LAND_COUNTY_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_LAND_COUNTY_TEXT",
      "Show value on the map?",
      value = FALSE
     ),
    ################################################################
     conditionalPanel(condition = ("input.X_LAND_COUNTY_TEXT"),
      sliderInput("Y_LAND_COUNTY_TEXT_NUM", "Number of top values to be shown on map:",
      min = 1, max = 10, value = 5))
    ################################################################
    ),
    ################################################################################ 
     mainPanel(
       htmlOutput( "MAIN_TEXT_LAND_COUNTY" ),
       plotOutput("LAND_COUNTY_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_LAND_COUNTY" ),
       dataTableOutput("LAND_COUNTY_TAB")
       )
     )
  
  )
)),
###########################___</LAND>___#######################################
################################################################################
################################################################################




################################################################################
################################################################################
###########################___<COMMERCIAL>___########################################
tabPanel("COMMERCIAL",icon=icon("city"),
 tabsetPanel(
  tabPanel("STATE",icon=icon("cube"),
   sidebarLayout(
    ################################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_COMMERCIAL_STATE_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_COMMERCIAL,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_COMMERCIAL_STATE_FILL != 'volume'"),
      selectInput("X_COMMERCIAL_STATE_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),
     ################################################################
     conditionalPanel(condition = ("input.X_COMMERCIAL_STATE_STAT == 'quantile' & input.X_COMMERCIAL_STATE_FILL != 'volume'"),
      sliderInput("X_COMMERCIAL_STATE_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_COMMERCIAL_STATE_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_COMMERCIAL_STATE_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_COMMERCIAL_STATE_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_COMMERCIAL_STATE_TEXT",
      "Show value on the map?",
      value = FALSE
     )
    ################################################################
    ),   ################################################################################
     mainPanel(
       htmlOutput( "MAIN_TEXT_COMMERCIAL_STATE" ),
       plotOutput("COMMERCIAL_STATE_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_COMMERCIAL_STATE" ),
       dataTableOutput("COMMERCIAL_STATE_TAB")
       )
     )
  ),
  
################################################################################
########################___<COMMERCIAL_COUNTY>___####################################
  tabPanel("COUNTY",icon=icon("cubes"),
     sidebarLayout(
    ############################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_COMMERCIAL_COUNTY_REGION_TYPE",
      label = "Choose region",
      choices = XV_REGION_TYPE_FOR_COUNTY ,
      selected = "Poland"),
     ################################################################    
     selectInput("X_COMMERCIAL_COUNTY_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_COMMERCIAL,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_COMMERCIAL_COUNTY_FILL != 'volume'"),
      selectInput("X_COMMERCIAL_COUNTY_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),     
     ################################################################
     conditionalPanel(condition = ("input.X_COMMERCIAL_COUNTY_STAT == 'quantile' & input.X_COMMERCIAL_COUNTY_FILL != 'volume'"),
      sliderInput("X_COMMERCIAL_COUNTY_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_COMMERCIAL_COUNTY_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_COMMERCIAL_COUNTY_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_COMMERCIAL_COUNTY_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_COMMERCIAL_COUNTY_TEXT",
      "Show value on the map?",
      value = FALSE
     ),
    ################################################################
     conditionalPanel(condition = ("input.X_COMMERCIAL_COUNTY_TEXT"),
      sliderInput("Y_COMMERCIAL_COUNTY_TEXT_NUM", "Number of top values to be shown on map:",
      min = 1, max = 10, value = 5))
    ################################################################
    ),
    ################################################################################ 
     mainPanel(
       htmlOutput( "MAIN_TEXT_COMMERCIAL_COUNTY" ),
       plotOutput("COMMERCIAL_COUNTY_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_COMMERCIAL_COUNTY" ),
       dataTableOutput("COMMERCIAL_COUNTY_TAB")
       )
     )
  
  )
)),
###########################___</COMMERCIAL>___#######################################
################################################################################
################################################################################







################################################################################
################################################################################
###########################___<WAREHOUSE>___########################################
tabPanel("WAREHOUSE",icon=icon("warehouse"),
 tabsetPanel(
  tabPanel("STATE",icon=icon("cube"),
   sidebarLayout(
    ################################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_WAREHOUSE_STATE_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_WAREHOUSE,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_WAREHOUSE_STATE_FILL != 'volume'"),
      selectInput("X_WAREHOUSE_STATE_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),
     ################################################################
     conditionalPanel(condition = ("input.X_WAREHOUSE_STATE_STAT == 'quantile' & input.X_WAREHOUSE_STATE_FILL != 'volume'"),
      sliderInput("X_WAREHOUSE_STATE_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_WAREHOUSE_STATE_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_WAREHOUSE_STATE_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_WAREHOUSE_STATE_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_WAREHOUSE_STATE_TEXT",
      "Show value on the map?",
      value = FALSE
     )
    ################################################################
    ),   ################################################################################
     mainPanel(
       htmlOutput( "MAIN_TEXT_WAREHOUSE_STATE" ),
       plotOutput("WAREHOUSE_STATE_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_WAREHOUSE_STATE" ),
       dataTableOutput("WAREHOUSE_STATE_TAB")
       )
     )
  ),
  
################################################################################
########################___<WAREHOUSE_COUNTY>___####################################
  tabPanel("COUNTY",icon=icon("cubes"),
     sidebarLayout(
    ############################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_WAREHOUSE_COUNTY_REGION_TYPE",
      label = "Choose region",
      choices = XV_REGION_TYPE_FOR_COUNTY ,
      selected = "Poland"),
     ################################################################    
     selectInput("X_WAREHOUSE_COUNTY_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_WAREHOUSE,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_WAREHOUSE_COUNTY_FILL != 'volume'"),
      selectInput("X_WAREHOUSE_COUNTY_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),     
     ################################################################
     conditionalPanel(condition = ("input.X_WAREHOUSE_COUNTY_STAT == 'quantile' & input.X_WAREHOUSE_COUNTY_FILL != 'volume'"),
      sliderInput("X_WAREHOUSE_COUNTY_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_WAREHOUSE_COUNTY_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_WAREHOUSE_COUNTY_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_WAREHOUSE_COUNTY_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_WAREHOUSE_COUNTY_TEXT",
      "Show value on the map?",
      value = FALSE
     ),
    ################################################################
     conditionalPanel(condition = ("input.X_WAREHOUSE_COUNTY_TEXT"),
      sliderInput("Y_WAREHOUSE_COUNTY_TEXT_NUM", "Number of top values to be shown on map:",
      min = 1, max = 10, value = 5))
    ################################################################
    ),
    ################################################################################ 
     mainPanel(
       htmlOutput( "MAIN_TEXT_WAREHOUSE_COUNTY" ),
       plotOutput("WAREHOUSE_COUNTY_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_WAREHOUSE_COUNTY" ),
       dataTableOutput("WAREHOUSE_COUNTY_TAB")
       )
     )
  
  )
)),
###########################___</WAREHOUSE>___#######################################
################################################################################
################################################################################





################################################################################
################################################################################
###########################___<GARAGE>___########################################
tabPanel("GARAGE",icon=icon("parking"),
 tabsetPanel(
  tabPanel("STATE",icon=icon("cube"),
   sidebarLayout(
    ################################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_GARAGE_STATE_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_GARAGE,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_GARAGE_STATE_FILL != 'volume'"),
      selectInput("X_GARAGE_STATE_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),
     ################################################################
     conditionalPanel(condition = ("input.X_GARAGE_STATE_STAT == 'quantile' & input.X_GARAGE_STATE_FILL != 'volume'"),
      sliderInput("X_GARAGE_STATE_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_GARAGE_STATE_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_GARAGE_STATE_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_GARAGE_STATE_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_GARAGE_STATE_TEXT",
      "Show value on the map?",
      value = FALSE
     )
    ################################################################
    ),   ################################################################################
     mainPanel(
       htmlOutput( "MAIN_TEXT_GARAGE_STATE" ),
       plotOutput("GARAGE_STATE_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_GARAGE_STATE" ),
       dataTableOutput("GARAGE_STATE_TAB")
       )
     )
  ),
  
################################################################################
########################___<GARAGE_COUNTY>___####################################
  tabPanel("COUNTY",icon=icon("cubes"),
     sidebarLayout(
    ############################################################################
    sidebarPanel(width = 3,
     ################################################################    
     selectInput("X_GARAGE_COUNTY_REGION_TYPE",
      label = "Choose region",
      choices = XV_REGION_TYPE_FOR_COUNTY ,
      selected = "Poland"),
     ################################################################    
     selectInput("X_GARAGE_COUNTY_FILL",
      label = "Choose type of data to display on map",
      choices = XV_FILL_GARAGE,
      selected = "price per area"),
     ################################################################    
     conditionalPanel(condition = ("input.X_GARAGE_COUNTY_FILL != 'volume'"),
      selectInput("X_GARAGE_COUNTY_STAT",
      label = "Choose type of statistic\n to display on map",
      choices = XV_STAT,
      selected = "quantile")),     
     ################################################################
     conditionalPanel(condition = ("input.X_GARAGE_COUNTY_STAT == 'quantile' & input.X_GARAGE_COUNTY_FILL != 'volume'"),
      sliderInput("X_GARAGE_COUNTY_QUA", "P value for quantile ( 0.5 = median )",
      min = 0, max = 1, value = 0.5)),
     ################################################################
     selectInput("X_GARAGE_COUNTY_COLOR",
      label = "Choose color to be used on map",
      choices = XV_COLORS,
      selected = "viridis"),
     ################################################################
     checkboxInput("X_GARAGE_COUNTY_COL",
      "Reverse colors?",
      value = FALSE
     ),
     ################################################################
     checkboxInput("X_GARAGE_COUNTY_LOG",
      "Logarithm transform?",
      value = FALSE
     ),
    ################################################################
    checkboxInput("X_GARAGE_COUNTY_TEXT",
      "Show value on the map?",
      value = FALSE
     ),
    ################################################################
     conditionalPanel(condition = ("input.X_GARAGE_COUNTY_TEXT"),
      sliderInput("Y_GARAGE_COUNTY_TEXT_NUM", "Number of top values to be shown on map:",
      min = 1, max = 10, value = 5))
    ################################################################
    ),
    ################################################################################ 
     mainPanel(
       htmlOutput( "MAIN_TEXT_GARAGE_COUNTY" ),
       plotOutput("GARAGE_COUNTY_MAP",height = "800px"),br(  ),
       htmlOutput( "TABLE_TEXT_GARAGE_COUNTY" ),
       dataTableOutput("GARAGE_COUNTY_TAB")
       )
     )
  
  )
))
###########################___</GARAGE>___#######################################
################################################################################
################################################################################





#navbarMenu("More",tabPanel("Summary"),tabPanel("Table")),
),
p(em("Developed by"),br("Przemyslaw Legumina"),style="text-align:center; font-family: times"),
p("Data regarding the presented property statistics are from April 2020.",style="text-align:center; font-family: times")
)
