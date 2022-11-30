URL_CORONA_DATA <- 'https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv'
XT_CORONA_ALL <- fread( URL_CORONA_DATA )
XT0 <- CLEAN_TABLE(XT_CORONA_ALL)
XT0[,FAKE_DATE:=DATE_TO_NUM( DATE )]
XT1 <- XT0[CONFIRMED > 0]
###___WITH_PROVINCE_STATE___###
XT_PROVINCE_STATE <- XT1[PROVINCE_STATE!='',
.(CONFIRMED=sum( CONFIRMED ),
DEATHS=sum( DEATHS ),
RECOVERED=sum( RECOVERED )
),by=.(COUNTRY_REGION,FAKE_DATE)]
XT_PROVINCE_STATE <- XT_PROVINCE_STATE[o( COUNTRY_REGION,FAKE_DATE )]
XT2 <-  XT1[,.(COUNTRY_REGION,FAKE_DATE,CONFIRMED,RECOVERED,DEATHS)]
XV_COUNTRY_WITH_PROVINCE_STATE <- unique(XT_PROVINCE_STATE$COUNTRY_REGION)
XT3  <- XT2[! COUNTRY_REGION %in% XV_COUNTRY_WITH_PROVINCE_STATE ]
#XT[PROVINCE_STATE=='',PROVINCE_STATE:='ALL']
#XT <- rbind(XT,XT_PROVINCE_STATE)
XT <- rbind(XT3,XT_PROVINCE_STATE)
XT <- CHANGE_COLNAME(XT,'CONFIRMED','VALIDATED')
XT[COUNTRY_REGION=="US",COUNTRY_REGION:="United States"]
XT[COUNTRY_REGION=="Korea, South",COUNTRY_REGION:="Korea"]
XT[COUNTRY_REGION=="Dominican Republic",COUNTRY_REGION:="Dominican Rep."]
XT[COUNTRY_REGION=="Czechia",COUNTRY_REGION:="Czech Rep."]
XT[COUNTRY_REGION=="Taiwan*",COUNTRY_REGION:="Taiwan"]
XT[COUNTRY_REGION=="Bosnia and Herzegovina",COUNTRY_REGION:="Bosnia and Herz."]
XT[COUNTRY_REGION=="Congo (Kinshasa)",COUNTRY_REGION:="Congo"]
XT[COUNTRY_REGION=="North Macedonia",COUNTRY_REGION:="Macedonia "]
XT[COUNTRY_REGION=="Cabo Verde",COUNTRY_REGION:="Cape Verde"]
XT[COUNTRY_REGION=="West Bank and Gaza",COUNTRY_REGION:="Palestine"]
XT[COUNTRY_REGION=="Cote d'Ivoire",COUNTRY_REGION:="Côte d'Ivoire"]
XT[COUNTRY_REGION=="Burma",COUNTRY_REGION:="Myanmar"]
XT[COUNTRY_REGION=="Central African Republic",COUNTRY_REGION:="Central African Rep. "]
XT[COUNTRY_REGION=="Congo (Brazzaville)",COUNTRY_REGION:="Dem. Rep. Congo"]

XT[is.na(XT)]  <- 0

XL <- lapply(unique(XT$COUNTRY_REGION), function( X ) XT[COUNTRY_REGION==X ])
XL <- lapply(XL,function( X ) X[,DAYS:=X[,.I]] )
ZT <- do.call( rbind,XL )
MAX_FAKE_DATE <- max(ZT$FAKE_DATE)
ZT[,DATE:=NUM_TO_DATE( FAKE_DATE )]
XT_COR_GROUP <- as.data.table(fn$sqldf( 'SELECT
 COUNTRY_REGION,
 DEATHS,
 VALIDATED,
 RECOVERED
FROM 
 (SELECT
 *
 FROM
 ZT
 WHERE FAKE_DATE = `MAX_FAKE_DATE`)
GROUP BY COUNTRY_REGION' ))
setnames(XT_COR_GROUP,c('COUNTRY','DEATHS','VALIDATED','RECOVERED' ))
ZT <- as.data.table(gather(ZT,VAR,Amount,-DAYS,-FAKE_DATE,-DATE,-COUNTRY_REGION))
###___<MAP_TABLE>___###
world <- ne_countries(scale = "medium", returnclass = "sf")
XM_COR <- left_join( world,XT_COR_GROUP,by=c( 'name'='COUNTRY' ) )
XM_COR[ which(is.na(XM_COR$DEATHS)),c( 'DEATHS','VALIDATED','RECOVERED' )]  <- 0

XM_COR_DT <- as.data.table(XM_COR)
XT_REGIONS <- XM_COR_DT[,.( name,region_un,region_wb )]
XT_REGIONS_GROUP <- merge(XT_COR_GROUP,XT_REGIONS,by.x='COUNTRY',by.y='name')
XT_REGIONS <- merge(XT,XT_REGIONS,by.x='COUNTRY_REGION',by.y='name')



X_COUNTRY <- sort(unique(ZT$COUNTRY_REGION))
X_STAT_TYPES  <- sort(unique(ZT$VAR))

#REGIONS:
XV_REGIONS_WB <- unique(XM_COR_DT[['region_wb']])
XV_REGIONS_UN <-c("Americas","Asia","Africa","Europe","Oceania")
XV_REGIONS  <- c( 'World', sort(c(XV_REGIONS_WB,XV_REGIONS_UN ))) 

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

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
 
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

server <- function(input, output, session) {
###___X_ONLY_WYBRANY_KRAJ___###
X_ONLY_WYBRANY_KRAJ <- reactive({
ZT[ZT$COUNTRY_REGION == input$X_WYBRANY_KRAJ, ]
})

output$REGION_TABLE = renderDataTable({
Y_FILL <- F_FILL(  )
Y_REGION_TYPE <- F_REGION_TYPE(  )
if ( Y_REGION_TYPE == 'World' ) {
  XT_ORD <- XT_COR_GROUP [o(- XT_COR_GROUP [[Y_FILL]] )]
 } else {  
  XT_ORD <- XT_REGIONS_GROUP[(region_wb==Y_REGION_TYPE) | ( region_un==Y_REGION_TYPE )]
  XT_ORD <- XT_ORD [o(- XT_ORD[[Y_FILL]] )]
 }
DT::datatable(XT_ORD,
  options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
})


X_ONLY_WYBRANY_KRAJ_2 <- reactive({
Y_WYBRANY_KRAJ <- input$X_WYBRANY_KRAJ
Y_REGION_TYPE <- F_REGION_TYPE(  )
 if ( Y_REGION_TYPE == 'World' ) {
   XT_ONE_COUNTRY <- XT[ COUNTRY_REGION == Y_WYBRANY_KRAJ][o(-FAKE_DATE )]
   XT_ONE_COUNTRY$DATE <- NUM_TO_DATE( XT_ONE_COUNTRY$FAKE_DATE )
   ZT <- XT_ONE_COUNTRY[,.(COUNTRY_REGION,DATE,VALIDATED,RECOVERED,DEATHS)]
 } else {  
  XT_ONE_COUNTRY <- XT_REGIONS[ COUNTRY_REGION == Y_WYBRANY_KRAJ][o(-FAKE_DATE )]
  XT_ONE_COUNTRY$DATE <- NUM_TO_DATE( XT_ONE_COUNTRY$FAKE_DATE )
 ZT <- XT_ONE_COUNTRY[,.(COUNTRY_REGION,DATE,VALIDATED,RECOVERED,DEATHS)]
 }
ZT
})
#DATE:=NUM_TO_DATE(FAKE_DATE)

F_FILL <- reactive({
input$X_FILL_TYPE
})

F_REGION_TYPE <- reactive({
input$X_REGION_TYPE
})


output$REGION_MAP  <-  renderPlot({
Y_FILL <- F_FILL(  )
Y_REGION_TYPE <- F_REGION_TYPE(  )
if ( Y_REGION_TYPE == 'World' ) {
 ZM  <- COR_MAP_WORLD( XM_COR,Y_FILL )
 } else {
 if ( Y_REGION_TYPE  %in% XV_REGIONS_UN ) {
  XM_COR_REG  <- XM_COR[XM_COR$region_un == Y_REGION_TYPE,]
 } else {
  XM_COR_REG  <- XM_COR[XM_COR$region_wb == Y_REGION_TYPE,]
 } 
 ZM  <- COR_MAP_REG( XM_COR_REG,Y_FILL )
}
ZM
})
###___<old>___###
output$XMAP2  <-  renderPlot({
Y_FILL <- F_FILL(  )

MAX_LOG <- floor(log1p(max(XM_COR[[Y_FILL]])))
MIN_LOG <- floor(log1p(min(XM_COR[[Y_FILL]])))
XV_BREAKS <- ROUND(expm1(c( ( MIN_LOG+1 ):MAX_LOG )))
XV_BREAKS <- ROUND(XV_BREAKS[seq_along(XV_BREAKS) %% 2 ==0],0)
XV_BREAKS <- c( ROUND(expm1(MIN_LOG),0),XV_BREAKS )
LEG_TIT <- 'Number of ' + tolower(Y_FILL) + '\ncases'
###___</LEGEND_AND_BREAK>___###
XT_POINTS_0 <- sf::st_point_on_surface(XM_COR)
XT_POINTS_MAX <- XT_POINTS_0[which.max(XT_POINTS_0[[Y_FILL]]),]
XT_POINTS_CHN <- XT_POINTS_0[XT_POINTS_0[['su_a3']]=='CHN',]
XT_POINTS  <-  rbind( XT_POINTS_CHN,XT_POINTS_MAX )
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- XT_POINTS[['su_a3']] +': ' + XT_POINTS[[Y_FILL]]
POINTS_TEXT_LEGEND  <-  'Displaying value for country with biggest amomunt and for China.'
ZM <- ggplot() + geom_sf(data = XM_COR,aes_string(fill=Y_FILL)) + 
 scale_fill_gradientn(trans='log1p',
                       colours=rev(plasma(20)),
                       breaks = XV_BREAKS,
                       labels = XV_BREAKS
 ) +
 labs(fill=LEG_TIT) +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 labs(caption = POINTS_TEXT_LEGEND) +
 theme(plot.caption = element_text(color = 'black', face="bold"))
ZM

})



###___output$X_TREND___###
output$X_TREND  <-  renderPlot({
serial <- X_ONLY_WYBRANY_KRAJ()
pl <- ggplot(serial, aes(DAYS,y=Amount, colour=VAR)) + geom_line(size=2) +
geom_point() + xlab("Number of days since 1 case")
if (input$X_Linia_Trendu) {
pl <- pl + geom_smooth(se=FALSE, method="lm", linetype=2)
}
pl
})

output$OT_COUNTRY = renderDataTable({
XT_ONE_COUNTRY <- X_ONLY_WYBRANY_KRAJ_2()
DT::datatable(XT_ONE_COUNTRY, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
})

###___output$X_TREND___###
output$X_TREND  <-  renderPlot({
serial <- X_ONLY_WYBRANY_KRAJ()
pl <- ggplot(serial, aes(DAYS,y=Amount, colour=VAR)) + geom_line(size=2) +
geom_point() + xlab("Number of days since 1 case")
if (input$X_Linia_Trendu) {
pl <- pl + geom_smooth(se=FALSE, method="lm", linetype=2)
}
pl
})





output$XMAP  <-  renderPlot({
CREATE_MAP(XM_COR,'DEATHS')
})


output$model_v = renderPrint({
XT_COUNTRY <- X_ONLY_WYBRANY_KRAJ()
XT_COUNTRY <- XT_COUNTRY[VAR=='VALIDATED']
summary(lm(Amount~DAYS, XT_COUNTRY))
})

output$model_d = renderPrint({
XT_COUNTRY <- X_ONLY_WYBRANY_KRAJ()
XT_COUNTRY <- XT_COUNTRY[VAR=='DEATHS']
summary(lm(Amount~DAYS, XT_COUNTRY))
})

output$model_r = renderPrint({
XT_COUNTRY <- X_ONLY_WYBRANY_KRAJ()
XT_COUNTRY <- XT_COUNTRY[VAR=='RECOVERED']
summary(lm(Amount~DAYS, XT_COUNTRY))
})

}


