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