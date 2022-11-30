SERVER <- function(input, output, session) {
################################___<HOUSE_STATE>___###########################################___<HOUSE_STATE_MAP>___#####___<renderPlot>___#######################
output$HOUSE_STATE_MAP  <-  renderPlot({
	XS_PROPERTY_TYPE = 'HOUSE'
	YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
	XS_REGION = 'STATE'
	Y_QUA <- ''
	################################################################
	if (input$X_HOUSE_STATE_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_HOUSE_STATE_COLOR }
	if (input$X_HOUSE_STATE_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
	if (input$X_HOUSE_STATE_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }
	Y_FILL <- gsub(' ','_',toupper(input$X_HOUSE_STATE_FILL))
	if ( input$X_HOUSE_STATE_STAT == 'mode' ) { Y_FUN <- MODE}
	if ( input$X_HOUSE_STATE_STAT == 'interquartile range'  & input$X_HOUSE_STATE_FILL != 'volume') { Y_FUN <- IQR}
	if ( input$X_HOUSE_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_HOUSE_STATE_STAT))}
	if ( input$X_HOUSE_STATE_STAT == 'quantile'  & input$X_HOUSE_STATE_FILL != 'volume') { 
	 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_HOUSE_STATE_QUA))
	 Y_QUA <- input$X_HOUSE_STATE_QUA * 100
	} else {
	 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
	}

	if ( input$X_HOUSE_STATE_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

	colnames(XT_GR) <-c(XS_REGION,'VAL')
	 
	if ( input$X_HOUSE_STATE_COL ) {
	 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
	} else {
	 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
	}
	XTM <- left_join(SHP_WOJ, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
	if ( input$X_HOUSE_STATE_LOG ) {
		XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
	} else {
	 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
	}
	if ( input$X_HOUSE_STATE_FILL == 'volume' ) {
	LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
	} else {
	LEG_TIT <- toTitleCase(input$X_HOUSE_STATE_STAT) + Y_QUA + ' of ' + tolower(input$X_HOUSE_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
	}
	ZM <- XM + labs(fill=LEG_TIT)
	if ( ( input$X_HOUSE_STATE_TEXT ) ) {
	XT_POINTS <- sf::st_point_on_surface(XTM)
	XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
	XT_COORDS$POINTS_TEXT <- NUM2K_XV(XT_POINTS[['VAL']])
	POINTS_TEXT_LEGEND  <-  'Displaying value ' + ' ' + Y_FILL
	ZM <- ZM +
	 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
	 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
ZM
})
##########___<HOUSE_STATE_TAB>___#####___<renderDataTable>___#################
output$HOUSE_STATE_TAB = renderDataTable({
ZT
})
##########___</HOUSE_STATE_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_HOUSE>_<renderUI>___################################
output$TABLE_TEXT_HOUSE_STATE <- renderUI({
XS_PROPERTY_TYPE = 'HOUSE'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_HOUSE_STATE_STAT == 'quantile'  & input$X_HOUSE_STATE_FILL != 'volume') { Y_QUA <- input$X_HOUSE_STATE_QUA * 100 }

if ( input$X_HOUSE_STATE_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_HOUSE_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_HOUSE_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_HOUSE>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_HOUSE>_<renderUI>___################################
output$MAIN_TEXT_HOUSE_STATE <- renderUI({
	XS_PROPERTY_TYPE = 'HOUSE'
	Y_QUA = ''
	XS_REGION = 'STATE'
	if ( input$X_HOUSE_STATE_STAT == 'quantile'  & input$X_HOUSE_STATE_FILL != 'volume') { Y_QUA <- input$X_HOUSE_STATE_QUA * 100 }
	if ( input$X_HOUSE_STATE_FILL == 'volume') { 
		ZS <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION)
	} else {
		ZS <- tools::toTitleCase(input$X_HOUSE_STATE_STAT) + Y_QUA + 
		' of ' + tolower(input$X_HOUSE_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION)
	}
	if ( input$X_HOUSE_STATE_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
	if ( input$X_HOUSE_STATE_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
	if ( input$X_HOUSE_STATE_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
	if ( input$X_HOUSE_STATE_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
	if ( input$X_HOUSE_STATE_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
	if ( input$X_HOUSE_STATE_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }
	ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_HOUSE>_</renderUI>___##############################
################################################################################



################################################################################
################################___</HOUSE_STATE>___############################
################################################################################

################################################################################
##########___<HOUSE_COUNTY_MAP>___#####___<renderPlot>___#######################
output$HOUSE_COUNTY_MAP  <-  renderPlot({
	XS_PROPERTY_TYPE = 'HOUSE'
	YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
	XS_REGION = 'COUNTY'
	Y_QUA <- ''
	Y_REGION_TYPE_FOR_COUNTY <- input$X_HOUSE_COUNTY_REGION_TYPE
	XS_REGION_NAME  <- 'Poland'
	################################################################
	if (input$X_HOUSE_COUNTY_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_HOUSE_COUNTY_COLOR }
	if (input$X_HOUSE_COUNTY_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
	if (input$X_HOUSE_COUNTY_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }
	Y_FILL <- gsub(' ','_',toupper(input$X_HOUSE_COUNTY_FILL))
	if ( input$X_HOUSE_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE}
	if ( input$X_HOUSE_COUNTY_STAT == 'interquartile range'  & input$X_HOUSE_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
	if ( input$X_HOUSE_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_HOUSE_COUNTY_STAT))}
	if ( input$X_HOUSE_COUNTY_STAT == 'quantile'  & input$X_HOUSE_COUNTY_FILL != 'volume') { 
		XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_HOUSE_COUNTY_QUA))
		Y_QUA <- input$X_HOUSE_COUNTY_QUA * 100
	} else {
		XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
	}
	if ( input$X_HOUSE_COUNTY_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}
	colnames(XT_GR) <-c(XS_REGION,'VAL')
	if ( input$X_HOUSE_COUNTY_COL ) {
		YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
	} else {
		YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
	}
	################################################################
	XTM <- left_join(SHP_POW, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
	if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
		XTM <- XTM[XTM$WOJ %in% Y_REGION_TYPE_FOR_COUNTY,] 
		XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
	}
	################################################################
	if ( input$X_HOUSE_COUNTY_LOG ) {
		XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
	} else {
		XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
	}
	if ( input$X_HOUSE_COUNTY_FILL == 'volume' ) {
		LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
	} else {
		LEG_TIT <- toTitleCase(input$X_HOUSE_COUNTY_STAT) + Y_QUA + ' of ' + tolower(input$X_HOUSE_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
	}
	ZM <- XM + labs(fill=LEG_TIT)
	###___<ADDING TEXT>___###
	if ( ( input$X_HOUSE_COUNTY_TEXT ) ) {
		XT_POINTS_0 <- sf::st_point_on_surface(XTM)
		Y_HOUSE_COUNTY_TOP_NUM <- input$Y_HOUSE_COUNTY_TEXT_NUM
		XV_TOP <- sapply( seq.int( Y_HOUSE_COUNTY_TOP_NUM) , function(X) NTH(as.double(na.omit(XT_POINTS_0[['VAL']])), X, descending = T))
		XT_POINTS  <-  XT_POINTS_0[XT_POINTS_0[['VAL']]  %in% XV_TOP,]
		XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
		XT_COORDS$POINTS_TEXT <- SUB(XT_POINTS$JPT_NAZWA_,8,10) + ': ' + NUM2K_XV(XT_POINTS[['VAL']])
		POINTS_TEXT_LEGEND  <-  'Displaying value for ' + XV_TOP + ' amounts by choosen regoin.'
		ZM <- ZM +
			geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
			geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
		# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
	}
return(ZM)
})
##########___</HOUSE_COUNTY_MAP>___#####___</renderPlot>___####################
################################################################################

################################################################################
##########___<HOUSE_COUNTY_TAB>___#####___<renderDataTable>___#################
output$HOUSE_COUNTY_TAB = renderDataTable({
XS_PROPERTY_TYPE = 'HOUSE'
Y_REGION_TYPE_FOR_COUNTY <- input$X_HOUSE_COUNTY_REGION_TYPE
XS_REGION = 'COUNTY'
XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
YT <- XT[PROPERTY_TYPE == XS_PROPERTY_TYPE & STATE == Y_REGION_TYPE_FOR_COUNTY ]
} else {
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_HOUSE_COUNTY_FILL))
if ( input$X_HOUSE_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_HOUSE_COUNTY_STAT == 'interquartile range'  & input$X_HOUSE_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_HOUSE_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_HOUSE_COUNTY_STAT))}
if ( input$X_HOUSE_COUNTY_STAT == 'quantile' & input$X_HOUSE_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_HOUSE_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_HOUSE_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</HOUSE_COUNTY_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_HOUSE>_<renderUI>___################################
output$TABLE_TEXT_HOUSE_COUNTY <- renderUI({
XS_REGION = 'COUNTY'
XS_PROPERTY_TYPE = 'HOUSE'
Y_QUA = ''
Y_REGION_TYPE_FOR_COUNTY <- input$X_HOUSE_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
XS_REGION = 'COUNTY'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}


if ( input$X_HOUSE_COUNTY_STAT == 'quantile'  & input$X_HOUSE_COUNTY_FILL != 'volume') { Y_QUA <- input$X_HOUSE_COUNTY_QUA * 100 }

if ( input$X_HOUSE_COUNTY_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_HOUSE_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_HOUSE_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_HOUSE>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_HOUSE>_<renderUI>___################################
output$MAIN_TEXT_HOUSE_COUNTY <- renderUI({
	XS_PROPERTY_TYPE = 'HOUSE'
	Y_QUA = ''
	Y_REGION_TYPE_FOR_COUNTY <- input$X_HOUSE_COUNTY_REGION_TYPE
	XS_REGION_NAME  <- 'Poland'
	XS_REGION = 'COUNTY'
	if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
		XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
	}
	if ( input$X_HOUSE_COUNTY_STAT == 'quantile'  & input$X_HOUSE_COUNTY_FILL != 'volume') { Y_QUA <- input$X_HOUSE_COUNTY_QUA * 100 }
	if ( input$X_HOUSE_COUNTY_FILL == 'volume') { 
		ZS <-  'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION)
	} else {
		ZS <- tools::toTitleCase(input$X_HOUSE_COUNTY_STAT) + Y_QUA + 
		' of ' + tolower(input$X_HOUSE_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION)
	}
	if ( input$X_HOUSE_COUNTY_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
	if ( input$X_HOUSE_COUNTY_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
	if ( input$X_HOUSE_COUNTY_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
	if ( input$X_HOUSE_COUNTY_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
	if ( input$X_HOUSE_COUNTY_FILL == 'land area') { ZS <- ZS + ' (in square meters).' } 
	if ( input$X_HOUSE_COUNTY_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }
	ZS <-  '<h3>' + ZS 
	HTML(ZS)
})
############___</TABLE_TEXT_HOUSE>_</renderUI>___##############################
################################################################################

################################################################################
############___<observe>_<sliderInput>___######################################
observe({
	XS_PROPERTY_TYPE = 'HOUSE'
	Y_REGION_TYPE_FOR_COUNTY <- input$X_HOUSE_COUNTY_REGION_TYPE
	YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
	if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
		YT <- YT[YT$STATE %in% Y_REGION_TYPE_FOR_COUNTY,] 
	}
	XS_REGION = 'COUNTY'
	Y_FILL <- gsub(' ','_',toupper(input$X_HOUSE_COUNTY_FILL))
	if ( input$X_HOUSE_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
	if ( input$X_HOUSE_COUNTY_STAT == 'interquartile range'  & input$X_HOUSE_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
	if ( input$X_HOUSE_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_HOUSE_COUNTY_STAT))}
	if ( input$X_HOUSE_COUNTY_STAT == 'quantile' & input$X_HOUSE_COUNTY_FILL != 'volume' ) { 
	 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_HOUSE_COUNTY_QUA))
	} else {
	 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
	}
	################################################################
	XT_VOL <- GR(YT,XBY=XS_REGION)
	if ( input$X_HOUSE_COUNTY_FILL != 'volume' ) {
	XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
	XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
	} else {
		XT_GR <- XT_VOL
		XT_GR <- XT_GR[o(- XT_GR[['N']] )]
	}
	val <- nrow( XT_GR )
	updateSliderInput(session, "Y_HOUSE_COUNTY_TEXT_NUM", value = 5,
	min = 1, max = val, step = 1)
})
###########___</observe>_<sliderInput>___######################################
################################################################################

##############################################################################################################
##############################################################################################################
################################___</HOUSE_STATE>___###########################################################
##############################################################################################################
##############################################################################################################





##############################################################################################################
##############################################################################################################
################################___<APARTMENT_STATE>___###########################################################
##############################################################################################################
##############################################################################################################

################################################################################
##########___<APARTMENT_STATE_MAP>___#####___<renderPlot>___#######################
output$APARTMENT_STATE_MAP  <-  renderPlot({
XS_PROPERTY_TYPE = 'APARTMENT'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'STATE'
Y_QUA <- ''
################################################################
if (input$X_APARTMENT_STATE_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_APARTMENT_STATE_COLOR }
if (input$X_APARTMENT_STATE_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
if (input$X_APARTMENT_STATE_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }
Y_FILL <- gsub(' ','_',toupper(input$X_APARTMENT_STATE_FILL))
if ( input$X_APARTMENT_STATE_STAT == 'mode' ) { Y_FUN <- MODE}
if ( input$X_APARTMENT_STATE_STAT == 'interquartile range'  & input$X_APARTMENT_STATE_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_APARTMENT_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_APARTMENT_STATE_STAT))}
if ( input$X_APARTMENT_STATE_STAT == 'quantile'  & input$X_APARTMENT_STATE_FILL != 'volume') { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_APARTMENT_STATE_QUA))
 Y_QUA <- input$X_APARTMENT_STATE_QUA * 100
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}

if ( input$X_APARTMENT_STATE_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

colnames(XT_GR) <-c(XS_REGION,'VAL')
 
if ( input$X_APARTMENT_STATE_COL ) {
 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
} else {
 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
}
################################################################
XTM <- left_join(SHP_WOJ, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
if ( input$X_APARTMENT_STATE_LOG ) {
  XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
} else {
 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
}
if ( input$X_APARTMENT_STATE_FILL == 'volume' ) {
LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
} else {
LEG_TIT <- toTitleCase(input$X_APARTMENT_STATE_STAT) + Y_QUA + ' of ' + tolower(input$X_APARTMENT_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
}
ZM <- XM + labs(fill=LEG_TIT)
###___<ADDING TEXT>___###
if ( ( input$X_APARTMENT_STATE_TEXT ) ) {
XT_POINTS <- sf::st_point_on_surface(XTM)
#XT_POINTS_MAX <- XT_POINTS_0[which.max(XT_POINTS_0[[Y_FILL]]),]
#XT_POINTS_CHN <- XT_POINTS_0[XT_POINTS_0[['su_a3']]=='CHN',]
#XT_POINTS  <-  rbind( XT_POINTS_CHN,XT_POINTS_MAX )
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- NUM2K_XV(XT_POINTS[['VAL']])
POINTS_TEXT_LEGEND  <-  'Displaying value ' + ' ' + Y_FILL
ZM <- ZM +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
}
###___</ADDING TEXT>___###
return( ZM )
})
#########___</renderPlot>___###################################################
################################################################################

################################################################################
##########___<APARTMENT_STATE_TAB>___#####___<renderDataTable>___#################
output$APARTMENT_STATE_TAB = renderDataTable({
XS_PROPERTY_TYPE = 'APARTMENT'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'STATE'
Y_FILL <- gsub(' ','_',toupper(input$X_APARTMENT_STATE_FILL))
if ( input$X_APARTMENT_STATE_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_APARTMENT_STATE_STAT == 'interquartile range'  & input$X_APARTMENT_STATE_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_APARTMENT_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_APARTMENT_STATE_STAT))}
if ( input$X_APARTMENT_STATE_STAT == 'quantile' & input$X_APARTMENT_STATE_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_APARTMENT_STATE_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_APARTMENT_STATE_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='STATE')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</APARTMENT_STATE_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_APARTMENT>_<renderUI>___################################
output$TABLE_TEXT_APARTMENT_STATE <- renderUI({
XS_PROPERTY_TYPE = 'APARTMENT'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_APARTMENT_STATE_STAT == 'quantile'  & input$X_APARTMENT_STATE_FILL != 'volume') { Y_QUA <- input$X_APARTMENT_STATE_QUA * 100 }

if ( input$X_APARTMENT_STATE_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_APARTMENT_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_APARTMENT_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})


################################################################################
############___<TABLE_TEXT_APARTMENT>_<renderUI>___################################
output$MAIN_TEXT_APARTMENT_STATE <- renderUI({
XS_PROPERTY_TYPE = 'APARTMENT'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_APARTMENT_STATE_STAT == 'quantile'  & input$X_APARTMENT_STATE_FILL != 'volume') { Y_QUA <- input$X_APARTMENT_STATE_QUA * 100 }
if ( input$X_APARTMENT_STATE_FILL == 'volume') { 
 ZS <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION)
 } else {
ZS <- tools::toTitleCase(input$X_APARTMENT_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_APARTMENT_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION)
}
if ( input$X_APARTMENT_STATE_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
if ( input$X_APARTMENT_STATE_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
if ( input$X_APARTMENT_STATE_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
if ( input$X_APARTMENT_STATE_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_APARTMENT_STATE_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_APARTMENT_STATE_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_APARTMENT>_</renderUI>___##############################
################################################################################


############___</TABLE_TEXT_APARTMENT>_</renderUI>___##############################
################################################################################



################################################################################
################################___</APARTMENT_STATE>___############################
################################################################################

################################################################################
##########___<APARTMENT_COUNTY_MAP>___#####___<renderPlot>___#######################
output$APARTMENT_COUNTY_MAP  <-  renderPlot({
XS_PROPERTY_TYPE = 'APARTMENT'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'COUNTY'
Y_QUA <- ''
Y_REGION_TYPE_FOR_COUNTY <- input$X_APARTMENT_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
################################################################
if (input$X_APARTMENT_COUNTY_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_APARTMENT_COUNTY_COLOR }
if (input$X_APARTMENT_COUNTY_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
if (input$X_APARTMENT_COUNTY_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }

Y_FILL <- gsub(' ','_',toupper(input$X_APARTMENT_COUNTY_FILL))

if ( input$X_APARTMENT_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE}
if ( input$X_APARTMENT_COUNTY_STAT == 'interquartile range'  & input$X_APARTMENT_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_APARTMENT_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_APARTMENT_COUNTY_STAT))}
if ( input$X_APARTMENT_COUNTY_STAT == 'quantile'  & input$X_APARTMENT_COUNTY_FILL != 'volume') { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_APARTMENT_COUNTY_QUA))
 Y_QUA <- input$X_APARTMENT_COUNTY_QUA * 100
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
if ( input$X_APARTMENT_COUNTY_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

colnames(XT_GR) <-c(XS_REGION,'VAL')

if ( input$X_APARTMENT_COUNTY_COL ) {
 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
} else {
 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
}
################################################################
XTM <- left_join(SHP_POW, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XTM <- XTM[XTM$WOJ %in% Y_REGION_TYPE_FOR_COUNTY,] 
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}
################################################################
if ( input$X_APARTMENT_COUNTY_LOG ) {
  XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
} else {
 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
}
if ( input$X_APARTMENT_COUNTY_FILL == 'volume' ) {
LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
} else {
LEG_TIT <- toTitleCase(input$X_APARTMENT_COUNTY_STAT) + Y_QUA + ' of ' + tolower(input$X_APARTMENT_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
}
ZM <- XM + labs(fill=LEG_TIT)
###___<ADDING TEXT>___###
if ( ( input$X_APARTMENT_COUNTY_TEXT ) ) {
XT_POINTS_0 <- sf::st_point_on_surface(XTM)
Y_APARTMENT_COUNTY_TOP_NUM <- input$Y_APARTMENT_COUNTY_TEXT_NUM
XV_TOP <- sapply( seq.int( Y_APARTMENT_COUNTY_TOP_NUM) , function(X) NTH(as.double(na.omit(XT_POINTS_0[['VAL']])), X, descending = T))
XT_POINTS  <-  XT_POINTS_0[XT_POINTS_0[['VAL']]  %in% XV_TOP,]
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- SUB(XT_POINTS$JPT_NAZWA_,8,10) + ': ' + NUM2K_XV(XT_POINTS[['VAL']])
POINTS_TEXT_LEGEND  <-  'Displaying value for ' + XV_TOP + ' amounts by choosen regoin.'
ZM <- ZM +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
}
return( ZM )
})
##########___</APARTMENT_COUNTY_MAP>___#####___</renderPlot>___####################
################################################################################

################################################################################
##########___<APARTMENT_COUNTY_TAB>___#####___<renderDataTable>___#################
output$APARTMENT_COUNTY_TAB = renderDataTable({
XS_PROPERTY_TYPE = 'APARTMENT'
Y_REGION_TYPE_FOR_COUNTY <- input$X_APARTMENT_COUNTY_REGION_TYPE
XS_REGION = 'COUNTY'
#XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
YT <- XT[PROPERTY_TYPE == XS_PROPERTY_TYPE & STATE == Y_REGION_TYPE_FOR_COUNTY ]
} else {
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_APARTMENT_COUNTY_FILL))
if ( input$X_APARTMENT_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_APARTMENT_COUNTY_STAT == 'interquartile range'  & input$X_APARTMENT_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_APARTMENT_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_APARTMENT_COUNTY_STAT))}
if ( input$X_APARTMENT_COUNTY_STAT == 'quantile' & input$X_APARTMENT_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_APARTMENT_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_APARTMENT_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</APARTMENT_COUNTY_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_APARTMENT>_<renderUI>___################################
output$TABLE_TEXT_APARTMENT_COUNTY <- renderUI({
XS_PROPERTY_TYPE = 'APARTMENT'
Y_QUA = ''
Y_REGION_TYPE_FOR_COUNTY <- input$X_APARTMENT_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
XS_REGION = 'COUNTY'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}


if ( input$X_APARTMENT_COUNTY_STAT == 'quantile'  & input$X_APARTMENT_COUNTY_FILL != 'volume') { Y_QUA <- input$X_APARTMENT_COUNTY_QUA * 100 }

if ( input$X_APARTMENT_COUNTY_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_APARTMENT_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_APARTMENT_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_APARTMENT>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_APARTMENT>_<renderUI>___################################
output$MAIN_TEXT_APARTMENT_COUNTY <- renderUI({
XS_PROPERTY_TYPE = 'APARTMENT'
Y_QUA = ''
Y_REGION_TYPE_FOR_COUNTY <- input$X_APARTMENT_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
XS_REGION = 'COUNTY'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}
if ( input$X_APARTMENT_COUNTY_STAT == 'quantile'  & input$X_APARTMENT_COUNTY_FILL != 'volume') { Y_QUA <- input$X_APARTMENT_COUNTY_QUA * 100 }

if ( input$X_APARTMENT_COUNTY_FILL == 'volume') { 
 ZS <-  'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION)
 } else {
ZS <- tools::toTitleCase(input$X_APARTMENT_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_APARTMENT_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION)
}

if ( input$X_APARTMENT_COUNTY_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
if ( input$X_APARTMENT_COUNTY_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
if ( input$X_APARTMENT_COUNTY_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
if ( input$X_APARTMENT_COUNTY_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_APARTMENT_COUNTY_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_APARTMENT_COUNTY_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }


ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_APARTMENT>_</renderUI>___##############################
################################################################################

################################################################################
############___<observe>_<sliderInput>___######################################
observe({
XS_REGION = 'COUNTY'
XS_PROPERTY_TYPE = 'APARTMENT'
Y_REGION_TYPE_FOR_COUNTY <- input$X_APARTMENT_COUNTY_REGION_TYPE
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 YT <- YT[YT$STATE %in% Y_REGION_TYPE_FOR_COUNTY,] 
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_APARTMENT_COUNTY_FILL))
if ( input$X_APARTMENT_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_APARTMENT_COUNTY_STAT == 'interquartile range'  & input$X_APARTMENT_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_APARTMENT_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_APARTMENT_COUNTY_STAT))}
if ( input$X_APARTMENT_COUNTY_STAT == 'quantile' & input$X_APARTMENT_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_APARTMENT_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_APARTMENT_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
val <- nrow( XT_GR )
updateSliderInput(session, "Y_APARTMENT_COUNTY_TEXT_NUM", value = 5,
min = 1, max = val, step = 1)
})
###########___</observe>_<sliderInput>___######################################
################################################################################

##############################################################################################################
##############################################################################################################
################################___</APARTMENT_STATE>___###########################################################
##############################################################################################################
##############################################################################################################











































##############################################################################################################
##############################################################################################################
################################___<LAND_STATE>___###########################################################
##############################################################################################################
##############################################################################################################

################################################################################
##########___<LAND_STATE_MAP>___#####___<renderPlot>___#######################
output$LAND_STATE_MAP  <-  renderPlot({
XS_PROPERTY_TYPE = 'LAND'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'STATE'
Y_QUA <- ''
################################################################
if (input$X_LAND_STATE_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_LAND_STATE_COLOR }
if (input$X_LAND_STATE_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
if (input$X_LAND_STATE_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }
Y_FILL <- gsub(' ','_',toupper(input$X_LAND_STATE_FILL))
if ( input$X_LAND_STATE_STAT == 'mode' ) { Y_FUN <- MODE}
if ( input$X_LAND_STATE_STAT == 'interquartile range'  & input$X_LAND_STATE_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_LAND_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_LAND_STATE_STAT))}
if ( input$X_LAND_STATE_STAT == 'quantile'  & input$X_LAND_STATE_FILL != 'volume') { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_LAND_STATE_QUA))
 Y_QUA <- input$X_LAND_STATE_QUA * 100
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}

if ( input$X_LAND_STATE_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

colnames(XT_GR) <-c(XS_REGION,'VAL')
 
if ( input$X_LAND_STATE_COL ) {
 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
} else {
 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
}
################################################################
XTM <- left_join(SHP_WOJ, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
if ( input$X_LAND_STATE_LOG ) {
  XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
} else {
 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
}
if ( input$X_LAND_STATE_FILL == 'volume' ) {
LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
} else {
LEG_TIT <- toTitleCase(input$X_LAND_STATE_STAT) + Y_QUA + ' of ' + tolower(input$X_LAND_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
}
ZM <- XM + labs(fill=LEG_TIT)
###___<ADDING TEXT>___###
if ( ( input$X_LAND_STATE_TEXT ) ) {
XT_POINTS <- sf::st_point_on_surface(XTM)
#XT_POINTS_MAX <- XT_POINTS_0[which.max(XT_POINTS_0[[Y_FILL]]),]
#XT_POINTS_CHN <- XT_POINTS_0[XT_POINTS_0[['su_a3']]=='CHN',]
#XT_POINTS  <-  rbind( XT_POINTS_CHN,XT_POINTS_MAX )
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- NUM2K_XV(XT_POINTS[['VAL']])
POINTS_TEXT_LEGEND  <-  'Displaying value ' + ' ' + Y_FILL
ZM <- ZM +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
}
###___</ADDING TEXT>___###
return( ZM )
})
#########___</renderPlot>___###################################################
################################################################################

################################################################################
##########___<LAND_STATE_TAB>___#####___<renderDataTable>___#################
output$LAND_STATE_TAB = renderDataTable({
XS_PROPERTY_TYPE = 'LAND'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'STATE'
Y_FILL <- gsub(' ','_',toupper(input$X_LAND_STATE_FILL))
if ( input$X_LAND_STATE_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_LAND_STATE_STAT == 'interquartile range'  & input$X_LAND_STATE_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_LAND_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_LAND_STATE_STAT))}
if ( input$X_LAND_STATE_STAT == 'quantile' & input$X_LAND_STATE_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_LAND_STATE_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_LAND_STATE_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='STATE')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</LAND_STATE_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_LAND>_<renderUI>___################################
output$TABLE_TEXT_LAND_STATE <- renderUI({
XS_PROPERTY_TYPE = 'LAND'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_LAND_STATE_STAT == 'quantile'  & input$X_LAND_STATE_FILL != 'volume') { Y_QUA <- input$X_LAND_STATE_QUA * 100 }

if ( input$X_LAND_STATE_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_LAND_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_LAND_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_LAND>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_LAND>_<renderUI>___################################
output$MAIN_TEXT_LAND_STATE <- renderUI({
XS_PROPERTY_TYPE = 'LAND'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_LAND_STATE_STAT == 'quantile'  & input$X_LAND_STATE_FILL != 'volume') { Y_QUA <- input$X_LAND_STATE_QUA * 100 }
if ( input$X_LAND_STATE_FILL == 'volume') { 
 ZS <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION)
 } else {
ZS <- tools::toTitleCase(input$X_LAND_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_LAND_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION)
}
if ( input$X_LAND_STATE_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
if ( input$X_LAND_STATE_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
if ( input$X_LAND_STATE_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
if ( input$X_LAND_STATE_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_LAND_STATE_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_LAND_STATE_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_LAND>_</renderUI>___##############################
################################################################################



################################################################################
################################___</LAND_STATE>___############################
################################################################################

################################################################################
##########___<LAND_COUNTY_MAP>___#####___<renderPlot>___#######################
output$LAND_COUNTY_MAP  <-  renderPlot({
XS_PROPERTY_TYPE = 'LAND'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'COUNTY'
Y_QUA <- ''
Y_REGION_TYPE_FOR_COUNTY <- input$X_LAND_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
################################################################
if (input$X_LAND_COUNTY_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_LAND_COUNTY_COLOR }
if (input$X_LAND_COUNTY_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
if (input$X_LAND_COUNTY_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }

Y_FILL <- gsub(' ','_',toupper(input$X_LAND_COUNTY_FILL))

if ( input$X_LAND_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE}
if ( input$X_LAND_COUNTY_STAT == 'interquartile range'  & input$X_LAND_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_LAND_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_LAND_COUNTY_STAT))}
if ( input$X_LAND_COUNTY_STAT == 'quantile'  & input$X_LAND_COUNTY_FILL != 'volume') { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_LAND_COUNTY_QUA))
 Y_QUA <- input$X_LAND_COUNTY_QUA * 100
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
if ( input$X_LAND_COUNTY_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

colnames(XT_GR) <-c(XS_REGION,'VAL')

if ( input$X_LAND_COUNTY_COL ) {
 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
} else {
 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
}
################################################################
XTM <- left_join(SHP_POW, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XTM <- XTM[XTM$WOJ %in% Y_REGION_TYPE_FOR_COUNTY,] 
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}
################################################################
if ( input$X_LAND_COUNTY_LOG ) {
  XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
} else {
 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
}
if ( input$X_LAND_COUNTY_FILL == 'volume' ) {
LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
} else {
LEG_TIT <- toTitleCase(input$X_LAND_COUNTY_STAT) + Y_QUA + ' of ' + tolower(input$X_LAND_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
}
ZM <- XM + labs(fill=LEG_TIT)
###___<ADDING TEXT>___###
if ( ( input$X_LAND_COUNTY_TEXT ) ) {
XT_POINTS_0 <- sf::st_point_on_surface(XTM)
Y_LAND_COUNTY_TOP_NUM <- input$Y_LAND_COUNTY_TEXT_NUM
XV_TOP <- sapply( seq.int( Y_LAND_COUNTY_TOP_NUM) , function(X) NTH(as.double(na.omit(XT_POINTS_0[['VAL']])), X, descending = T))
XT_POINTS  <-  XT_POINTS_0[XT_POINTS_0[['VAL']]  %in% XV_TOP,]
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- SUB(XT_POINTS$JPT_NAZWA_,8,10) + ': ' + NUM2K_XV(XT_POINTS[['VAL']])
POINTS_TEXT_LEGEND  <-  'Displaying value for ' + XV_TOP + ' amounts by choosen regoin.'
ZM <- ZM +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
}
return( ZM )
})
##########___</LAND_COUNTY_MAP>___#####___</renderPlot>___####################
################################################################################

################################################################################
##########___<LAND_COUNTY_TAB>___#####___<renderDataTable>___#################
output$LAND_COUNTY_TAB = renderDataTable({
XS_PROPERTY_TYPE = 'LAND'
XS_REGION = 'COUNTY'
Y_REGION_TYPE_FOR_COUNTY <- input$X_LAND_COUNTY_REGION_TYPE
#XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
YT <- XT[PROPERTY_TYPE == XS_PROPERTY_TYPE & STATE == Y_REGION_TYPE_FOR_COUNTY ]
} else {
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_LAND_COUNTY_FILL))
if ( input$X_LAND_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_LAND_COUNTY_STAT == 'interquartile range'  & input$X_LAND_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_LAND_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_LAND_COUNTY_STAT))}
if ( input$X_LAND_COUNTY_STAT == 'quantile' & input$X_LAND_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_LAND_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_LAND_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</LAND_COUNTY_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_LAND>_<renderUI>___################################
output$TABLE_TEXT_LAND_COUNTY <- renderUI({
XS_PROPERTY_TYPE = 'LAND'
Y_QUA = ''
XS_REGION = 'COUNTY'
Y_REGION_TYPE_FOR_COUNTY <- input$X_LAND_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}


if ( input$X_LAND_COUNTY_STAT == 'quantile'  & input$X_LAND_COUNTY_FILL != 'volume') { Y_QUA <- input$X_LAND_COUNTY_QUA * 100 }

if ( input$X_LAND_COUNTY_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_LAND_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_LAND_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_LAND>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_LAND>_<renderUI>___################################
output$MAIN_TEXT_LAND_COUNTY <- renderUI({
XS_PROPERTY_TYPE = 'LAND'
XS_REGION = 'COUNTY'
Y_QUA = ''
Y_REGION_TYPE_FOR_COUNTY <- input$X_LAND_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}
if ( input$X_LAND_COUNTY_STAT == 'quantile'  & input$X_LAND_COUNTY_FILL != 'volume') { Y_QUA <- input$X_LAND_COUNTY_QUA * 100 }

if ( input$X_LAND_COUNTY_FILL == 'volume') { 
 ZS <-  'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION)
 } else {
ZS <- tools::toTitleCase(input$X_LAND_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_LAND_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION)
}

if ( input$X_LAND_COUNTY_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
if ( input$X_LAND_COUNTY_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
if ( input$X_LAND_COUNTY_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
if ( input$X_LAND_COUNTY_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_LAND_COUNTY_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_LAND_COUNTY_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }


ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_LAND>_</renderUI>___##############################
################################################################################

################################################################################
############___<observe>_<sliderInput>___######################################
observe({
XS_REGION = 'COUNTY'
XS_PROPERTY_TYPE = 'LAND'
Y_REGION_TYPE_FOR_COUNTY <- input$X_LAND_COUNTY_REGION_TYPE
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 YT <- YT[YT$STATE %in% Y_REGION_TYPE_FOR_COUNTY,] 
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_LAND_COUNTY_FILL))
if ( input$X_LAND_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_LAND_COUNTY_STAT == 'interquartile range'  & input$X_LAND_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_LAND_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_LAND_COUNTY_STAT))}
if ( input$X_LAND_COUNTY_STAT == 'quantile' & input$X_LAND_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_LAND_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_LAND_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
val <- nrow( XT_GR )
updateSliderInput(session, "Y_LAND_COUNTY_TEXT_NUM", value = 5,
min = 1, max = val, step = 1)
})
###########___</observe>_<sliderInput>___######################################
################################################################################

##############################################################################################################
##############################################################################################################
################################___</LAND_STATE>___###########################################################
##############################################################################################################
##############################################################################################################



























##############################################################################################################
##############################################################################################################
################################___<COMMERCIAL_STATE>___###########################################################
##############################################################################################################
##############################################################################################################

################################################################################
##########___<COMMERCIAL_STATE_MAP>___#####___<renderPlot>___#######################
output$COMMERCIAL_STATE_MAP  <-  renderPlot({
XS_PROPERTY_TYPE = 'COMMERCIAL'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'STATE'
Y_QUA <- ''
################################################################
if (input$X_COMMERCIAL_STATE_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_COMMERCIAL_STATE_COLOR }
if (input$X_COMMERCIAL_STATE_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
if (input$X_COMMERCIAL_STATE_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }
Y_FILL <- gsub(' ','_',toupper(input$X_COMMERCIAL_STATE_FILL))
if ( input$X_COMMERCIAL_STATE_STAT == 'mode' ) { Y_FUN <- MODE}
if ( input$X_COMMERCIAL_STATE_STAT == 'interquartile range'  & input$X_COMMERCIAL_STATE_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_COMMERCIAL_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_COMMERCIAL_STATE_STAT))}
if ( input$X_COMMERCIAL_STATE_STAT == 'quantile'  & input$X_COMMERCIAL_STATE_FILL != 'volume') { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_COMMERCIAL_STATE_QUA))
 Y_QUA <- input$X_COMMERCIAL_STATE_QUA * 100
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}

if ( input$X_COMMERCIAL_STATE_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

colnames(XT_GR) <-c(XS_REGION,'VAL')
 
if ( input$X_COMMERCIAL_STATE_COL ) {
 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
} else {
 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
}
################################################################
XTM <- left_join(SHP_WOJ, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
if ( input$X_COMMERCIAL_STATE_LOG ) {
  XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
} else {
 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
}
if ( input$X_COMMERCIAL_STATE_FILL == 'volume' ) {
LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
} else {
LEG_TIT <- toTitleCase(input$X_COMMERCIAL_STATE_STAT) + Y_QUA + ' of ' + tolower(input$X_COMMERCIAL_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
}
ZM <- XM + labs(fill=LEG_TIT)
###___<ADDING TEXT>___###
if ( ( input$X_COMMERCIAL_STATE_TEXT ) ) {
XT_POINTS <- sf::st_point_on_surface(XTM)
#XT_POINTS_MAX <- XT_POINTS_0[which.max(XT_POINTS_0[[Y_FILL]]),]
#XT_POINTS_CHN <- XT_POINTS_0[XT_POINTS_0[['su_a3']]=='CHN',]
#XT_POINTS  <-  rbind( XT_POINTS_CHN,XT_POINTS_MAX )
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- NUM2K_XV(XT_POINTS[['VAL']])
POINTS_TEXT_LEGEND  <-  'Displaying value ' + ' ' + Y_FILL
ZM <- ZM +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
}
###___</ADDING TEXT>___###
return( ZM )
})
#########___</renderPlot>___###################################################
################################################################################

################################################################################
##########___<COMMERCIAL_STATE_TAB>___#####___<renderDataTable>___#################
output$COMMERCIAL_STATE_TAB = renderDataTable({
XS_PROPERTY_TYPE = 'COMMERCIAL'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'STATE'
Y_FILL <- gsub(' ','_',toupper(input$X_COMMERCIAL_STATE_FILL))
if ( input$X_COMMERCIAL_STATE_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_COMMERCIAL_STATE_STAT == 'interquartile range'  & input$X_COMMERCIAL_STATE_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_COMMERCIAL_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_COMMERCIAL_STATE_STAT))}
if ( input$X_COMMERCIAL_STATE_STAT == 'quantile' & input$X_COMMERCIAL_STATE_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_COMMERCIAL_STATE_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_COMMERCIAL_STATE_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='STATE')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</COMMERCIAL_STATE_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_COMMERCIAL>_<renderUI>___################################
output$TABLE_TEXT_COMMERCIAL_STATE <- renderUI({
XS_PROPERTY_TYPE = 'COMMERCIAL'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_COMMERCIAL_STATE_STAT == 'quantile'  & input$X_COMMERCIAL_STATE_FILL != 'volume') { Y_QUA <- input$X_COMMERCIAL_STATE_QUA * 100 }

if ( input$X_COMMERCIAL_STATE_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_COMMERCIAL_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_COMMERCIAL_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_COMMERCIAL>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_COMMERCIAL>_<renderUI>___################################
output$MAIN_TEXT_COMMERCIAL_STATE <- renderUI({
XS_PROPERTY_TYPE = 'COMMERCIAL'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_COMMERCIAL_STATE_STAT == 'quantile'  & input$X_COMMERCIAL_STATE_FILL != 'volume') { Y_QUA <- input$X_COMMERCIAL_STATE_QUA * 100 }
if ( input$X_COMMERCIAL_STATE_FILL == 'volume') { 
 ZS <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION)
 } else {
ZS <- tools::toTitleCase(input$X_COMMERCIAL_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_COMMERCIAL_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION)
}
if ( input$X_COMMERCIAL_STATE_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
if ( input$X_COMMERCIAL_STATE_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
if ( input$X_COMMERCIAL_STATE_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
if ( input$X_COMMERCIAL_STATE_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_COMMERCIAL_STATE_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_COMMERCIAL_STATE_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_COMMERCIAL>_</renderUI>___##############################
################################################################################



################################################################################
################################___</COMMERCIAL_STATE>___############################
################################################################################

################################################################################
##########___<COMMERCIAL_COUNTY_MAP>___#####___<renderPlot>___#######################
output$COMMERCIAL_COUNTY_MAP  <-  renderPlot({
XS_PROPERTY_TYPE = 'COMMERCIAL'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'COUNTY'
Y_QUA <- ''
Y_REGION_TYPE_FOR_COUNTY <- input$X_COMMERCIAL_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
################################################################
if (input$X_COMMERCIAL_COUNTY_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_COMMERCIAL_COUNTY_COLOR }
if (input$X_COMMERCIAL_COUNTY_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
if (input$X_COMMERCIAL_COUNTY_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }

Y_FILL <- gsub(' ','_',toupper(input$X_COMMERCIAL_COUNTY_FILL))

if ( input$X_COMMERCIAL_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE}
if ( input$X_COMMERCIAL_COUNTY_STAT == 'interquartile range'  & input$X_COMMERCIAL_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_COMMERCIAL_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_COMMERCIAL_COUNTY_STAT))}
if ( input$X_COMMERCIAL_COUNTY_STAT == 'quantile'  & input$X_COMMERCIAL_COUNTY_FILL != 'volume') { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_COMMERCIAL_COUNTY_QUA))
 Y_QUA <- input$X_COMMERCIAL_COUNTY_QUA * 100
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
if ( input$X_COMMERCIAL_COUNTY_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

colnames(XT_GR) <-c(XS_REGION,'VAL')

if ( input$X_COMMERCIAL_COUNTY_COL ) {
 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
} else {
 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
}
################################################################
XTM <- left_join(SHP_POW, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XTM <- XTM[XTM$WOJ %in% Y_REGION_TYPE_FOR_COUNTY,] 
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}
################################################################
if ( input$X_COMMERCIAL_COUNTY_LOG ) {
  XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
} else {
 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
}
if ( input$X_COMMERCIAL_COUNTY_FILL == 'volume' ) {
LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
} else {
LEG_TIT <- toTitleCase(input$X_COMMERCIAL_COUNTY_STAT) + Y_QUA + ' of ' + tolower(input$X_COMMERCIAL_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
}
ZM <- XM + labs(fill=LEG_TIT)
###___<ADDING TEXT>___###
if ( ( input$X_COMMERCIAL_COUNTY_TEXT ) ) {
XT_POINTS_0 <- sf::st_point_on_surface(XTM)
Y_COMMERCIAL_COUNTY_TOP_NUM <- input$Y_COMMERCIAL_COUNTY_TEXT_NUM
XV_TOP <- sapply( seq.int( Y_COMMERCIAL_COUNTY_TOP_NUM) , function(X) NTH(as.double(na.omit(XT_POINTS_0[['VAL']])), X, descending = T))
XT_POINTS  <-  XT_POINTS_0[XT_POINTS_0[['VAL']]  %in% XV_TOP,]
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- SUB(XT_POINTS$JPT_NAZWA_,8,10) + ': ' + NUM2K_XV(XT_POINTS[['VAL']])
POINTS_TEXT_LEGEND  <-  'Displaying value for ' + XV_TOP + ' amounts by choosen regoin.'
ZM <- ZM +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
}
return( ZM )
})
##########___</COMMERCIAL_COUNTY_MAP>___#####___</renderPlot>___####################
################################################################################

################################################################################
##########___<COMMERCIAL_COUNTY_TAB>___#####___<renderDataTable>___#################
output$COMMERCIAL_COUNTY_TAB = renderDataTable({
XS_PROPERTY_TYPE = 'COMMERCIAL'
Y_REGION_TYPE_FOR_COUNTY <- input$X_COMMERCIAL_COUNTY_REGION_TYPE
XS_REGION = 'COUNTY'
#XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
YT <- XT[PROPERTY_TYPE == XS_PROPERTY_TYPE & STATE == Y_REGION_TYPE_FOR_COUNTY ]
} else {
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_COMMERCIAL_COUNTY_FILL))
if ( input$X_COMMERCIAL_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_COMMERCIAL_COUNTY_STAT == 'interquartile range'  & input$X_COMMERCIAL_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_COMMERCIAL_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_COMMERCIAL_COUNTY_STAT))}
if ( input$X_COMMERCIAL_COUNTY_STAT == 'quantile' & input$X_COMMERCIAL_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_COMMERCIAL_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_COMMERCIAL_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</COMMERCIAL_COUNTY_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_COMMERCIAL>_<renderUI>___################################
output$TABLE_TEXT_COMMERCIAL_COUNTY <- renderUI({
XS_PROPERTY_TYPE = 'COMMERCIAL'
Y_QUA = ''
XS_REGION = 'COUNTY'
Y_REGION_TYPE_FOR_COUNTY <- input$X_COMMERCIAL_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}


if ( input$X_COMMERCIAL_COUNTY_STAT == 'quantile'  & input$X_COMMERCIAL_COUNTY_FILL != 'volume') { Y_QUA <- input$X_COMMERCIAL_COUNTY_QUA * 100 }

if ( input$X_COMMERCIAL_COUNTY_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_COMMERCIAL_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_COMMERCIAL_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_COMMERCIAL>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_COMMERCIAL>_<renderUI>___################################
output$MAIN_TEXT_COMMERCIAL_COUNTY <- renderUI({
XS_PROPERTY_TYPE = 'COMMERCIAL'
Y_QUA = ''
XS_REGION = 'COUNTY'
Y_REGION_TYPE_FOR_COUNTY <- input$X_COMMERCIAL_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}
if ( input$X_COMMERCIAL_COUNTY_STAT == 'quantile'  & input$X_COMMERCIAL_COUNTY_FILL != 'volume') { Y_QUA <- input$X_COMMERCIAL_COUNTY_QUA * 100 }

if ( input$X_COMMERCIAL_COUNTY_FILL == 'volume') { 
 ZS <-  'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION)
 } else {
ZS <- tools::toTitleCase(input$X_COMMERCIAL_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_COMMERCIAL_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION)
}

if ( input$X_COMMERCIAL_COUNTY_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
if ( input$X_COMMERCIAL_COUNTY_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
if ( input$X_COMMERCIAL_COUNTY_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
if ( input$X_COMMERCIAL_COUNTY_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_COMMERCIAL_COUNTY_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_COMMERCIAL_COUNTY_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }


ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_COMMERCIAL>_</renderUI>___##############################
################################################################################

################################################################################
############___<observe>_<sliderInput>___######################################
observe({
XS_REGION = 'COUNTY'
XS_PROPERTY_TYPE = 'COMMERCIAL'
Y_REGION_TYPE_FOR_COUNTY <- input$X_COMMERCIAL_COUNTY_REGION_TYPE
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 YT <- YT[YT$STATE %in% Y_REGION_TYPE_FOR_COUNTY,] 
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_COMMERCIAL_COUNTY_FILL))
if ( input$X_COMMERCIAL_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_COMMERCIAL_COUNTY_STAT == 'interquartile range'  & input$X_COMMERCIAL_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_COMMERCIAL_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_COMMERCIAL_COUNTY_STAT))}
if ( input$X_COMMERCIAL_COUNTY_STAT == 'quantile' & input$X_COMMERCIAL_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_COMMERCIAL_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_COMMERCIAL_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
val <- nrow( XT_GR )
updateSliderInput(session, "Y_COMMERCIAL_COUNTY_TEXT_NUM", value = 5,
min = 1, max = val, step = 1)
})
###########___</observe>_<sliderInput>___######################################
################################################################################

##############################################################################################################
##############################################################################################################
################################___</COMMERCIAL_STATE>___###########################################################
##############################################################################################################
##############################################################################################################





























##############################################################################################################
##############################################################################################################
################################___<WAREHOUSE_STATE>___###########################################################
##############################################################################################################
##############################################################################################################

################################################################################
##########___<WAREHOUSE_STATE_MAP>___#####___<renderPlot>___#######################
output$WAREHOUSE_STATE_MAP  <-  renderPlot({
XS_PROPERTY_TYPE = 'WAREHOUSE'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'STATE'
Y_QUA <- ''
################################################################
if (input$X_WAREHOUSE_STATE_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_WAREHOUSE_STATE_COLOR }
if (input$X_WAREHOUSE_STATE_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
if (input$X_WAREHOUSE_STATE_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }
Y_FILL <- gsub(' ','_',toupper(input$X_WAREHOUSE_STATE_FILL))
if ( input$X_WAREHOUSE_STATE_STAT == 'mode' ) { Y_FUN <- MODE}
if ( input$X_WAREHOUSE_STATE_STAT == 'interquartile range'  & input$X_WAREHOUSE_STATE_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_WAREHOUSE_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_WAREHOUSE_STATE_STAT))}
if ( input$X_WAREHOUSE_STATE_STAT == 'quantile'  & input$X_WAREHOUSE_STATE_FILL != 'volume') { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_WAREHOUSE_STATE_QUA))
 Y_QUA <- input$X_WAREHOUSE_STATE_QUA * 100
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}

if ( input$X_WAREHOUSE_STATE_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

colnames(XT_GR) <-c(XS_REGION,'VAL')
 
if ( input$X_WAREHOUSE_STATE_COL ) {
 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
} else {
 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
}
################################################################
XTM <- left_join(SHP_WOJ, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
if ( input$X_WAREHOUSE_STATE_LOG ) {
  XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
} else {
 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
}
if ( input$X_WAREHOUSE_STATE_FILL == 'volume' ) {
LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
} else {
LEG_TIT <- toTitleCase(input$X_WAREHOUSE_STATE_STAT) + Y_QUA + ' of ' + tolower(input$X_WAREHOUSE_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
}
ZM <- XM + labs(fill=LEG_TIT)
###___<ADDING TEXT>___###
if ( ( input$X_WAREHOUSE_STATE_TEXT ) ) {
XT_POINTS <- sf::st_point_on_surface(XTM)
#XT_POINTS_MAX <- XT_POINTS_0[which.max(XT_POINTS_0[[Y_FILL]]),]
#XT_POINTS_CHN <- XT_POINTS_0[XT_POINTS_0[['su_a3']]=='CHN',]
#XT_POINTS  <-  rbind( XT_POINTS_CHN,XT_POINTS_MAX )
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- NUM2K_XV(XT_POINTS[['VAL']])
POINTS_TEXT_LEGEND  <-  'Displaying value ' + ' ' + Y_FILL
ZM <- ZM +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
}
###___</ADDING TEXT>___###
return( ZM )
})
#########___</renderPlot>___###################################################
################################################################################

################################################################################
##########___<WAREHOUSE_STATE_TAB>___#####___<renderDataTable>___#################
output$WAREHOUSE_STATE_TAB = renderDataTable({
XS_PROPERTY_TYPE = 'WAREHOUSE'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'STATE'
Y_FILL <- gsub(' ','_',toupper(input$X_WAREHOUSE_STATE_FILL))
if ( input$X_WAREHOUSE_STATE_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_WAREHOUSE_STATE_STAT == 'interquartile range'  & input$X_WAREHOUSE_STATE_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_WAREHOUSE_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_WAREHOUSE_STATE_STAT))}
if ( input$X_WAREHOUSE_STATE_STAT == 'quantile' & input$X_WAREHOUSE_STATE_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_WAREHOUSE_STATE_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_WAREHOUSE_STATE_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='STATE')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</WAREHOUSE_STATE_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_WAREHOUSE>_<renderUI>___################################
output$TABLE_TEXT_WAREHOUSE_STATE <- renderUI({
XS_PROPERTY_TYPE = 'WAREHOUSE'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_WAREHOUSE_STATE_STAT == 'quantile'  & input$X_WAREHOUSE_STATE_FILL != 'volume') { Y_QUA <- input$X_WAREHOUSE_STATE_QUA * 100 }

if ( input$X_WAREHOUSE_STATE_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_WAREHOUSE_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_WAREHOUSE_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_WAREHOUSE>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_WAREHOUSE>_<renderUI>___################################
output$MAIN_TEXT_WAREHOUSE_STATE <- renderUI({
XS_PROPERTY_TYPE = 'WAREHOUSE'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_WAREHOUSE_STATE_STAT == 'quantile'  & input$X_WAREHOUSE_STATE_FILL != 'volume') { Y_QUA <- input$X_WAREHOUSE_STATE_QUA * 100 }
if ( input$X_WAREHOUSE_STATE_FILL == 'volume') { 
 ZS <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION)
 } else {
ZS <- tools::toTitleCase(input$X_WAREHOUSE_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_WAREHOUSE_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION)
}
if ( input$X_WAREHOUSE_STATE_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
if ( input$X_WAREHOUSE_STATE_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
if ( input$X_WAREHOUSE_STATE_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
if ( input$X_WAREHOUSE_STATE_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_WAREHOUSE_STATE_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_WAREHOUSE_STATE_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_WAREHOUSE>_</renderUI>___##############################
################################################################################



################################################################################
################################___</WAREHOUSE_STATE>___############################
################################################################################

################################################################################
##########___<WAREHOUSE_COUNTY_MAP>___#####___<renderPlot>___#######################
output$WAREHOUSE_COUNTY_MAP  <-  renderPlot({
XS_PROPERTY_TYPE = 'WAREHOUSE'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'COUNTY'
Y_QUA <- ''
Y_REGION_TYPE_FOR_COUNTY <- input$X_WAREHOUSE_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
################################################################
if (input$X_WAREHOUSE_COUNTY_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_WAREHOUSE_COUNTY_COLOR }
if (input$X_WAREHOUSE_COUNTY_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
if (input$X_WAREHOUSE_COUNTY_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }

Y_FILL <- gsub(' ','_',toupper(input$X_WAREHOUSE_COUNTY_FILL))

if ( input$X_WAREHOUSE_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE}
if ( input$X_WAREHOUSE_COUNTY_STAT == 'interquartile range'  & input$X_WAREHOUSE_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_WAREHOUSE_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_WAREHOUSE_COUNTY_STAT))}
if ( input$X_WAREHOUSE_COUNTY_STAT == 'quantile'  & input$X_WAREHOUSE_COUNTY_FILL != 'volume') { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_WAREHOUSE_COUNTY_QUA))
 Y_QUA <- input$X_WAREHOUSE_COUNTY_QUA * 100
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
if ( input$X_WAREHOUSE_COUNTY_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

colnames(XT_GR) <-c(XS_REGION,'VAL')

if ( input$X_WAREHOUSE_COUNTY_COL ) {
 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
} else {
 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
}
################################################################
XTM <- left_join(SHP_POW, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XTM <- XTM[XTM$WOJ %in% Y_REGION_TYPE_FOR_COUNTY,] 
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}
################################################################
if ( input$X_WAREHOUSE_COUNTY_LOG ) {
  XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
} else {
 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
}
if ( input$X_WAREHOUSE_COUNTY_FILL == 'volume' ) {
LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
} else {
LEG_TIT <- toTitleCase(input$X_WAREHOUSE_COUNTY_STAT) + Y_QUA + ' of ' + tolower(input$X_WAREHOUSE_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
}
ZM <- XM + labs(fill=LEG_TIT)
###___<ADDING TEXT>___###
if ( ( input$X_WAREHOUSE_COUNTY_TEXT ) ) {
XT_POINTS_0 <- sf::st_point_on_surface(XTM)
Y_WAREHOUSE_COUNTY_TOP_NUM <- input$Y_WAREHOUSE_COUNTY_TEXT_NUM
XV_TOP <- sapply( seq.int( Y_WAREHOUSE_COUNTY_TOP_NUM) , function(X) NTH(as.double(na.omit(XT_POINTS_0[['VAL']])), X, descending = T))
XT_POINTS  <-  XT_POINTS_0[XT_POINTS_0[['VAL']]  %in% XV_TOP,]
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- SUB(XT_POINTS$JPT_NAZWA_,8,10) + ': ' + NUM2K_XV(XT_POINTS[['VAL']])
POINTS_TEXT_LEGEND  <-  'Displaying value for ' + XV_TOP + ' amounts by choosen regoin.'
ZM <- ZM +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
}
return( ZM )
})
##########___</WAREHOUSE_COUNTY_MAP>___#####___</renderPlot>___####################
################################################################################

################################################################################
##########___<WAREHOUSE_COUNTY_TAB>___#####___<renderDataTable>___#################
output$WAREHOUSE_COUNTY_TAB = renderDataTable({
XS_PROPERTY_TYPE = 'WAREHOUSE'
Y_REGION_TYPE_FOR_COUNTY <- input$X_WAREHOUSE_COUNTY_REGION_TYPE
XS_REGION = 'COUNTY'
#XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
YT <- XT[PROPERTY_TYPE == XS_PROPERTY_TYPE & STATE == Y_REGION_TYPE_FOR_COUNTY ]
} else {
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_WAREHOUSE_COUNTY_FILL))
if ( input$X_WAREHOUSE_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_WAREHOUSE_COUNTY_STAT == 'interquartile range'  & input$X_WAREHOUSE_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_WAREHOUSE_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_WAREHOUSE_COUNTY_STAT))}
if ( input$X_WAREHOUSE_COUNTY_STAT == 'quantile' & input$X_WAREHOUSE_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_WAREHOUSE_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_WAREHOUSE_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</WAREHOUSE_COUNTY_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_WAREHOUSE>_<renderUI>___################################
output$TABLE_TEXT_WAREHOUSE_COUNTY <- renderUI({
XS_PROPERTY_TYPE = 'WAREHOUSE'
Y_QUA = ''
XS_REGION = 'COUNTY'
Y_REGION_TYPE_FOR_COUNTY <- input$X_WAREHOUSE_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}


if ( input$X_WAREHOUSE_COUNTY_STAT == 'quantile'  & input$X_WAREHOUSE_COUNTY_FILL != 'volume') { Y_QUA <- input$X_WAREHOUSE_COUNTY_QUA * 100 }

if ( input$X_WAREHOUSE_COUNTY_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_WAREHOUSE_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_WAREHOUSE_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_WAREHOUSE>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_WAREHOUSE>_<renderUI>___################################
output$MAIN_TEXT_WAREHOUSE_COUNTY <- renderUI({
XS_PROPERTY_TYPE = 'WAREHOUSE'
XS_REGION = 'COUNTY'
Y_QUA = ''
Y_REGION_TYPE_FOR_COUNTY <- input$X_WAREHOUSE_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}
if ( input$X_WAREHOUSE_COUNTY_STAT == 'quantile'  & input$X_WAREHOUSE_COUNTY_FILL != 'volume') { Y_QUA <- input$X_WAREHOUSE_COUNTY_QUA * 100 }

if ( input$X_WAREHOUSE_COUNTY_FILL == 'volume') { 
 ZS <-  'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION)
 } else {
ZS <- tools::toTitleCase(input$X_WAREHOUSE_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_WAREHOUSE_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION)
}

if ( input$X_WAREHOUSE_COUNTY_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
if ( input$X_WAREHOUSE_COUNTY_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
if ( input$X_WAREHOUSE_COUNTY_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
if ( input$X_WAREHOUSE_COUNTY_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_WAREHOUSE_COUNTY_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_WAREHOUSE_COUNTY_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }


ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_WAREHOUSE>_</renderUI>___##############################
################################################################################

################################################################################
############___<observe>_<sliderInput>___######################################
observe({
XS_REGION = 'COUNTY'
XS_PROPERTY_TYPE = 'WAREHOUSE'
Y_REGION_TYPE_FOR_COUNTY <- input$X_WAREHOUSE_COUNTY_REGION_TYPE
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 YT <- YT[YT$STATE %in% Y_REGION_TYPE_FOR_COUNTY,] 
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_WAREHOUSE_COUNTY_FILL))
if ( input$X_WAREHOUSE_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_WAREHOUSE_COUNTY_STAT == 'interquartile range'  & input$X_WAREHOUSE_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_WAREHOUSE_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_WAREHOUSE_COUNTY_STAT))}
if ( input$X_WAREHOUSE_COUNTY_STAT == 'quantile' & input$X_WAREHOUSE_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_WAREHOUSE_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_WAREHOUSE_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
val <- nrow( XT_GR )
updateSliderInput(session, "Y_WAREHOUSE_COUNTY_TEXT_NUM", value = 5,
min = 1, max = val, step = 1)
})
###########___</observe>_<sliderInput>___######################################
################################################################################

##############################################################################################################
##############################################################################################################
################################___</WAREHOUSE_STATE>___###########################################################
##############################################################################################################
##############################################################################################################


















##############################################################################################################
##############################################################################################################
################################___<GARAGE_STATE>___###########################################################
##############################################################################################################
##############################################################################################################

################################################################################
##########___<GARAGE_STATE_MAP>___#####___<renderPlot>___#######################
output$GARAGE_STATE_MAP  <-  renderPlot({
XS_PROPERTY_TYPE = 'GARAGE'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'STATE'
Y_QUA <- ''
################################################################
if (input$X_GARAGE_STATE_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_GARAGE_STATE_COLOR }
if (input$X_GARAGE_STATE_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
if (input$X_GARAGE_STATE_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }
Y_FILL <- gsub(' ','_',toupper(input$X_GARAGE_STATE_FILL))
if ( input$X_GARAGE_STATE_STAT == 'mode' ) { Y_FUN <- MODE}
if ( input$X_GARAGE_STATE_STAT == 'interquartile range'  & input$X_GARAGE_STATE_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_GARAGE_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_GARAGE_STATE_STAT))}
if ( input$X_GARAGE_STATE_STAT == 'quantile'  & input$X_GARAGE_STATE_FILL != 'volume') { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_GARAGE_STATE_QUA))
 Y_QUA <- input$X_GARAGE_STATE_QUA * 100
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}

if ( input$X_GARAGE_STATE_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

colnames(XT_GR) <-c(XS_REGION,'VAL')
 
if ( input$X_GARAGE_STATE_COL ) {
 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
} else {
 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
}
################################################################
XTM <- left_join(SHP_WOJ, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
if ( input$X_GARAGE_STATE_LOG ) {
  XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
} else {
 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
}
if ( input$X_GARAGE_STATE_FILL == 'volume' ) {
LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
} else {
LEG_TIT <- toTitleCase(input$X_GARAGE_STATE_STAT) + Y_QUA + ' of ' + tolower(input$X_GARAGE_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
}
ZM <- XM + labs(fill=LEG_TIT)
###___<ADDING TEXT>___###
if ( ( input$X_GARAGE_STATE_TEXT ) ) {
XT_POINTS <- sf::st_point_on_surface(XTM)
#XT_POINTS_MAX <- XT_POINTS_0[which.max(XT_POINTS_0[[Y_FILL]]),]
#XT_POINTS_CHN <- XT_POINTS_0[XT_POINTS_0[['su_a3']]=='CHN',]
#XT_POINTS  <-  rbind( XT_POINTS_CHN,XT_POINTS_MAX )
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- NUM2K_XV(XT_POINTS[['VAL']])
POINTS_TEXT_LEGEND  <-  'Displaying value ' + ' ' + Y_FILL
ZM <- ZM +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
}
###___</ADDING TEXT>___###
return( ZM )
})
#########___</renderPlot>___###################################################
################################################################################

################################################################################
##########___<GARAGE_STATE_TAB>___#####___<renderDataTable>___#################
output$GARAGE_STATE_TAB = renderDataTable({
XS_PROPERTY_TYPE = 'GARAGE'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'STATE'
Y_FILL <- gsub(' ','_',toupper(input$X_GARAGE_STATE_FILL))
if ( input$X_GARAGE_STATE_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_GARAGE_STATE_STAT == 'interquartile range'  & input$X_GARAGE_STATE_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_GARAGE_STATE_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_GARAGE_STATE_STAT))}
if ( input$X_GARAGE_STATE_STAT == 'quantile' & input$X_GARAGE_STATE_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_GARAGE_STATE_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_GARAGE_STATE_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='STATE')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</GARAGE_STATE_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_GARAGE>_<renderUI>___################################
output$TABLE_TEXT_GARAGE_STATE <- renderUI({
XS_PROPERTY_TYPE = 'GARAGE'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_GARAGE_STATE_STAT == 'quantile'  & input$X_GARAGE_STATE_FILL != 'volume') { Y_QUA <- input$X_GARAGE_STATE_QUA * 100 }

if ( input$X_GARAGE_STATE_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_GARAGE_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_GARAGE_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_GARAGE>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_GARAGE>_<renderUI>___################################
output$MAIN_TEXT_GARAGE_STATE <- renderUI({
XS_PROPERTY_TYPE = 'GARAGE'
Y_QUA = ''
XS_REGION = 'STATE'
if ( input$X_GARAGE_STATE_STAT == 'quantile'  & input$X_GARAGE_STATE_FILL != 'volume') { Y_QUA <- input$X_GARAGE_STATE_QUA * 100 }
if ( input$X_GARAGE_STATE_FILL == 'volume') { 
 ZS <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION)
 } else {
ZS <- tools::toTitleCase(input$X_GARAGE_STATE_STAT) + Y_QUA + 
' of ' + tolower(input$X_GARAGE_STATE_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland wih volume by ' + tolower(XS_REGION)
}
if ( input$X_GARAGE_STATE_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
if ( input$X_GARAGE_STATE_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
if ( input$X_GARAGE_STATE_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
if ( input$X_GARAGE_STATE_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_GARAGE_STATE_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_GARAGE_STATE_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_GARAGE>_</renderUI>___##############################
################################################################################



################################################################################
################################___</GARAGE_STATE>___############################
################################################################################

################################################################################
##########___<GARAGE_COUNTY_MAP>___#####___<renderPlot>___#######################
output$GARAGE_COUNTY_MAP  <-  renderPlot({
XS_PROPERTY_TYPE = 'GARAGE'
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
XS_REGION = 'COUNTY'
Y_QUA <- ''
Y_REGION_TYPE_FOR_COUNTY <- input$X_GARAGE_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
################################################################
if (input$X_GARAGE_COUNTY_COLOR %in% XV_COL_VIR ) {Y_COLOR <- 'viridis::' + input$X_GARAGE_COUNTY_COLOR }
if (input$X_GARAGE_COUNTY_COLOR == 'terrain' ) {Y_COLOR <- 'grDevices::terrain.colors' }
if (input$X_GARAGE_COUNTY_COLOR == 'gray') {Y_COLOR <- 'grDevices::gray.colors' }

Y_FILL <- gsub(' ','_',toupper(input$X_GARAGE_COUNTY_FILL))

if ( input$X_GARAGE_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE}
if ( input$X_GARAGE_COUNTY_STAT == 'interquartile range'  & input$X_GARAGE_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_GARAGE_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_GARAGE_COUNTY_STAT))}
if ( input$X_GARAGE_COUNTY_STAT == 'quantile'  & input$X_GARAGE_COUNTY_FILL != 'volume') { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_GARAGE_COUNTY_QUA))
 Y_QUA <- input$X_GARAGE_COUNTY_QUA * 100
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
if ( input$X_GARAGE_COUNTY_FILL == 'volume' ) {XT_GR <- GR(YT,XBY=XS_REGION)}

colnames(XT_GR) <-c(XS_REGION,'VAL')

if ( input$X_GARAGE_COUNTY_COL ) {
 YV_COLORS <- (eval(parse(text=Y_COLOR))(nrow(XT_GR)))
} else {
 YV_COLORS <- rev(eval(parse(text=Y_COLOR))(nrow(XT_GR)))
}
################################################################
XTM <- left_join(SHP_POW, XT_GR, by = c('JPT_NAZWA_' = XS_REGION ))
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XTM <- XTM[XTM$WOJ %in% Y_REGION_TYPE_FOR_COUNTY,] 
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}
################################################################
if ( input$X_GARAGE_COUNTY_LOG ) {
  XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS,Y_TRANS='log1p')
} else {
 XM <- OTODOM_MAP(XTM,Y_FILL='VAL',YV_COLORS=YV_COLORS)
}
if ( input$X_GARAGE_COUNTY_FILL == 'volume' ) {
LEG_TIT <- 'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in Poland by ' + tolower(XS_REGION) + '.'
} else {
LEG_TIT <- toTitleCase(input$X_GARAGE_COUNTY_STAT) + Y_QUA + ' of ' + tolower(input$X_GARAGE_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
}
ZM <- XM + labs(fill=LEG_TIT)
###___<ADDING TEXT>___###
if ( ( input$X_GARAGE_COUNTY_TEXT ) ) {
XT_POINTS_0 <- sf::st_point_on_surface(XTM)
Y_GARAGE_COUNTY_TOP_NUM <- input$Y_GARAGE_COUNTY_TEXT_NUM
XV_TOP <- sapply( seq.int( Y_GARAGE_COUNTY_TOP_NUM) , function(X) NTH(as.double(na.omit(XT_POINTS_0[['VAL']])), X, descending = T))
XT_POINTS  <-  XT_POINTS_0[XT_POINTS_0[['VAL']]  %in% XV_TOP,]
XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
XT_COORDS$POINTS_TEXT <- SUB(XT_POINTS$JPT_NAZWA_,8,10) + ': ' + NUM2K_XV(XT_POINTS[['VAL']])
POINTS_TEXT_LEGEND  <-  'Displaying value for ' + XV_TOP + ' amounts by choosen regoin.'
ZM <- ZM +
 geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
 geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') 
# + labs(caption = POINTS_TEXT_LEGEND) + theme(plot.caption = element_text(color = 'black', face="bold"))
}
return( ZM )
})
##########___</GARAGE_COUNTY_MAP>___#####___</renderPlot>___####################
################################################################################

################################################################################
##########___<GARAGE_COUNTY_TAB>___#####___<renderDataTable>___#################
output$GARAGE_COUNTY_TAB = renderDataTable({
XS_REGION = 'COUNTY'
XS_PROPERTY_TYPE = 'GARAGE'
Y_REGION_TYPE_FOR_COUNTY <- input$X_GARAGE_COUNTY_REGION_TYPE
#XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
YT <- XT[PROPERTY_TYPE == XS_PROPERTY_TYPE & STATE == Y_REGION_TYPE_FOR_COUNTY ]
} else {
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_GARAGE_COUNTY_FILL))
if ( input$X_GARAGE_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_GARAGE_COUNTY_STAT == 'interquartile range'  & input$X_GARAGE_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_GARAGE_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_GARAGE_COUNTY_STAT))}
if ( input$X_GARAGE_COUNTY_STAT == 'quantile' & input$X_GARAGE_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_GARAGE_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_GARAGE_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
XT_GR  <- CHANGE_COLNAME(XT_GR,'N','Amount')
ZT <- DT::datatable(XT_GR,options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
return( ZT )
})
##########___</GARAGE_COUNTY_TAB>___#####___</renderDataTable>___###############
################################################################################

################################################################################
############___<TABLE_TEXT_GARAGE>_<renderUI>___################################
output$TABLE_TEXT_GARAGE_COUNTY <- renderUI({
XS_PROPERTY_TYPE = 'GARAGE'
Y_QUA = ''
XS_REGION = 'COUNTY'
Y_REGION_TYPE_FOR_COUNTY <- input$X_GARAGE_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}


if ( input$X_GARAGE_COUNTY_STAT == 'quantile'  & input$X_GARAGE_COUNTY_FILL != 'volume') { Y_QUA <- input$X_GARAGE_COUNTY_QUA * 100 }

if ( input$X_GARAGE_COUNTY_FILL == 'volume') { 
 ZS <- 'Table of volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION) + '.'
 } else {
ZS <- 'Table of ' +
(input$X_GARAGE_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_GARAGE_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION) + '.'
}
ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_GARAGE>_</renderUI>___##############################
################################################################################

################################################################################
############___<TABLE_TEXT_GARAGE>_<renderUI>___################################
output$MAIN_TEXT_GARAGE_COUNTY <- renderUI({
XS_PROPERTY_TYPE = 'GARAGE'
Y_QUA = ''
XS_REGION = 'COUNTY'
Y_REGION_TYPE_FOR_COUNTY <- input$X_GARAGE_COUNTY_REGION_TYPE
XS_REGION_NAME  <- 'Poland'
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 XS_REGION_NAME <- Y_REGION_TYPE_FOR_COUNTY + ' state'
}
if ( input$X_GARAGE_COUNTY_STAT == 'quantile'  & input$X_GARAGE_COUNTY_FILL != 'volume') { Y_QUA <- input$X_GARAGE_COUNTY_QUA * 100 }

if ( input$X_GARAGE_COUNTY_FILL == 'volume') { 
 ZS <-  'Volume of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' by ' + tolower(XS_REGION)
 } else {
ZS <- tools::toTitleCase(input$X_GARAGE_COUNTY_STAT) + Y_QUA + 
' of ' + tolower(input$X_GARAGE_COUNTY_FILL) + ' of ' + tolower(XS_PROPERTY_TYPE) + 's' + ' in ' + XS_REGION_NAME + ' wih volume by ' + tolower(XS_REGION)
}

if ( input$X_GARAGE_COUNTY_FILL == 'volume') { ZS  <-  ZS + ' (as number of ads).' }
if ( input$X_GARAGE_COUNTY_FILL == 'rooms') { ZS <- ZS + ' (number of rooms per property).' }
if ( input$X_GARAGE_COUNTY_FILL == 'price') { ZS <- ZS + ' (in polish zloty).' }
if ( input$X_GARAGE_COUNTY_FILL == 'area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_GARAGE_COUNTY_FILL == 'land area') { ZS <- ZS + ' (in square meters).' }
if ( input$X_GARAGE_COUNTY_FILL == 'price per area') { ZS <- ZS + ' (in PLN/m2).' }


ZS <-  '<h3>' + ZS 
HTML(ZS)
})
############___</TABLE_TEXT_GARAGE>_</renderUI>___##############################
################################################################################

################################################################################
############___<observe>_<sliderInput>___######################################
observe({
XS_REGION = 'COUNTY'
XS_PROPERTY_TYPE = 'GARAGE'
Y_REGION_TYPE_FOR_COUNTY <- input$X_GARAGE_COUNTY_REGION_TYPE
YT <- XT[PROPERTY_TYPE== XS_PROPERTY_TYPE ]
if ( Y_REGION_TYPE_FOR_COUNTY != 'Poland'  ) {
 YT <- YT[YT$STATE %in% Y_REGION_TYPE_FOR_COUNTY,] 
}
XS_REGION = 'COUNTY'
Y_FILL <- gsub(' ','_',toupper(input$X_GARAGE_COUNTY_FILL))
if ( input$X_GARAGE_COUNTY_STAT == 'mode' ) { Y_FUN <- MODE} 
if ( input$X_GARAGE_COUNTY_STAT == 'interquartile range'  & input$X_GARAGE_COUNTY_FILL != 'volume') { Y_FUN <- IQR}
if ( input$X_GARAGE_COUNTY_STAT %in% XV_STAT_SAME_NAME ) { Y_FUN <- eval(parse(text=input$X_GARAGE_COUNTY_STAT))}
if ( input$X_GARAGE_COUNTY_STAT == 'quantile' & input$X_GARAGE_COUNTY_FILL != 'volume' ) { 
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,function( X ) quantile(X,input$X_GARAGE_COUNTY_QUA))
} else {
 XT_GR <- GR(YT,XBY=XS_REGION,XWHAT=Y_FILL,FUN=Y_FUN)
}
################################################################
XT_VOL <- GR(YT,XBY=XS_REGION)
if ( input$X_GARAGE_COUNTY_FILL != 'volume' ) {
XT_GR <- merge(XT_GR,XT_VOL,by='COUNTY')
XT_GR <- XT_GR[o(- XT_GR[[Y_FILL]],-N )]
} else {
XT_GR <- XT_VOL
XT_GR <- XT_GR[o(- XT_GR[['N']] )]
}
val <- nrow( XT_GR )
updateSliderInput(session, "Y_GARAGE_COUNTY_TEXT_NUM", value = 5,
min = 1, max = val, step = 1)
})
###########___</observe>_<sliderInput>___######################################
################################################################################

##############################################################################################################
##############################################################################################################
################################___</GARAGE_STATE>___###########################################################
##############################################################################################################
##############################################################################################################


}