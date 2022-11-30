ACR0 <- function(XS,XN=1) {
	if (any( grepl( ' ',XS ) )) {
		XS <- strsplit(XS, " ")
	} else {
		xs <- strsplit(XS, "_")
	}
	sapply(xs, function(x) toupper(paste(substring(x, 1, XN), collapse = "")))
}
ACR <- function(X,X_BY=' ',XN=1) {
	X <- strsplit(X, X_BY)
sapply(X, function(x) toupper(paste(substring(x, 1, XN), collapse = "")))
}
################################___ADD_AFT_NTH___###############################
ADD_AFT_NTH <- function(XV,XP,ADD_AFTER=1) {
	ZV <- vector()
	k <- 1
	for ( i in seq_along(XV)) {
		ZV[k] <- XV[i]
		if ( i %% ( ADD_AFTER ) ==0 ) {
		ZV[k+1] <- XP
		k <- k+1
		}
		k <- k+1
	}
ZV	
}
#################################___ADD_COLOR___################################
ADD_COLOR <- function(XS){
	paste0('<font color=#ffff00>',XS,'</font>')
}
##################################___ADD_ROW___#################################
ADD_ROW <- function( XT,XV ) {
	if ( length(XV)!=1 ) {
		ZT <- rbindlist(list(XT, as.list(XV)))
	} else {
		ZT <- rbindlist(list(
			XT,
			as.list(rep(XV,ncol(XT)))
			))
	}
ZT
}
################################___ADD_TO_LIST___###############################
ADD_TO_LIST <- function(XL,X) {
  XL.LEN <- LEN(XL)
  XL[[XL.LEN + 1]] <- X
	XL
}
##############################___AGGREGATE_TABLE___#############################
AGGREGATE_TABLE <- function(XT,...) {
	XV_FUN_NAME <- tolower(sapply(match.call(expand.dots=FALSE)$`...`,CH))
	if (length(XV_FUN_NAME)==0L) stop('There is no function.')
	XV_FUN_NAME <- gsub('avg','mean',XV_FUN_NAME)
	DT <- data.table::data.table
	XT.ROWNAMES <- NA
	if (!suppressWarnings(isTRUE(all(as.numeric(rownames(XT))==seq(nrow(XT)))))) { #CHECKING IF ROWNAMES EXISTS
		XT.ROWNAMES <- rownames(XT)
		XT <- DT(XROWNAMES=rownames(XT),XT)
	} else {
		XT <- DT(XT)	
	}
	XT.COLNAMES <- colnames(XT)
	XT.NROW <- nrow(XT)
	XT.CL <- sapply(XT,class)
	################################################################
	XW_NUM <- which(XT.CL %in% c('numeric','integer'))
	XT_NUM <- XT[,XW_NUM,with=FALSE]
	XL_FUN_VAL <- list()
	for ( i in seq_along(XV_FUN_NAME)) {
		X_FUN_NAME <- get(XV_FUN_NAME[i])
		XL_FUN_VAL[[i]] <- apply(XT_NUM,2L,function(X) X_FUN_NAME(X) )
	}
	XT_FUN_VAL <- do.call(rbind,XL_FUN_VAL)
	remove(XL_FUN_VAL)
	XV_FUN_NAME <- toupper(XV_FUN_NAME)
	################################################################
	XW_NOT_NUM <- which(!XT.CL  %in%  c('numeric','integer'))
	if (length(XW_NOT_NUM)==0) {
		XV_FUN_COL <- c(rep.int(c(rep('-')),nrow(XT)),XV_FUN_NAME)
		XT <- rbind(XT,XT_FUN_VAL)
		XT <- data.table::data.table(FUN=XV_FUN_COL,XT)
	} else if (length(XW_NOT_NUM)==1) {
		XT_FUN_VAL <- DT(XV_FUN_NAME,XT_FUN_VAL)
		setnames(XT_FUN_VAL,'XV_FUN_NAME',XT.COLNAMES[XW_NOT_NUM])
		XT <- rbind(XT,XT_FUN_VAL)[,..XT.COLNAMES]
	} else {
		XT_DESCRIBTION <- DT(toupper(XV_FUN_NAME),matrix('-',length(XV_FUN_NAME),length(XW_NOT_NUM)-1))
		colnames(XT_DESCRIBTION) <- colnames(XT)[XW_NOT_NUM]
		XT_FUN_VAL <- DT(XT_FUN_VAL,XT_DESCRIBTION)[,..XT.COLNAMES]
		XT <- rbind(XT,XT_FUN_VAL)
	}
	if ( !all(is.na(XT.ROWNAMES)) ) {
		XROWNAMES <- make.names(XT$XROWNAMES,unique=T)
		XT <- as.data.frame(XT[,-1])
		rownames(XT) <- XROWNAMES
	}
XT
}
#################################___AGR_WHICH___################################
AGR_WHICH <- function( XT,XC_BY,XC_AGR,AGR ) {
	XQUERY <- paste0('SELECT * FROM XT JOIN (SELECT ',XC_BY,',',AGR,'(',XC_AGR,') AS ',XC_AGR,' FROM XT GROUP BY ',XC_BY,') USING (',XC_BY,',',XC_AGR,')')
CLEAN_TABLE(sqldf(XQUERY))
}
####################################___AS___####################################
AS <- function(X, Y) {
	X.CH <- RMQ(deparse(substitute(X)))
	assign(X.CH, Y, envir = .GlobalEnv)
}
###############################___AS_FAKE_NULL___###############################
AS_FAKE_NULL <- function(X) {
	if (IS_FAKE_NULL( X )) {
	Z <- 1
	} else {
	Z <- 0
	}
Z
}
##################################___AS_HTML___#################################
AS_HTML <- function(XV){
	writeLines(XV,'HTML_TEMP.HTML',useBytes = TRUE)
	readLines('HTML_TEMP.HTML',encoding = "UTF-8")
}
##################################___AS_NUM___##################################
AS_NUM <- function(X) {
	if (is.vector(X)) {
		X <- SAP(X, AS_NUM_0)
	}
	else if (IS_TABLE(X)) {
		XT <- DT(X)
		XCOLNAMES0 <- colnames(XT)
		XBT <- AP(XT, 1:2, IS_NUM)
		XV_COL_NUM <- W(AP(XBT, 2, ALL))
		XV_COL_NUM_NOT <- W(!AP(XBT, 2, ALL))
		XT_NUM <- XT[, XV_COL_NUM, with = FALSE]
		XT_NUM <- AP(XT_NUM, 1:2, function(X) as.numeric(AS_NUM_0(X)))
		XT_NUM_NOT <- XT[, XV_COL_NUM_NOT, with = FALSE]
		XT <- DT(XT_NUM, XT_NUM_NOT)
		X <- XT[, XCOLNAMES0, with = FALSE]
		rm(XT)
	}
	else {
		X <- "NOT VECTOR OR TABLE"
	}
X
}
#################################___AS_NUM_0___#################################
AS_NUM_0 <- function(X) {
	Z <- GSUB(X, ",", ".")
	Z <- GSUB(Z, " ", "")
	if (IS_NUM(Z)) {
		X <- as.numeric(as.character(Z))
	}
	X
}
####################################___ASD___###################################
ASD <- function(X,Y) {
	#X <- XNAME
	#RM(X,XL)
	#XL <- 1
	#X <- 'XL$A$B'
	X <- DSR(X)
	X.SPL <- SPL(X,'\\$')
	X1 <- X.SPL[1]
	X1 <- P(X1,'$',X.SPL[2])
	XEPT <- P(X1,' <- L()')
	XTC <- TC(EPT(XEPT))
	if(!XTC$OK) {
		XEPT <- P(X.SPL[1],'<-L()')
		EPT(XEPT)
		XEPT <- P(X1,' <- L()')
		EPT(XEPT)
	}
	X1 <- P(X1,'$',X.SPL[LEN(X.SPL)])
	if ( IS_CH(Y) ) {
		XEPT <- P(X1,' <- "',Y,'"')	
	} else {
		XEPT <- P(X1,' <- ',Y)	
	}
EPT(XEPT)
}
####################################___ASL___###################################
ASL <- function(XL,X) {
	XL.CH <- RMQ(deparse(substitute(XL)))
	XL <- TC(eval( parse(text = XL.CH) , envir=.GlobalEnv))
	if ( EX(XL) ) {
		XL[[LEN(XL)+1]] <- X
		AS(XL,XL)
	} else {
		AS(XL,list(X))
	}
}
###################################___ASL0___###################################
ASL0 <- function(XL,X) {
	XL.CH <- RMQ(deparse(substitute(XL)))
	XL <- TC(eval( parse(text = XL.CH) , envir=.GlobalEnv))
	if ( XL$OK ) {
		
		AS(XL.CH,L(XL$VALUE,X))
	} else {
		AS(XL.CH,X)
	}
}
##################################___ASSIGN___##################################
ASSIGN <- function(XV1_OLD,XV_NEW){
	assign(deparse(substitute(x)), XV2, env=.GlobalEnv)
}
################################___ASSIGN_MANY___###############################
ASSIGN_MANY <- function(..., values, envir=parent.frame()) {
  vars <- as.character(substitute(...()))
  values <- rep(values, length.out=length(vars))
  for(i in seq_along(vars)) {
  assign(vars[[i]], values[[i]], envir)
  }
}
#################################___ASSIGN_NA___################################
ASSIGN_NA <- function(X,XY) {
	if (IS_TABLE(X)) {
		X <- ADF(X)
		XW <- W(is.na(XT),arr.ind=T)
		X[XW] <- XY
		X <- DT(X)
	} else {
		X[W(is.na(X))] <- XY
	}
X
}
####################################___AVQ___###################################
AVQ <- function(X,XN=3) {
	R(AVG(QU(X,XN)))
}
#####################################___B___####################################
B <- function(XS,X_START,X_END) {
unname(sapply(XS,function(X) SUB0(X,X_START,X_END)))
}#zmienic SUB0
####################################___B0___####################################
B0 <- function(XS,X_START,X_END) {
if ( is.numeric(X_START) & is.numeric(X_END)   ) {
ZS <- stri_sub(XS,X_START,X_END)
} else {
if ( is.numeric(X_START)) {
XN_START <- X_START
} else {
XN_START <- as.numeric(stri_locate_first_regex(XS,X_START)[1,2]) + 1
}
XS <- stri_sub(XS,XN_START,-1)
if ( is.numeric(X_END)) {
XN_END <- X_END
} else {
XN_END <- as.numeric( stri_locate_first_regex(XS,X_END)[1,1]) - 1
}
ZS <- stri_sub(XS,1,XN_END)
}
ZS
}
#################################___CAP_CASE___#################################
CAP_CASE <- function(x) {
	CAP_CASE_0 <- function(x) {
	x <- LC( x )
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
  sep="", collapse=" ")
	}
SAP( x,CAP_CASE_0 )
}
###################################___catt___###################################
catt <- function(input = NULL){
  cat(paste0(paste(rep("\t", ntab), collapse = ""), input))
}
##############################___CHANGE_COLNAME___##############################
CHANGE_COLNAME <- function(XT,XC_OLD,XC_NEW ){
	names(XT)[names(XT) == XC_OLD] <- XC_NEW
XT
}
################################___CHECK_HTML___################################
CHECK_HTML <- function( XU ) {
	if ( all(class(XU) == c("xml_document","xml_node"))) {
	XRH <- XU
	} else {
	XRH <- read_html(XU)
	}
	XHN <- html_nodes(XRH, 'body') %>% html_text(  )
	XHN <- gsub("[[:punct:]]", " ",XHN)
	XHN <- (gsub("\t+", "\n",XHN))
	XHN <- (gsub("\n+", "\n",XHN))
	XHN <- gsub("\\h+", " ",XHN,perl = TRUE)
	SPL_XHN <- SPL( XHN , "\n" )
	closeAllConnections()
SPL_XHN[SPL_XHN != '']
}
##############################___CHECK_HTML_BODY___#############################
CHECK_HTML_BODY <- function( XU ) {
	if ( all(class(XU) == c("xml_document","xml_node"))) {
	XRH <- XU
	} else {
	XRH <- read_html(XU)
	}
	XHN <- html_nodes(XRH, 'body') %>% html_text(  )
	XHN <- gsub("[[:punct:]]", " ",XHN)
	XHN <- (gsub("\t+", "\n",XHN))
	XHN <- (gsub("\n+", "\n",XHN))
	XHN <- gsub("\\h+", " ",XHN,perl = TRUE)
	SPL_XHN <- SPL( XHN , "\n" )
	closeAllConnections()
SPL_XHN[SPL_XHN != '']
}
#################################___CHECK_ID___#################################
CHECK_ID <- function( XT ) {
	any(stri_detect_regex(colnames(XT),'^ID$'))
}
###########################___CHECK_NUMBER_OF_PAGES___##########################
CHECK_NUMBER_OF_PAGES <- function( XU) {
		if ( all(class(XU) == c("xml_document","xml_node"))) {
  XRH <- XU
  } else {
  XRH <- READ_HTML(XU)
  }
	XV_NUMBER_OF_PAGES <- trimws(html_nodes( XRH,'ul.pager li' )  %>% html_text())
	max(as.numeric(XV_NUMBER_OF_PAGES[IS_NUM(XV_NUMBER_OF_PAGES)]))
}
#################################___CLEAN_DOM___################################
CLEAN_DOM <- function(XS,DOM) {
	XS <- gsub(paste0('<\\s*/\\s*',DOM,'.*?>'),paste0('</',DOM,'>'),XS, ignore.case = T)
return( XS )
}
##############################___CLEAN_DOM_LINE___##############################
CLEAN_DOM_LINE <- function(XS,DOM) {
	XS <- trimws( XS )
	#if ( stri_sub( XS,1,1 ) == '<' & stri_sub( XS,-1,-1 ) == '>' ) {
	XS <- gsub(paste0("<" + DOM + ".*?>"), paste0("<" + DOM + ">"), XS, ignore.case = TRUE)
	#}
	return( XS )
}
############################___CLEAN_HTML_COMMENTS___###########################
CLEAN_HTML_COMMENTS <- function(XRL) {
XV_START <- which(SUB(trimws(XRL),1,4)=='<!--')
XV_END <- which(SUB(trimws(XRL),1,3)=='-->')
for ( i in seq_along(XV_START) ) {
X_START <- 	XV_START[i]
X_END <- XV_END[min(which(XV_END > X_START))]
XV_XRL <- XRL[X_START:X_END]
X_CHECK  <- 	
	any(grepl('<table', XV_XRL,ignore.case = T) ) |
	any(grepl('<div', XV_XRL,ignore.case = T) ) |
	any(grepl('<p ', XV_XRL,ignore.case = T) ) |
	any(grepl('<a ', XV_XRL,ignore.case = T) )
if ( !X_CHECK ) {
	XRL[X_START:X_END]  <- ''
}}
XRL
}
###############################___CLEAN_MEMORY___###############################
CLEAN_MEMORY <- function(N=10) { 
	for (i in 1:N) gc() 
	gc()
	gctorture(FALSE)
}
###############################___CLEAN_STRING___###############################
CLEAN_STRING <- function(XCH,UPPER_CASE=FALSE ) {
	XCH <- gsub("[[:space:]]+", " ",XCH)	
	XCH <- trimws(XCH)
	if (UPPER_CASE) {
		XCH <- stri_trans_toupper(XCH)
	}
XCH
}
##############################___CLEAN_STRING_0___##############################
CLEAN_STRING_0 <- function( XS,UPPER_CASE=FALSE ) {
	XS <- gsub(" +", " ",XS)
	XS <- trimws( XS )
	if (UPPER_CASE) {
		XS <- stri_trans_toupper(XS)
	}
XS
}
##############################___CLEAN_STRING_2___##############################
CLEAN_STRING_2 <- function(XCH,UPPER_CASE=FALSE ) {
	XCH <- GSUB(XCH, "[^[:print:][:cntrl:]]", "")
	XCH <- gsub("[[:space:]]+", " ",XCH)	
	XCH <- trimws(XCH)
	if (UPPER_CASE) {
		XCH <- stri_trans_toupper(XCH)
	}
XCH
}
##########################___CLEAN_STRING_DIACRITIC___##########################
CLEAN_STRING_DIACRITIC <- function( XS ) {
	stri_trans_general(XS,"Latin-ASCII")
}
##########################___CLEAN_STRING_FROM_PUNCT___#########################
CLEAN_STRING_FROM_PUNCT <- function( X ) {
	for ( i in SEQ( XV_PUNCT_0 ) ) {
		X <- GSUB(X,'\\' + XV_PUNCT_0[i] + '+',XV_PUNCT_0[i])
	}
	X <- GSUB(X,'\\\\+','\\\\')
X
}
##########################___CLEAN_STRING_WIKIPEDIA___##########################
CLEAN_STRING_WIKIPEDIA <- function( XS ) {
	XS <- iconv(XS,'utf-8')
	XS <- GSUB(XS,'\\(\\d\\)','')
	XS <- GSUB(XS,'\\[.*?\\]','')
	XS
}
################################___CLEAN_TABLE___###############################
CLEAN_TABLE <- function( XT ) {
	XT <- DT(XT)
	library( janitor )
	XT <- clean_names( XT ,case = "all_caps")
	XT <- AS_NUM( XT )
	for (i in which(sapply(XT, class) == "character")) {
		XT[[i]]  <- CLEAN_STRING(XT[[i]])
	}
XT
}
###############################___CLEAN_TABLE_0___##############################
CLEAN_TABLE_0 <- function( XT ) {
	XT <- DT( XT )
	WHICH_ROW_REMOVE <- vector(  )
	k <- 1
	XCOLNAMES <- colnames( XT )
	for ( i in SEQ( XT ) ) {
		if (ALL(XT[i]==XCOLNAMES) | IS_BLANK_ALL(XT[i]) ) {
			WHICH_ROW_REMOVE[k] <- i
			k<-k+1
		}
	}
XT[!WHICH_ROW_REMOVE]	
}
###########################___CLEAN_TABLE_WIKIPEDIA___##########################
CLEAN_TABLE_WIKIPEDIA <- function(XT,X_ROW_AS_COL=T) {
	if (IS_COL_AS_ROW( XT ) & X_ROW_AS_COL) {
		XT <- ROW_AS_COL(XT)		
	}
	colnames(XT) <- MAKE_NAMES(colnames(XT))
	XT <- AP(XT,1:2,CLEAN_STRING_WIKIPEDIA )
	XT <- AP(XT,1:2,AS_NUM_0)
CLEAN_TABLE(XT)
}
############################___CLEAN_TRIM_BRACKETS___###########################
CLEAN_TRIM_BRACKETS <- function( XS ) {
	XS <- trimws(gsub('<\\s*','<',XS))
	XS <- trimws(gsub('\\s*>','>',XS))
	XS <- trimws(gsub('<\\s*/\\s*','</',XS))
	XS <- trimws(gsub('\\(\\s*','(',XS))
	XS <- trimws(gsub('\\s*))','))',XS))	
	XS <- trimws(gsub('\\{\\s*','{',XS))
	XS <- trimws(gsub('\\s*}','}',XS))
return(XS)
}
###############################___CLEAN_VECTOR___###############################
CLEAN_VECTOR <- function( XV ) {
	XV[!SAP(XV, IS_BLANK)]
}
##############################___CLEAN_WIKIPEDIA___#############################
CLEAN_WIKIPEDIA <- function(XS) {
	XS <- gsub('<div id="siteSub" class="noprint">From Wikipedia, the free encyclopedia</div>','',XS,ignore.case = T,perl = T)
	XS <- gsub('<div id="siteSub" class="noprint">From Wikipedia, the free encyclopedia</div>','',XS,ignore.case = T,perl = T)
	XS <- gsub('<a class="mw-jump-link" href="#mw-head">Jump to navigation</a>','',XS,ignore.case = T,perl = T)
	XS <- gsub('<li class="toclevel.*?">.*?</li>','',XS,ignore.case = T,perl = T)
	XS <- gsub('<span class="mw-editsection.*?>.*?</span>','',XS,ignore.case = T,perl = T)
XS
}
####################################___COL___###################################
COL <- function(XT,XCOL) {
	XT <- DT(XT)
XT[,c(GREPV(colnames(XT),XCOL)),with=F]
}
################################___COL_AS_LIST___###############################
COL_AS_LIST <- function(XT,XC,XC_TAG) {
	XC <- RMQ(deparse(substitute(XC)))
	XL <- LAP(XT[[XC]],c)#XL <- LAP(YTS$AVQ,c)
	if ( !missing(XC_TAG) ) {
		XC_TAG <- RMQ(deparse(substitute(XC_TAG)))
		names(XL) <- XT[[XC_TAG]]
	}
XL
}
###################################___COL0___###################################
COL0 <- function( YT,XS ) {
 YT[,c(GREP(colnames(YT),XS)),with=F]
}
##############################___COLON_AS_NUMBER___#############################
COLON_AS_NUMBER <- function(XCH) {
	eval(parse(text=(XCH)))
}
##########################___COMMA_TO_DOT_AS_NUMBER___##########################
COMMA_TO_DOT_AS_NUMBER <- function(XV) {
	ZV <- XV
	XV <- gsub(',','.',XV)
	XV <- gsub(' ','',XV)
	if ( IS_NUM( XV ) ) {
		ZV <- as.numeric(as.character(XV))
	}
ZV
}
###########################___CONVERT_MIN_TO_NUMBER___##########################
CONVERT_MIN_TO_NUMBER <- function(XS) {
	XV_SPL <- SPL( XS,':' )
	XMIN <- as.numeric(XV_SPL[1])
	XSEC <- as.numeric(XV_SPL[2])/60
ROUND( XMIN+XSEC )
}
################################___COR_MAP_REG___###############################
COR_MAP_REG <- function( XM_COR,Y_FILL ) {
	MAX_LOG <- floor(log1p(max(XM_COR[[Y_FILL]])))
	MIN_LOG <- floor(log1p(min(XM_COR[[Y_FILL]])))
	XV_BREAKS <- ROUND(expm1(c( ( MIN_LOG+1 ):MAX_LOG )))
	XV_BREAKS <- ROUND(XV_BREAKS[seq_along(XV_BREAKS) %% 2 ==0],0)
	XV_BREAKS <- c( ROUND(expm1(MIN_LOG),0),XV_BREAKS )
	XV_LABELS  <- c('0 or no data',XV_BREAKS[-1])
	XV_COLORS  <- c( 'gray80', rev(plasma(20) ))
	LEG_TIT <- 'Number of ' + tolower(Y_FILL) + '\ncases'
	XT_POINTS_0 <- sf::st_point_on_surface(XM_COR)
	XV_TOP_3 <- 	sapply(c(1:3), function(X) NTH(XT_POINTS_0[[Y_FILL]], X, descending = T))
	XT_POINTS  <-  XT_POINTS_0[XT_POINTS_0[[Y_FILL]]  %in% XV_TOP_3,]
	XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
	XT_COORDS$POINTS_TEXT <- XT_POINTS[['su_a3']] +': ' + XT_POINTS[[Y_FILL]]
	POINTS_TEXT_LEGEND  <-  'Displaying precise value for top 3 amounts.'
	ZM <- ggplot() + 
		geom_sf(data = XM_COR,aes_string(fill=Y_FILL)) + 
		scale_fill_gradientn(trans='log1p',
			colours=XV_COLORS,
			breaks = XV_BREAKS,
			labels = XV_LABELS
		) + 
		labs(fill=LEG_TIT) +
		geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
		geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
		labs(caption = POINTS_TEXT_LEGEND) +
		theme(plot.caption = element_text(color = 'black', face="bold")) +
		theme(panel.background = element_rect(fill = "grey95"),
			axis.ticks = element_blank(),
			axis.text = element_blank(),
			panel.grid = element_line(color = "white", size = 0.5)
		)
ZM
}
###############################___COR_MAP_WORLD___##############################
COR_MAP_WORLD <- function( XM_COR,Y_FILL ) {
	MAX_LOG <- floor(log1p(max(XM_COR[[Y_FILL]])))
	MIN_LOG <- floor(log1p(min(XM_COR[[Y_FILL]])))
	XV_BREAKS <- ROUND(expm1(c( ( MIN_LOG+1 ):MAX_LOG )))
	XV_BREAKS <- ROUND(XV_BREAKS[seq_along(XV_BREAKS) %% 2 ==0],0)
	XV_BREAKS <- c( ROUND(expm1(MIN_LOG),0),XV_BREAKS )
	XV_LABELS <- c('0 or no data',XV_BREAKS[-1])
	XV_COLORS <- c( 'gray80', rev(plasma(20) ))
	LEG_TIT <- 'Number of ' + tolower(Y_FILL) + '\ncases'
	XT_POINTS_0 <- sf::st_point_on_surface(XM_COR)
	XT_POINTS_MAX <- XT_POINTS_0[which.max(XT_POINTS_0[[Y_FILL]]),]
	XT_POINTS_CHN <- XT_POINTS_0[XT_POINTS_0[['su_a3']]=='CHN',]
	XT_POINTS  <-  rbind( XT_POINTS_CHN,XT_POINTS_MAX )
	XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
	XT_COORDS$POINTS_TEXT <- XT_POINTS[['su_a3']] +': ' + XT_POINTS[[Y_FILL]]
	POINTS_TEXT_LEGEND  <-  'Displaying precise value for country with biggest amomunt and for China.'
	ZM <- ggplot() + 
		geom_sf(data = XM_COR,aes_string(fill=Y_FILL)) + 
		scale_fill_gradientn(trans='log1p',
			colours=XV_COLORS,
			breaks = XV_BREAKS,
			labels = XV_LABELS
		) +
		labs(fill=LEG_TIT) +
		geom_label(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
		geom_text(data = XT_COORDS, aes(X, Y, label = POINTS_TEXT),color = 'black') +
		labs(caption = POINTS_TEXT_LEGEND) +
		theme(plot.caption = element_text(color = 'black', face="bold")) +
		theme(panel.background = element_rect(fill = "skyblue"),
			axis.ticks = element_blank(),
			axis.text = element_blank(),
			panel.grid = element_line(color = "white", size = 0.5)
		)
ZM
}
#################################___COR_TABLE___################################
COR_TABLE <- function( XT,XCOL ) {
XT_VAR <- XT[,.SD, .SDcols = !c(XCOL)]
ZV <- vector(  )
for ( i in 1:ncol( XT_VAR ) ) {
ZV[i] <- ROUND(cor( XT[[XCOL]],XT_VAR[[i]] ))
}
data.table(VAR=colnames(XT_VAR),COR=ZV)
}
################################___COUNT_TRUE___################################
COUNT_TRUE <- function( XV ) length(which(XV))
###############################___COUNT_UNIQUE___###############################
COUNT_UNIQUE <- function( XT ) {
	XT <- as.data.table(XT)
	XCOLNAMES <- colnames(XT)
	CNT <- sapply( seq_along(XCOLNAMES) ,function( X ) length(unique(na.omit(XT[[X]]))))
data.table(XCOLNAMES,CNT)
}
################################___COUNT_WORDS___###############################
COUNT_WORDS <- function( XS ) {
	str_count(XS, '\\w+')
}
################################___CREATE_DIR___################################
CREATE_DIR <- function(DIR_NAME = "TEMP_DIR") {
	if (!dir.exists(DIR_NAME)) {
		dir.create(DIR_NAME)
	}
}
################################___DATA_TABLE___################################
DATA_TABLE <- function( XT , XV=c(5, 30, 50)) {
	DT::datatable(XT, options = list(lengthMenu = XV, pageLength = nrow( XT )))
}
################################___DATE_2_DAYS___###############################
DATE_2_DAYS <- function(XD_MIN,XD_MAX) {
DATE <- as.character(seq(XD_MIN,XD_MAX,by='days'))
DAYS <- seq.int( as.numeric(XD_MAX - XD_MIN) +1)
return(data.table(DAYS,DATE)	)	
}
#############################___DATE_AS_YEAR_NUM___#############################
DATE_AS_YEAR_NUM <- function( XV ) {
	SAP( XV,DATE_AS_YEAR_NUM_0 )
}
############################___DATE_AS_YEAR_NUM_0___############################
DATE_AS_YEAR_NUM_0 <- function( X ) {
	XSPL <- SPL( X,'-' )
	YEAR <- AS_NUM(B( XSPL[1],3,4 ))
	MONTH <- AS_NUM(XSPL[2])
	DAY <- AS_NUM( XSPL[3] )
YEAR + MONTH/100 + DAY/10000	
}
############################___DATE_AS_YEAR_NUM_1___############################
DATE_AS_YEAR_NUM_1 <- function( X ) {
	XSPL <- SPL( X,'-' )
	YEAR <- AS_NUM(B( XSPL[1],3,4 ))
	MONTH <- AS_NUM(XSPL[2])
	DAY <- AS_NUM( XSPL[3] )
YEAR + MONTH/100 + DAY/10000	
}
################################___DATE_TO_NUM___###############################
DATE_TO_NUM <- function( XV ) {
	XV <- SUB( XV,6,-1 )
AS_NUM(gsub( '-','.',XV ))
}
#################################___DEL_QUOTE___################################
DEL_QUOTE <- function( X ) {
	X <- deparse(substitute(X))
	Q <- GREPL( X,'^"' ) & GREPL( X,'"$' )
	L(X,Q)
}
################################___DELETE_DOM___################################
DELETE_DOM <- function(XV,DOM) {
DOM_START  <- "<" + DOM + ".*>"
DOM_END <- "</" + DOM + ">"
XV_DOM_START <- grep(DOM_START, XV, ignore.case = TRUE)
XV_DOM_END <- grep(DOM_END, XV, ignore.case = TRUE)
if ( length(XV_DOM_START) > 0 & (length(XV_DOM_START)=length(XV_DOM_END))  ) {
for(i in seq_along(XV_DOM_START)  ) {XV[XV_DOM_START[i]:XV_DOM_END[i]] <- ''}
}
XV[XV!='']
}
#############################___DELETE_DOM_INSIDE___############################
DELETE_DOM_INSIDE <- function(XV,DOM) {
DOM_START  <- "<" + DOM + ".*>"
DOM_END <- "</" + DOM + ">"
XV_DOM_START <- grep(DOM_START, XV, ignore.case = TRUE)
XV_DOM_END <- grep(DOM_END, XV, ignore.case = TRUE)
if ( length(XV_DOM_START) > 0 & (length(XV_DOM_START)=length(XV_DOM_END))  ) {
for(i in seq_along(XV_DOM_START)  ) {XV[XV_DOM_START[i]:XV_DOM_END[i]] <- ''}
}
XV[XV!='']
}
##############################___DELETE_DOM_LINE___#############################
DELETE_DOM_LINE <- function(XV,DOM) {
DOM <- "<" + DOM + ".*>"
xv <- gsub(DOM, '', XV, ignore.case = TRUE)
}
############################___DELETE_DOM_OUTSIDE___############################
DELETE_DOM_OUTSIDE <- function(XV,DOM) {
DOM_START  <- "<" + DOM + ".*>"
DOM_END <- "</" + DOM + ">"
XN_BODY_START <- min(grep(DOM_START, XV, ignore.case = TRUE))
XN_BODY_END <- max(grep(DOM_END, XV, ignore.case = TRUE))
XV[(XN_BODY_START):(XN_BODY_END)]
}
#############################___DELETE_EMPTY_DOM___#############################
DELETE_EMPTY_DOM <- function(XS,DOM) {
	if( length(XS )>1)	XS <- paste0(XS,collapse='')
	XV <- SPL_AFTER(XS,P0("</",DOM,">"))
	XV <- SPL_BEFORE(XV,P0("<",DOM))
	XV <- GSUB(XV, P0("<",DOM,".*?></",DOM,">"),"")
P0( XV,collapse = '' )
}
##################################___DEV_OFF___#################################
DEV_OFF <- function() {
	XTC <- L(  )
	for ( i in 1:100 ) {
		XTC[[i]] <- TC(dev.off(i))
	}
	TC(dev.off())
}
##################################___DIV_AVG___#################################
DIV_AVG <- function( XV ) {
  ROUND( XV /( mean( XV )) )
}
################################___DIV_AVG_NTH___###############################
DIV_AVG_NTH <- function(XV,XN) {
  XV_XN  <- sort(XV,decreasing = T)
  AVG <- ROUND(mean(XV_XN[1:XN]))
  ROUND( XV /( AVG) )
}
###############################___DO_FILE_NAMES___##############################
DO_FILE_NAMES <- function(X='XFILE.TXT',N=0) {
	X <- deparse(substitute(X))
	if (IS_QUOTE( X )) {
		X <- GSUB( X,'"','' )
		X <- GSUB( X,"'","'" )	
	}
	if ( S( X,'\\.' ) ) {
		X.SPL <- SPL( 'XFILE.TEXT','\\.' )
		XV_FILE_NAMES <- X.SPL[2] + "%0" + N + "d." + X.SPL[2]
	} else {
		XV_FILE_NAMES <- X + "%0" + N + "d." + "TXT"
	}
sprintf(XV_FILE_NAMES,N+1)	
}
##################################___DOW_SUB___#################################
DOW_SUB <- function( SUB_ID ,SUB_DIR='TEMP_DIR') {
	CREATE_DIR( SUB_DIR )
	ZU <- 'https://www.opensubtitles.org/en/download/vrf-108d030f/sub/' + SUB_ID
	FILE_PATH <- P0( SUB_DIR ,"/dataset.zip")
	download.file(ZU, dest=FILE_PATH, mode="wb") 
unzip(FILE_PATH, exdir =  SUB_DIR)
}
###############################___DOWNLOAD_HTML___##############################
DOWNLOAD_HTML <- function( XU,X_FILE='TEMP.HTML') {
	TEMP_FILE <- tempfile()
	download.file( XU ,destfile = TEMP_FILE, mode="wb")
	XRL <- suppressWarnings(readLines(TEMP_FILE,encoding = 'UTF-8'))
	XRL <- SPL_HTML( XRL )
	XRL <- XRL[XRL!='']
	writeLines(XRL,X_FILE,useBytes = TRUE)
}
####################################___DS___####################################
DS <- function(X) {
	deparse(substitute(X))
}
##################################___DS_TEST___#################################
DS_TEST <- function(X) {
	#XAGR=YAGR I YAGR
	XS <- substitute(X)
	XSD <- deparse(XS)
	XDS <- deparse(substitute(X))
	RXS <- RMQ(XS)
	RXSD <-RMQ(XSD) 
	RXDS <- RMQ(XDS)
L(XS,XSD,XDS,RXS,RXSD,RXDS)
}
###############################___DT_NA_TO_VAL___###############################
DT_NA_TO_VAL <- function( XT,VAL ) {
	XT <- as.data.frame(XT)
	XT[W(is.na(XT),arr.ind=T)] <- VAL
as.data.table( XT )
}
###############################___DUMP_LSF_STR___###############################
DUMP_LSF_STR <- function( ) {
LSF_STR <- lsf.str(envir = .GlobalEnv)
LSF_STR <- LSF_STR[!UP(LSF_STR) %in% c('A','C','UP','O','LAP','SAP','AP','SEQ','RL','P0')]
TEMP_FILE <- tempfile()
dump(LSF_STR, file = TEMP_FILE)
XRL <- readLines(TEMP_FILE)
XRL <- GSUB_INDEN(XRL,4,2)
ZV <- vector()
if (any( GREPL(XRL[1:20] ,'<- function') )) {
 XRL <- gsub("<-function", "<- function",XRL)
 XRL <- gsub("<- function", "<-\nfunction",XRL)
 TEMP_FILE <- tempfile( )
 writeLines(XRL,TEMP_FILE)
 XRL <- readLines(TEMP_FILE)
}
for (i in 1:(length(XRL) - 2)) {
 if ( IS_FUNCTION_START(XRL,i) ) {
	 XS <- SUB(XRL[i], 1, " ")
	 FUN_NAME <- HASH_NAME(XS,80,3)
	 XS_FUN <- XRL[i + 1]
	 ZV[i] <- FUN_NAME
	 ZV[i + 1] <- paste0(XRL[i], " ", XS_FUN)
	 if (trimws(XRL[i+2]) == '{' & SUB( ZV[i + 1],-1,-1 )!='{' ) {
		 ZV[i + 1] <- paste0(ZV[i + 1] ,'{' )
		 XRL[i+2] <- ''
		 }
	 #if ( i > 1 ) ZV[i-1] <- trimws( ZV[i-1] ) 
	 }
 else {
 if (XRL[i] != XS_FUN) {
	 	ZV[i] <- XRL[i]
 }
 }
 }
 ZV <- na.omit(ZV[ZV != ""])
 ZV <- c(ZV, "}")
writeLines(ZV, paste0("LSF_STR_", GSUB(Sys.Date(), "-", "_"), ".r"))
}
####################################___EX___####################################
EXIST01 <- function(X) {
	XCH <- RMQ(deparse(substitute(X)))
	if ( XCH=='X' | XCH=='.X' ) {
		Z <- exists('X', envir = globalenv())	
	} else if ( B(XCH,1,1)=='.' ) {
		XCH <- B(XCH,2,-1)
		Z <- .EX(CH(eval( parse(text = XCH) , envir=.GlobalEnv)))
	} else if ( exists(XCH) ) {
			Z <- T
	} else if ( S(XCH,'\\$') | S(XCH,'\\[.*?".*?]') | S(XCH,"\\[.*?'.*?]") ) {
		XTC <- TC(eval( parse(text = XCH) , envir=.GlobalEnv))
		if ( !XTC$OK ) {
		Z <- F
	} else if ( is.null(XTC$VALUE) ) {
		Z <- F
	} else {
		Z <- T
	}
	} else {
		Z <- F	
	}
Z
}
####################################___EX0___###################################
EXIST00 <- function(X) {
	if ( exists(X) ) {
		Z <- T
	} else if ( S(X,'\\$') | S(X,'\\[.*?".*?]') | S(X,"\\[.*?'.*?]") ) {
		XTC <- TC(eval( parse(text = X) , envir=.GlobalEnv))
		if ( !XTC$OK ) {
			Z <- F
		} else if ( is.null(XTC$VALUE) ) {
			Z <- F
		} else {
			Z <- T
		}
	} else {
		Z <- F	
	}
Z
}
###################################___EXIST___##################################
EXIST <- function( full_index_path ){
  tryCatch({
  len_element = length(full_index_path)
  exists_indicator = ifelse(len_element > 0, T, F)
  return(exists_indicator)
  }, error = function(e) {
  return(F)
  })
}
##################################___EXTRACT___#################################
EXTRACT <- function(XS,XP,FIX=TRUE) {
	ifelse(FIX,
								ZS <- unique(as.vector(stri_extract_all_regex(XS,XP, regex = list(case_insensitive = TRUE),simplify = TRUE))),
								ZS <- unique(as.vector(stri_extract_all_fixed(XS,XP, opts_fixed = list(case_insensitive = TRUE),simplify = TRUE)))
	)
return(trimws(ZS))
}
################################___EXTRACT_DOM___###############################
EXTRACT_DOM <- function(XS) {
	XS <- EXTRACT(XS, "<.*?>")[1]
	XS <- trimws(SUB(XS,2,-2))
	if ( SUB(XS,1,1)=='/' & !is.na(XS) ) {
		XS <- trimws(SUB(XS,2,-1))
	}
	if (grepl(' ',XS)) {
		XS <- SUB(XS,1,' ')	
	}
return(XS)	
}
#####################################___f___####################################
###################################___F_SRT___##################################
FSRT <- function( XFILE ) {
	#XFILE='S01E01.SRT'
	if ( length(XFILE) > 1 ) {
		XRL <- trimws( XFILE )
		XRL <- XRL[XRL!='']
	} else {
		XRL <- trimws(readLines( XFILE,encoding = '"UTF-8"' ))
		XRL <- XRL[XRL!='']	
	}
	XMAX <- max(na.omit(as.numeric(tail(XRL))))
	XV_TEXT <- vector(  )
	XV_ID <- vector(  )
	XL_DF_TIME <- list(  )
	k <- 0
	for ( i in seq_along( XRL ) ) {
		print(c(i,k))
		XRLi <- XRL[i]
		if ( IS_NUM_0( XRLi ) ) {
			k <- as.numeric(XRLi)
			XV_ID[k] <- k
		} else if ( stri_detect_fixed(XRLi,'-->') ) {
			XL_DF_TIME[[k]] <- F_SRT_TIME(XRLi)
		} else {
			XV_TEXT[k] <- XRLi
		}
		if ( k== XMAX ) {break}
	}
	XT_TIME <- do.call(rbind,XL_DF_TIME)
	XT <- data.table(ID=XV_ID,XT_TIME,TEXT=XV_TEXT,FILE_NAME = rep( XFILE, length(XV_TEXT) ))
XT
}
#############################___F_SRT_CHANGE_TIME___############################
F_SRT_CHANGE_TIME <- function( XT ) {
ZT <- copy(XT)
for ( i in seq.int(nrow(XT)-1)  ) {
	YT <- XT[c( i,i+1 ),1:7]
if ( YT[1,START_HOUR] == YT[2,START_HOUR] ) {
	XVAL1 <-  YT[1,END_MIN] + YT[1,END_SEC]/100
	XVAL2 <-  YT[2,START_MIN] + YT[1,START_SEC]/100
	NEW_VAL <- mean(XVAL1,XVAL2)
	
	ZT[i,END_MIN:=floor(NEW_VAL)]
	ZT[i,END_SEC:= 100 * (NEW_VAL%%1 - 0.00001)]
	ZT[i+1,END_MIN:=floor(NEW_VAL)]
	ZT[i+1,START_SEC:= 100 * ( NEW_VAL%%1 + 0.00001) ]	
} 
}
as.data.table( ZT)
}
#################################___F_SRT_OLD___################################
F_SRT_OLD <- function( XFILE ) {
	if ( length(XFILE) > 1 ) {
		XRL <- trimws( XFILE )
		XRL <- XRL[XRL!='']
	} else {
		XRL <- trimws(readLines( XFILE,encoding = '"UTF-8"' ))
		XRL <- XRL[XRL!='']	
	}
	XV_TEXT <- vector(  )
	XV_ID <- vector(  )
	XL_DF_TIME <- list(  )
	for ( i in seq_along( XRL ) ) {
		XRLi <- XRL[i]
		XRLi <- gsub('d<U+00BB>z','',XRLi)
		if ( IS_NUM_0( XRLi ) ) { #<_IF_01>#
			k <- as.numeric(XRLi)
			X_CHECK_TEXT <- TRUE
			XV_ID[k] <- k
		} else {#</_IF_01>#
			if ( stri_detect_fixed(XRLi,'-->') ) {#<_IF_02>#
				XL_DF_TIME[[k]] <- F_SRT_TIME(XRLi)
			} else {#</_IF_02>#
				if ( X_CHECK_TEXT ) {#<_IF_03>#
					XV_TEXT[k] <- stri_trans_toupper( XRLi )
				} else {#</_IF_03>#
					XV_TEXT[k] <- paste(XV_TEXT[k],' ',stri_trans_toupper( XRLi ) ) #<_IF_04>#
				}
			}
		}
		}#</_IF_04>#
	XT_TIME <- do.call(rbind,XL_DF_TIME)
	XT <- data.table(ID=XV_ID,XT_TIME,TEXT=XV_TEXT,FILE_NAME = rep( XFILE, length(XV_TEXT) ))
XT
}
################################___F_SRT_TIME___################################
F_SRT_TIME <- function(XS) {
	XV_SPL_ARROW <- SPL(XS,'-->')
	XS_START <- XV_SPL_ARROW[1]
	XS_END <- XV_SPL_ARROW[2]
	XV_SPL_START <- SPL(XS_START,':')
	XV_SPL_END <- SPL(XS_END,':')
	START_HOUR <- as.numeric(XV_SPL_START[1])
	START_MIN <- as.numeric(XV_SPL_START[2])
	START_SEC <- as.numeric(gsub(',','.',XV_SPL_START[3]))
	END_HOUR <- as.numeric(XV_SPL_END[1])
	END_MIN <- as.numeric(XV_SPL_END[2])
	END_SEC <- as.numeric(gsub(',','.',XV_SPL_END[3]))
	#c(START_HOUR,START_MIN,START_SEC,END_HOUR,END_MIN,END_SEC)
	START <- 3600*START_HOUR+60*START_MIN+START_SEC
	END <- 3600*END_HOUR+60*END_MIN+END_SEC
	data.frame(START,END,START_HOUR,START_MIN,START_SEC,END_HOUR,END_MIN,END_SEC )
}
###################################___F_SUM___##################################
F_SUM <- function(XT) {
 	XT <- as.matrix(XT)
	XT[is.na( XT )] <- 0
	XT <- CLEAN_TABLE(XT)
  YT  <- as.data.frame( XT )
  XIND <- which(
  sapply(YT, class) == 'numeric'
  )
  ROW_SUM  <- apply(YT[,XIND],1,sum)
  XT <- cbind(XT,ROW_SUM)
  YT  <- as.data.frame( XT )
  XIND <- which(
  sapply(YT, class) == 'numeric'
  )
  XV_SUM_COL <- rep('-',ncol( XT )) 
  XV_SUM_COL[XIND] <- apply(YT[,XIND],2,sum)
  XW <- which( XV_SUM_COL == '-' )
  if ( !IS_BLANK( XW )  ) {
  XW_MIN  <- min( XW )
  }
  XV_SUM_COL[XW_MIN] <- 'SUM'
  XT <- rbind( as.data.frame( XT ) ,XV_SUM_COL)
as.data.table( XT )
}
##################################___F_TEST___##################################
F_TEST <- function( ... ) {
XL <- list(...)
XL
}
#################################___F_TEST_2___#################################
F_TEST_2 <- function( ... ) {
	XL <- deparse(substitute(L(...)))
	XL <- B( XL,3,-2 )
	XL <- SPL( XL,',' )
	XL
}
################################___F_TEST_DATA___###############################
F_TEST_DATA <- function(XFILE = "TEST_DATA.R") {
  XV <- readLines(XFILE, warn = FALSE, encoding = "UTF-8")
  XV <- XV[1:(min(which(XV == "")) - 1)]
  SCR_START <- which(GREPL(XV, "#.*?<.*?_SCR_.*?>"))
  SCR_END <- which(GREPL(XV, "#.*?<.*?/SCR_.*?>"))
  WL(XV[SCR_START:SCR_END], "SCR_TEST_DATA.R")
  source("SCR_TEST_DATA.R")
  XV[SCR_START:SCR_END] <- ""
  XV <- XV[XV != ""]
  WHICH_HASH <- which(STR_START(XV, "##########"))
  WHICH_HASH <- setdiff(WHICH_HASH, c(1, length(XV)))
  XL <- list()
  k <- 1
  for (i in seq_along(WHICH_HASH)) {
  YV <- XV[k:(WHICH_HASH[i] - 1)]
  XL[[i]] <- YV[!(STR_START(YV, "##########"))]
  k <- WHICH_HASH[i] + 1
  }
  XL[[i + 1]] <- XV[k:length(XV)]
  XV <<- XL[[1]]
  XS <<- paste0(XL[[1]], collapse = "")
  XL <<- XL
list(XL[[1]], XS)
}
##################################___F_WAIT___##################################
F_WAIT <- function(x) {
	p1 <- proc.time()
	x_time <- abs(round(rnorm(1,x,1),4))
	Sys.sleep(x_time)
	#proc.time() - p1 # The cpu usage should be negligible
}
FILE_REMOVE <- function(XF) {
	if (file.exists(XF)) {
		file.remove(XF)
	}	
}
#################################___FILE_SIZE___################################
FILE_SIZE <- function( X ) {
	X.SIZE <- utils:::format.object_size(X,units='MB')
	X.SIZE <- AS_NUM(B(X.SIZE,1,' '))
	if (X.SIZE==0) {
		X.SIZE <- utils:::format.object_size(X,units='KB')
		X.SIZE <- AS_NUM(B(X.SIZE,1,' '))
		X.SIZE <- R(X.SIZE/1024,4)
	}
X.SIZE
}
###############################___FIND_NOT_JOIN___##############################
FIND_NOT_JOIN <- function
(XT1,
XT2,
XC_KEY_1,
XC_KEY_2,
XC_VAL_1,
XC_VAL_2
){
if ( missing(XC_VAL_1) || missing(XC_VAL_2) ) {
XT1 <- as.data.table(XT1)
XT2 <- as.data.table(XT2)
XT1 <- as.data.frame(XT1[,XC1,with=FALSE])
XT2 <- as.data.frame(XT2[,XC2,with=FALSE])
} else {
XT1 <- as.data.table(XT1)
XT2 <- as.data.table(XT2)
XT1 <- as.data.frame(XT1[,c( XC_KEY_1,XC_VAL_1 ),with=FALSE])
XT2 <- as.data.frame(XT2[,c( XC_KEY_2,XC_VAL_2 ),with=FALSE])
}
YT1 <- fn$sqldf('SELECT * FROM
XT1
LEFT JOIN
XT2 ON $XC1 = $XC2
WHERE $XC2 IS NULL')
YT2 <- fn$sqldf('SELECT * FROM
XT2
LEFT JOIN
XT1 ON $XC2 = $XC1
WHERE $XC1 IS NULL')
CLEAN_TABLE(rbind(YT1,YT2))
}
####################################___FLS___###################################
FLS <- function(XN) {
	XLS <- ls(envir = .GlobalEnv)
	obj.class <- napply(XLS, function(x) as.character(class(x))[1])
	obj.mode <- napply(XLS, mode)
	obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
	obj.prettysize <- napply(XLS, function(x) {
		format(utils::object.size(x), units = "auto")
	})
	obj.size <- napply(XLS, object.size)
	ZT <- DT(NAME = XLS, obj.type, obj.size, obj.prettysize)
	setnames(ZT, c("NAME", "TYPE", "SIZE", "SIZE2"))
	ZT <- ZT[O(-SIZE)]
	if (!missing(XN)) ZT <- ZT[1:XN]
ZT
}
###################################___FNORM___##################################
FNORM <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
###################################___FSEL___###################################
FSEL <- function(XPATH = "select.sql") {
	XV <- readLines(XPATH, warn = FALSE, encoding = "UTF-8")
	XV <- XV[1:(min(which(XV == "")) - 1)]
	XS_SELECT <- paste0(XV, collapse = " ")
	TC <- TRY_CATCH(sqldf(XS_SELECT))
	if (TC$OK) {
		Z <- list(SEL = XS_SELECT, VAL = TC$VALUE)
	}
	else {
		Z <- L(TC, XS_SELECT)
	}
Z
}
################################################################################
FUNCTION_NAMES <- trimws(SUB(GREP( ZV,' <- function' ),1,'<-'))
################################################################################
FUN_START <- c(grep("<- function", ZV),length( ZV ))
XL <- list(  )
for ( i in seq_along(FUN_START[-length(FUN_START)]) ) {
if ( i!=length(FUNCTION_NAMES) ) {
FUN <- paste0(ZV[FUN_START[i]:(FUN_START[i+1]-1)],collapse = '\n')
} else {
FUN <- paste0(ZV[FUN_START[i]:length(ZV)],collapse = '\n')
}
NAME <- FUNCTION_NAMES[i]
XL[[i]] <- data.table(NAME,FUN)
}
}
####################################___gct___###################################
gct <- function () {
	gctorture(FALSE)
	gc()
}
###############################___GET_BASE_FUN___###############################
GET_BASE_FUN <- function(FUN='base') {
	PACKAGE <- P("package:",FUN)
	as.character(lsf.str(PACKAGE))	
}
#########################___GET_CURRENT_FILE_LOCATION___########################
GET_CURRENT_FILE_LOCATION <- function(){
	this_file <- commandArgs() %>% 
		tibble::enframe(name = NULL) %>%
		tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
		dplyr::filter(key == "--file") %>%
		dplyr::pull(value)
	if (length(this_file)==0)
	{
		this_file <- rstudioapi::getSourceEditorContext()$path
	}
return(dirname(this_file))
}
##################################___GET_FUN___#################################
GET_FUN <- function(X,XN=-1) {
	X_LSF_STR <- lsf.str(envir = .GlobalEnv)
	X_FUN <- SV(X_LSF_STR,X)
	TEMP_FILE <- tempfile()
	dump(X_FUN, file = TEMP_FILE)
	XRL <- readLines(TEMP_FILE,encoding='UTF-8')
	XZV <- vector()
	XV_FUN_START <- vector()
	XV_FUN_NAME <- vector()
	k=1
	for (i in 1:(length(XRL) - 2)) {
		if (IS_FUNCTION_START(XRL, i)) {
			XV_FUN_START <- c(XV_FUN_START,i)
			XS <- SUB(XRL[i], 1, " ")
			XV_FUN_NAME[k] <- XS
			k <- k+1
			FUN_NAME <- HASH_NAME(XS, 80, 3)
			XS_FUN <- XRL[i + 1]
			XZV[i] <- FUN_NAME
			XZV[i + 1] <- paste0(XRL[i], " ", XS_FUN)
			if (TRIM(XRL[i + 2]) == "{" & SUB(XZV[i + 1], -1, -1) != "{") {
				XZV[i + 1] <- paste0(XZV[i + 1], "{")
				XRL[i + 2] <- ""
			}
		}
		else {
			if (XRL[i] != XS_FUN) {
				XZV[i] <- XRL[i]
			}
		}
	}
	XZV <- C(XZV,XRL[(LEN(XRL)-1):(LEN(XRL))])
	XV9 <- XZV
	if( LEN(X_FUN)>1 ){
		XL <- L()
		for ( i in SEQ(XV_FUN_START[-1]) ) {
			XL[[i]] <- XZV[XV_FUN_START[i]:(XV_FUN_START[i+1]-1)]
			XL[[i]] <- GSUB_INDEN(XL[[i]], 4, 2)
		}
		XL[[i+1]] <- XZV[XV_FUN_START[i+1]:LEN(XZV)]
		XL[[i+1]] <- GSUB_INDEN(XL[[i+1]], 4, 2)
		XW <- SW(XV_FUN_NAME,X)
		XL <- XL[XW]
		XV9 <- UL(XL)
		print(XL)
	}#IF
	XV9 <- GSUB_INDEN(na.omit(XV9[XV9 != ""]), 4, 2)
	if (XN==0) WLX(XV9)
	print(XV9)
	rm(XZV,FUN_NAME,XV_FUN_NAME,XV9,X_LSF_STR)
}
###########################___GET_FUN_FROM_PACKAGE___###########################
GET_FUN_FROM_PACKAGE <- function(FUN=c('base','data.table','stringi','stringr','rvest')) {
	XL <- L()
	for ( i in SEQ(FUN) ) {
		PACKAGE <- P("package:",FUN[i])
		XL[[i]] <- as.character(lsf.str(PACKAGE))
	}
DC(c,XL)
}
#############################___GET_FUN_FROM_VEC___#############################
GET_FUN_FROM_VEC <- function(X,XRL='R/F.R') {
	if (LEN(XRL)==1 ) XRL <- RL(XRL)
	XFUN_START <- '^'+X+' <- function\\('
	XFUN_START <- W(GREPL(XRL,XFUN_START))-1
	XRL <- XRL[XFUN_START:LEN(XRL)]
	XFUN_END <- min(W(S(XRL,'^}$')))
	XFUN <- XRL[1:XFUN_END]
INDEN42(XFUN)
}
############################___GET_FUN_WITH_INSIDE___###########################
GET_FUN_WITH_INSIDE <- function(X) {
	X1 <- all.names(body(X))
	X2 <- GET_FUN_FROM_PACKAGE(c("base", "data.table", "stringi", "stringr", "rvest"))
	XFUN_INSIDE <- setdiff(X1, X2)
	XFUN_INSIDE <- XFUN_INSIDE[NCHAR(XFUN_INSIDE) > 3]
	XRL <- RL("R/F.R")
	XL <- L()
	for (i in SEQ(XFUN_INSIDE)) {
		XL[[i]] <- GET_FUN_FROM_VEC(XFUN_INSIDE[i], XRL)
	}
XL <- C(GET_FUN_FROM_VEC(X, XRL), XL)
}
##############################___GET_HREF_TABLE___##############################
GET_HREF_TABLE <- function(XU,ALL_ATTRS=FALSE) {
  if ( all(class(XU) == c("xml_document","xml_node"))) {
  XRH <- XU
  } else {
  XRH <- read_html(XU)
  }
  XL_ATTRS_ALL <- html_nodes(XRH,'a') %>% html_attrs()
  XV_TEXT <- stri_trans_toupper(html_nodes(XRH,'a') %>% html_text())
  XV_TEXT <- gsub("[\n\r\t\v\f]", "",XV_TEXT)
  XV_ATTRS <- unique(unlist(lapply(XL_ATTRS_ALL,names)))
  X_LEN <- length(XL_ATTRS_ALL)
  ZT <- as.data.frame(t(unlist(XL_ATTRS_ALL[[1]])))
  for ( i in 2:X_LEN ) {
  ZT <- bind_rows( ZT , as.data.frame(t(unlist(XL_ATTRS_ALL[[i]]))) )
  }
  ZT <- data.table(text=XV_TEXT,ZT)
  ZT <- setnames(ZT,gsub("\\bXI\\b", "XI_ORG",colnames(ZT)))
  ZT[, XI := .I]
  colnames(ZT) <- stri_trans_toupper(colnames(ZT))
  if ( !ALL_ATTRS ) {
  ZT <- ZT[,c('XI','TEXT','HREF')]
  #colnames(ZT) <- c('XI','XT','XH')
  ZT[ , `:=`(XN=.N) , by = .(TEXT,HREF) ]
  }
  closeAllConnections()
  as.data.table(ZT)
}
#################################___GET_HTML___#################################
GET_HTML <- function( XU ) {
	if ( all( class(XU) %in% c("xml_document","xml_node",'xml_nodeset') ) ) {
  XRH <- XU
  } else {
  XRH <- READ_HTML_0(XU)
  }
XRH	
}
##############################___GET_HTML_ATTRS___##############################
GET_HTML_ATTRS <- function(XU,xall=FALSE) {
  if ( all(class(XU) == c("xml_document","xml_node"))) {
  XRH <- XU
  } else { 
  if ( length(XU) > 1) {
  TEMP_FILE <- tempfile()
  writeLines(XU,TEMP_FILE)
  XRH <- read_html(TEMP_FILE)
  } else {
  XRH <- READ_HTML(XU)
  }
  }
  XL_ATTRS_ALL <- html_nodes(XRH,'a') %>% html_attrs()
  XV_TEXT <- stri_trans_toupper(html_nodes(XRH,'a') %>% html_text())
  XV_TEXT <- gsub("[\n\r\t\v\f]", "",XV_TEXT)
  XV_ATTRS <- unique(unlist(lapply(XL_ATTRS_ALL,names)))
  X_LEN <- length(XL_ATTRS_ALL)
  ZT <- as.data.frame(t(unlist(XL_ATTRS_ALL[[1]])))
  for ( i in 2:X_LEN ) {
  ZT <- bind_rows( ZT , as.data.frame(t(unlist(XL_ATTRS_ALL[[i]]))) )
  }
  ZT <- data.table(text=XV_TEXT,ZT)
  ZT <- setnames(ZT,gsub("\\bXI\\b", "XI_ORG",colnames(ZT)))
  ZT[, XID := .I]
  colnames(ZT) <- stri_trans_toupper(colnames(ZT))
  ZT[ , `:=`( TOT=.N) , by = .(TEXT,HREF) ]
  if (!xall) {
  ZT <- ZT[,c('TEXT','HREF','XID')]
  #colnames(ZT) <- c('XI','XT','XH')
  ZT[ , `:=`( TOT=.N) , by = .(TEXT,HREF) ]
  }
  closeAllConnections()
  as.data.table(ZT)
}
##############################___GET_HTML_TABLE___##############################
GET_HTML_TABLE <- function( XU ) {
 ZL <- list()
 TEMP_FILE <- tempfile()
 download.file(XU, destfile = TEMP_FILE, mode = "wb")
 XRL <- suppressWarnings(readLines(TEMP_FILE, encoding = "UTF-8"))
 XRL <- CLEAN_TRIM_BRACKETS(XRL)
 XS <- paste0(XRL, collapse = "")
 XS <- gsub("[[:space:]]+", " ", XS)
 XS <- GSUB(XS, "<TABLE.*?>", "<TABLE>")
 XS <- GSUB(XS, "</TABLE.*?>", "</TABLE>")
 XV <- SPL_HTML(XS)
 XV_TABLE_START <- which(XV == ".<TABLE>")
 XV_TABLE_END <- which(XV == "</TABLE>")
 for (i in seq_along(XV_TABLE_START)) {
 ZL[[i]] <- XV[XV_TABLE_START[i]:XV_TABLE_END[i]]
 ZL[[i]] <- paste0(ZL[[i]], collapse = "")
 ZL[[i]] <- GSUB(ZL[[i]], "<!--.*?-->", " ")
 ZL[[i]] <- GSUB(ZL[[i]], "<DIV.*?>", " ")
 ZL[[i]] <- GSUB(ZL[[i]], "</DIV>", " ")
 ZL[[i]] <- GSUB(ZL[[i]], "<SPAN.*?>", " ")
 ZL[[i]] <- GSUB(ZL[[i]], "</SPAN>", " ")
 ZL[[i]] <- GSUB(ZL[[i]], "&nbsp;|&#xe035|&#xe061", "")
 ZL[[i]] <- SPL_BEFORE(ZL[[i]], "<TR>")
 ZL[[i]] <- GSUB(ZL[[i]], "<br.*?/>", "")
 }
 ZV <- unlist(ZL)
 ZV <- CLEAN_STRING(ZV)
 ZV <- GSUB(ZV, "</TABLE>", "</TABLE>\n")
 WL(ZV, "HTML_TABLE.HTML")
 XRH <- read_html("HTML_TABLE.HTML")
 ZL <- html_nodes(XRH, "table") %>% html_table(fill = TRUE)
 ZL <- lapply(ZL, as.data.table)
 if (X_ROW_AS_COL) {
 ZL <- L(ZL, ROW_AS_COL)
 }
 ZL
}
#########################___GET_PARSE_HTML_TABLE_LIST___########################
GET_PARSE_HTML_TABLE_LIST <- function(XV) {
	if (! length(XV) > 1 ) { XV <- READ_HTML(XV,VEC=T) 
	XV_TABLE_START <- which(GREPL(XV,'^<TABLE'))
	XV_TABLE_END <- which(GREPL(XV,'^</TABLE'))
	XV_TABLE_AMOUNT <- seq_along(XV_TABLE_START)
	XL_TABLE <- lapply(XV_TABLE_AMOUNT,function( X ) XV[XV_TABLE_START[X]:XV_TABLE_END[X]])
lapply(XL_TABLE,PARSE_HTML_TABLE)
}
#################################___GET_YEAR___#################################
GET_YEAR <- function() {
	unname(AS_NUM(B0(Sys.Date(),1,4)))
}
###################################___GREP___###################################
GREP <- function(XV,XS) {
	grep(XS,XV,ignore.case = T,value = TRUE)
}
###################################___GREPL___##################################
GREPL <- function(XV,XS) {
	XLEN_V <- length(XV)
	XLEN_S <- length(XS)
	if ( XLEN_V == XLEN_S & XLEN_S > 1 ) {
		ZV <- vector(  )
		for ( i in seq_along(XV ) ) {
		 	ZV[i] <- grepl(XS[i], XV[i],ignore.case = T,perl = T)
		}
		Z <- ZV	
	} else {
		Z <- grepl(XS, XV,ignore.case = T,perl = T)
	}
Z
}
###################################___GREPV___##################################
GREPV <- function(XV,XS,VAL=TRUE) {
	grep(XS, XV,ignore.case = T,value = VAL,perl = T)
}
###################################___GREPW___##################################
GREPW <- function(XV,XS) {
	which(GREPL(XV,XS))
}
###################################___GSUB___###################################
GSUB <- function( XS,XP_OLD,XP_NEW ) {
	XTC <- TC(gsub(XP_OLD,XP_NEW,XS,ignore.case = T,perl = T))
		if ( XTC$OK ) {
		X <- XTC$VALUE
	} else {
		X <- XS
	}
X
}
##################################___GSUB_EX___#################################
GSUB_EX <- function(X,A,Z) {
	A <- P('^',A,'$')
GSUB(X,A,Z)
}
##############################___GSUB_HTML_STYLE___#############################
GSUB_HTML_STYLE <- function(XCH,XTAG) {
	XCH <- GSUB(XCH, P("<",XTAG,".*?>"), P("<",XTAG,">"))
	XCH <- GSUB(XCH, P("</",XTAG,".*?>"), P("</",XTAG,">"))
XCH
}
###############################___GSUB_HTML_TAG___##############################
GSUB_HTML_TAG <- function(XCH,XTAG) {
	XTAG <- P("<",XTAG,".*?>.*?</",XTAG,">")
	XCH <- GSUB(XCH,XTAG,"")
XCH
}
################################___GSUB_INDEN___################################
GSUB_INDEN <- function(XV, INDEN_OLD, INDEN_NEW) {
	library(gsubfn)
	#XV <- GSUB(XV,'\t','  ')
	MAX_INDEN <- max(nchar(strapplyc(XV, "^ +")))
	for (i in 1:ceiling(MAX_INDEN/INDEN_OLD)) {
		OLD <- P0("^", strrep(" ", INDEN_OLD + (i - 1) * INDEN_NEW))
		NEW <- strrep(" ", INDEN_NEW * i)
		XV <- GSUB(XV,OLD,NEW)
	}
XV
}
###############################___GSUB_INDEN_XS___##############################
GSUB_INDEN_XS <- function( XS,INDEN_OLD,INDEN_NEW ) {
	GSUB( XS,P0('^',strrep(' ',INDEN_OLD)),strrep(' ',INDEN_NEW) )
}

#################################___HASH_NAME___################################
HASH_NAME <- function(XS,WORD_LEN,UND_VOL) {
  if ( missing(WORD_LEN) & missing(UND_VOL) ) {
  XS <- paste0('#_', XS, '_#')
  } else {
  if ( missing(WORD_LEN) | missing(UND_VOL) ) { 
  if ( !missing(WORD_LEN) ) {
  XS <- paste0('_', XS, '_')
  XS.NCHAR <- nchar(XS)
  HASH_NCHAR_LEFT <- ceiling((WORD_LEN - XS.NCHAR) / 2)
  HASH_NCHAR_RIGHT <- floor((WORD_LEN - XS.NCHAR) / 2)
  HASH_LEFT <- strrep("#", HASH_NCHAR_LEFT)
  HASH_RIGHT <- strrep("#", HASH_NCHAR_RIGHT)
  XS <- paste0(HASH_LEFT, XS, HASH_LEFT)
  if ( nchar(XS) > WORD_LEN ) { XS <- SUB( XS,1,-2 )} 
  } else {
  }
  } else {
  if ( nchar( XS ) < ( WORD_LEN - 2*UND_VOL - 1 ) ) {
  XS <- paste0(strrep('_',UND_VOL), XS, strrep('_',UND_VOL))
  XS.NCHAR <- nchar(XS)
  HASH_NCHAR_LEFT <- ceiling((WORD_LEN - XS.NCHAR) / 2)
  HASH_NCHAR_RIGHT <- floor((WORD_LEN - XS.NCHAR) / 2)
  HASH_LEFT <- strrep("#", HASH_NCHAR_LEFT)
  HASH_RIGHT <- strrep("#", HASH_NCHAR_RIGHT)
  XS <- paste0(HASH_LEFT, XS, HASH_LEFT)
  if ( nchar(XS) > WORD_LEN ) { XS <- SUB( XS,1,-2 )} 
  }}}
  XS
}
####################################___HAT___###################################
HAT <- function(XU,TAG) {
#html_attrs_table
	XRH <- RH(XU)
	if ( missing( TAG ) ) {
		XMISS <- T
		TAG <- 'a'
	} else {
		XMISS <- F
	}
	if(COUNT_WORDS(TAG) == 1) TAG <- LC(TAG)
	XL_ATTRS_ALL <- html_nodes(XRH,TAG) %>% html_attrs()
	XV_ATTRS <- unique(unlist(lapply(XL_ATTRS_ALL,names)))
	X_LEN <- length(XL_ATTRS_ALL)
	XL_ATTRS_ALL <- OKL( XL_ATTRS_ALL )
	ZT <- as.data.frame(t(unlist(XL_ATTRS_ALL[[1]])))
	if ( X_LEN>1 ) {
		for ( i in 2:X_LEN ) {
			ZT <- bind_rows( ZT , as.data.frame(t(unlist(XL_ATTRS_ALL[[i]]))) )
		}
	}
	################################################################
	XV_TEXT <- stri_trans_toupper(html_nodes(XRH,TAG) %>% html_text())
  XV_TEXT <- gsub("[\n\r\t\v\f]", " ",XV_TEXT)
	XV_TEXT <- gsub("[[:space:]]+", " ",XV_TEXT)
	ZT <- data.table(TEXT=XV_TEXT,ZT)
	################################################################
	ZT <- as.data.table(setnames(ZT,gsub("\\bR\\b", "R_ORG",colnames(ZT))))
	ZT[, R := .I]
	colnames(ZT) <- stri_trans_toupper(colnames(ZT))
	if ( XMISS ) {
		ZT <- ZT[,c('R','TEXT','HREF')]
		ZT[ , N:=.N , by = .(TEXT,HREF) ]
	}
CLEAN_TABLE(ZT)
}
###################################___HATR___###################################
HATR <- function(XU,TAG) {
	XRH <- RH(XU)
	html_elements(XRH,TAG) %>% html_attrs(  )
}
#################################___HEAD_TAIL___################################
HEAD_TAIL <- function( X, SHORT_COL=FALSE ) {
if ( IS_LT( X ) ) {
Z <- lapply(X, function( Y ) rbind( head( Y,2 ),tail( Y,2 ) ) )
if ( SHORT_COL ) {
Z <- 	lapply(Z, function( Y ) Y[,1:min( ncol( Y ),5 )])
}
} else {
if ( IS_TABLE( X ) ) {
Z <- unique(rbind( head( X,3 ),tail( X,3 )	))
} else {
Z <- c( head( X,3 ),tail( X,3 ) )
}}
return(Z)
}
###############################___HIST_IS_HASH___###############################
HIST_IS_HASH <- function(XV, i) {
	iXV <- XV[i]
	str_starts(iXV, "##------ ") & (!(str_starts(iXV, "##------\\* Session start at") & str_ends(iXV, " \\*------##")))
}
#############################___HIST_IS_REDUNDANT___############################
HIST_IS_REDUNDANT <- function(XV, i) {
	iXV <- XV[i]
	iXV_BEF <- XV[i-1]
	iXV_AFT <- XV[i+1]
	HASH_BEF <- (str_starts(iXV_BEF, "##------ ") & str_ends(iXV_BEF, " ------##")) & !HIST_IS_SESSION(XV,i-1)
	HASH_AFT <- (str_starts(iXV_BEF, "##------ ") & str_ends(iXV_AFT, " ------##")) & !HIST_IS_SESSION(XV,i+1)
	IS_iXV_TRASH <- !(GREPL( iXV ,'<-') | GREPL( iXV ,':=') | GREPL( iXV ,' = ') | GREPL( iXV,'setnames'))
	#NOT_AFT <- GREPL( iXV_AFT ,'<-') | GREPL( iXV_AFT ,':=') | GREPL( iXV_AFT ,' = ') | GREPL( iXV_AFT ,'setnames')
	#list( iXV_BEF,iXV,iXV_AFT )
	if (  all( HASH_BEF,HASH_AFT,IS_iXV_TRASH ) ) {
		Z <- TRUE
	} else {
		Z <- FALSE
	}
Z
}
##############################___HIST_IS_SESSION___#############################
HIST_IS_SESSION <- function(XV, i) {
	iXV <- XV[i]
	(str_starts(iXV, "##------\\* Session start at") & str_ends(iXV, " \\*------##"))
}
##############################___HOUR_TO_SECONDS___#############################
HOUR_TO_SECONDS <- function(XS_HOURS) {
	period_to_seconds(hms(XS_HOURS) )
}
###############################___HOW_LONG_AGO___###############################
HOW_LONG_AGO <- function( XDATE,R=0 ) {
	library( lubridate )
	TODAY.DAY = yday(Sys.Date())
	TODAY.YEAR = year(Sys.Date())
	XDATE.DAY = yday(XDATE)
	XDATE.YEAR = year(XDATE)
	DAYS <- (TODAY.YEAR - XDATE.YEAR) * 365 + ( TODAY.DAY - XDATE.DAY )
	HOURS <-R(as.numeric(difftime(Sys.time(  ) , XDATE,units="hours"))/24,4)
  DAYS + HOURS
}
####################################___HT___####################################
HT <- function(XU,xall=FALSE) {
	XRH <- READ_HTML( XU )
	ZL <- html_nodes(XRH, 'table') %>% html_table(fill = TRUE)
	ZL <- lapply(ZL, as.data.table)
	if (!xall) {
		XNROW <- sapply(ZL, nrow)
		XW <- min(which(XNROW==max(XNROW)))
		Z <- ZL[[XW]]
	} else {
		Z <- ZL
	}
	XHAS <- HAS( XRH ,'table')
	ID <- XHAS$ID
	ID[which(is.na(ID))] <- 'X' + which(is.na(ID))
	if ( !is.null(ID) ) {names( Z ) <- ID}
	Z
}
###################################___HTEXT___##################################
HTEXT <- function(XU,TAG) {
	XRH <- RH(XU)
	html_elements(XRH,TAG) %>% html_text(  )
}
####################################___HTL___###################################
HTL <- function(XU) {
	XRH <- RH(XU)
	ZL <- html_nodes(XRH, "table") %>% html_table(fill = TRUE)
	ZL <- LAP( ZL, CLEAN_TABLE )
ZL
}
##################################___HTL_ID___##################################
HTL_ID <- function(XU) {
	XRH <- GH(XU_PL)
	ZL <- html_nodes(XRH, "table") %>% html_table(fill = TRUE)
	ID <- HAT( XRH ,'table')$ID
	ID[which(is.na(ID))] <- 'X' + which(is.na(ID))
	ID <- UP(GSUB( ID,'-','_' ))
	ID <- GSUB( ID,'_+','_' )
	names( ZL )  <- ID
	ZL <- LAP( ZL, CLEAN_TABLE )
ZL
}
##############################___HTML_ATR_TABLE___##############################
HTML_ATR_TABLE <- function( XU,TAG='table') {
	library( dplyr )
	XRH <- RH(XU)
	XRH <- RH(XRH)
	XL_ATTRS_ALL <- html_nodes(XRH,TAG) %>% html_attrs()
	XV_ATTRS <- unique(unlist(lapply(XL_ATTRS_ALL,names)))
	X_LEN <- length(XL_ATTRS_ALL)
	ZT <- as.data.frame(t(unlist(XL_ATTRS_ALL[[1]])))
	if ( X_LEN>1 ) {
		for ( i in 2:X_LEN ) {
			ZT <- bind_rows( ZT , as.data.frame(t(unlist(XL_ATTRS_ALL[[i]]))) )
		}
	}
	ZT <- as.data.table(setnames(ZT,gsub("\\bR\\b", "R_ORG",colnames(ZT))))
	ZT[, R := .I]
	colnames(ZT) <- stri_trans_toupper(colnames(ZT))
CLEAN_TABLE(ZT)
}
##############################___HTML_SELECTORS___##############################
HTML_SELECTORS <- function( XU,ATR='table',ALL=F) {
	library( dplyr )
	XRH <- RH(XU)
	XL_ATTRS_ALL <- html_nodes(XRH,ATR) %>% html_attrs()
	XV_ATTRS <- unique(unlist(lapply(XL_ATTRS_ALL,names)))
	X_LEN <- length(XL_ATTRS_ALL)
	ZT <- as.data.frame(t(unlist(XL_ATTRS_ALL[[1]])))
	if ( X_LEN>1 ) {
		for ( i in 2:X_LEN ) {
			ZT <- bind_rows( ZT , as.data.frame(t(unlist(XL_ATTRS_ALL[[i]]))) )
		}
	}
	ZT <- as.data.table(setnames(ZT,gsub("\\bR\\b", "R_ORG",colnames(ZT))))
	ZT[, R := .I]
	colnames(ZT) <- stri_trans_toupper(colnames(ZT))
	if (!ALL) {
		ZT <- SEL_COL( ZT,'XR|CLASS|ID' )
		if ( ncol( ZT )==4 ) {
			colnames(ZT) <- c('XR','XC','XS','XID')
		}}
ZT
}
#################################___HTML_TAG___#################################
HTML_TAG <- function(X,TAG) {
	TAG <- ifelse(IS_CHAR(TAG),TAG,12)
	TAG_START <- P0( '<',TAG,'>' )
	TAG_END <- P0( '</',TAG,'>' )
	C(TAG_START,X,TAG_END)
}
####################################___IB___####################################
IB <- function(X,V=FALSE,STRICT = FALSE){
	if(is.function(X)) {Z <- FALSE}
	if ( !V ) {
		Z <- (is.null(X) | length(X) == 0 | is.na(X) | X=="")
		if ( !STRICT ) {Z <- Z | (gsub( '[[:space:]]+','',X ) == '')}
	} else {
		Z <- all(is.null(X) | length(X) == 0 | is.na(X) | X=="")
		if ( !STRICT ) {Z <- Z || (gsub( '[[:space:]]+','',X ) == '')}
	}
	Z
}
####################################___ICO___###################################
ICO <- function( X ) {
	invisible(capture.output( X )	)
}
####################################___IF___####################################
IF <- function(XQ,X1,X0) {
	if (XQ) {
		if ( is.function(X1) ) {
			Z <- X1()
		} else {
			Z <- X1
		}
	} else {
		if ( is.function(X0) ) {
			Z <- X0()
		} else {
			Z <- X0
		}
	}
Z
}
###############################___IF_ELSE_FAKE___###############################
IF_ELSE_FAKE <- function(...) {
if (!exists("A") & exists("B")) {
  C= B
} else if (exists("A") & !exists("B")) {
  C= A
} else if (exists("A") & exists("B")) {
  C= rbind(B,A)
} else {C <- NULL}
}
####################################___IN___####################################
IN <- function(XV1,XV2) {
	XV1[XV1 %in% XV2]
}
################################___INCREASE_i___################################
INCREASE_i <- function(X) {
	if ( missing( X ) ) {
		i <<- i + 1
	} else {
		i <<- X
	}
print( i )	
}
##################################___INDEN42___#################################
INDEN42 <- function(ZV) {
	ZV <- GSUB(ZV,'\t',' ')
	XV <- strapplyc(SV(ZV,'^ '), "^\\s+")
	XINDEN <- strapplyc(SV(ZV,'^ '), "^\\s+")
	XINDEN <- NCH(XINDEN)
	if (max(XINDEN)==0) {
		ZV[2:(LEN(ZV)-1)] <- P('\t',ZV[2:(LEN(ZV)-1)])
	}	
	if ( min(XINDEN)==4 ) {
		MAX_INDEN <- max(nchar(strapplyc(SV(ZV,'^ '), "^\\s+")))
		for (i in (MAX_INDEN/2):1 ) {
			OLD <- P0("^", strrep(" ", 4*i))
			NEW <- strrep('\t',i)
			ZV <- GSUB(ZV,OLD,NEW)
		}
	}
ZV
}
#################################___IS_BLANK___#################################
IS_BLANK <- function(X, false.triggers=FALSE){
	if(is.function(X)) return(FALSE) # Some of the tests below trigger
	if(length(X) > 0) return(FALSE) # Some of the tests below trigger
	return(
	is.null(X) || # Actually this line is unnecessary since
	length(X) == 0 || # length(NULL) = 0, but I like to be clear
	all(is.na(X)) ||
	all(X=="") ||
	(false.triggers && all(!X)) ||
	grepl("^\\s*$",X)
	)
}
################################___IS_BLANK_1___################################
IS_BLANK_1 <- function(X) {
	isTRUE(is.function(X)) | isTRUE(is.null(X)) | isTRUE(length(X) == 0) | isTRUE(is.na(X) | isTRUE(X == ""))
}
################################___IS_BLANK_2___################################
IS_BLANK_2 <- function(X, V = FALSE) {
	isTRUE(is.function(X)) | isTRUE(is.null(X)) | isTRUE(length(X) == 0) | isTRUE(is.na(X) | isTRUE(X == ""))
}
###############################___IS_BLANK_ALL___###############################
IS_BLANK_ALL <- function( X ) {
	ALL( IS_BLANK( X ) )
}
###############################___IS_BLANK_LIST___##############################
IS_BLANK_LIST <- function( XL ) {
	(is.null(XL) & class(XL) == "NULL" & class(XL) == "logical" & length(XL) != 0)
}
###############################___IS_BLANK_OLD___###############################
IS_BLANK_OLD <- function(X,V=FALSE){
  if( isTRUE(is.function(X)) ) {Z <- FALSE}
  if ( !V ) {
  Z <- (isTRUE(is.null(X)) | isTRUE(length(X) == 0) | isTRUE(is.na(X) | isTRUE(X=="")))
  if ( !STRICT ) {Z <- Z | (gsub( '[[:space:]]+','',X ) == '')}
  } else {
  Z <- all(isTRUE(is.null(X)) | isTRUE(length(X) == 0) | isTRUE(is.na(X) | X==""))
  if ( !STRICT ) {Z <- Z || (gsub( '[[:space:]]+','',X ) == '')}
  }
  Z
}
###############################___IS_COL_AS_ROW___##############################
IS_COL_AS_ROW <- function( XT ) { 
	(LEN(colnames( XT ))>2 & 
	LEN(unique( colnames( XT )))==1 & 
	max(nchar( colnames( XT )))>3
	) | 
	(LEN(W(colnames(XT) == U2( XT[1] ))) / LEN( colnames( XT ) ) > 0.5)
}
###################################___IS_EQ___##################################
IS_EQ <- function(target, current, ...)  {
	XTC <- TC0(all.equal(target, current, ...) )
ifelse(isTRUE(XTC),T,F)
}
##################################___IS_ESC___##################################
IS_ESC <- function( X ) {
	X <- deparse(substitute(X))
	if (IS_QUOTE( X )) {
		X <- GSUB( X,'^\\"','' )
		X <- GSUB( X,'\\"$','' )
	}
S( X,'^\\\\|^/' )
}
################################___IS_ESC_NUM___################################
IS_ESC_NUM <- function( X ) {
	X <- deparse(substitute(X))
	if (IS_QUOTE( X )) {
		X <- GSUB( X,'^\\"','' )
		X <- GSUB( X,'\\"$','' )
	}
	Q1 <- S( X,'^\\\\|^/' )
	X2 <- GSUB( X,'^\\\\|^/','')
	Q2 <- S( X2,'^\\d' )
Q1 & Q2
}
###############################___IS_FAKE_NULL___###############################
IS_FAKE_NULL <- function(X) {
(IS_BLANK(X) | trimws(X)=='' | trimws(X)=='<NA>'  | is.na( X ) )  
}
#############################___IS_FUNCTION_START___############################
IS_FUNCTION_START <- function( XV,i ) {
  (stri_sub(XV[i], -2, -1) == "<-" & stri_sub(XV[i + 1], 1, 8) == "function")
}
#############################___IS_HIST_REDUNDANT___############################
IS_HIST_REDUNDANT <- function(XV, i) {
	iXV <- XV[i]
	iXV_BEF <- XV[i-1]
	iXV_AFT <- XV[i+1]
	HASH_BEF <- (str_starts(iXV_BEF, "##------ ") & str_ends(iXV_BEF, " ------##"))
	HASH_AFT <- (str_starts(iXV_BEF, "##------ ") & str_ends(iXV_AFT, " ------##"))
	NOT_BEF <- GREPL( iXV_BEF ,'<-') | GREPL( iXV_BEF ,':=') | GREPL( iXV_BEF ,' = ') | GREPL( iXV_BEF ,'setnames')
	NOT_AFT <- GREPL( iXV_AFT ,'<-') | GREPL( iXV_AFT ,':=') | GREPL( iXV_AFT ,' = ') | GREPL( iXV_AFT ,'setnames')
	if (  any(HIST_IS_SESSION( XV,i ),NOT_BEF,NOT_AFT) ) {
		Z <- TRUE
	} else {
		Z <- FALSE
	}
Z
}
###################################___IS_NA___##################################
IS_NA <- function(X) {
	suppressWarnings(is.na(X))
}
################################################################################
IF_ER <- function(X,XER=NA) {
	XTC <- TC(X)
	if ( XTC$OK ) {
		XTC$VALUE
	} else {
		XER
	}
}
##################################___IS_NAN___##################################
IS_NAN <- function(X) {
	IF_ER(is.nan(X),F)
}
###############################___IS_NOT_NUM_XV___##############################
IS_NOT_NUM_XV <- function(XV) {
	XV <- na.omit(XV_TEXT_NUMBERS)
(all( !suppressWarnings( is.na(as.numeric(as.character(XV)))  )))
}
##################################___is_num___##################################
is_num <- function(X){
	is.numeric(as_num(X))
}
##################################___IS_NUM___##################################
IS_NUM <- function( XV ) {
	(all( !suppressWarnings( is.na(as.numeric(as.character(na.omit(XV))))  )))
}
#################################___IS_NUM_0___#################################
IS_NUM_0 <- function( X ) !suppressWarnings( is.na(as.numeric(as.character((X)))))
################################___IS_NUM_ALL___################################
IS_NUM_ALL <- function( XV ) ALL( IS_NUM( XV ) )
###################################___IS_OK___##################################
IS_OK <- function(X, XZ = NA) {
	XZ <- OK(X)
if ( is.na(XZ) ) {
	F
} else {
	T
}
}
##################################___is_pipe___#################################
is_pipe <- function(pipe) {
  identical(pipe, quote(`%>%`))   ||
  identical(pipe, quote(`%T>%`))  ||
  identical(pipe, quote(`%<>%`))  ||
  identical(pipe, quote(`%$%`)) ||
  identical(pipe, quote(`%pipe%`)) # <- added line
}
#################################___IS_PRIME___#################################
IS_PRIME <- function(num) {
   if (num == 2) {
  TRUE
   } else if (any(num %% 2:(num-1) == 0)) {
  FALSE
   } else { 
  TRUE
   }
}
#################################___IS_QUOTE___#################################
IS_QUOTE <- function( X ) {
	GREPL( X,'^"' ) & GREPL( X,'"$' )	
}
################################___IS_SESSION___################################
IS_SESSION <- function( iXV) {
	(str_starts(iXV, "##------\\* Session start at") & str_ends(iXV, " \\*------##"))
}
#################################___IS_TABLE___#################################
IS_TABLE <- function( XT ) {
	if ( is.matrix(XT) | is.data.frame(XT) | is.data.table(XT) ) {
		TRUE
	} else {
		FALSE
	}
}
##################################___IS_TRUE___#################################
IS_TRUE <- function(X) {
	XTC <- TC(X)
	if ( !XTC$OK ) {
		Z <- F
	} else if (IS_EQ(XTC$VAL,T)) {
		Z <- T
	} else {
		Z <- F
	}
Z
}
#####################################___J___####################################
J <- function(x, y,...){
	DT(merge(x, y, ...))
}
####################################___J0___####################################
J0 <- function(...) {
	XL <- L(...)
	XL.TBL <- XL[W(SAP(XL,IS_TABLE))]
	XL.CH <- UL(XL[W(SAP(XL,IS_CH))])
DT(Reduce(function(XT1,XT2) merge(XT1, XT2, by = XL.CH),XL.TBL))
}
##############################___JOIN_MAP_TABLE___##############################
JOIN_MAP_TABLE <- function(SHP_FILE_NAME,XT,SHP_COL,TAB_COL) {
	if ( str_to_upper(SUB( SHP_FILE_NAME,-4,-1 ))!= ".SHP" ) { SHP_FILE_NAME <- SHP_FILE_NAME + '.SHP'}
	XT_SHP <- read_sf(SHP_FILE_NAME)
	XT_GEO <- XT_SHP %>% select(NAME = SHP_COL, geometry)
	Encoding(XT_GEO$NAME)  <- 'UTF-8'
	ZT <- left_join(XT_GEO, XT, by = c('NAME' = TAB_COL))
ZT
}
#####################################___L___####################################
L <- function(...) {
	names <- as.list(substitute(list(...)))[-1L]
	setNames(list(...), names)
}
###############################___LEADING_ZEROS___##############################
LEADING_ZEROS <- function( X, FINAL_NCHAR = 2) {
	X_NCHAR <- nchar( X )
	if ( FINAL_NCHAR > X_NCHAR ) { X <- paste0(strrep( 0, FINAL_NCHAR - nchar(X)),X) } 
	X
}
####################################___LF___####################################
LF <- function(X_PATH=getwd(  ),XS='') {
	if (missing(X_PATH)) {X_PATH <- getwd()}
	if (X_PATH=='ALL' || X_PATH==0) {
	X_PATH <- 'C:/'
	XV_ALL_FILES <- list.files(X_PATH,full.names = TRUE,ignore.case = TRUE, recursive = TRUE,include.dirs=TRUE)
	} else {
	XV_ALL_FILES <- list.files(X_PATH,full.names = TRUE,ignore.case = TRUE, recursive = TRUE)
	}
GREP( XV_ALL_FILES,XS )
}
###################################___LF_WD___##################################
LF_WD <- function(XS='') {
	XV_ALL_FILES <- list.files(getwd(),full.names = TRUE,ignore.case = TRUE, recursive = TRUE)
	GREP( XV_ALL_FILES,XS )
}
####################################___LF0___###################################
LF0 <- function(path = ".", pattern = NULL, all.files = FALSE, 
  full.names = T, recursive = F, ignore.case = FALSE, 
  include.dirs = FALSE, no.. = FALSE) {
	list.files(path,pattern,full.names=full.names)
}
####################################___LFT___###################################
LFT <- function(XP,...) {
#list_file_table
	XL <- list()
	XLF <- LF0(XP,...)
	XV_MTIME <- XV_HOW_LONG_AGO <- XV_SIZE <- vector()
	for (i in seq_along(XLF)) {
		X.FILE_INFO <- file.info((XLF[i]))
		XV_MTIME[i] <- as.character(X.FILE_INFO$mtime)
		XV_SIZE[i] <- FILE_SIZE(X.FILE_INFO$size)
		XV_HOW_LONG_AGO[i] <- HOW_LONG_AGO(X.FILE_INFO$mtime)
	}
XT <- data.table(MTIME = XV_MTIME, HLA = XV_HOW_LONG_AGO, NAME = basename(XLF), PATH = XLF, SIZE = XV_SIZE)
XT$SIZE <- as.numeric(XT$SIZE)
XT[o(HLA)]
}
###################################___LIKE___###################################
LIKE <- function(XV,XS) {
XV_SPL <- unlist(stri_split_fixed(XS,',')) #OK
XVB_SPL_OK <- !stri_sub(XV_SPL,1,1) %in% c('!','-')
if ( any( XVB_SPL_OK ) ) {
XV_SPL_OK <- XV_SPL[XVB_SPL_OK]
XV_SPL_NOK <- stri_sub(XV_SPL[!XVB_SPL_OK],2,-1)
XL_XV_OK <- lapply(XV_SPL_OK,function(X) flike(XV ,X))
XL_XV_NOK <- lapply(XV_SPL_NOK,function(X) flike(XV ,X))
ZV_OK <- Reduce(intersect,XL_XV_OK)
ZV <- setdiff(ZV_OK,unlist( XL_XV_NOK ))
} else {
XV_XS_NOT <- stri_sub(XV_SPL,2,-1)
XL_OK <-  lapply( XV_XS_NOT , function( X ) NOT_LIKE( XV, X ))
ZV <- Reduce(intersect,XL_OK)
}
ZV
}
###################################___LIKE0___##################################
LIKE0 <- function(XV,XS) {
	XV_SPL <- unlist(stri_split_fixed(XS,','))
	IS_OK <- !stri_sub(XV_SPL,1,1) %in% c('!','-')
	XV_SPL_OK <- XV_SPL[IS_OK]
	XV_SPL_NOK <- stri_sub(XV_SPL[!IS_OK],2,-1)
	XL_VAL_OK <- lapply(XV_SPL_OK,function(X) flike(XV ,X))
	XL_VAL_NOK <- lapply(XV_SPL_NOK,function(X) flike(XV ,X))
	ZV_OK <- Reduce(intersect,XL_VAL_OK)
Reduce(setdiff,list( ZV_OK , XL_VAL_OK ))
}
################################___LIST_FILES___################################
LIST_FILES <- function(XS,X_PATH) {
	if (missing(X_PATH)) {X_PATH <- getwd()}
	if (X_PATH=='ALL' || X_PATH==0) {
		X_PATH <- 'C:/'
		XV_ALL_FILES <- list.files(X_PATH,full.names = TRUE,
			ignore.case = TRUE, recursive = TRUE,include.dirs=TRUE
		)
	} else {
		XV_ALL_FILES <- list.files(X_PATH,full.names = TRUE,ignore.case = TRUE, recursive = TRUE)
	}
	GREP(XV_ALL_FILES,XS)
}
################################___LIST_TO_DT___################################
LIST_TO_DT <- function(XL) {
	data.table(structure( unname(do.call(Map,c(c,XL)) )  , class = "data.table")	)
}
###############################___LOAD_PACKAGES___##############################
LOAD_PACKAGES <- function(...) {
 libs<-unlist(list(...))
 req<-unlist(lapply(libs,require,character.only=TRUE))
 need<-libs[req==FALSE]
 if(length(need)>0){ 
  install.packages(need)
  lapply(need,require,character.only=TRUE)
 }
}

###################################___lsos___###################################
lsos <- function(..., n=10) {
.ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
################################___LSOS_CLEAN___################################
LSOS_CLEAN <- function(  ) {
	print(lsos())
	XT_LSOS <- lsos(  )
	XT_LSOS.ROWNAMES <- rownames( XT_LSOS )
	XT_LSOS.TYPE <- XT_LSOS[,'Type']
	
	XT <- XT_LSOS[XT_LSOS.TYPE %in% c('data.table', 'matrix', 'data.frame'),]
	XT.ROWNAMES <- rownames( XT )
	for ( i in  1:nrow(XT)) {
		WT(get( XT.ROWNAMES[i]),P0(XT.ROWNAMES[i],'.CSV' ) )
		rm( list=XT.ROWNAMES[i] , envir = .GlobalEnv )
	}
	
	XT <- XT_LSOS[XT_LSOS.TYPE %in% c('list', 'character'),]
	XT.ROWNAMES <- rownames( XT )
	for ( i in  1:nrow(XT)) {
		WL(unlist(get( XT.ROWNAMES[i])),P0(XT.ROWNAMES[i],'.TXT' ) )
		rm( list=XT.ROWNAMES[i] , envir = .GlobalEnv )
	}
	print(lsos())
}
################################___MAKE_NAMES___################################
MAKE_NAMES <- function(X) {
	MAKE_NAMES_0 <- function(X) {
		X <- CLEAN_STRING( X )
		X <- GSUB(X,'_+','_')
		X <- GSUB(X,'[^[:^punct:]_]','_')
		X <- GSUB(X,' ','_')
		X <- GSUB(X,'_+','_')
		X <- GSUB(X,'_$','')
		X <- make.names(X,unique=T)
		UC(GSUB( X,'\\.','' ))	
	}
	if ( is.vector( X ) ) {
		X <- MAKE_NAMES_0( X )	
	} else {
		if( IS_TABLE(X) ) {
			colnames(X) <- MAKE_NAMES_0( colnames(X) )
		} else {
			X  <- 'NOT VECTOR OR TABLE'	
		}
	}
X
}
####################################___MAP___###################################
MAP <- function(XTM,XY,XTRANS='identity',XCOLOR) {
	if (MIS(XY)) {
		XTM.CL <- SAP(XTM,CL)
		XTM.TAG <- colnames(XTM)
		XW <- max(W(XTM.CL=='numeric'))
		XY <- XTM.TAG[XW]
	}
	if (MIS(XCOLOR)) {
		XCOLOR <- blues9
	}
	XPLOT <- ggplot() + geom_sf(data = XTM,aes_string(fill=XY)) + 
		scale_fill_gradientn(trans=XTRANS,colours=XCOLOR)
XPLOT
}
##################################___MAP_ID___##################################
MAP_ID <- function(XTM,XY,XCOLOR) {
	if (MIS(XY)) {
		XTM.CL <- SAP(XTM,CL)
		XTM.TAG <- colnames(XTM)
		XW <- max(W(XTM.CL=='numeric'))
		XY <- XTM.TAG[XW]
	}
	if ( MIS(XCOLOR) ) XCOLOR<-c( rev(viridis::viridis(100) ))
################################################################
ggplot() + 
geom_sf(data = XTM,aes_string(fill=XY)) + 
scale_fill_gradientn(colours=XCOLOR)
################################################################
}
##################################___MAP_LOG___#################################
MAP_LOG <- function(XTM,XY,XCOLOR) {
	if (MIS(XY)) {
		XTM.CL <- SAP(XTM,CL)
		XTM.TAG <- colnames(XTM)
		XW <- max(W(XTM.CL=='numeric'))
		XY <- XTM.TAG[XW]
	}
	if ( MIS(XCOLOR) ) XCOLOR<-c( rev(viridis::viridis(100) ))
	MAX_LOG <- log1p(max(na.omit(XTM[[XY]])))
	MIN_LOG <- log1p(min(na.omit(XTM[[XY]])))
	XV_BREAKS <- ROUND(expm1( seq(MIN_LOG, MAX_LOG,  length.out=6) ) )
	XV_BREAKS <- ROUND(XV_BREAKS[seq_along(XV_BREAKS) %% 2 ==0],0)
	XV_BREAKS <- c( ROUND(expm1(MIN_LOG),0),XV_BREAKS )
	XV_LABELS  <- XV_BREAKS
################################################################
ggplot() + geom_sf(data = XTM,aes_string(fill=XY)) + 
scale_fill_gradientn(trans='log1p',
	colours=XCOLOR,
	breaks = XV_BREAKS,
	labels = XV_LABELS,
)
################################################################
}
#################################___MAP_LOG1___#################################
MAP_LOG1 <- function(XTM,XY,XCOLOR) {
	if (MIS(XY)) {
		XTM.CL <- SAP(XTM,CL)
		XTM.TAG <- colnames(XTM)
		XW <- max(W(XTM.CL=='numeric'))
		XY <- XTM.TAG[XW]
	}
	if (MIS(XCOLOR)) {
		XCOLOR <- blues9
	}
	XPLOT <- ggplot() + geom_sf(data = XTM,aes_string(fill=XY)) + 
		scale_fill_gradientn(trans='log1p',colours=XCOLOR)
XPLOT
}
###################################___MAP0___###################################
MAP0 <- function(XTM,XY,XTRANS='identity',XCOLOR) {
	if (MIS(XY)) {
		XTM.CL <- SAP(XTM,CL)
		XTM.TAG <- colnames(XTM)
		XW <- max(W(XTM.CL=='numeric'))
		XY <- XTM.TAG[XW]
	}
	if (MIS(XCOLOR)) {
		XCOLOR <- blues9
	}
	XPLOT <- ggplot() + geom_sf(data = XTM,aes_string(fill=XY)) + 
		scale_fill_gradientn(trans=XTRANS,colours=XCOLOR)
XPLOT
}
###############################___MAX_NCHAR_ROW___##############################
MAX_NCHAR_ROW <- function( XT ,FUN=NCHAR) {
	MAX(AP(AP(XT,1:2,FUN ),1,SUM))
}
###################################___MAXV___###################################
MAXV <- function(XV,XN=0) {
	SAP( XV,function( X ) MAX(X,XN) )
}
####################################___MC___####################################
MC <- function(...) {
	CH(match.call(expand.dots=FALSE)$`...`)
}
##################################___MC_LAP___##################################
MC_LAP <- function(...) {
	LAP(match.call(expand.dots=FALSE)$`...`,CH)
}
####################################___MC0___###################################
MC0 <- function(...) {
  XL <- match.call(expand.dots = FALSE)$`...`
XL
}
#################################___MERGE_ALL___################################
MERGE_ALL <- function( XT1,XT2,COL) {
  merge(XT1, XT2, by = COL, all = TRUE)
}
################################___MERGE_LIST___################################
MERGE_LIST <- function( XL,COL) {
  Reduce(function(XT1,XT2) merge(XT1, XT2, by = COL),XL)
}
##############################___MERGE_LIST_ALL___##############################
MERGE_LIST_ALL <- function( XL,COL) {
  Reduce(function(XT1,XT2) merge(XT1, XT2, by = COL, all = TRUE),XL)
}
##############################___MERGE_LIST_LEFT___#############################
MERGE_LIST_LEFT <- function( XL,COL) {
  Reduce(function(XT1,XT2) merge(XT1, XT2, by = COL, all.x = TRUE),XL)
}
################################___MIN_AS_NUM___################################
MIN_AS_NUM <- function( XV ) {
	unname(SAP( XV,MIN_AS_NUM_0 ))
}
###############################___MIN_AS_NUM_0___###############################
MIN_AS_NUM_0 <- function(X0) {
	XV_SPL <- SPL( X0,':' )
	XMIN <- as.numeric(XV_SPL[1])
	XSEC <- as.numeric(XV_SPL[2])/60
R( XMIN+XSEC )
}
###############################___MIN_AS_NUMBER___##############################
MIN_AS_NUMBER <- function(XV) {
	xv <- trimws(XV)
	ZV <- XV
	XV <- na.omit(XV)
	if ( IS_NUM(SUB( XV,1,2 )) & IS_NUM(SUB( XV,4,5 )) & all(SUB( XV,3,3 )==':') ) {
		ZV <- 	sapply( ZV, CONVERT_MIN_TO_NUMBER)
	}
ZV
}
##################################___MIN_MAX___#################################
MIN_MAX <- function(XV,XMIN=0,XMAX=1) {
	XV_MIN_MAX_NORM <- MIN_MAX_NORM(XV)
	XV_MIN_MAX_NORM * ( XMAX-XMIN ) + XMIN
}
###############################___MIN_MAX_NORM___###############################
MIN_MAX_NORM <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
###################################___MKDIR___##################################
MKDIR <- function(XDIR) {
	XWD <- getwd(  )
	XV <- SPL(XDIR,'/')
	for ( i in seq_along(XV)) {
		if (!dir.exists(XV[i])) {dir.create(XV[i])}
		setwd( XV[i] )
	}
}
##################################___MKDIR0___##################################
MKDIR0 <- function(X) {
	X <- deparse(substitute(X))
	if (IS_QUOTE( X )) {
		X <- GSUB( X,'"','' )
		X <- GSUB( X,"'","'" )	
	} else {
		if ( G( X,':' )  ) {
			X <- GSUB(X,':',':/')
		}
	}
if (!dir.exists(X)) {dir.create(X)}
}
###################################___MODE___###################################
MODE <- function(XV) {
  uniqv <- unique(XV)
  uniqv[which.max(tabulate(match(XV, uniqv)))]
}
##################################___MODIFY___##################################
MODIFY <- function(XV1_OLD,XV_NEW){
	assign(deparse(substitute(x)), XV2, env=.GlobalEnv)
}
##################################___MPG2SEC___#################################
MPG2SEC <- function(XS) {
XV_SPL <- SPL( XS,':' )
XMIN <- as.numeric(XV_SPL[1])
XSEC <- as.numeric(XV_SPL[2])/60
return( ROUND( XMIN+XSEC ) )
}
###########################___MULTIPLICATION_TABLE___###########################
MULTIPLICATION_TABLE <- function(XT,X) {
	XT <- as.data.table(XT)
	XT.TAG <- colnames(XT)
	XT.CL <- sapply(XT,class)
	XW_NUM <- which(XT.CL %in% c('numeric','integer'))
	XW_CHAR <- which(!XT.CL %in% c('numeric','integer'))
	XT_NUM <- XT[,..XW_NUM] %*0% X
	XT_CHAR <- XT[,..XW_CHAR]
	XT <- cbind(XT_CHAR ,XT_NUM )
XT[,XT.TAG,with=F][]
}
################################___mutate_all___################################
mutate_all <- function (.tbl, .funs, ...) {
  lifecycle::signal_superseded("1.0.0", "mutate_all()", "across()")
  check_grouped(.tbl, "mutate", "all", alt = TRUE)
  funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), 
  ..., .caller = "mutate_all")
  mutate(.tbl, !!!funs)
}
#################################___namedList___################################
namedList <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)),deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
  setNames(L,nm)
}
##################################___napply___##################################
napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = 1)))
####################################___NCH___###################################
NCH <- function(X, type = "chars", allowNA = FALSE, keepNA = NA) {
	if (is.list(X)) {
		SAP(X,NCH0)
	} else {
		NCH0(X)
	}
}
###################################___NCH0___###################################
NCH0 <- function(X, type = "chars", allowNA = FALSE, keepNA = NA) {
	if (ALL(IS_BL(X))) {
		0
	} else {
		nchar(X, type = "chars", allowNA = FALSE, keepNA = NA)
	}
}
####################################___NFL___###################################
NFL <- function( XL,XN=1 ) {
	SAP( XL, function( X ) X[XN] )
}
#################################___NOT_WARN___#################################
NOT_WARN <- function (expr) {
  ops <- options(warn = -1)
  on.exit(options(ops))
  withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
}
####################################___NTH___###################################
NTH <- function (x, n, order_by = NULL, default = default_missing(x)) {
  if (length(n) != 1 || !is.numeric(n)) {
  abort("`n` must be a single integer.")
  }
  n <- trunc(n)
  if (n == 0 || n > length(x) || n < -length(x)) {
  return(default)
  }
  if (n < 0) {
  n <- length(x) + n + 1
  }
  if (is.null(order_by)) {
  x[[n]]
  }
  else {
  x[[order(order_by)[[n]]]]
  }
}
###############################___NTH_FROM_LIST___##############################
NTH_FROM_LIST <- function( XL,XN=1 ) {
	SAP( XL, function( X ) X[XN] )
}
####################################___NUM___###################################
NUM <- function(XT) {
	XT.CL <- sapply(XT,class)
	XW_NUM <- which(XT.CL %in% c('numeric','integer'))
	XT_NUM <- XT[,XW_NUM,with=FALSE]
XT_NUM
}
################################___NUM_TO_DATE___###############################
NUM_TO_DATE <- function( XV ) {
	XL <- SPL( XV,'\\.' )
	ZV <- vector(  )
	for ( i in seq_along(XL) ) {
		X_DAY <- XL[[i]][2]
		X_MON <- XL[[i]][1]
		if ( nchar( X_DAY ) == 1 ) X_DAY <- 0+X_DAY
		if ( nchar( X_MON ) == 1 ) X_MON <- 0+X_MON
		ZV[i] <- X_MON + '-' + X_DAY
	}
ZV
}
#################################___NUM2K_XV___#################################
NUM2K_XV <- function(XVN) {
	ZV <- vector(  )
	if ( ( IS_NUM(XVN) ) & (as.double(median( AS_NUM(XVN) )) >= 10000) ) {
		for ( i in seq_along( XVN ) ){
			XN <- XVN[i]
			if ( XN >= 10000 ) {
			 XN <- ROUND(XN/1000,0) + 'K'
			} 
			if ( XN >= 1000 & XN < 10000 ) {
			 XN <- ROUND(XN/1000,1) + 'K'
			}
			if ( XN >= 100 & XN < 1000 ) {
			 XN <- ROUND(XN/1000,2) + 'K'
			}
			if ( XN < 100 & is.numeric(XN)) {
			 XN <- ROUND(XN/1000,3) + 'K'
			}
			ZV[i] <- XN
		}
	} else {
	ZV <- XVN
	}
ZV
}
####################################___OK___####################################
OK <- function(X, XZ = NA) {
	#XTC <- TC(L()[1])
	if (XTC$OK) {
		if (!IS_BLANK(X) ) {
			XTC$VALUE
		} else {
			XZ
		}
	}
	else {
		XZ
	}
}
##################################___OK_ALL___##################################
OK_ALL <- function( X ) ALL( OK( X ) )
##################################___OK_OLD___##################################
OK_OLD <- function(X) {
	if ( is.vector(X) ) {
		X[!SAP(X,IS_BLANK)]
	} else {
		XTC <- TC(X)
		if ( XTC$OK ) {
			XTC$VALUE
		} else {
			0
		}
	}
}
####################################___OK0___###################################
OK0 <- function(X) {
	XTC <- TC(X)
	if ( XTC$OK ) {
		XTC$VALUE
	} else {
		XTC$ERROR
	}
}
####################################___OKE___###################################
OKE <- function(X) {
	XTC <- TC(X)
	if ( XTC$OK ) {
		XTC$VALUE
	} else {
		"ER"
	}
}
####################################___OKL___###################################
OKL <- function( XL ) {
	XL[	SAP(XL,function( X ) ALL(IS_BLANK( X )))] <- NA
XL
}
####################################___OKV___###################################
OKV <- function(X) {
	if(A(IS_BLANK(X))) {
		NULL
	} else {
	 X	
	}
}
#############################___OPENSUB_PARSE_NFO___############################
OPENSUB_PARSE_NFO <- function(X) {
	ID <- SUB(X,'//','/')
	XRL <- suppressWarnings(readLines(X))
	XRL <- GSUB(XRL,'U','')
	XRL <- GSUB(XRL,'?','')
	XRL <- GSUB(XRL,'?','')
	XRL <- GSUB(XRL,'?','')
	XRL <- CLEAN_STRING(XRL)
	XRL <- XRL[XRL!='']
	IMBD_SCORE <- GREP( XRL,'^IMDB Score')
	FPS <- GREP( XRL,'^FPS')
	IMBD_LINK <- GREP( XRL,'^IMDB LINK')
	SCORE <- AS_NUM(SUB(IMBD_SCORE,':',' /'))
	VOTES <- AS_NUM(SUB(IMBD_SCORE,'\\(',' votes'))
	FPS <- AS_NUM(SUB(FPS,':',-1))
	data.table(ID,FPS,SCORE,VOTES)
}
##########################___OPENSUB_PARSE_SRT_NAME___##########################
OPENSUB_PARSE_SRT_NAME <- function(X) {
	ID <- SUB(X,'//','/')
	X.EPISODE_NUMBER <- UP(SUB(X,'- ',' -'))
	if ( !is.na(X.EPISODE_NUMBER) ) {
		SEASON <- SUB(X.EPISODE_NUMBER,1,'X')
		EPISODE <- SUB(UP(X.EPISODE_NUMBER),'X',-1)	
	} else {
		X.EPISODE_NUMBER <- SUB(X,1,'\\.HDTV.X')
		X.EPISODE_NUMBER <- SUB(X.EPISODE_NUMBER,-6,-1)
		SEASON <- SUB(X.EPISODE_NUMBER,'S','E')
		EPISODE <- SUB(X.EPISODE_NUMBER,'E',-1)
	}
	data.table(ID,SEASON,EPISODE)
}
################################___OPENSUB_URL___###############################
OPENSUB_URL <- function( XS ) {
	if ( SUB(XS,1,1)=='/' ) {
		P0( 'https://www.opensubtitles.org', XS)		
	} else {
		P0( 'https://www.opensubtitles.org/', )
	}
}
####################################___OT___####################################
OT <- function(XT,XC){
#ORDER TABLE BY COLUMN
	XC <- deparse(substitute(XC))
	if ( str_starts(XC,'-') ) {
		XC <- B(XC,2,-1 )	
		ZT <- XT[o( -get( XC ) )]
	} else {
		ZT <- XT[o( get( XC ) )]	
	}
ZT
}
#######################___OTODOM_CHECK_NUMBER_OF_PAGES___#######################
OTODOM_CHECK_NUMBER_OF_PAGES <- function( XU) {
	XRH <- read_html( XU )
	XV_NUMBER_OF_PAGES <- trimws(html_nodes( XRH,'ul.pager li' )  %>% html_text())
	#X_NUMBER_OF_PAGES <- max(na.omit(AS_NUM( XV_NUMBER_OF_PAGES )))
	max(as.numeric(XV_NUMBER_OF_PAGES[IS_NUM(XV_NUMBER_OF_PAGES)]))
}
################################___OTODOM_MAP___################################
OTODOM_MAP <- function(XM,Y_FILL='VAL',Y_TRANS="identity",YV_COLORS=c( rev(viridis::viridis(100) ))) {
	if (Y_TRANS == "log1p"  ) {
		MAX_LOG <- log1p(max(na.omit(XM[[Y_FILL]])))
		MIN_LOG <- log1p(min(na.omit(XM[[Y_FILL]])))
		XV_BREAKS <- ROUND(expm1( seq(MIN_LOG, MAX_LOG,  length.out=6) ) )
		XV_BREAKS <- ROUND(XV_BREAKS[seq_along(XV_BREAKS) %% 2 ==0],0)
		XV_BREAKS <- c( ROUND(expm1(MIN_LOG),0),XV_BREAKS )
		XV_LABELS  <- XV_BREAKS
		ZM <- ggplot() + geom_sf(data = XM,aes_string(fill=Y_FILL)) + 
			scale_fill_gradientn(trans='log1p',
			colours=YV_COLORS,
			breaks = XV_BREAKS,
			labels = XV_LABELS
		)
	} else {
		ZM <-  ggplot() + geom_sf(data = XM,aes_string(fill=Y_FILL)) + 
			scale_fill_gradientn(trans=Y_TRANS,
			colours=YV_COLORS
		)
	}
ZM
}
################################___PARSE_HTML___################################
PARSE_HTML <- function( XU ) {
XRH <- as.character(read_html( XU ))
XRH <- unlist(strsplit(XRH, "(?<=>)", perl=TRUE))
XRH <-   unlist(strsplit(XRH, "<(?=<)", perl = T))
writeLines(XRH,'XRH.HTML',useBytes = TRUE)
XRL <- readLines('XRH.HTML',encoding = 'UTF-8')
XRL <- XRL[XRL!='']
###___PARSING_HTML___###
###___<BODY>___###
XN_BODY_START <- grep('<BODY.*>', XRL, ignore.case = TRUE)
XN_BODY_END <- grep('</BODY.*>', XRL, ignore.case = TRUE)
XRL <- XRL[(XN_BODY_START+1):(XN_BODY_END-1)]
###___</BODY>___###
###___<SCRIPT>___###
XV_SCRIPT_START <- grep('<SCRIPT.*>', XRL, ignore.case = TRUE)
XV_SCRIPT_END <- grep('</SCRIPT>', XRL, ignore.case = TRUE)
if ( length(XV_SCRIPT_START) > 0 & (length(XV_SCRIPT_START)=length(XV_SCRIPT_END))  ) {
for(i in seq_along(XV_SCRIPT_START)  ) {XRL[XV_SCRIPT_START[i]:XV_SCRIPT_END[i]] <- ''}
}
XRL <- XRL[XRL!='']
###___</SCRIPT>___###
###___<STYLE>___###
XV_STYLE_START <- grep('<STYLE.*>', XRL, ignore.case = TRUE)
XV_STYLE_END <- grep('</STYLE>', XRL, ignore.case = TRUE)
if ( length(XV_STYLE_START) > 0 & (length(XV_STYLE_START)=length(XV_STYLE_END))  ) {
for(i in seq_along(XV_STYLE_START)  ) {XRL[XV_STYLE_START[i]:XV_STYLE_END[i]] <- ''}
}
XRL <- XRL[XRL!='']
###___</STYLE>___###
XRL
}
#############################___PARSE_HTML_TABLE___#############################
PARSE_HTML_TABLE <- function( XV ) {
	#XV = XL_TABLE[[i]]
	if ( any(GREPL(XV,'<U+00A6>')) ) {XV <- GSUB( XV,'<U+00A6>','|' )}
	XV <- GSUB(XV,'<TR.*?>','<TR>')
	XV <- GSUB(XV,'<TH.*?>','<TH>')
	XV <- GSUB(XV,'<TD.*?>','<TD>')
	XS <- paste0(XV,collapse='')
	ZV <- SPL_BEFORE(XS,'<TR>')
	ZV <- GSUB( ZV,'<TR>[[:space:]]*?<TH>','' )
	ZV <- GSUB( ZV,'<TR>[[:space:]]*?<TD>','' )
	ZV <- GSUB( ZV,'</TH>[[:space:]]*?<TH>','<U+00A6>' )
	ZV <- GSUB( ZV,'</TD>[[:space:]]*?<TD>','<U+00A6>' )
	ZV <- GSUB( ZV,'</TH>[[:space:]]*?<TD>','<U+00A6>' )
	ZV <- GSUB( ZV,'</TH>[[:space:]]*?</TR>','' )
	ZV <- GSUB( ZV,'</TD>[[:space:]]*?</TR>','' )
	ZV <- GSUB(ZV,'<TABLE.*?>','')
	ZV <- GSUB(ZV,'</TABLE.*?>','')
	ZV <- GSUB(ZV,'<TBODY.*?>','')
	ZV <- GSUB(ZV,'</TBODY.*?>','')
	ZV <- GSUB( ZV,'&nbsp;','' )
	if (IS_BLANK(ZV)) {
		ZT <- NA
	} else {
		XT <- VEC_TO_DT( ZV,SEP='<U+00A6>')
		ZT <- XT[! apply(XT,1, function( X ) all(IS_BLANK( X,T )))]
	}
ZT
}
########################___PARSE_HTML_TABLE_TO_VECTOR___########################
PARSE_HTML_TABLE_TO_VECTOR <- function(XV) {
	XV <- ZL[[i]]
	XV <- GSUB(X,'<DIV.*?>','' )
	XV <- GSUB(X,'</DIV>','' )
	XV <- GSUB(X,'<SPAN.*?>','' )
	XV <- GSUB(X,'</SPAN>','' )
	ZV <- GSUB(ZV, "&nbsp;|&#xe035|&#xe061", "")
	XV <- GSUB(X,'&#xe035','' )
	XV <- GSUB(XV, "<TR.*?>", "<TR>")
	XV <- GSUB(XV, "<TH.*?>", "<TH>")
	XV <- GSUB(XV, "<TD.*?>", "<TD>")
	XS <- paste0(XV, collapse = "")
	ZV <- SPL_BEFORE(XS, "<TR>")
	ZV <- GSUB(ZV, "<TR><TH>", "")
	ZV <- GSUB(ZV, "<TR><TD>", "")
	ZV <- GSUB(ZV, "</TH><TH>", "<U+00A6>")
	ZV <- GSUB(ZV, "</TD><TD>", "<U+00A6>")
	ZV <- GSUB(ZV, "</TH><TD>", "<U+00A6>")
	ZV <- GSUB(ZV, "</TH></TR>", "")
	ZV <- GSUB(ZV, "</TD></TR>", "")
	ZV <- GSUB(ZV, "<TABLE.*?>", "")
	ZV <- GSUB(ZV, "</TABLE.*?>", "")
	ZV <- GSUB(ZV, "<TBODY.*?>", "")
	ZV <- GSUB(ZV, "</TBODY.*?>", "")
	ZV <- GSUB(ZV, "<TH>", "")
	ZV <- GSUB(ZV, "&nbsp;|&#xe035|&#xe061", "")
ZV[ZV!='']
	WL( ZV )
	XT <- VEC_TO_DT(ZV, SEP = "<U+00A6>")
	WL( ZV )
	ZT <- XT[!apply(XT, 1, function(X) IS_BLANK(X, T))]
	WT( ZT[,-c(1:2)],'ZT.CSV' )
	WL( ZV )
}
###############################___PARSE_LSF_STR___##############################
PARSE_LSF_STR <- function( XS_FILE_NAME ) {
	XRL <- trimws(readLines( XS_FILE_NAME ))
	ZV <- vector()
	###___<_FOR_i_>___###
	for ( i in 1:(length(XRL)-1 )) {
	if ( stri_sub(XRL[i],-2,-1)=='<-' & stri_sub(XRL[i+1],1,8)=='function') {
		XS_FUN <- XRL[i+1]
		ZV[i] <- paste0(XRL[i],' ',XS_FUN)
	} else {
	if ( XRL[i] != XS_FUN ) {
		ZV[i] <- XRL[i]
	}}}
	ZV <- na.omit(ZV[ZV!=''])
	################################################################################
	################################################################################
	FUNCTION_NAMES <- trimws(SUB(GREP( ZV,' <- function' ),1,'<-'))
	#write(FUNCTION_NAMES,file=XS_FILE_NAME,append=TRUE)
	################################################################################
	################################################################################
	FUN_START <- c(grep("<- function", ZV),length( ZV ))
	XL <- list(  )
	for ( i in seq_along(FUN_START[-length(FUN_START)]) ) {
		FUN <- paste0(ZV[FUN_START[i]:(FUN_START[i+1]-1)],collapse = '\n')
		NAME <- FUNCTION_NAMES[i]
		XL[[i]] <- data.table(NAME,FUN)
	}
	ZT <- do.call( rbind,XL )
ZT
}
#############################___PARSE_LSF_STR_OLD___############################
PARSE_LSF_STR_OLD <- function( XRL ) {
  XV_FUN_START <- which( GREPL(XRL,'<- function' ))
  XIND <- union( XV_FUN_START,XV_FUN_START-1 )
  XIND <- setdiff( seq_along( XRL ), XIND )
  XRL[XIND] <- P0( ' ',XRL[XIND] )
  XRL[XV_FUN_START] <- P0( strrep( '#',79),'\n', XRL[XV_FUN_START])
  XRL
}
####################################___PC___####################################
PC <- function( XV ,XCOLLAPSE='') {
paste0(XV,collapse = XCOLLAPSE)
}
####################################___PDF___###################################
PDF <- function(XCH) {
	txt <- XCH
	pdf("X.pdf", paper="a4r")
	plot.new()
	text(x=.1, y=.1, txt)  # first 2 numbers are xy-coordinates within [0, 1]
	text(.5, .5, txt, font=2, cex=1.5)
	text(.9, .9, txt, font=4, cex=2, col="#F48024")
	dev.off()	
}
####################################___PIE___###################################
PIE <- function(XT,XN=0.1) {
	XNCOL <- ncol( XT ) - 2
	XCOL <- colnames(XT)[1:XNCOL]
	XV_LABELS <- vector(  )
	for ( i in 1:nrow( XT ) ) {
		XV_XS <- unname(unlist(XT[i,1:XNCOL]))
		XS <- paste0(XV_XS,collapse = '_')
		XS <- XS + '_' + XT[i,PCT] + '%'
		XV_LABELS[i] <- XS
	}
	XS_MAIN <- 'Pie chart of ' + stri_trans_tolower(paste0(XCOL,collapse = ' '))
pie3D(XT$N, labels = XV_LABELS,explode=XN, main=XS_MAIN)
}
###################################___plot___###################################
plot <- function(...){
  grDevices::dev.new()
  graphics::plot(...)
}
###################################___PLOT___###################################
PLOT <- function(XV,FUN,...) {
	plot( XV, FUN(XV),asp = 1)
	abline(v=SEQ_RANGE_2(XV), col="lightgray", lty="dotted")
	abline(h=SEQ_RANGE_2(YV), col="lightgray", lty="dotted")
}
#################################___PLOT_VEC___#################################
PLOT_VEC <- function(XV,YV,...) {
plot( XV, YV,asp = 1)
abline(v=SEQ_RANGE( XV ), col="lightgray", lty="dotted")
abline(h=SEQ_RANGE( XV ), col="lightgray", lty="dotted")
}
##############################___PLUS_AS_NUMBER___##############################
PLUS_AS_NUMBER <- function(XV) {
	XV <- trimws(XV)
	ZV <- XV
	XV <- na.omit(XV)
	if ( all(SUB( XV,1,1 )=='+' | SUB( XV,1,1 )=='-') & ( IS_NUM(SUB( XV,2,-1 ))) ) {
		ZV <- 	as.numeric(gsub( '\\+','',ZV ))
	}
ZV
}
#############################___POLAND_STATE_MAP___#############################
POLAND_STATE_MAP <- function( XT,STATE_COL_NAME = "STATE",X_FILL = "PRICE",X_FILL_COLOR_LOW='grey80',X_FILL_COLOR_HIGH='grey20' ,X_TEXT,X_TEXT_COLOR='red' ) {
	STATE <- read_sf("WOJ.shp")
	STATE <- STATE %>% select(NAME = JPT_NAZWA_, geometry)
	Encoding(STATE$NAME)  <- 'UTF-8'
	STATE <- left_join(STATE, XT, by = c("NAME" = STATE_COL_NAME))
	Z_MAP <- ggplot() + geom_sf(data = STATE,aes_string(fill=X_FILL)) +
	scale_fill_gradient(low=X_FILL_COLOR_LOW, high=X_FILL_COLOR_HIGH)
	if (!missing( X_TEXT  )  ) {
		STATE_points <- sf::st_point_on_surface(STATE)
		STATE_coords <- as.data.frame(sf::st_coordinates(STATE_points))
		STATE_coords$Y_TEXT <- unname(unlist(as.data.table(STATE[,X_TEXT])[,1]))
		if ( IS_NUM(STATE_coords$Y_TEXT) ) {#<IF>#
			STATE_coords$Y_TEXT <- ROUND(STATE_coords$Y_TEXT)
			XV <- SPL_WHICH(STATE_coords$Y_TEXT,'\\.')
			X_MIN_XV <- min(nchar(XV))
		if( X_MIN_XV > 4 ) {
			STATE_coords$Y_TEXT <- SPL_WHICH(as.numeric(XV)/1000,'\\.') + 'K'
		}}#</IF>#
		Z_MAP <- Z_MAP +
			geom_text(data = STATE_coords, aes(X, Y, label = Y_TEXT), color = X_TEXT_COLOR) +
			labs(caption = X_TEXT) + theme(plot.caption = element_text(color = X_TEXT_COLOR, face="bold"))
	}
Z_MAP
}
############################___POLAND_STATE_MAP_OK___###########################
POLAND_STATE_MAP_OK <- function( XT,STATE_COL_NAME = "STATE",X_FILL = "PRICE" ,X_TEXT ) {
	STATE <- read_sf("WOJ.shp")
	STATE <- STATE %>% select(NAME = JPT_NAZWA_, geometry)
	Encoding(STATE$NAME)  <- 'UTF-8'
	STATE <- left_join(STATE, XT, by = c("NAME" = STATE_COL_NAME))
	Z_MAP <- ggplot() + geom_sf(data = STATE,aes_string(fill=X_FILL)) +
	scale_fill_gradient(low='grey80', high='grey20')
	if (!missing( X_TEXT  )  ) {
	STATE_points <- sf::st_point_on_surface(STATE)
	STATE_coords <- as.data.frame(sf::st_coordinates(STATE_points))
	STATE_coords$Y_TEXT <- unname(unlist(as.data.table(STATE[,X_TEXT])[,1]))
	if ( IS_NUM(STATE_coords$Y_TEXT) ) {#<IF>#
		STATE_coords$Y_TEXT <- ROUND(STATE_coords$Y_TEXT)
		XV <- SPL_WHICH(STATE_coords$Y_TEXT,'\\.')
		X_MIN_XV <- min(nchar(XV))
		if( X_MIN_XV > 4 ) {
			STATE_coords$Y_TEXT <- SPL_WHICH(as.numeric(XV)/1000,'\\.') + 'K'
		}
	}#</IF>#
	Z_MAP <- Z_MAP + geom_text(data = STATE_coords, aes(X, Y, label = Y_TEXT), colour = "white")
	}
Z_MAP
}
################################___POWER_TRANS___###############################
POWER_TRANS <- function(XV,Y) {
	(XV^Y - 1)/Y
}
################################___PRINT_TIME___################################
PRINT_TIME <- function(TIME=0, ... ) {
	XV <- as.vector( list( ... ) )
	Sys.sleep(TIME)
	print( paste0(XV , collapse = '_|_' ))
}
####################################___PRT___###################################
PRT <- function( ... ) {
	XV <- UL(L(...))
	MC <- as.character(match.call())[-1]
	DT( VAR=XV,VAL=MC )
	print(DT( VAR=MC,VAL=XV ))
}
####################################___PS___####################################
PS <- function( ... , sep = ' ' ) {
	paste( ...,sep=sep )
}
####################################___PSL___###################################
PSL <- function(...) {
	XL <- L( ... )
	XV <- SAP( XL,DEL_SL )
	XV <- XV[XV!='']
	paste0(XV,collapse='/' )
}
####################################___PU___####################################
PU <- function( ... , sep = '_' ) {
	paste( ...,sep=sep )
}
####################################___Q0___####################################
Q <- function(  ) fn$sqldf(GET_SQL(  ))
####################################___QN___####################################
QN <- function(df){
  df_rank <- apply(df,2,rank,ties.method="min")
  df_sorted <- data.frame(apply(df, 2, sort))
  df_mean <- apply(df_sorted, 1, mean)
   
  index_to_mean <- function(my_index, my_mean){
  return(my_mean[my_index])
  }
   
  df_final <- apply(df_rank, 2, index_to_mean, my_mean=df_mean)
  rownames(df_final) <- rownames(df)
  return(df_final)
}
####################################___QU___####################################
QU <- function( XV,XN=3 ){
	i <- 1/(XN+1)
	i0 <- 1/(XN+1)
	ZV <- vector(  )
	k <- 1
	while ( i < 1 ) {
		ZV[k] <- ROUND(quantile( XV ,i),1)
		k <- k+1
		i <- i + i0
		if ( k > 1000 ) break
	}
ZV
}
##################################___QU_OLD___##################################
QU_OLD <- function( XV,XN=3 ){
	i <- 1/(XN+1)
	i0 <- 1/(XN+1)
	ZV <- vector(  )
	k <- 1
	while ( i < 1 ) {
	ZV[k] <- ROUND(quantile( XV ,i),1)
	k <- k+1
	i <- i + i0
	if ( k > 1000 ) {break}
}
ZV
}
####################################___QU1___###################################
QU1 <- function(X) {
	quantile(X,0.1)	
}
####################################___QU2___###################################
QU2 <- function(X) {
	quantile(X,0.2)	
}
####################################___QU3___###################################
QU3 <- function(X) {
	quantile(X,0.3)	
}
####################################___QU4___###################################
QU4 <- function(X) {
	quantile(X,0.4)	
}
####################################___QU5___###################################
QU5 <- function(X) {
	quantile(X,0.5)	
}
####################################___QU6___###################################
QU6 <- function(X) {
	quantile(X,0.6)	
}
####################################___QU7___###################################
QU7 <- function(X) {
	quantile(X,0.7)	
}
####################################___QU8___###################################
QU8 <- function(X) {
	quantile(X,0.8)	
}
####################################___QU9___###################################
QU9 <- function(X) {
	quantile(X,0.9)	
}
##########################___quantile_normalisation___##########################
quantile_normalisation <- function(df){
  df_rank <- apply(df,2,rank,ties.method="min")
  df_sorted <- data.frame(apply(df, 2, sort))
  df_mean <- apply(df_sorted, 1, mean)
   
  index_to_mean <- function(my_index, my_mean){
  return(my_mean[my_index])
  }
   
  df_final <- apply(df_rank, 2, index_to_mean, my_mean=df_mean)
  rownames(df_final) <- rownames(df)
  return(df_final)
}
#####################################___R___####################################
R <- function(X,DIGITS=2) {
	if (is.vector(X)) {
		X <- ROUND2(X,DIGITS)
	} else {
		for (i in which(sapply(X,class) == "numeric")) {
			X[[i]] = ROUND2(X[[i]],DIGITS)
		}
	}
X
}
####################################___R0___####################################
R0 <- function(X,DIGITS) {
 if ( missing( DIGITS ) ) {
  if (X < 1) {
   Z <- round2(X,3)
  } else {
   if (X >= 1 & X < 1000) {
  Z <- round2(X,2)  
   } else {
  Z <- round2(X,0)
   }
  }
 } else {
  Z <- Z <- round2(X,DIGITS)
 }
Z
}
###################################___RANK___###################################
RANK <- function( XV,XV_LEN ){
	if (  missing(XV_LEN)) {XV_LEN <- length(XV)}
	XV_LEN_NORM <- c(1:XV_LEN)/XV_LEN
	XV_BREAKS <- unique(sapply(XV_LEN_NORM, function( X ) quantile( XV,X )))
	XL_AFTER_BREAK <- lapply(XV_BREAKS, function( X ) XV[XV > X])
	ZL <- list(  )
	ZL[[1]] <- setdiff(XV,XL_AFTER_BREAK[[1]])
	for( i in ( 2:(length(XL_AFTER_BREAK)) )) {
	ZL[[i]] <- 	setdiff(XL_AFTER_BREAK[[i-1]],XL_AFTER_BREAK[[i]])
	}
	ZV <- XV
	for ( i in seq_along(ZL) ) {
	ZV[which(XV  %in% ZL[[i]]) ] <- i
	}
ZV
}
###################################___RBIND___##################################
RBIND <- function(XT1,XT2) {
	if (ncol(XT1) == ncol(XT2)) {
	colnames(XT2) <- colnames(XT1)
	ZT  <- rbind(XT1,XT2)
	} else {
	ZT <- list()
	ZT[[1]]  <- 'DIFFERENT NUMBERS OF COLUMNS'
	ZT[[2]]  <- list( colnames( XT1 ),colnames( XT2 ) )
	}
ZT
}
###############################___rbind.fill.DT___##############################
rbind.fill.DT <- function(ll) {
  all.names <- lapply(ll, names)
  unq.names <- unique(unlist(all.names))
  ll.m <- rbindlist(lapply(seq_along(ll), function(x) {
  tt <- ll[[x]]
  setattr(tt, 'class', c('data.table', 'data.frame'))
  data.table:::settruelength(tt, 0L)
  invisible(alloc.col(tt))
  tt[, c(unq.names[!unq.names %chin% all.names[[x]]]) := NA_character_]
  setcolorder(tt, unq.names)
  }))
}
#################################___RBIND_OLD___################################
RBIND_OLD <- function(XT1,XT2) {
	if ( ncol( XT1 ) == ncol( XT2 ) ) {
		colnames( XT2 ) <- colnames( XT1 )
		ZT  <- rbind(XT1,XT2)
	} else {
		ZT <- list()
		ZT[[1]]  <- 'DIFFERENT NUMBERS OF COLUMNS'
		ZT[[2]]  <- list( colnames( XT1 ),colnames( XT2 ) )
	}
ZT
}
##########################___READ_CORNERSTONE_TABLE___##########################
READ_CORNERSTONE_TABLE <- function( FILE ) {
	XRL <- suppressWarnings(readLines(FILE,encoding = 'UTF-8'))
	XRL <- CLEAN_TRIM_BRACKETS(XRL) 
	XS <- paste0(XRL,collapse='')
	XS <- gsub('[[:space:]]+',' ', XS)
	###___<BODY>___###
	XS <- GSUB(XS,'<BODY.*?>','<BODY>')
	XS <- GSUB(XS,'</BODY.*?>','</BODY>')
	XS <- P0('<BODY>',SUB(XS,'<BODY>','</BODY>'),'</BODY>')
	###___<SCRIPT_&_COMMENTS>___###
	XS <- GSUB(XS, "<SCRIPT.*?>", "<SCRIPT>")
	XS <- GSUB(XS, "</SCRIPT.*?>", "</SCRIPT>")
	XV <- SPL_HTML( XS )
	XV_SCRIPT_START <- which(XV == "<SCRIPT>")
	XV_SCRIPT_END <- which(XV == "</SCRIPT>")
	for (i in seq_along(XV_SCRIPT_START)) {
		XV[XV_SCRIPT_START[i]:XV_SCRIPT_END[i]] <- ""
	}
	XV <- XV[XV!='']
	XV <- GSUB(XV, "<!--.*?-->", "")
	XV <- GSUB(XV, "<br.*?/>", "")
	XV <- XV[XV!='']
	XT <- PARSE_HTML_TABLE(XV)
	XT <- ROW_AS_COL( XT ,2)
	XT <- CLEAN_TABLE( XT[,c( 1:3,6 )] )
XT[1:W(XT$USER=='Report Criteria')-1]
}
#################################___READ_HTML___################################
READ_HTML <- function( XU,READ_HTML_VER=T) {
	if ( all(class(XU) == c("xml_document","xml_node"))) {
		Z <- XU
	} else if ( READ_HTML_VER ) {
		XRH <- (as.character(read_html( XU)))
		XRH <- unlist(strsplit(XRH, "(?<=>)", perl=TRUE))
		XRH <- unlist(strsplit(XRH, "<(?=<)", perl = T))
		writeLines(XRH,'XRH.HTML',useBytes = TRUE)
		XRL <- readLines('XRH.HTML',encoding = 'UTF-8')
		XRL <- XRL[XRL!='']
		XRL <- gsub( '<!-- ','<',  XRL)
		XRL <- gsub( '<!--','<',  XRL)
		XRL <- gsub( ' -->','>',  XRL)
		XRL <- gsub( '-->','>',  XRL)
		#writeLines(XRL,'XRH3.HTML',useBytes = TRUE)
		#XRL  <-  DELETE_DOM_LINE(XRL,'EMPTY')
		###___PARSING_HTML___###
		XRL <- XRL[XRL!='']
		XRL <- trimws(XRL[XRL!=''])
		writeLines(XRL,'XRH.HTML',useBytes = TRUE)
		Z <- read_html('XRH.HTML')
	} else {
	Z <-  read_html(XU)
}}
Z
}
################################___READ_HTML_0___###############################
READ_HTML_0 <- function( XU,AS_CHAR=FALSE ) {
	XRH <- UP(as.character(read_html( XU )))
	XRH <- unlist(strsplit(XRH, "(?<=>)", perl=TRUE))
	XRH <- unlist(strsplit(XRH, "<(?=<)", perl = T))
	writeLines(XRH,'XRH.HTML',useBytes = TRUE)
	XRL <- readLines('XRH.HTML',encoding = 'UTF-8')
	XRL <- XRL[XRL!='']
	#writeLines(XRL,'XRH.HTML',useBytes = TRUE)
	XRL <- gsub( '<!-- ','<',  XRL)
	#writeLines(XRL,'XRH2.HTML',useBytes = TRUE)
	XRL <- gsub( '<!--','<',  XRL)
	XRL <- gsub( ' -->','>',  XRL)
	XRL <- gsub( '-->','>',  XRL)
	#writeLines(XRL,'XRH3.HTML',useBytes = TRUE)
	#XRL  <-  DELETE_DOM_LINE(XRL,'EMPTY')
	###___PARSING_HTML___###
	XRL <- XRL[XRL!='']
	XRL <- DELETE_DOM_OUTSIDE(XRL,'BODY')
	XRL <- DELETE_DOM_INSIDE(XRL,'SCRIPT')
	XRL <- DELETE_DOM_INSIDE(XRL,'STYLE')
	XRL <- DELETE_DOM_INSIDE(XRL,'META')
	XRL <- DELETE_DOM_INSIDE(XRL,'SPAN')
	XRL <- DELETE_DOM_INSIDE(XRL,'SVG')
	XRL <- DELETE_DOM_INSIDE(XRL,'USE')
	XRL <- SIMPLIFY_DOM( XRL,'TABLE')
	XRL <- SIMPLIFY_DOM( XRL,'TH')
	XRL <- SIMPLIFY_DOM( XRL,'TR')
	XRL <- SIMPLIFY_DOM( XRL,'TD')
	XRL <- SIMPLIFY_DOM( XRL,'DIV')
	XRL <- SIMPLIFY_DOM( XRL,'LI')
	XRL <- SIMPLIFY_DOM( XRL,'TR')
	XRL <- SIMPLIFY_DOM( XRL,'UL')
	XRL <- SIMPLIFY_DOM( XRL,'B')
	XRL <- SIMPLIFY_DOM( XRL,'IFRAME')
	XRL <- SIMPLIFY_DOM( XRL,'I')
	XRL <- gsub("[\n\r\t\v\f]", "",XRL)
	writeLines(XRL,'XRH.HTML',useBytes = TRUE)
	if( AS_CHAR ) {
		Z <- readLines('XRH.HTML',encoding = 'UTF-8')
	} else {
		Z <- read_html('XRH.HTML')
	}
return( Z )
}
###############################___READ_SRT_FILE___##############################
READ_SRT_FILE <- function( XFILE ) {
	if ( length(XFILE) > 1 ) {
		XRL <- trimws( XFILE )
		XRL <- XRL[XRL!='']
	} else {
		XRL <- trimws(readLines( XFILE,encoding = '"UTF-8"' ))
		XRL <- XRL[XRL!='']	
	}
	XMAX <- max(suppressWarnings( max(na.omit(as.numeric(tail(XRL))))) , max(AS_NUM(XRL[IS_NUM_0( XRL )])) )
	XV_TEXT <- vector(  )
	XV_ID <- vector(  )
	XL_DF_TIME <- list(  )
	XRL <- trimws(readLines( XFILE,encoding = '"UTF-8"' ))
	XRL <- CLEAN_STRING( XRL,T )
	XRL <- CLEAN_STRING_GER(XRL)
	XRL <- XRL[XRL!='']	
	XV_NUM = vector()
	XV_TEXT = vector()
	XV_START = vector()
	XV_END = vector()
	XV_END_OLD = vector()
	SRT_START <- SRT_END <- vector()
	for ( i in 1:(XMAX-1)) {
		if ( !isTRUE(which(XRL==i+1)>0 ) ) {
				XV <- XRL[which(XRL==i):length(XRL)]
				XV_NUM[i] = XV[1]
				SRT_START[i] <- trimws(SUB(XV[2],1,'-->'))
				SRT_END[i] <- trimws(SUB(XV[2],'-->',-1))
				XV_SECONDS <- SRT_TO_SECONDS(XV[2])
				XV_START[i] = XV_SECONDS['START']
				XV_END_OLD[i] = XV_SECONDS['START']
				XV_END[i] <- XV_END_OLD[i]
				XV_TEXT[i] = paste( XV[3:max(3,length( XV )-2)] ,collapse = ' ')
			break
		} else {
			XV <- XRL[which(XRL==i):(which(XRL==i+1)+1)]
			XV_NUM[i] = XV[1]
			SRT_START[i] <- trimws(SUB(XV[2],1,'-->'))
			SRT_END[i] <- trimws(SUB(XV[2],'-->',-1))
			XV_SECONDS <- SRT_TO_SECONDS(XV[2])
			XV_START[i] = XV_SECONDS['START']
			XV_END_OLD[i] = XV_SECONDS['START']
			XV_END[i] <- SRT_TO_SECONDS(XV[length(XV)])['START']
			XV_END[i] <- round(weighted.mean(c(XV_END_OLD[i],XV_END[i]), c(1,9 )),3)
			XV_TEXT[i] = paste( XV[3:max(3,length( XV )-2)] ,collapse = ' ')
		}}
	data.table(NUM=XV_NUM,TEXT=XV_TEXT,START=XV_START,END=XV_END,END_OLD=XV_END_OLD,SRT_START,SRT_END)	
}
################################___READ_TABLE___################################
READ_TABLE <- function( XFILE ) {
	XFILE.EXT <- tools::file_ext(XFILE)
	if ( UP(XFILE.EXT)  == 'CSV' ) {
		Z <- fread( XT )
	}
	if ( UP(XFILE.EXT)  == 'ZIP' ) {
		TEMP_FILE <- tempfile()
		Z <- fread( unzip(XFILE, exdir =  TEMP_FILE) )	
	}
	if ( UP(XFILE.EXT)  == 'XLSX' ) {
		Z <- readxl::read_xlsx(XFILE)
	}
		if ( UP(XFILE.EXT)  == 'XLS' ) {
		Z <- readxl::read_xls(XFILE)
	}
CLEAN_TABLE(Z)
}
###############################___REM_DUP_NEXT___###############################
REM_DUP_NEXT <- function( XV ) {
	XV_EQ <- c(F,XV[1:( LEN( XV )-1 )]==XV[2:( LEN( XV ) )])
	XV[!XV_EQ]
}
##########################___REMOVE_COLNAMES_AS_ROWS___#########################
REMOVE_COLNAMES_AS_ROWS <- function(XT) {
XCOLNAMES <- colnames(XT)
for ( i in 1:nrow( XT ) ) {
if	(SIMILAR_VECTORS(XCOLNAMES,unname(unlist( XT[i] )))) {
XT <- XT[-XV_DUP_ROWS_NUMBER]
}
}
XT
}
##############################___REMOVE_FROM_VEC___#############################
REMOVE_FROM_VEC <- function(XV,...) {
	YV <- unlist( list( ... ) )
	VAR <- names( YV )
	if ( is.null(VAR) | isTRUE(UP(SUB(VAR,1,2) %in% c('LI','GR'))) ) {
		ZV <- grep(YV, XV,ignore.case = T,value = T,invert = T)
	}
	if (isTRUE( UP(SUB(VAR,1,2)) == 'ST' )) {
		X_NCHAR <- nchar( YV )	
		ZV  <- XV[!SUB( XV,1,X_NCHAR ) == YV ]
		ZV <- grep(YV, ZV,ignore.case = T,value = T,invert = T)
	}
	if (isTRUE( UP(SUB(VAR,-3,-1)) == 'END' )) {
		X_NCHAR <- nchar( YV )	
		ZV  <- XV[!SUB( XV,-X_NCHAR, -1 ) == YV ]
		ZV <- grep(YV, ZV,ignore.case = T,value = T,invert = T)
	}
assign(deparse(substitute(XV)), ZV, env=.GlobalEnv)
}
#######################___REMOVE_ROWS_SAME_AS_COLNAMES___#######################
REMOVE_ROWS_SAME_AS_COLNAMES <- function( XT ) {
	XT <- DT( XT )
	XT[!W(SAP(SEQ(XT), function( X ) ALL(XT[X]==colnames( XT ))))]	
}
#################################___REP_INDEN___################################
REP_INDEN <- function(ZV) {
	MAX_INDEN <- max(nchar(strapplyc(SV(ZV,'^ '), "^\\s+")))
	ZV <- GSUB(ZV,'\t',' ')
	for (i in (MAX_INDEN/2):1 ) {
		OLD <- P0("^", strrep(" ", 2*i))
		NEW <- strrep("\t", i)
		ZV <- GSUB(ZV,OLD,NEW)
	}
ZV
}
###################################___REP0___###################################
REP0 <- function (x,times) {
  if (!is.character(x)) 
  x <- as.character(x)
  .Internal(strrep(x, as.integer(times)))
}
###############################___REPAIR_INDEN___###############################
REPAIR_INDEN <- function(ZV) {
	MAX_INDEN <- max(nchar(strapplyc(SV(ZV,'^ '), "^\\s+")))
	ZV <- GSUB(ZV,'\t',' ')
	for (i in (MAX_INDEN/2):1 ) {
		OLD <- P0("^", strrep(" ", 2*i))
		NEW <- strrep("\t", i)
		ZV <- GSUB(ZV,OLD,NEW)
	}
ZV
}
####################################___RH___####################################
RH <- function(XU, ENC = "UTF-8") {
	if ( all(class(XU) == c("xml_document","xml_node"))) {
		XRH <- XU
	} else {
  	library(htmltidy)
	  XRH <- read_html(XU)
	  XRH <- CH(html_nodes(XRH, "body"))
	  XRH <- GSUB(XRH, "<!--", "<DIV>")
	  XRH <- GSUB(XRH, "-->", "</DIV>")
	  NOT_WARN(ICO(X_TIDY_HTML <- tidy_html(XRH, list(TidyDocType = "html5", TidyHideComments = TRUE))))
	  TEMP_FILE <- tempfile()
	  WL(X_TIDY_HTML, TEMP_FILE)
	  XRH <- read_html(TEMP_FILE, encoding = ENC)
	}
XRH
}
####################################___RH0___###################################
RH0 <- function (x, encoding = "", ..., options = c("RECOVER", "NOERROR", 
  "NOBLANKS")) 
{
  UseMethod("read_html")
}
####################################___RL2___###################################
RL2 <- function(X) {
	suppressWarnings(readLines(X,encoding = 'UTF-8'))	
}
#################################___RM_QUOTE___#################################
RM_QUOTE <- function(X) {
	QUTOE_CONDITION <- grepl("^\"",X) & grepl("\"$",X)
ifelse(QUTOE_CONDITION,stringi::stri_sub(XCH,2L,-2L),X)
}
#################################___RM_TRASH___#################################
RM_TRASH <- function() {
	XT_FLS <- FLS()
	XT_FLS[TYPE != "function"]
	XT_FLS <- XT_FLS[TYPE != "function"]
	XV_RM <- XT_FLS[SIZE < 100000, NAME]
RM(list = XV_RM)
}
####################################___RMF___###################################
RMF <- function(XF) {
	if (file.exists(XF)) {
		file.remove(XF)
	}	
}
####################################___RMQ___###################################
RMQ <- function(X) {
	Q <- GREPL(X, "^\"") & GREPL(X, "\"$")
	IF(Q, B(X, 2, -2), X)
}
###################################___ROUND___##################################
ROUND <- function(X,XN=2) {
	if (is.vector(X)) {
	round(X,XN)
	} else {
	for(i in which(sapply(X, class) == "numeric")) {X[[i]] = round(X[[i]],XN) }
	X
	}
}
##################################___ROUND0___##################################
ROUND0 <- function(XV,DIGITS) {
	XV.MIN <- min(XV)
	if ( missing( DIGITS )) {
  	if (XV.MIN < 0.1) {
			Z <- round2(XV,3)
		} else if (XV.MIN >= 0.1 & XV.MIN < 1) {
			Z <- round2(XV,2)
		} else if (XV.MIN >= 1 & XV.MIN < 100) {
			Z <- round2(X,1)
		} else if (XV.MIN >= 100) {
			Z <- round2(X,0)
		}
	} else {
		Z <- round2(X,DIGITS)
	}
Z
}
##################################___ROUND2___##################################
ROUND2 <- function(X,XN=2) {
	X.SIGN = sign(X)
	Y = abs(X)*10^XN
	Y = Y + 0.5
	Y = trunc(Y)
	Y = Y/10^XN
Y*X.SIGN
}
################################___ROW_AS_COL___################################
ROW_AS_COL <- function(XT,WHICH_ROW=1){
	XT <- as.data.table(XT)
  colnames( XT ) <- CLEAN_STRING(unname(unlist(XT[WHICH_ROW,])))
  XT <- XT[(WHICH_ROW+1):nrow( XT ),]
  XT <- clean_names( XT ,case = "all_caps")
XT	
}
################################___ROW_BY_MAX___################################
ROW_BY_MAX <- function( XT,XC_BY,XC_MAX) {
	XQUERY <- paste0('SELECT * FROM XT JOIN (SELECT ',XC_BY,',MAX(',XC_MAX,') AS ',XC_MAX,' FROM XT GROUP BY ',XC_BY,') USING (',XC_BY,',',XC_MAX,')')
	sqldf( XQUERY )
}
####################################___RT___####################################
RT <- function( XFILE ) {
#READ_TABLE
	XFILE.EXT <- tools::file_ext(XFILE)
	if ( UP(XFILE.EXT)  == 'CSV' ) {
		Z <- fread( XT )
	}
	if ( UP(XFILE.EXT)  == 'ZIP' ) {
		TEMP_FILE <- tempfile()
		Z <- fread( unzip(XFILE, exdir =  TEMP_FILE) )	
	}
	if ( UP(XFILE.EXT)  == 'XLSX' ) {
		Z <- readxl::read_xlsx(XFILE)
	}
		if ( UP(XFILE.EXT)  == 'XLS' ) {
		Z <- readxl::read_xls(XFILE)
	}
CLEAN_TABLE(Z)
}
#####################################___S___####################################
S <- function(X,...,ignore.case = T,perl = T,fixed=F,useBytes=F) {
	XS <- match.call(expand.dots=FALSE)$`...`
	XS <- sapply(XS,function(X) paste0(X,collapse = ''))
	XZ <- vector()
	#----------------------------------------------------------#
	S01 <- function(X,XS,ignore.case = T,perl = T,fixed=F,useBytes=F) {
		XB <- T
		for (i in seq_along(XS) ) {
			iXS <- XS[i]
			XQ <- S1(iXS,'!') | S1(iXS,'-')
			if (XQ) {
				iXS <- B(iXS,2,-1)
				XB <- !grepl(iXS,X,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)	
			} else {
				XB <- grepl(iXS,X,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
			}
			if (!XB) break
		}
	XB
	}
	#----------------------------------------------------------#
	for ( i in seq_along(X) ) {
		XZ[i] <- S01(X[i],XS,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
	}
XZ
}
####################################___S1___####################################
S1 <- function(XV,XS,ignore.case = T, perl = T, fixed = FALSE,useBytes = FALSE) {
	XS <- paste0('^',XS)
	grepl(XS,XV,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
}
###################################___S_01___###################################
S_01 <- function(X,...,ignore.case = F,perl = F,fixed=F,useBytes=F) {
XS <- UL(list(...))
#X <- XVR
#XS <- 'TH'
XZ <- vector()
#__________________________________________________________#
S01 <- function(X,XS,ignore.case = F,perl = F,fixed=F,useBytes=F) {
XZ <- vector()	
for (i in SEQ(XS) ) {
	iXS <- XS[i]
	if ( S1(iXS,'!') | S1(iXS,'-')  ) {
		XZ[i] <- !grepl(iXS, X,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
	} else {
		XZ[i] <- grepl(iXS, X,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)	
	}
}#FOR
#__________________________________________________________#
all(XZ)
}
for ( i in SEQ(X) ) {
	XZ[i] <- S01(X,XS,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
	print(i)
}
XZ
}
#################################___S_EXACTLY___################################
S_EXACTLY <- function(XV,X) {
	S(XV,P('(?<!\\',X,')\\',X,'(?!\\',X,')'))
}
###################################___S_OLD___##################################
S_OLD <- function(XV, ... ,ignore.case = T, perl = T, fixed = FALSE,useBytes = FALSE) {
	XL <- list(...)
	YV <- UL( XL )
	ZV <- vector(  )
	YL <- L(  )
	for ( i in SEQ(XV) ) {
		YL[[i]] <- L()
		for ( j in SEQ( YV ) ) {
			YL[[i]][j] <- grepl(YV[j], XV[i],ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)	
		}
	}
UNM(UL(LAP( YL,  function( X ) all( UL(X )))))
}
####################################___S0___####################################
S0 <- function(XV,XS,ignore.case=T,perl=T,fixed=F,useBytes=F) {
	grepl(XS,XV,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
}
####################################___S01___###################################
S01 <- function(X,XS,ignore.case = T,perl = T,fixed=F,useBytes=F) {
XZ <- vector()
XB <- T
#__________________________________________________________#
for (i in SEQ(XS) ) {
	iXS <- XS[i]
	XB <- grepl(iXS,X,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
	XQ <- S1(iXS,'!') | S1(iXS,'-')
	if (XQ) XB <- !XB
	#print(L(i,XB))
	if (!XB) break
}#FOR
#__________________________________________________________#
XB
}
####################################___S0V___###################################
S0V <- function(XV,XS,ignore.case=T,perl=T,fixed=F,useBytes=F) {
	grep(XS,XV,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes,value=T)
}
####################################___S0W___###################################
S0W <- function(XV,XS,ignore.case=T,perl=T,fixed=F,useBytes=F) {
	grep(XS,XV,ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
}  
####################################___S2___####################################
S2 <- function(XV, XS ,ignore.case = T, perl = T, fixed = FALSE,useBytes = FALSE) {
	'DUPA'
}
###################################___SALL___###################################
SALL <- function(X,...) {
	all(S(X,...))
}
################################___SAME_LENGTH___###############################
SAME_LENGTH <- function( ... ) {
XL <- list(...)
all(diff(sapply(XL,length))==0)
}
###################################___SANY___###################################
SANY <- function(X,...) {
	any(S(X,...))
}
##################################___SAP_OLD___#################################
SAP_OLD <- function( X , FUN,..., simplify = T, USE.NAMES = T) {
	unname(sapply(X, FUN, ..., simplify = simplify, USE.NAMES = USE.NAMES))
}
###################################___SAPX___###################################
SAPX <- function(Y,FUN) {
	Y <- RMQ(deparse(substitute(Y)))
	FUN <- RMQ(deparse(substitute(FUN)))
	FUN <- P('function(X) ',FUN)
	XCALL <- P('SAP(',Y,',',FUN,')')
	eval(parse(text = XCALL), envir = .GlobalEnv)
	#XCALL
	#do.call(XCALL)
	#L(Y,FUN)
}
###################################___SAPX0___##################################
SAPX0 <- function(Y,FUN) {
	Y <- RMQ(deparse(substitute(Y)))
	FUN <- RMQ(deparse(substitute(FUN)))
	FUN <- P('function(X) ',FUN)
	XCALL <- P('SAP(',Y,',',FUN,')')
	XCALL
	#do.call(XCALL)
	#L(Y,FUN)
}
###################################___SCALE___##################################
SCALE <- function( X ) {
  if ( IS_TABLE(X) ) {
  X <- as.data.table( X )
  XIND <- sapply(X, class)  
  XT_NUM <- X[,which(XIND == 'numeric'),with=FALSE]  
  XT_CHAR <- X[,which(XIND == 'character'),with=FALSE]
  XT_NUM <- apply(XT_NUM,2,function( Y ) ROUND(scale( Y ) ))
  Z <- cbind(XT_CHAR ,XT_NUM )
  } else {
  Z  <- as.vector(ROUND( scale( X ) ))
  }
  Z 
}
#################################___SCALE_AVG___################################
SCALE_AVG <- function( XV ,XN=2) {
  round( (XV - ( mean( XV )) ) /( mean( XV )) ,XN)
}
######################___SCIENTIFIC_NOTATION_TO_NUMERIC___######################
SCIENTIFIC_NOTATION_TO_NUMERIC <- function( X ) {
	if ( GSUB(X,'\\d|\\.','') %in% c( 'E','EXP','EEX','EE','EX' ) ) {
		X.NUM <- AS_NUM(SUB(X,1,'E'))
		X.POWER <- AS_NUM(SUB(X,'E',-1))
		Z <- X.NUM * (10^X.POWER)
	} else {
		Z <- X
	}
Z
}
#############################___SECONDS_TO_HOURS___#############################
SECONDS_TO_HOURS <- function(XSEC) {
	X_HOUR <- floor(XSEC/3600)
	X_MINUTE <- floor((XSEC - 3600 * X_HOUR)/60)
	X_SECONDS <- floor( XSEC - (3600*X_HOUR + 60*X_MINUTE) )
	X_MILISECONDS <- floor( ( XSEC - floor(XSEC) ) * 1000)
	Z_HOUR <- LEADING_ZEROS( X_HOUR )
	Z_MINUTE <- LEADING_ZEROS(X_MINUTE)
	Z_SECONDS <- LEADING_ZEROS( X_SECONDS )
	Z_MILISECONDS <- LEADING_ZEROS(X_MILISECONDS)
paste0(Z_HOUR,':',Z_MINUTE,':',Z_SECONDS,',',Z_MILISECONDS)
}
##############################___SECONDS_TO_SRT___##############################
SECONDS_TO_SRT <- function(START,END) {
	START <- SECONDS_TO_HOURS( START )
	END <- SECONDS_TO_HOURS( END )
	paste0(START,' --> ',END)
}
####################################___SEL___###################################
SEL <- function( XT,XC,XS ) {
	XQ <- 'SELECT * FROM XT WHERE ' + XC + ' ' + XS
sqldf( XQ )
}
##################################___SEL_COL___#################################
SEL_COL <- function(XT,XS) {
as.data.table(dplyr::select(XT,matches(XS)))
}
#################################___SEQ_RANGE___################################
SEQ_RANGE <- function(XV, STEP=1 ) {
seq( floor(min( XV )) , ceiling( max( XV ) ),STEP )
}
################################___SEQ_RANGE_2___###############################
SEQ_RANGE_2 <- function(XV, STEP=1 ) {
	seq( floor(min( XV ))-1 , ceiling( max( XV ) )+1,STEP )
}
###################################___SHORT___##################################
SHORT <- function(X,XN=10) {
	if ( is.list(XL) & !IS_TABLE(X)) {
		X <- LAP(X,function(X) SHORT0(X,XN))
	} else {
		X <- SHORT0(X,XN)
	}
X	
}
##################################___SHORT0___##################################
SHORT0 <- function(X,XN=10) {
	if ( IS_TABLE(X) ) {
		X <- AP(X,1:2,function(X0) trimws(B(X0,1,XN)))
	} else {
		X <- trimws(B(X,1,XN))
	}
X	
}
##############################___SIMILAR_VECTORS___#############################
SIMILAR_VECTORS <- function(XV1,XV2) {
	if (length(XV1)==length(XV2)) {
	Z <- TRUE
	for ( i in 1:length(XV1) ) {
	if( !(grepl( XV1[i],XV2[i] ) | grepl( XV2[i],XV1[i] )) | is.na(grepl( XV1[i],XV2[i] ) | grepl( XV2[i],XV1[i] ))  ){
	Z <- FALSE
	break
	}
	}
	} else {
	Z <- FALSE
	}
Z
}
###############################___SIMPLIFY_DOM___###############################
SIMPLIFY_DOM <- function(XV,DOM) {
	DOM_ORG <- "<" + DOM + ".*>"
	DOM_SHORT <- "<" + DOM + ">"
trimws(gsub(DOM_ORG, DOM_SHORT, XV, ignore.case = TRUE))
}
###################################___SIZE___###################################
SIZE <- function( X ) {
	if ( is.vector( X ) ) {
		length(X)
	} else {
		nrow( X )
	}
}
####################################___SPL___###################################
SPL <- function(XV,XS=' ') {
	if( length(XV)==1 ) {
		trimws(unlist( str_split(unlist(XV),XS) ))
	} else {
		lapply(XV, function( X )	trimws(unlist( str_split(unlist(X),XS) )))
	}
}
#################################___SPL_AFTER___################################
SPL_AFTER <- function(XV,XP) {
 unlist(strsplit(XV, paste0('(?i)(?<=',XP,')'), perl = TRUE))
}
################################___SPL_BEFORE___################################
SPL_BEFORE <- function(XV,XP) {
	unlist(lapply(XV,function( X ) SPL_BEFORE_0( X,XP )))
}
###############################___SPL_BEFORE_0___###############################
SPL_BEFORE_0 <- function(X,XP) {
	if ( any(GREPL(X,XP)) ) {
		if ( STR_START(X,XP) ) {
			X <- unlist(strsplit( X, P0('(?i)',XP) ,perl = T) )
			X <- c(X[1],P0(XP,X[-1]))	
			X <- X[-1]	
		} else {
		X <- unlist(strsplit( X, P0('(?i)',XP) ,perl = T) )
		X <- c(X[1],P0(XP,X[-1]))	
		}
}
X
}
##############################___SPL_BEFORE_OLD___##############################
SPL_BEFORE_OLD <- function(XV,XP) {
	XV <- SPL_AFTER(XS,P0("</",DOM,">"))
		if ( any(GREPL(XV,XP)) ) {
		XV <- unlist(strsplit( XV, P0('(?i)',XP) ,perl = T) )
		XV <- c(XV[1],P0(XP,XV[-1]))	
	} 
XV
}
#################################___SPL_HTML___#################################
SPL_HTML <- function( XS ) {
	XV <- SPL_BEFORE(SPL_AFTER(XS,'>') ,'<' )
	XV <- SPL_BEFORE(SPL_AFTER(XS,'>') ,'<' )
XV
}
#################################___SPL_WHICH___################################
SPL_WHICH <- function(XV,XS=' ',XN=1) {
	XL <- SPL(XV,XS)
sapply(XL, function( X ) X[[XN]])
}
#################################___SPL_WITH___#################################
SPL_WITH <- function(XS,XP) {
	SPL_AFTER(SPL_BEFORE(XS,XP),XP)
}
################################___SPL_WITH_0___################################
SPL_WITH_0 <- function( XS , XP ) {
	XS <- gsub( XP, paste0( ' ',XP,' ' ),XS ,ignore.case = T,perl = T)
	ZV <- trimws(unlist(strsplit(XS, paste0('(?i)(?<=',XP,')'), perl = TRUE)))
	ZV <- trimws(unlist(strsplit(ZV, paste0('(?i)(?=.',XP,')'), perl = TRUE)))
ZV[ZV!='']
}
################################___SPL_WITH_2___################################
SPL_WITH_2 <- function( XS , XP ) {
	ZV <- unlist(strsplit(XS, paste0('(?i)(?<=',XP,')'), perl = TRUE))
	ZV <- unlist(strsplit(ZV, paste0('(?i)(?=.',XP,')'), perl = TRUE))
	ZV <- gsub( '[[:space:]]+', ' ',ZV ,ignore.case = T,perl = T)
	if ( length(ZV)>1)  {
		for (i in 1:(length( ZV )-1)) {
			if ( (ZV[i+1] ==' '  ) ) {
				ZV[i] = paste0(ZV[i],' ')
				ZV[i+1] =''
			}
		}
	}
	ZV <- ZV[ZV!='']
ZV[ZV!=' ']
}
################################___SPL_WITH_AB___###############################
SPL_WITH_AB <- function(XS,XP) {
	SPL_AFTER(SPL_BEFORE(XS,XP),XP)
}
################################___SPL_WITH_BA___###############################
SPL_WITH_BA <- function(XS,XP) {
	SPL_BEFORE(SPL_AFTER(XS,XP),XP)
}
##############################___SRT_TO_2SECONDS___#############################
SRT_TO_2SECONDS <- function(XS) {
	ZV <- SPL(XS,'-->')
	names(ZV) <- c('START','END')
	ZV
}
##############################___SRT_TO_SECONDS___##############################
SRT_TO_SECONDS <- function(XS) {
	ZV <- HOUR_TO_SECONDS(SPL(XS,'-->'))
	names(ZV) <- c('START','END')
	ZV
}
####################################___ST___####################################
ST <- function(XT,XCOL,XCH) {
#SEARCH TABLE
	XT <- DT(XT)
	XCOL <- RMQ(deparse(substitute(XCOL)))
XT[GREPL(get(XCOL),XCH)]
}
##################################___ST_ODL___##################################
ST_ODL <- function( XT, ... ) {
	XT <- DT( XT )
	XMC <- MC( ... )
	L( XT,XMC[[1]],XMC[[2]] )
}
################################___STAT_TABLE___################################
STAT_TABLE <- function( XT ) {
	XT <- as.data.table(XT)
	XCOLNAMES <- colnames(XT)
	UNIQUE <- sapply(XCOLNAMES ,function( X ) nrow(unique(na.omit(XT[,X,with=FALSE]))))
	MEAN <- suppressWarnings( sapply(XT ,mean) )
	SD <- suppressWarnings( sapply(XT ,sd) )
	MEDIAN <- unname(suppressWarnings( sapply(XT ,median) ))
	MIN <- unname(suppressWarnings( sapply(XT ,min) ))
	MAX <- unname(suppressWarnings( sapply(XT ,max) ))
	XNULL <- suppressWarnings(apply(sapply(XT,is.na),2,sum))
(data.table(XCOLNAMES,UNIQUE,MEAN,SD,MEDIAN,MIN,MAX,XNULL))
}
#############################___STAT_TABLE_NUM_0___#############################
STAT_TABLE_NUM_0 <- function( XT ) {
	XT <- as.data.table(XT)
	XIND <- sapply(XT, class)
	XT <- XT[,which(XIND == 'numeric'),with=FALSE]
	XCOLNAMES <- colnames(XT)
	UNIQUE <- sapply(XCOLNAMES ,function( X ) nrow(unique(na.omit(XT[,X,with=FALSE]))))
	MEAN <- suppressWarnings( sapply(XT ,mean) )
	SD <- suppressWarnings( sapply(XT ,sd) )
	MEDIAN <- unname(suppressWarnings( sapply(XT ,median) ))
	MIN <- unname(suppressWarnings( sapply(XT ,min) ))
	MAX <- unname(suppressWarnings( sapply(XT ,max) ))
	XNULL <- suppressWarnings(apply(sapply(XT,is.na),2,sum))
	(data.table(COL=XCOLNAMES,UNIQUE,MEAN,SD,MEDIAN,MIN,MAX,XNULL))
}
####################################___SUB___###################################
SUB <- function(XS,X_START,X_END) {
unname(sapply(XS,function(X) SUB0(X,X_START,X_END)))
}
##################################___SUB_BW___##################################
SUB_BW <- function(XV,A,Z) {
	#SUB_BW(ROTACJA[1,1],' ',-1)
	XV <- UNM(UL( XV ))
	#XL <- L( ' ',-1 )
	XL <- L(A,Z)
	if( (IS_NUM(A) & !IS_NUM(Z)) | (!IS_NUM(A) & IS_NUM(Z)) ){
		XV_IS_NUM <- SAP(XL ,IS_NUM )
		XCH <- XL[[W(!XV_IS_NUM)]]
		XN <- XL[[W(XV_IS_NUM)]]
	} else {
		Z <- 'ONE OF A OR Z NEEDS TO BE NUMERIC AND THE OTHER CHARACTER' 
	}
 if ( XV_IS_NUM[1] ) {
 	XV[S(XV,XCH)] <- B( XV[S(XV,XCH)],XN,XCH)
 } else {
 	XV[S(XV,XCH)] <- B( XV[S(XV,XCH)],XCH,XN)
 }
XV
}
#################################___SUB_LIKE___#################################
SUB_LIKE <- function( XV,XS ) {
	XV[which(XV %like% XS)] <- XS
XV
}
#################################___SUB_START___################################
SUB_START <- function( XV,XS ) {
 	XS <- UP( XS )
 	Y_NCHAR <- nchar( XS )
 	XV <- UP( XV )
 	SUB( XV,1,Y_NCHAR )==XS
}
###################################___SUB0___###################################
SUB0 <- function(XS,X_START,X_END) {
	if ( is.numeric(X_START) & is.numeric(X_END)   ) {
	ZS <- stri_sub(XS,X_START,X_END)
	} else {
	if ( is.numeric(X_START)) {
		XN_START <- X_START
	} else {
		XN_START <- as.numeric(stri_locate_first_regex(XS,X_START)[1,2]) + 1
	}
		XS <- stri_sub(XS,XN_START,-1)
		if ( is.numeric(X_END)) {
		XN_END <- X_END
	} else {
		XN_END <- as.numeric( stri_locate_first_regex(XS,X_END)[1,1]) - 1
	}
		ZS <- stri_sub(XS,1,XN_END)
	}
ZS
}
####################################___SUM___###################################
SUM <- function(..., na.rm = T) {
	sum(..., na.rm = T)
}
####################################___SV___####################################
SV <- function(XV, ... ,ignore.case = T, perl = T,fixed = F,useBytes = F) {
	XL <- list(...)
	YV <- UL( XL )
	ZV <- vector(  )
	YL <- L(  )
	for ( i in SEQ(XV) ) {
		YL[[i]] <- L()
		for ( j in SEQ( YV ) ) {
			YL[[i]][j] <-  grepl(YV[j], XV[i],ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
		}
	}
XV[UL(LAP( YL,  function( X ) all( UL(X ))))]
}
####################################___SW___####################################
SW <- function(XV, ... ,ignore.case = T, perl = T,fixed = F,useBytes = F) {
	XL <- list(...)
	YV <- UL( XL )
	ZV <- vector(  )
	YL <- L(  )
	for ( i in SEQ(XV) ) {
		YL[[i]] <- L()
		for ( j in SEQ( YV ) ) {
			YL[[i]][j] <-  grepl(YV[j], XV[i],ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
		}
	}
unname(W(UL(LAP( YL,  function( X ) all( UL(X ))))))
}
###############################___SYSTIME_NAME___###############################
SYSTIME_NAME <- function(XS) {
	XS <- paste0(XS,'_',GSUB(Sys.time(), "-", "_"))
	XS <- GSUB(XS, ":", "_")
	XS <- GSUB(XS, " ", "_")
	XS
}
####################################___T1___####################################
T1 <- function(XT) {
	XT <- DT(colnames(XT),t(XT[1]))
setnames(XT,c('X','Y'))[]
}
####################################___TAB___###################################
TAB <- function( XS,XP='' ) {
	ZT <- as.data.table(tabyl(unlist(strsplit( XS,XP ))))
	setnames(ZT,c( 'PAT','N','PCT' ))
	ZT <- ZT[order( -PCT )]
	ZT[,PCT:=100*round(PCT,4)][]
}
#################################___TAB_DIFF___#################################
TAB_DIFF <- function( XT1,XT2,COL) {
  XT1 <- as.data.table( XT1 )
  XT2 <- as.data.table( XT2 )
  XT1 <- DT(XT1[,COL,with=F],ID1= XT1[,.I])
  XT2 <- DT(XT2[,COL,with=F],ID2= XT2[,.I])
  ZT <- merge(XT1, XT2, by = COL, all = TRUE)
 ZT 
}
###################################___TABYL___##################################
TABYL <- function( XV ) {
  XT <- CLEAN_TABLE(tabyl(XV))
  XCOLNAMES <- colnames( XT )
  setnames(XT,c( XCOLNAMES[1],'N','PCT' ))
  #ZT <- ZT[order( -PCT )]
  XT[,PCT:=100*round(PCT,4)][]
}

####################################___TC___####################################
TC <- function(X) {
	XTC <- tryCatch(X, error = function(XER) list(OK=FALSE,ERROR=as.character( XER )))
	#IS_EQ(X,XTC)
	if ( IS_EQ(X,XTC) ) {
		Z <- list(OK=T,VALUE=X)
	} else {
		Z <- list(OK=F,ERROR=trimws(XTC$ERROR))
	}
Z
}
##################################___TC_NEW___##################################
TC_NEW <- function(X) {
	XTC <- tryCatch(X, error = function(XER) list(OK=FALSE,ERROR=as.character( XER )))
#IS_EQ(X,XTC)
	if ( IS_EQ(X,XTC) ) {
		Z <- list(OK=T,VALUE=X)
	} else {
		Z <- list(OK=F,ERROR=trimws(XTC$ERROR))
	}
Z
}
##################################___TC_OLD___##################################
TC_OLD <- function(X) {
 TC <- tryCatch(X, error = function(XER) list(OK=FALSE,ERROR=as.character( XER )))
 TC <- as.list( TC )
 	if ( is.null(TC$OK) ) {
	 Z <- list(OK=T,VALUE=X)
	} else {
	 Z <- list(OK=F,ERROR=trimws(TC$ERROR))
	}
	return( Z )
}
####################################___TC0___###################################
TC0 <- function(X) {
	tryCatch(X, error = function(XER) list(OK=FALSE,ERROR=as.character( XER )))
}
####################################___TC2___###################################
TC2 <- function(X) {
	XTC <- tryCatch(X, error = function(XER) list(OK=FALSE,ERROR=as.character( XER )))
	XTC <- as.list( XTC )
 	if ( is.null(XTC$OK) | XTC$OK ) {
	 Z <- list(OK=T,VALUE=X)
	} else {
	 Z <- list(OK=F,ERROR=trimws(XTC$ERROR))
	}
Z
}
####################################___TCB___###################################
TCB <- function( X ) {
	if ( !TRY_CATCH(X)$OK ) break
}
#################################___TEXT_PLOT___################################
TEXT_PLOT <- function(plotname, XS){
	par(mar=c(0,0,0,0))
	pdf(paste0(plotname, ".pdf"))
	plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
	text(x = 0.5, y = 0.5, XS, cex = 4, col = "black", family="serif", font=2, adj=0.5)
	dev.off()
}
###############################___TEXT_TO_PLOT___###############################
TEXT_TO_PLOT <- function(XV, X_SIZE = 8, X_COLOR = "BLACK") {
	text = paste0(XV, collapse = "\n")
	XPLOT <- ggplot() + annotate("text", x = 4, y = 25, size = X_SIZE, label = text, color = X_COLOR) + theme_void()
	XPLOT
}
#############################___TEXT_TO_PLOT_RET___#############################
TEXT_TO_PLOT_RET <- function(XV,X_SIZE=8,X_COLOR="BLACK") {
	text = paste0(XV,collapse = '\n')
	XPLOT <- ggplot() + 	annotate("text", x = 4, y = 25, size=X_SIZE, label = text,color=X_COLOR)	+ theme_void()
return( XPLOT )
}
###################################___TRIM___###################################
TRIM <- function( X ) {
	XTC <- TC(trimws(X))
		if ( XTC$OK ) {
		X <- XTC$VALUE
	} else {
		X <- gsub('^ +','',X)
		X <- gsub(' +$','',X)
	}
X
}
#################################___TRIM_VEC___#################################
TRIM_VEC <- function(XV) {
	XV[XV!='']	
}
#################################___TRY_CATCH___################################
TRY_CATCH <- function(X) {
	TRY_CATCH_0 <- tryCatch( X , error = function(e) NULL)
	if ( IS_BLANK( TRY_CATCH_0 ) ) {
		Z <- NA
	} else {
		Z <- TRY_CATCH_0
	}
Z
}
##############################___TRY_CATCH_ERROR___#############################
TRY_CATCH_ERROR <- function(X) {
tryCatch(X, error = function(XER) list(OK=NULL,ERROR=as.character( XER )))
}
####################################___U2___####################################
U2 <- function( X ) unname(unlist( X ))
####################################___UA___####################################
UA <- function(XU,TAG) {
	XRH <- RH(XU)
	if ( missing( TAG ) ) {
		XMISS <- T
		TAG <- 'a'
	} else {
		XMISS <- F
	}
	if(COUNT_WORDS(TAG) == 1) TAG <- LC(TAG)
	XL_ATTRS_ALL <- html_nodes(XRH,TAG) %>% html_attrs()
	XV_ATTRS <- unique(unlist(lapply(XL_ATTRS_ALL,names)))
	X_LEN <- length(XL_ATTRS_ALL)
	XL_ATTRS_ALL <- OKL( XL_ATTRS_ALL )
	ZT <- as.data.frame(t(unlist(XL_ATTRS_ALL[[1]])))
	if ( X_LEN>1 ) {
		for ( i in 2:X_LEN ) {
			ZT <- bind_rows( ZT , as.data.frame(t(unlist(XL_ATTRS_ALL[[i]]))) )
		}
	}
	################################################################
	XV_TEXT <- stri_trans_toupper(html_nodes(XRH,TAG) %>% html_text())
  XV_TEXT <- gsub("[\n\r\t\v\f]", " ",XV_TEXT)
	XV_TEXT <- gsub("[[:space:]]+", " ",XV_TEXT)
	ZT <- data.table(TEXT=XV_TEXT,ZT)
	################################################################
	ZT <- as.data.table(setnames(ZT,gsub("\\bR\\b", "R_ORG",colnames(ZT))))
	ZT[, R := .I]
	colnames(ZT) <- stri_trans_toupper(colnames(ZT))
	if ( XMISS ) {
		ZT <- ZT[,c('R','TEXT','HREF')]
		ZT[ , N:=.N , by = .(TEXT,HREF) ]
	}
CLEAN_TABLE(ZT)
}
##################################___UNCAMEL___#################################
UNCAMEL <- function(x) {
toupper(gsub("([a-z0-9])([A-Z])", "\\1_\\2", gsub("(.)([A-Z][a-z]+)", "\\1_\\2", x)))
}
################################___UNCAMEL_OLD___###############################
UNCAMEL_OLD <- function(x) {
toupper(gsub("([a-z0-9])([A-Z])", "\\1_\\2", gsub("(.)([A-Z][a-z]+)", "\\1_\\2", x)))
}
####################################___UNM___###################################
UNM <- function (obj, force = FALSE) {
  if (!is.null(names(obj))) 
  names(obj) <- NULL
  if (!is.null(dimnames(obj)) && (force || !is.data.frame(obj))) 
  dimnames(obj) <- NULL
obj
}
#################################___UNZIP_CSV___################################DA
UNZIP_CSV <- function( XFILE ) {
	TEMP_FILE <- tempfile()
	unzip(XFILE, exdir =  TEMP_FILE)
fread( TEMP_FILE )
}
################################___UNZIP_TABLE___###############################
UNZIP_TABLE <- function( XFILE ) {
	fread( unzip(XFILE, exdir =  TEMP_FILE) )
}
####################################___UPD___###################################
UPD <- function( XT,XR,XC,VAL ) {
#UPDATE DATA TABLE
 if ( IS_NUM( XR ) ) {
 	XR <- AS_NUM( XR )
 	XT[XR][[XC]] <- VAL
 } else {
 	 FVAR( XR )
 	 XT[get(X)==Y][[XC]] <- VAL
 }
XT		
}
#################################___UPD_BY_S___#################################
UPD_BY_S <- function( XT,XS ) {
	XT[W(AP(XT,1:2,function( X ) S( X,XS ) ),arr.ind=T)] <- XS
XT
}
##################################___UPD_ROW___#################################
UPD_ROW <- function( XT , XN_ROW_NUM ,XV ) {
XT[XN_ROW_NUM, (colnames(XT)):= as.list(XV)]
}
################################___URL_AS_BAT___################################
URL_AS_BAT <- function(XV,XF="URL.BAT") {
	#XP_FIREFOX <- "C:/pl/FIREFOX/firefox-sdk/bin/firefox.exe"
	#XP <- "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
	XRL <- XV
	#XRL <- B(XV,1,' ')
	#XRL <- na.omit(SUB(XRL, 1, "\\?"))
	ZV <- 'start chrome "' + XRL + '"'
	ZV <- c("@echo off", ZV)
	PING <- 'ping 192.0.2.2 -n 1 -w 5000 > nul'
	ZV <- ADD_NTH( ZV,PING )
WL(ZV,XF)	
}
################################___url_exists___################################
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {	
	suppressPackageStartupMessages({
		require("httr", quietly = FALSE, warn.conflicts = FALSE)
	})
	
	# you don't need thse two functions if you're alread using `purrr`
	# but `purrr` is a heavyweight compiled pacakge that introduces
	# many other "tidyverse" dependencies and this doesnt.
	
	capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
		tryCatch(
			list(result = code, error = NULL),
			error = function(e) {
				if (!quiet)
					message("Error: ", e$message)
				
				list(result = otherwise, error = e)
			},
			interrupt = function(e) {
				stop("Terminated by user", call. = FALSE)
			}
		)
	}
	
	safely <- function(.f, otherwise = NULL, quiet = TRUE) {
		function(...) capture_error(.f(...), otherwise, quiet)
	}
	
	sHEAD <- safely(httr::HEAD)
	sGET <- safely(httr::GET)
	
	# Try HEAD first since it's lightweight
	res <- sHEAD(x, ...)
	
	if (is.null(res$result) || 
					((httr::status_code(res$result) %/% 200) != 1)) {
		
		res <- sGET(x, ...)
		
		if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors
		
		if (((httr::status_code(res$result) %/% 200) != 1)) {
			if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
			return(non_2xx_return_value)
		}
		
		return(TRUE)
		
	} else {
		return(TRUE)
	}
	
}
################################___URL_EXISTS___################################
URL_EXISTS <- function(XU) {
	Z <- URL_EXISTS_0( XU )
	if (is.na(Z)) {	Z <- FALSE} 
return( Z )
}
###############################___URL_EXISTS_0___###############################
URL_EXISTS_0 <- url_exists
#################################___URL_TABLE___################################
URL_TABLE <- function(XU,ALL_ATTRS=FALSE) {
  if ( all(class(XU) == c("xml_document","xml_node"))) {
  XRH <- XU
  } else {
  XRH <- read_html(XU)
  }
  XL_ATTRS_ALL <- html_nodes(XRH,'a') %>% html_attrs()
  XV_TEXT <- stri_trans_toupper(html_nodes(XRH,'a') %>% html_text())
  XV_TEXT <- gsub("[\n\r\t\v\f]", "",XV_TEXT)
  XV_ATTRS <- unique(unlist(lapply(XL_ATTRS_ALL,names)))
  X_LEN <- length(XL_ATTRS_ALL)
  ZT <- as.data.frame(t(unlist(XL_ATTRS_ALL[[1]])))
  for ( i in 2:X_LEN ) {
  ZT <- bind_rows( ZT , as.data.frame(t(unlist(XL_ATTRS_ALL[[i]]))) )
  }
  ZT <- data.table(text=XV_TEXT,ZT)
  ZT <- setnames(ZT,gsub("\\bXI\\b", "XI_ORG",colnames(ZT)))
  ZT[, XI := .I]
  colnames(ZT) <- stri_trans_toupper(colnames(ZT))
  if ( !ALL_ATTRS ) {
  ZT <- ZT[,c('XI','TEXT','HREF')]
  #colnames(ZT) <- c('XI','XT','XH')
  ZT[ , `:=`(XN=.N) , by = .(TEXT,HREF) ]
  }
  closeAllConnections()
  as.data.table(ZT)
}
#################################___uses_arg___#################################
uses_arg <- function(x,arg) 
  is.function(fx <- get(x)) && 
  arg %in% names(formals(fx))
####################################___W0___####################################
W0 <- function(X) {
	X <- W(X)
	if ( IS_BLANK(X) ) {
		0
	} else {
		X
	}
}
####################################___WH___####################################
WH <- function( XU,X_FILE='TEMP.HTML') {
	XRH <- READ_HTML(XU,T)
	writeLines(SPL_HTML(XRH),X_FILE,useBytes = TRUE)
}
#############################___WHICH_ROW_MAX_BY___#############################
WHICH_ROW_MAX_BY <- function( XT,XC_MAX,XC_BY) {
	XQUERY <- paste0('SELECT * FROM XT JOIN (SELECT ',XC_BY,',MAX(',XC_MAX,') AS ',XC_MAX,' FROM XT GROUP BY ',XC_BY,') USING (',XC_BY,',',XC_MAX,')')
	sqldf( XQUERY )
}
###########################___WHICH_TABLE_FROM_LIST___##########################
WHICH_TABLE_FROM_LIST <- function(XL,...) {
	XW_TABLE <- which(sapply(XL,IS_TABLE))
	XL <- XL[XW_TABLE]
	XT <- DT(do.call(rbind,LAP(XL,dim)))
	setnames(XT,c('NR','NC'))
	XT[,ID:=.I]
	
	YL <- LAP(match.call(expand.dots=FALSE)$`...`,CH)
	WNR <- W(NTH_FROM_LIST(YL,1)=='NR')
	if ( LEN(WNR)==0L ) {
		WNR <- 0
	} else {
		WNR <- YL[[WNR]][-1]
		WNR <- U(UL(SAP(WNR,function(X) eval(parse(text=X)))))	
	}
	
	WNC <- W(NTH_FROM_LIST(YL,1)=='NC')
	if ( LEN(WNC)==0L ) {
		WNC <- 0
	} else {
		WNC <- YL[[WNC]][-1]
		WNC <- U(UL(SAP(WNC,function(X) eval(parse(text=X)))))	
	}
	if ( WNR!=0 & WNC!=0 ) {
		XI <- XT[NR %in% WNR & NC %in% WNC,ID]
	} else if (WNR==0) {
		XI <- XT[NC %in% WNC,ID]
	} else {
		XI <- XT[NR %in% WNR,ID]
	}
XL[XI]
}
####################################___WHL___###################################
WHL <- function(XL,XFILE='X.HTML',XSTYLE) {
	if(missing(XSTYLE)) XSTYLE <- c("<style>\ntr:nth-child(even) {\n  background-color:yellow;\n\ttext-align:right;\n}\nth:nth-child(even) {\n  background-color:orange;\n\tcolor:blue;\n\tfont-weight: bold;\n}\nth:nth-child(odd) {\n  background-color:orange;\n\tcolor:black;\n\tfont-weight: bold;\n}\n\ntd:nth-child(even) {\n\tcolor:blue;\n\tfont-weight: bold;\n\ttext-align:right;\n}\ntd:nth-child(odd) {\n  color:black;\n\tfont-weight: bold;\n\ttext-align:right;\n}\n</style>\n")
	#print(xtable(xt), type="html", file="xt.html")
	FR(XFILE)
	for ( i in SEQ(XL) ) {
		iXL <- XL[[i]]
		if (IS_TABLE(iXL)) {
			ZT <- FHT(iXL)
			write('<div style = "clear:both;"></div>', file=XFILE,append = TRUE)
			write('<br>', file=XFILE,append = TRUE)
			write('<table border=1 align=left>', file=XFILE,append = TRUE)
			write.table(ZT, file=XFILE,quote = FALSE,row.names = FALSE,append = TRUE,col.names = FALSE)
			write('</table>', file=XFILE,append = TRUE)
			write('<br>', file=XFILE,append = TRUE)
		} else {
			write('<div style = "clear:both;"></div>', file=XFILE,append = TRUE)
			write('<P>', file=XFILE,append = TRUE)
			write(iXL, file=XFILE,append = TRUE)
			write('</P>', file=XFILE,append = TRUE)
			write('<br>', file=XFILE,append = TRUE)
		}
	}
	write(XSTYLE, file=XFILE,append = TRUE)	
}
####################################___WHT___###################################
WHT <- function(XT,XFILE='X.HTML',XSTYLE) {
	if(missing(XSTYLE)) XSTYLE <- c("<style>\ntr:nth-child(even) {\n  background-color:yellow;\n\ttext-align:right;\n}\nth:nth-child(even) {\n  background-color:orange;\n\tcolor:blue;\n\tfont-weight: bold;\n}\nth:nth-child(odd) {\n  background-color:orange;\n\tcolor:black;\n\tfont-weight: bold;\n}\n\ntd:nth-child(even) {\n\tcolor:blue;\n\tfont-weight: bold;\n\ttext-align:right;\n}\ntd:nth-child(odd) {\n  color:black;\n\tfont-weight: bold;\n\ttext-align:right;\n}\n</style>\n")
	#print(xtable(xt), type="html", file="xt.html")
	FR(XFILE)
	ZT <- FHT(XT)
	write('<div style = "clear:both;"></div>', file=XFILE,append = TRUE)
	write('<br>', file=XFILE,append = TRUE)
	write('<table border=1 align=left>', file=XFILE,append = TRUE)
	write.table(ZT, file=XFILE,quote = FALSE,row.names = FALSE,append = TRUE,col.names = FALSE)
	write('</table>', file=XFILE,append = TRUE)
	write('<br>', file=XFILE,append = TRUE)
	write(XSTYLE, file=XFILE,append = TRUE)
}
###################################___WIKI___###################################
WIKI <- function( XS ) {
paste0('https://en.wikipedia.org/wiki/',XS)
}
##################################___WIKI_EN___#################################
WIKI_EN <- function(X) {
	paste0('https://en.m.wikipedia.org/wiki/',XS)
}
##################################___WIKI_ES___#################################
WIKI_ES <- function(XS) {
	paste0('https://es.m.wikipedia.org/wiki/',XS)
}
##################################___WIKI_PL___#################################
WIKI_PL <- function(X) {
	paste0('https://pl.m.wikipedia.org/wiki/',XS)
}
#############################___WIKI_ROCHE_PARSE___#############################
WIKI_ROCHE_PARSE <- function( CHEM_NAME ) {
	XU <- WIKI( CHEM_NAME )
	XRH <- read_html(XU)
	XV_INFO_ALL <- html_nodes(XRH, 'table.infobox tr') %>% html_text(  )
	CHEM_NAME <- html_nodes(XRH, 'h1') %>% html_text(  )
	FORMULA <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'ormula',-1))))
	MOLAR_MASS <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'olar mass',-1))))
	LEGAL_STATUS <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'egal status\n','\n'))))
	EL_HALF_LIFE <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'limination half-life',-1))))
	METABOLISM <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'etabolism',-1))))
	METABOLITES <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'etabolites',-1))))
	EXCREATION <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'xcretion',-1))))
	ROUTES_OF_ADMINISTRATION <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'outes ofadministration',-1))))
	PROTEIN_BINDING <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'rotein binding',-1))))
	BIOAVAILABILITY <-trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'ioavailability',-1))))
	ZT <- data.table(CHEM_NAME,
	FORMULA,
	MOLAR_MASS,
	LEGAL_STATUS,
	EL_HALF_LIFE,
	METABOLISM,
	METABOLITES,
	EXCREATION,
	ROUTES_OF_ADMINISTRATION,
	PROTEIN_BINDING,
	BIOAVAILABILITY)
return(ZT)
}
#########################___WIKI_ROCHE_PARSE_PRODUCT___#########################
WIKI_ROCHE_PARSE_PRODUCT <- function( PRODUCT_NAME ) {
	XU <- paste0('https://en.wikipedia.org/w/index.php?title=',PRODUCT_NAME,'&redirect=yes')
	XRH <- read_html(XU)
	XV_INFO_ALL <- html_nodes(XRH, 'table.infobox tr') %>% html_text(  )
	CHEM_NAME <- html_nodes(XRH, 'h1') %>% html_text(  )
	FORMULA <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'ormula',-1))))
	MOLAR_MASS <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'olar mass',-1))))
	LEGAL_STATUS <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'egal status\n','\n'))))
	EL_HALF_LIFE <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'limination half-life',-1))))
	METABOLISM <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'etabolism',-1))))
	METABOLITES <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'etabolites',-1))))
	EXCREATION <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'xcretion',-1))))
	ROUTES_OF_ADMINISTRATION <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'outes ofadministration',-1))))
	PROTEIN_BINDING <- trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'rotein binding',-1))))
	BIOAVAILABILITY <-trimws(as.vector(na.omit(SUB(XV_INFO_ALL,'ioavailability',-1))))
	ZT <- data.table(CHEM_NAME,
	FORMULA,
	MOLAR_MASS,
	LEGAL_STATUS,
	EL_HALF_LIFE,
	METABOLISM,
	METABOLITES,
	EXCREATION,
	ROUTES_OF_ADMINISTRATION,
	PROTEIN_BINDING,
	BIOAVAILABILITY)
return(ZT)
}
####################################___WL___####################################
WL <- function (text, con = stdout(), sep = "\n", useBytes = FALSE) {
  if (!is.character(text)) 
    stop("can only write character objects")
  if (is.character(con)) {
    con <- file(con, "w")
    on.exit(close(con))
  }
  .Internal(writeLines(text, con, sep, useBytes))
}
###################################___WL_TF___##################################
WL_TF <- function(X) {
	TEMP_FILE <- tempfile()
	writeLines(XV,TEMP_FILE,useBytes = TRUE)
}
####################################___WLX___###################################
WLX <- function(XV) {
	X.R <- RL('X.R')
	XBR <- strrep('#',80)
	XV <- c(XV,XBR,'\n',X.R)
print(head(XV))
WL(XV, "X.R")
}
###################################___word___###################################
word <- function(C, k) paste(rep.int(C, k), collapse = "")
##############################___WRITE_FAKE_SRT___##############################
WRITE_FAKE_SRT <- function(XV, TIME_START=0,TIME_PERIOD = 10,REPEAT = 0,LEN_OF_LINE=10) {
	if (IS_TABLE(XV)) {
		XV <- apply(XV, 1, function(X) paste0(X, collapse = " | "))
	}
	X_TIME <- TIME_START
	YV <- vector()
	ZV <- vector()
	XL <- list(  )
	for (i in seq_along(XV)) {
		for ( j in seq_along(REPEAT )) {
			YV[1] <- i
			YV[2] <- SECONDS_TO_SRT(X_TIME + REPEAT[j],X_TIME + REPEAT[j]+LEN_OF_LINE)
			YV[3] <- XV[i]
			YV[4] <- ""
			ZV <- c(ZV, YV)
		}
		X_TIME <- X_TIME + TIME_PERIOD
	}
	ZV
}
################################___WRITE_HTML___################################
WRITE_HTML <- function( XU,X_FILE='TEMP.HTML') {
	XRH <- (as.character(READ_HTML( XU )))
	XRH <- unlist(strsplit(XRH, "(?<=>)", perl=TRUE))
	XRH <- unlist(strsplit(XRH, "<(?=<)", perl = T))
	XRH <- gsub("[\n\r\t\v\f]", "",XRH)
	XRH <- XRH[XRH!='']
writeLines(XRH,X_FILE,useBytes = TRUE)
}
################################___WRITE_LIST___################################
WRITE_LIST <- function(XL,XF,XNAMES=T,XHASH=T) {
	if ( !XNAMES ) {
		XV <- UL(XL)
		WL(XV,XF)
	} else {
		iNAMES <- names(XL)
		for ( i in SEQ(XL) ) {
			iXV <- XL[[i]]
			
		}
	}
	for ( i in SEQ(XL) ) {
		iXV <- XL[[i]]
		iNAMES <- names(XL)[i]
		if ( B(iXV[LEN(iXV)],1,1)=='#' ) {
			XLAST <- iXV[LEN(iXV)]
			iXV <- iXV[-LEN(iXV)]
		}
		if (S0(iNAMES,'-')) iNAMES <- GSUB(iNAMES,MATCH1('-'),'(')
		if (S0(iNAMES,':')) {
			iNAMES <- GSUB(iNAMES,MATCH1(':'),'(')
			XQ <- B(iNAMES,-1,-1)!=','
			if(XQ) iNAMES <- P(iNAMES,',')
		}	
		XN <- str_count(iNAMES,'\\(')+1
		XCH_BRACET <- strrep(')',XN)
		iNAMES <- P(iNAMES,'(')
		iXV <- c(iNAMES,iXV,XCH_BRACET)
		XI <- 2:(LEN(iXV)-1)
		iXV[XI] <- P('\t',iXV[XI])
		if (exists('XLAST')) {
			iXV <- c(iXV,XLAST)
			RM(XLAST)
		}
	XL[[i]] <- iXV	
	}
WL(UL(XL))
}
##############################___WRITE_LIST_HTML___#############################
WRITE_LIST_HTML <- function(XL,XFILE='X.HTML',XSTYLE) {
	if(missing(XSTYLE)) XSTYLE <- c("<style>\ntr:nth-child(even) {\n  background-color:yellow;\n\ttext-align:right;\n}\nth:nth-child(even) {\n  background-color:orange;\n\tcolor:blue;\n\tfont-weight: bold;\n}\nth:nth-child(odd) {\n  background-color:orange;\n\tcolor:black;\n\tfont-weight: bold;\n}\n\ntd:nth-child(even) {\n\tcolor:blue;\n\tfont-weight: bold;\n\ttext-align:right;\n}\ntd:nth-child(odd) {\n  color:black;\n\tfont-weight: bold;\n\ttext-align:right;\n}\n</style>\n")
	#print(xtable(xt), type="html", file="xt.html")
	FR(XFILE)
	XL.NAMES <- names(XL)
	for ( i in SEQ(XL) ) {
		if ( !is.null(XL.NAMES[i]) ) {
			XTEXT <- P('<H6>',XL.NAMES[i],'</H6>')
			write(XTEXT, file=XFILE,append = TRUE)
		}
		if ( IS_TABLE(XL[[i]]) ) {
			ZT <- FHT(XL[[i]])
			write('<div style = "clear:both;"></div>', file=XFILE,append = TRUE)
			write('<table border=1>', file=XFILE,append = TRUE)
			write.table(ZT, file=XFILE,quote = FALSE,row.names = FALSE,append = TRUE,col.names = FALSE)
			write('</table>', file=XFILE,append = TRUE)
			#write('<br>', file=XFILE,append = TRUE)
		} else {
			XTEXT <- P('<P>',XL[[i]],'</P>')
			write(XTEXT, file=XFILE,append = TRUE)
		}
	}
write(XSTYLE, file=XFILE,append = TRUE)
}
##############################___WRITE_SRT_FILE___##############################
WRITE_SRT_FILE <- function( XT ) {
ZV  <- vector(  )
k <- 1
for ( i in seq.int(nrow(XT))  ) {
YL  <- XT[i,]
XS_HR  <- str_pad( YL$START_HOUR,2,pad="0" )
XS_MIN  <- str_pad( YL$START_MIN,2,pad="0" )
XS_SEC <- str_pad( floor(YL$START_SEC),2,pad="0" ) + ',' + SUB(YL$START_SEC %%1,3,5)
XS_BEFORE <- paste0(c(XS_HR,XS_MIN,XS_SEC),collapse = ':')
XS_HR  <- str_pad( YL$END_HOUR,2,pad="0" )
XS_MIN  <- str_pad( YL$END_MIN,2,pad="0" )
XS_SEC <- str_pad( floor(YL$END_SEC),2,pad="0" ) + ',' + SUB(YL$END_SEC %%1,3,5)
XS_AFTER <- paste0(c(XS_HR,XS_MIN,XS_SEC),collapse = ':')
ZV[k] <- YL$ID
ZV[k+1] <- XS_BEFORE + ' --> ' + XS_AFTER
ZV[k+2] <- XT[i,TEXT]
k <- length(ZV)+1
}
}
###############################___WRITE_TO_SRT___###############################
WRITE_TO_SRT <- function(XT,XF='X.SRT') {
XV <- vector()
XL <- L()
for ( i in 1:nrow(XT)) {
	XV[1] <- i
	XV[2] <- SECONDS_TO_SRT( XT[i,START],XT[i,END])
	XV[3] <- XT[i,TEXT]
	XV[4] <- ''
	XL[[i]] <- XV
}
XV <- UL(XL)
WL(XV,XF)
}
####################################___WT___####################################
WT <- function(XT,XS_FILE,XS_SEP=';') {
	if ( missing(XS_FILE) ) {
		XS_FILE = deparse(substitute(XT))
		XS_FILE = paste0(XS_FILE,'.csv')
	}
write.table(XT,XS_FILE,sep=XS_SEP,quote = FALSE,row.names = FALSE,col.names = TRUE,fileEncoding = "UTF-8")
}
###################################___WTFL___###################################
WTFL <- function(XL,...) {
	XW_TABLE <- which(sapply(XL,IS_TABLE))
	XL <- XL[XW_TABLE]
	XT <- DT(do.call(rbind,LAP(XL,dim)))
	setnames(XT,c('NR','NC'))
	XT[,ID:=.I]
	
	YL <- LAP(match.call(expand.dots=FALSE)$`...`,CH)
	WNR <- W(NTH_FROM_LIST(YL,1)=='NR')
	if ( LEN(WNR)==0L ) {
		WNR <- 0
	} else {
		WNR <- YL[[WNR]][-1]
		WNR <- U(UL(SAP(WNR,function(X) eval(parse(text=X)))))	
	}
	
	WNC <- W(NTH_FROM_LIST(YL,1)=='NC')
	if ( LEN(WNC)==0L ) {
		WNC <- 0
	} else {
		WNC <- YL[[WNC]][-1]
		WNC <- U(UL(SAP(WNC,function(X) eval(parse(text=X)))))	
	}
	if ( WNR!=0 & WNC!=0 ) {
		XI <- XT[NR %in% WNR & NC %in% WNC,ID]
	} else if (WNR==0) {
		XI <- XT[NC %in% WNC,ID]
	} else {
		XI <- XT[NR %in% WNR,ID]
	}
XL[XI]
}
####################################___WTL___###################################
WTL <- function(XU,X_ROW_AS_COL=T) {
	if ( all(class(XU) == c("xml_document","xml_node"))) {
		XRH <- XU
	} else {
		XRH <- read_html(XU)
	}
	ZL <- html_nodes(XRH, "table") %>% html_table(fill = TRUE)
	ZL <- LAP(ZL, as.data.table)
	if (X_ROW_AS_COL) {
		ZL <- LAP(ZL, CLEAN_TABLE_WIKIPEDIA)
	} else {
		ZL <- LAP(ZL, function( X ) CLEAN_TABLE_WIKIPEDIA( X,X_ROW_AS_COL=F ))
	}
	ZL
}
