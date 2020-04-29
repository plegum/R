options(stringsAsFactors=FALSE)
options(scipen = 999)
library( tidyverse )
library( ggplot2 )
library( reshape2 )
library( rvest )
library( stringi )
library( data.table )
library( dplyr )
library(shiny)
library(ggplot2)
library(stringr)
library(stringi)
library(readxl)
library(sqldf)
library(dplyr)
library(data.table)
library(rvest)
library( janitor )
library(shiny)
library(rsconnect)
library(viridis)
library(rnaturalearth)
library( sf )
library( rgeos)
library(rnaturalearthdata)
library( DT )
library( Rfast )
################################################################################
NTH <- Rfast::nth
################################################################################
COR_MAP_REG  <- function( XM_COR,Y_FILL ) {
  MAX_LOG <- floor(log1p(max(XM_COR[[Y_FILL]])))
  MIN_LOG <- floor(log1p(min(XM_COR[[Y_FILL]])))
  XV_BREAKS <- ROUND(expm1(c( ( MIN_LOG+1 ):MAX_LOG )))
  XV_BREAKS <- ROUND(XV_BREAKS[seq_along(XV_BREAKS) %% 2 ==0],0)
  XV_BREAKS <- c( ROUND(expm1(MIN_LOG),0),XV_BREAKS )
  XV_LABELS  <- c('0 or no data',XV_BREAKS[-1])
  XV_COLORS  <- c( 'gray80', rev(plasma(20) ))
  LEG_TIT <- 'Number of ' + tolower(Y_FILL) + '\ncases'
  ###___</LEGEND_AND_BREAK>___###
  XT_POINTS_0 <- sf::st_point_on_surface(XM_COR)
  XV_TOP_3 <- 	sapply(c(1:3), function(X) NTH(XT_POINTS_0[[Y_FILL]], X, descending = T))
  XT_POINTS  <-  XT_POINTS_0[XT_POINTS_0[[Y_FILL]]  %in% XV_TOP_3,]
  XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
  XT_COORDS$POINTS_TEXT <- XT_POINTS[['su_a3']] +': ' + XT_POINTS[[Y_FILL]]
  POINTS_TEXT_LEGEND  <-  'Displaying precise value for top 3 amounts.'
  ZM <-
    ggplot() + geom_sf(data = XM_COR,aes_string(fill=Y_FILL)) + 
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
          panel.grid = element_line(color = "white", size = 0.5))
  ZM
}
##########
COR_MAP_WORLD  <- function( XM_COR,Y_FILL ) {
  MAX_LOG <- floor(log1p(max(XM_COR[[Y_FILL]])))
  MIN_LOG <- floor(log1p(min(XM_COR[[Y_FILL]])))
  XV_BREAKS <- ROUND(expm1(c( ( MIN_LOG+1 ):MAX_LOG )))
  XV_BREAKS <- ROUND(XV_BREAKS[seq_along(XV_BREAKS) %% 2 ==0],0)
  XV_BREAKS <- c( ROUND(expm1(MIN_LOG),0),XV_BREAKS )
  XV_LABELS  <- c('0 or no data',XV_BREAKS[-1])
  XV_COLORS  <- c( 'gray80', rev(plasma(20) ))
  LEG_TIT <- 'Number of ' + tolower(Y_FILL) + '\ncases'
  ###___</LEGEND_AND_BREAK>___###
  XT_POINTS_0 <- sf::st_point_on_surface(XM_COR)
  XT_POINTS_MAX <- XT_POINTS_0[which.max(XT_POINTS_0[[Y_FILL]]),]
  XT_POINTS_CHN <- XT_POINTS_0[XT_POINTS_0[['su_a3']]=='CHN',]
  XT_POINTS  <-  rbind( XT_POINTS_CHN,XT_POINTS_MAX )
  XT_COORDS <- as.data.frame(sf::st_coordinates(XT_POINTS))
  XT_COORDS$POINTS_TEXT <- XT_POINTS[['su_a3']] +': ' + XT_POINTS[[Y_FILL]]
  POINTS_TEXT_LEGEND  <-  'Displaying precise value for country with biggest amomunt and for China.'
  ZM <-
    ggplot() + geom_sf(data = XM_COR,aes_string(fill=Y_FILL)) + 
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
          panel.grid = element_line(color = "white", size = 0.5))
  ZM
}
################################################################################
CHANGE_COLNAME <- function( XT,XC_OLD,XC_NEW ){
  names(XT)[names(XT) == XC_OLD] <- XC_NEW
  XT
}
################################################################################
`+` <- function(x, y) {
  if(is.character(x) || is.character(y)) {
    return(stri_c(x, y))
  } else {
    .Primitive("+")(x, y)
  }
}
################################################################################
ADD_COL <- function(XT,X_COL_VAL) {
  cbind(XT,XC=rep( X_COL_VAL,nrow( XT ) ))
}
################################################################################
ADD_ROW <- function( XT,XV ) {
  if ( length(XV)!=1 ) {
    ZT <- rbindlist(list(XT, as.list(XV)))
  } else {
    ZT <- rbindlist(list(XT,
                         as.list(rep(XV,ncol(XT)))
    ))
  }
  return( ZT )
}
################################################################################
APP_TO_XAPP <- function(APP='APP.R',XAPP='XAPP.R') {
  XRL <- readLines(APP)
  #XRL <- trimws(XRL)
  #XRL <- XRL[XRL!='']
  #XRL <- XRL[SUB( XRL,1,1 )!='#']
  XT  <- XT_SHINY[NCHAR>3]
  for ( i in 1:nrow( XT ) ) {
    XRL <- 	gsub( "\\b" + XT[i,F_NAME_ORG] + "\\b" ,XT[i,F_NAME_MY],XRL )
  }
  WL( XRL,XAPP )
}
################################################################################
AS_HTML <- function( XV ){
  writeLines(XV,'HTML_TEMP.HTML',useBytes = TRUE)
  readLines('HTML_TEMP.HTML',encoding = "UTF-8")
}
################################################################################
as_num <- function(X){
  as.numeric(as.character(X))
}
################################################################################
AS_NUM <- function( X ) {
  if (is.vector( X ) ) {#_<IS_VECTOR>_#
    Z <- AS_NUM_0( X )
  } else { #_</IS_VECTOR>_#
    if ( IS_TABLE(X) ) {  #_</IS_TABLE>_#
      Z <- as.data.frame( X )
      Z <- as.data.table(mutate_all(Z,AS_NUM_0))
    } else {
      Z  <- 'NOT VECTOR OR TABLE'
    }}
  return(Z)
}
################################################################################
AS_NUM_0 <- function( X , DELETE_SPACE = TRUE ) {
  Z <- gsub(',','.',X)
  if ( DELETE_SPACE==T ) {
    Z <- gsub(' ','',Z)
  }
  if (IS_NUM( Z )) {
    Z <- as.numeric( as.character(Z) )
  } else {
    Z <- X
  }
  return( Z )
}
################################################################################
CAC <- function ()
{
  i <- sink.number(type = "message")
  if (i > 0L)
    sink(stderr(), type = "message")
  n <- sink.number()
  if (n > 0L)
    for (i in seq_len(n)) sink()
  gc()
  set <- getAllConnections()
  set <- set[set > 2L]
  for (i in seq_along(set)) close(getConnection(set[i]))
  invisible()
}
################################################################################
CHANGE_COLNAME <- function( XT,XC_OLD,XC_NEW ){
  names(XT)[names(XT) == XC_OLD] <- XC_NEW
  XT
}
################################################################################
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
################################################################################
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
################################################################################
CHECK_ID <- function( XT ) {
  any(stri_detect_regex(colnames(XT),'^ID$'))
}
################################################################################
CHOOSE_FANTASY_COLS_2 <- function(XT) {
  XT <- AS_NUM(XT)
  if ( CHECK_ID( XT ) ) {
    XT <- XT[,.(PLAYER,X3P,PTS,REB,AST,STL,BLK,TOV,FGM,FGA,FTM,FTA,G,ID)]
  } else {
    XT <- XT[,.(PLAYER,X3P,PTS,REB,AST,STL,BLK,TOV,FGM,FGA,FTM,FTA,G)]
  }
  return(na.omit(XT))
}
################################################################################
CLEAN_PLAYER_NAME <- function( XS_PLAYER_NAME ) {
  library( stringi )
  XS_PLAYER_NAME <- gsub('-',' ',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('\\.','',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub(' IIII','',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub(' III','',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub(' II','',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub("'",'',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub(' JR$','',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','C',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','E',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','A',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','C',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','O',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','O',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','O',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','A',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','S',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','Z',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','U',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','N',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','R',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','I',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','E',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','U',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','C',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','S',XS_PLAYER_NAME)
  XS_PLAYER_NAME <- gsub('?','Y',XS_PLAYER_NAME)
  XS_PLAYER_NAME
}
################################################################################
CLEAN_STRING <- function( XS,UPPER_CASE=FALSE ) {
  XS <- gsub(" +", " ",XS)
  XS <- trimws( XS )
  if ( UPPER_CASE ) {
    XS <- stri_trans_toupper(XS)
  }
  return( XS )
}
################################################################################
CLEAN_STRING_OLD <- function( XS,UPPER_CASE=TRUE ) {
  XS <- gsub("[[:punct:]]", "",XS)
  XS <- gsub(" +", " ",XS)
  XS <- trimws( XS )
  if ( UPPER_CASE ) {
    XS <- stri_trans_toupper(XS)
  }
  return( XS )
}
################################################################################
CLEAN_TABLE <- function( XT ) {
  library( janitor )
  XT <- clean_names( XT ,case = "all_caps")
  XT <- AS_NUM( XT )
  for (i in which(sapply(XT, class) == "character")) {
    XT[[i]]  <- CLEAN_STRING( XT[[i]] )
  }
  return( XT )
}
################################################################################
clean_up <- function(x) {
  stringr::str_replace_all(x, "[\r\t\n]", "")
}
################################################################################
COMMA_TO_DOT_AS_NUMBER <- function(XV) {
  #	XV <- c   ( NA,'1,2','6,2','1,4' )
  ZV <- XV
  XV <- gsub(',','.',XV)
  XV <- gsub(' ','',XV)
  if ( IS_NUM( XV ) ) {
    ZV <- as.numeric(as.character(XV))
  }
  ZV
}
################################################################################
CONVERT_MIN_TO_NUMBER <- function(XS) {
  XV_SPL <- SPL( XS,':' )
  XMIN <- as.numeric(XV_SPL[1])
  XSEC <- as.numeric(XV_SPL[2])/60
  return( ROUND( XMIN+XSEC ) )
}
################################################################################
COUNT_UNIQUE <- function( XT ) {
  XT <- as.data.table(XT)
  XCOLNAMES <- colnames(XT)
  CNT <- sapply( seq_along(XCOLNAMES) ,function( X ) length(unique(na.omit(XT[[X]]))))
  data.table(XCOLNAMES,CNT)
}
################################################################################
CREATE_MAP <- function(XT_MAP,
                       X_FILL,
                       X_TRANS='identity',
                       X_FILL_COLOR_LOW='grey90',
                       X_FILL_COLOR_HIGH='grey10',
                       X_TEXT,
                       X_TEXT_COLOR='red'
){
  Z_MAP <- ggplot() + geom_sf(data = XT_MAP,aes_string(fill=X_FILL)) +
    scale_fill_gradient(trans=X_TRANS,low=X_FILL_COLOR_LOW, high=X_FILL_COLOR_HIGH)
  if (!missing( X_TEXT  )  ) {
    XT_points <- sf::st_point_on_surface(XT_MAP)
    XT_coords <- as.data.frame(sf::st_coordinates(XT_points))
    XT_coords$Y_TEXT <- unname(unlist(as.data.table(XT_MAP[,X_TEXT])[,1]))
    if ( IS_NUM(XT_coords$Y_TEXT) ) {#<IF>#
      XT_coords$Y_TEXT <- ROUND(XT_coords$Y_TEXT)
      XV <- SPL_WHICH(XT_coords$Y_TEXT,'\\.')
      X_MIN_XV <- min(nchar(XV))
      if( X_MIN_XV > 4 ) {
        XT_coords$Y_TEXT <- SPL_WHICH(as.numeric(XV)/1000,'\\.') + 'K'
      }}#</IF>#
    Z_MAP <- Z_MAP +
      geom_text(data = XT_coords, aes(X, Y, label = Y_TEXT), color = X_TEXT_COLOR) +
      labs(caption = X_TEXT) + theme(plot.caption = element_text(color = X_TEXT_COLOR, face="bold"))
  }
  Z_MAP
}
################################################################################
DATA_TABLE <- function( XT , XV=c(5, 30, 50)) {
  DT::datatable(XT, options = list(lengthMenu = XV, pageLength = nrow( XT )))
}
################################################################################
DATE_TO_NUM <- function( XV ) {
  XV <- SUB( XV,6,-1 )
  AS_NUM(gsub( '-','.',XV ))
}
################################################################################
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
################################################################################
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
################################################################################
DELETE_DOM_LINE <- function(XV,DOM) {
  DOM <- "<" + DOM + ".*>"
  xv <- gsub(DOM, '', XV, ignore.case = TRUE)
}
################################################################################
DELETE_DOM_OUTSIDE <- function(XV,DOM) {
  DOM_START  <- "<" + DOM + ".*>"
  DOM_END <- "</" + DOM + ">"
  XN_BODY_START <- min(grep(DOM_START, XV, ignore.case = TRUE))
  XN_BODY_END <- max(grep(DOM_END, XV, ignore.case = TRUE))
  XV[(XN_BODY_START):(XN_BODY_END)]
}
################################################################################
f.lib <- function() {
  options(stringsAsFactors=FALSE)
  options(scipen = 999)
  library( tidyverse )
  library( ggplot2 )
  library( reshape2 )
  library( rvest )
  library( stringi )
  library( data.table )
  library( dplyr )
  library(shiny)
  library(ggplot2)
  library(stringr)
  library(stringi)
  library(readxl)
  library(sqldf)
  library(dplyr)
  library(data.table)
  library(rvest)
  library( janitor )
  library(shiny)
  library(rsconnect)
  library(track)
  library( sf )
  library(rnaturalearth)
  library(rnaturalearthdata)
  track.history.start("hist.r")
}
################################################################################
F_CHECK_NUMBER_OF_OFFERS <- function( X_PRICE_LOW,X_PRICE_HIGH ){
  XU <- 'https://www.otodom.pl/sprzedaz/dom/?search%5Bfilter_float_price%3Afrom%5D=' +
    X_PRICE_LOW +
    '&search%5Bfilter_float_price%3Ato%5D=' +
    X_PRICE_HIGH +
    '&search%5Border%5D=filter_float_price%3Aasc&nrAdsPerPage=72'
  XRH <- read_html( XU )
  XV_NUMBER_OF_OFFERS <- trimws(html_nodes( XRH,'ul.pager li' )  %>% html_text())
  X_NUMBER_OF_OFFERS <- max(na.omit(AS_NUM( XV_NUMBER_OF_OFFERS )))
  X_NUMBER_OF_OFFERS
}
################################################################################
F_CHECK_NUMBER_OF_PAGES <- function( XU) {
  XRH <- read_html( XU )
  XV_NUMBER_OF_PAGES <- trimws(html_nodes( XRH,'ul.pager li' )  %>% html_text())
  X_NUMBER_OF_PAGES <- max(na.omit(AS_NUM( XV_NUMBER_OF_PAGES )))
  X_NUMBER_OF_PAGES
}
################################################################################
F_CHECK_NUMBER_OF_PAGES_2 <- function( X ) {
  F_CHECK_NUMBER_OF_PAGES( X,X+100000 )
}
################################################################################
F_CLEAN_BASKETBALL_TABLE_YAHOO <- function( XT ) {
  XCOLNAMES <- XT[1,]
  XCOLNAMES <- stri_trans_toupper( XCOLNAMES )
  XCOLNAMES <- gsub('%','P',XCOLNAMES)
  XCOLNAMES <- gsub('\\$','DOL',XCOLNAMES)
  ZT <- XT[-1,]
  colnames( ZT ) <- XCOLNAMES
  ZT <- as.data.table(clean_names( ZT ,case = "all_caps"))
  XCOLNAMES <- colnames( ZT )
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'PRE_SEASON')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'OWNED')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'MPG')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'TOTAL')
  ZT <- setnames( ZT,XCOLNAMES )
  F_NORM(ZT)
}
################################################################################
F_CLEAN_BASKETBALL_TABLE_YAHOO_25 <- function( XT26_STATS ) {
  XCOLNAMES <- XT26_STATS[1,]
  XCOLNAMES[stri_detect_regex(names(XCOLNAMES),'Opp|OPP')] <- paste0('OPP_',XCOLNAMES[stri_detect_regex(names(XCOLNAMES),'Opp|OPP')])
  XCOLNAMES <- stri_trans_toupper( XCOLNAMES )
  XCOLNAMES <- gsub('%','P',XCOLNAMES)
  XCOLNAMES <- gsub('\\$','DOL',XCOLNAMES)
  ZT25 <- XT26_STATS[-1,]
  colnames( ZT25 ) <- XCOLNAMES
  if (all(ZT25$MPG == '-')) {
    ZT25$MPG <- 0
  } else {
    ZT25$MPG <- TRY_CATCH(unname(sapply(ZT25$MPG ,F_CONVERT_MIN)))
  }
  ZT25 <- as.data.table(clean_names( ZT25 ,case = "all_caps"))
  XCOLNAMES <- colnames( ZT25 )
  grep(' INJ\n', XT26_STATS[,2])
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'PRE_SEASON')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'OWNED')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'MPG')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'FAN_PTS')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'OPP')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'CURRENT')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'FORECA')
  ZT25 <- setnames( ZT25,XCOLNAMES )
  FGM <- FGA <- FTM <- FTA <- vector()
  for ( i in 1:nrow( ZT25 ) ) {
    FGMA <- TRY_CATCH(SPL(ZT25[i,FGM_A],'/'))
    FGM[i] <- AS_NUM( TRY_CATCH(gsub('\\D','',FGMA[1])))
    FGA[i] <- AS_NUM(TRY_CATCH(gsub('\\D','',FGMA[2])))
    FTMA <- TRY_CATCH(SPL(ZT25[i,FTM_A],'/'))
    FTM[i] <- AS_NUM(TRY_CATCH(gsub('\\D','',FTMA[1])))
    FTA[i] <- AS_NUM(TRY_CATCH(gsub('\\D','',FTMA[2])))
  }
  INJ  <- rep(0,nrow( ZT25 )  )
  INJ[grep(' INJ', ZT25[,PLAYERS])]  <- 1
  ZT25 <- CLEAN_TABLE( ZT25)
  ZT25 <- select(ZT25,matches('MPG|OPP|FAN_PTS|OWNER|GP|PRE_SEASON|CURRENT|OWNED|X3PTM|PTS|REB|AST|ST|BLK|TO|OPP'))
  ZT25 <- data.table(ZT25,FGM,FGA,FTM,FTA,INJ)
  ZT25[,( !sapply(ZT25, function( x ) all(is.na( x )))),with=FALSE]
}
################################################################################
F_CLEAN_BASKETBALL_TABLE_YAHOO_25_H2H_PROJ <- function( XT26_STATS ) {
  XCOLNAMES <- XT26_STATS[1,]
  XCOLNAMES <- stri_trans_toupper( XCOLNAMES )
  XCOLNAMES <- gsub('%','P',XCOLNAMES)
  XCOLNAMES <- gsub('\\$','DOL',XCOLNAMES)
  ZT25 <- XT26_STATS[-1,]
  colnames( ZT25 ) <- XCOLNAMES
  ZT25 <- as.data.table(clean_names( ZT25 ,case = "all_caps"))
  XCOLNAMES <- colnames( ZT25 )
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'PRE_SEASON')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'OWNED')
  XCOLNAMES <- SUB_LIKE(XCOLNAMES,'MPG')
  ZT25 <- setnames( ZT25,XCOLNAMES )
  FGM <- FGA <- FTM <- FTA <- vector()
  for ( i in 1:nrow( ZT25 ) ) {
    FGMA <- SPL(ZT25[i,FGM_A],'/')
    FGM[i] <- gsub('\\D','',FGMA[1])
    FGA[i] <- gsub('\\D','',FGMA[2])
    FTMA <- SPL(ZT25[i,FTM_A],'/')
    FTM[i] <- gsub('\\D','',FTMA[1])
    FTA[i] <- gsub('\\D','',FTMA[2])
  }
  ZT25[,.(GP,CURRENT,OWNED,X3PTM,PTS,REB,AST,ST,BLK,TO)]
  #ZT25[,.(GP,FGP,FTP,X3PTM,PTS,REB,AST,ST,BLK,TO)] #MPG=MPG2SEC(MPG),
}
################################################################################
F_CONVERT_MIN <- function(XS) {
  XV_SPL <- SPL( XS,':' )
  XMIN <- as.numeric(XV_SPL[1])
  XSEC <- as.numeric(XV_SPL[2])/60
  return( ROUND( XMIN+XSEC ) )
}
################################################################################
F_COUNT_UNIQUE <- function( XT ) {
  XT <- as.data.table(XT)
  XCOLNAMES <- colnames(XT)
  CNT <- sapply(XCOLNAMES ,function( X ) nrow(unique(na.omit(XT[,X,with=FALSE]))))
  data.table(XCOLNAMES,CNT)
}
################################################################################
F_DUMP <- function() {
  LSF_STR <- lsf.str()
  dump(LSF_STR,file='lsf_str.r')
}
################################################################################
F_FBX1 <- function() {
  xs_jutro <- Sys.Date()+1
  XU <- 'https://basketball.fantasysports.yahoo.com/nba/11377/7' #Z MYTEAM
  xu <- paste0(XU,'/team?&date=',xs_jutro,'&stat1=S&stat2=D')
  xrh <- read_html(xu)
  xs_table_a_class <- '.Table-px-sm a.Nowrap'
  xl_FBX1_players <- html_nodes(xrh,xs_table_a_class) %>% html_attrs()
  xv_FBX1_players <- sapply(xl_FBX1_players, function(x) x['href'])
  xv_FBX1_players_id <- F_GET_PLAYER_ID(xv_FBX1_players)
  FBX1 <- SALARY_FBX[ ID %in% xv_FBX1_players_id]
  FBX1 <- FBX1[o( -SALARY ),1:3]
  XSUM <- sum(FBX1[,SALARY])
  XMAKS <- 475 - XSUM
  FBX1 <- rbind(FBX1,data.table( t(c( PLAYER=0,ID=XMAKS,SALARY=XSUM )) ))
  FA_MAX <- as.numeric(FBX1[,SALARY]) + (unlist(FBX1[14,2]) - 15)
  FBX1 <- data.table(FBX1,FA_MAX)
  FBX1
}
################################################################################
F_FBX3 <- function() {
  xs_jutro <- Sys.Date()+1
  XU <- 'https://basketball.fantasysports.yahoo.com/nba/39925/10' #Z MYTEAM
  xu <- paste0(XU,'/team?&date=',xs_jutro,'&stat1=S&stat2=D')
  xrh <- read_html(xu)
  xs_table_a_class <- '.Table-px-sm a.Nowrap'
  xl_fbx3_players <- html_nodes(xrh,xs_table_a_class) %>% html_attrs()
  xv_fbx3_players <- sapply(xl_fbx3_players, function(x) x['href'])
  xv_fbx3_players_id <- F_GET_PLAYER_ID(xv_fbx3_players)
  FBX3 <- SALARY_FBX[ ID %in% xv_fbx3_players_id]
  FBX3 <- FBX3[o( -SALARY ),1:3]
  XSUM <- sum(FBX3[,SALARY])
  XMAKS <- 475 - XSUM
  FBX3 <- rbind(FBX3,data.table( t(c( PLAYER=0,ID=XMAKS,SALARY=XSUM )) ))
  FA_MAX <- as.numeric(FBX3[,SALARY]) + (unlist(FBX3[14,2]) - 15)
  FBX3 <- data.table(FBX3,FA_MAX)
  FBX3
}
################################################################################
F_FUNCTION_DUMP <- function() {
  #XS_FILE_NAME='lsf_str.r'
  dump(lsf.str(),file=lsf_str.r)
  XRL <- trimws(readLines( lsf_str.r ))
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
  writeLines(ZV,lsf_str.r)
  ################################################################################
  ################################################################################
  FUNCTION_NAMES <- trimws(SUB(GREP( ZV,' <- function' ),1,'<-'))
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
  fwrite(ZT,'LSF_STR.CSV')
  ZT
}
################################################################################
F_GET_OTODOM_PAGES_URL <- function( X_PRICE_LOW,X_PRICE_HIGH,X_TRANSACTION ='sprzedaz'){
  ZU <- 	'https://www.otodom.pl/' +
    X_TRANSACTION +
    '/?search%5Bfilter_float_price%3Afrom%5D=' +
    X_PRICE_LOW +
    '&search%5Bfilter_float_price%3Ato%5D=' +
    X_PRICE_HIGH +
    '&search%5Border%5D=filter_float_price%3Aasc&nrAdsPerPage=72'
  return( ZU )
}
################################################################################
F_GET_PLAYER_ID <- function(X) {
  as.numeric(str_sub(X,-4,-1))
}
################################################################################
F_NORM <- function(XDF){
  XDF <-  clean_names( XDF ,case = "all_caps")
  for (i in which(sapply(XDF, class) == "character")) {
    XDF[[i]] = stri_trans_toupper(trimws(XDF[[i]]))
  }
  ZDT <- as.data.table(XDF)
  ZDT <- setnames( ZDT, stri_trans_toupper( colnames( ZDT ) ) )
  return(ZDT)
}
################################################################################
F_PARSE_BOXSCORE_0NE_GAME <- function( XU ) {
  XL_ALL_TABLES <- unique(UT( XU ,T))
  XLEN  <- length(XL_ALL_TABLES)
  XT_HOME <- F_PARSE_BOXSCORE_0NE_TEAM(XL_ALL_TABLES[[XLEN-1]])
  XT_AWAY <- F_PARSE_BOXSCORE_0NE_TEAM(XL_ALL_TABLES[[XLEN-2]])
  XROW_HOME <- nrow(XT_HOME)
  XROW_AWAY <- nrow(XT_AWAY)
  XV_HOME_TOT <- XT_HOME[XROW_HOME]
  XV_AWAY_TOT <- XT_AWAY[XROW_AWAY]
  XT_ONE_GAME <- rbind( ADD_ROW(XT_HOME[-XROW_HOME],' - ' ) ,XT_AWAY[-XROW_AWAY] )
  XT_ONE_GAME_0  <- ADD_ROW(rbind(XV_HOME_TOT,XV_AWAY_TOT),' - ')
  XT_ONE_GAME <- rbind(XT_ONE_GAME_0,XT_ONE_GAME)
  XT_ONE_GAME <- ADD_ROW(XT_ONE_GAME,' *** ')
  XT_ONE_GAME <- ADD_ROW(XT_ONE_GAME,' *** ')
  return( XT_ONE_GAME )
}
################################################################################
F_PARSE_BOXSCORE_0NE_TEAM <- function(XT0) {
  XT <- copy( XT0 )
  setnames(  XT , unlist(XT[2,]))
  XT[Player=='Totals',Player:=XT[1,1]]
  XT <- F_NORM(XT[-( 1:2 )])
  XT <- return(remove_empty(XT[!POS %like% 'DNP',-2]))
  return(XT)
}
################################################################################
F_PARSE_BOXSCORE_0NE_TEAM_0 <- function(XT0) {
  XT <- copy( XT0 )
  setnames(  XT , unlist(c(XT[1,1],(XT[2,-1]))))
  XT <- F_NORM(XT[- (1:2) ])
  XT <- remove_empty(XT[!POS %like% 'DNP',-2])
  return( XT )
}
################################################################################
F_PARSE_NBC_BOXSCORE <- function() {
  XU_ALL_GAME <- 'https://scores.nbcsports.com/nba/scoreboard.asp'
  XUA <- URL_ATR( XU_ALL_GAME)
  XV_GAMES_URLS <- paste0('https://scores.nbcsports.com',XUA[XH %like% 'box',XH])
  if ( XV_GAMES_URLS == 'https://scores.nbcsports.com' ) {
    BOX_SCORE  <- BOX_SCORE_OLD
  } else {
    XL_ALL_GAMES <- lapply(XV_GAMES_URLS,F_PARSE_BOXSCORE_0NE_GAME)
    BOX_SCORE <- do.call(rbind,XL_ALL_GAMES)
  }
  return(BOX_SCORE)
}
################################################################################
F_PARSE_YAHOO_0 <- function( XU ) {
  if ( all(class(XU) == c("xml_document","xml_node") ) )  { ###checking if xu is xml already
    xrh <- XU
  } else {
    xrh <- read_html(XU)
  }
  XS <- "table tr a"
  TEXT <- html_nodes(xrh,XS) %>% html_text()
  xl <- html_nodes(xrh,"table tr a") %>% html_attrs()
  URL <- sapply(xl, function(x) x['href'])
  XT <- data.table(TEXT,URL)
  XT25 <- XT[ !(TEXT %like% '@')& (URL %like% 'sports') & (URL %like% 'yahoo') & !(URL %like% 'news') & !(URL %like% 'status') & !(TEXT %like% ' vs ' | TEXT %like% ' @ ')]
  ID <- F_GET_PLAYER_ID(XT25[,URL])
  PLAYER <- trimws(stri_trans_toupper( unname(sapply(XT25[,URL],F_PARSE_YAHOO_PLAYER))))
  #ZT25 <- data.table( ID,PLAYER )
  XL_TABLE <- html_nodes(xrh,'table') %>% html_table()
  XT26_STATS <- XL_TABLE[[2]]
  ZT <- F_CLEAN_BASKETBALL_TABLE_YAHOO_25(XT26_STATS )
  ZT <- data.table( PLAYER , ZT ,ID)
  ZT
}
################################################################################
F_PARSE_YAHOO_0_OLD <- function( XU ) {
  if ( all(class(XU) == c("xml_document","xml_node") ) )  { ###checking if xu is xml already
    xrh <- XU
  } else {
    xrh <- read_html(XU)
  }
  XS <- "table tr a"
  TEXT <- html_nodes(xrh,XS) %>% html_text()
  xl <- html_nodes(xrh,"table tr a") %>% html_attrs()
  URL <- sapply(xl, function(x) x['href'])
  XT <- data.table(TEXT,URL)
  XT25 <- XT[ !(TEXT %like% ' pm @') & !(TEXT %like% ' am @') & (URL %like% 'sports') & (URL %like% 'yahoo') & !(URL %like% 'news') & !(URL %like% 'status') & !(TEXT %like% ' vs ' | TEXT %like% ' @ ')]
  ID <- F_GET_PLAYER_ID(XT25[,URL])
  PLAYER <- trimws(stri_trans_toupper( unname(sapply(XT25[,URL],F_PARSE_YAHOO_PLAYER))))
  ZT25 <- data.table( ID,PLAYER )
  XL_TABLE <- html_nodes(xrh,'table') %>% html_table()
  XT26_STATS <- XL_TABLE[[2]]
  ZT <- F_CLEAN_BASKETBALL_TABLE_YAHOO_25(XT26_STATS )
  ZT <- data.table( PLAYER , ZT ,ID)
  ZT
}
################################################################################
F_PARSE_YAHOO_FBX_ALL <- function(x0) {
  xl <-list()
  for(i in 0:31) {
    xu0 <- 'https://basketball.fantasysports.yahoo.com/nba/11377/players?status=A&pos=P&cut_type=33&stat1=S_AL7&myteam=0&sort=PTS&sdir=1&count='
    xu <- paste0(xu0,25*i)
    xl[[i+1]] <- F_PARSE_YAHOO_0(xu)
  }
  do.call(rbind,xl)
}
################################################################################
F_PARSE_YAHOO_PLAYER <- function( XU ) {
  XRH <- read_html(XU)
  PLAYER <- html_nodes(XRH,'span.ys-name') %>% html_text()
  PLAYER
}
################################################################################
F_ROCHE_NULL <- function( ) {
  data.table(CHEM_NAME=NA,
             FORMULA=NA,
             MOLAR_MASS=NA,
             LEGAL_STATUS=NA,
             EL_HALF_LIFE=NA,
             METABOLISM=NA,
             METABOLITES=NA,
             EXCREATION=NA,
             ROUTES_OF_ADMINISTRATION=NA,
             PROTEIN_BINDING=NA,
             BIOAVAILABILITY=NA)
}
################################################################################
F_TEST <- function( ... ) {
  XL <- list(...)
  XL
}
################################################################################
F_TEST_FUNCTION <- function(  ) {
  X_TIME <- SUB( Sys.time(),':',5 )
  XN <- ceiling(F_CONVERT_MIN( X_TIME )) %% 2
  if ( XN == 0 ) {
    ZT_HTML  <- ZT[1:5,1:2]
  } else {
    ZT_HTML  <- ZT[6:25,1:2]
  }
  return(ZT_HTML)
}
################################################################################
fa <- function(xu) {
  if ( all(class(xu) == c("xml_document","xml_node"))) {
    xrh <- xu
  } else {
    xrh <- read_html(xu)
  }
  xl_attrs_all <- html_nodes(xrh,'a') %>% html_attrs()
  xv_text <- html_nodes(xrh,'a') %>% html_text()
  xv_attrs <- unique(unlist(lapply(xl_attrs_all,names)))
  #ZT <- cdf(c('text',xv_attrs))
  x_len <- length(xl_attrs_all)
  ZT <- as.data.frame(t(unlist(xl_attrs_all[[1]])))
  for ( i in 2:x_len ) {
    ZT <- bind_rows( ZT , as.data.frame(t(unlist(xl_attrs_all[[i]]))) )
  }
  ZT <- data.frame(text=xv_text,ZT)
  ZT
}
################################################################################
FANTASY_AVG_LAST7_URL <- function(XN) {
  'https://basketball.fantasysports.yahoo.com/nba/' + XN + '/players?status=ALL&pos=P&cut_type=33&stat1=S_AL7&myteam=0&sort=AR&sdir=1&count='
}
################################################################################
FANTASY_AVG_URL_ALL <- function(XN,i) {
  'https://basketball.fantasysports.yahoo.com/nba/' + XN + '/players?status=ALL&pos=P&cut_type=33&stat1=S_AS_2019&myteam=0&sort=AR&sdir=1&count=' + 25*i
}
################################################################################
FANTASY_AVG_URL_LAST_14 <- function(XN,i) {
  'https://basketball.fantasysports.yahoo.com/nba/' + XN + '/players?status=ALL&pos=P&cut_type=33&stat1=S_AL14&myteam=0&sort=AR&sdir=1&count='  + 25*i
}
################################################################################
FANTASY_AVG_URL_LAST_7 <- function(XN,i) {
  'https://basketball.fantasysports.yahoo.com/nba/' + XN + '/players?status=ALL&pos=P&cut_type=33&stat1=S_AL7&myteam=0&sort=AR&sdir=1&count=' + 25*i
}
################################################################################
FANTASY_AVG_URL_OPP <- function(XN,i) {
  'https://basketball.fantasysports.yahoo.com/nba/' + XN + '/players?status=ALL&pos=P&cut_type=33&stat1=O_O&myteam=0&sort=AR&sdir=1&count=' + 25*i
}
################################################################################
FBX <- function() {
  FBX1 <<- F_FBX1()
  FBX3 <<- F_FBX3()
  list(FBX1,FBX3)
}
################################################################################
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
################################################################################
FIND_NOT_JOIN_OLD <- function( XT1,XT2,XC1,XC2 ) {
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
  as.data.table(rbind(YT1,YT2))
}
################################################################################
flike <- function(XV,XS) {
  XV[grepl(XS, XV, ignore.case=TRUE)]
}
################################################################################
FP_POLICZ_SREDNIE_FBX <- function( XN_LEAGUE_ID ) {
  XU <- 'https://basketball.fantasysports.yahoo.com/nba/' + XN_LEAGUE_ID + '/standings?opt_out=1'
  XRH <- read_html(XU)
  XL_HN <- html_nodes(XRH,'table') %>% html_table()
  XT_STANDINGS_TOTAL  <- F_CLEAN_BASKETBALL_TABLE_YAHOO(XL_HN[[2]])
  XT_STANDINGS_GP  <- F_CLEAN_BASKETBALL_TABLE_YAHOO(XL_HN[[3]])
  XT_STANDINGS_AVG <- AS_NUM(merge( XT_STANDINGS_GP[,.( TEAM,GP )], XT_STANDINGS_TOTAL[,.( TEAM,TOTAL )] ))
  XT_STANDINGS_AVG[,AVG:=round(TOTAL/GP,2)]
  ZT <- XT_STANDINGS_AVG[o( -AVG )]
  return( ZT )
}
################################################################################
FP_POLICZ_SREDNIE_FBX1 <- function( XN_LEAGUE_ID ) {
  XU <- 'https://basketball.fantasysports.yahoo.com/nba/' + XN_LEAGUE_ID + '/standings?opt_out=1'
  XRH <- read_html(XU)
  XL_HN <- html_nodes(XRH,'table') %>% html_table()
  XT_STANDINGS_TOTAL  <- F_CLEAN_BASKETBALL_TABLE_YAHOO(XL_HN[[2]])
  XT_STANDINGS_GP  <- F_CLEAN_BASKETBALL_TABLE_YAHOO(XL_HN[[3]])
  XT_STANDINGS_AVG <- merge( XT_STANDINGS_GP[,.( TEAM,GP )],
                             XT_STANDINGS_TOTAL[,.( TEAM,TOTAL )] )
  XT_STANDINGS_AVG[,AVG:=round(TOTAL/GP,2)]
  ZT <- XT_STANDINGS_AVG[o( -AVG )]
  return( ZT )
}
################################################################################
FSAL <- function(XS) {
  SALARY_FBX[INI==XS]
}
################################################################################
ftest <- function(...) {
  list(...)
}
################################################################################
ftest2 <- function(...) {
  XL <- list(...)
  lapply(XL,`[`)
}
################################################################################
ftest3 <- function(...) { #weird
  XL <- list(...)
  as.list(XL)
}
################################################################################
ftest4 <- function(...) { #weird
  XL <- list(...)
  ZL <- list()
  for (i in 1: length(XL) ) {
    ZL[i] <- XL[i]
  }
  ZL
}
################################################################################
fun <- function (x, ...)
  UseMethod("mean")
################################################################################
FUN_DUMP <- function() {
  LSF_STR <- lsf.str()
  dump(LSF_STR,file='lsf_str.r')
  XRL <- trimws(readLines( 'lsf_str.r' ))
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
  writeLines(ZV,'lsf_str.r')
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
################################################################################
FXV <- function(XS) {
  paste0(SPL(XS,''),collapse = '')
}
################################################################################
gct <- function ()
{
  gctorture(FALSE)
  gc()
}
################################################################################
GET_SQL <- function(FILEPATH='select.sql'){
  con = file(FILEPATH, "r")
  sql.string <- ""
  while (TRUE){
    line <- readLines(con, n = 1)
    if ( length(line) == 0 ){
      break
    }
    line <- gsub("\\t", " ", line)
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    sql.string <- paste(sql.string, line)
  }
  close(con)
  return(sql.string)
}
################################################################################
GREP <- function(XV,XS) {
  grep(XS, XV,ignore.case = T,value = TRUE)
}
################################################################################
HAAS <- function(XU,ALL=FALSE) {
  XRH <- READ_HTML(XU)
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
  if (!ALL) {
    ZT <- SEL_COL( ZT,'XI|TEXT|HREF|CLASS|ID')
    if ( ncol( ZT )==5 ) {
      colnames(ZT) <- c('XI','XT','XH','XC','XID')
      ZT[ , `:=`(XN=.N) , by = .(XT,XH) ]
    } else {
      ZT <- SEL_COL( ZT,'XI|TEXT|HREF')
      colnames(ZT) <- c('XI','XT','XH')
      ZT[ , `:=`(XN=.N) , by = .(XT,XH) ]
    }
  }
  closeAllConnections()
  ZT <- as.data.table(ZT)
  ZT
}
################################################################################
HAS <- function( XU,ATR='a',ALL=FALSE) {
  if ( ATR=='a' ) {
    ZT <- HAAS( XU,ALL )
  } else {
    XRH <- READ_HTML(XU)
    XL_ATTRS_ALL <- html_nodes(XRH,ATR) %>% html_attrs()
    XV_ATTRS <- unique(unlist(lapply(XL_ATTRS_ALL,names)))
    X_LEN <- length(XL_ATTRS_ALL)
    ZT <- as.data.frame(t(unlist(XL_ATTRS_ALL[[1]])))
    if ( X_LEN>1 ) {
      for ( i in 2:X_LEN ) {
        ZT <- bind_rows( ZT , as.data.frame(t(unlist(XL_ATTRS_ALL[[i]]))) )
      }
    }
    ZT <- as.data.table(setnames(ZT,gsub("\\bXI\\b", "XI_ORG",colnames(ZT))))
    ZT[, XI := .I]
    colnames(ZT) <- stri_trans_toupper(colnames(ZT))
    if (!ALL) {
      ZT <- SEL_COL( ZT,'XI|CLASS|ID' )
      if ( ncol( ZT )==4 ) {
        colnames(ZT) <- c('XI','XC','XS','XID')
      }}
  }
  ZT
}
################################################################################
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
################################################################################
ini <- function(XS,XN=1) {
  if (any( grepl( ' ',XS ) )) {
    xs <- strsplit(XS, " ")
  } else {
    xs <- strsplit(XS, "_")
  }
  sapply(xs, function(x) toupper(paste(substring(x, 1, XN), collapse = "")))
}
################################################################################
IS_BLANK <- function(X, false.triggers=FALSE){
  if(is.function(X)) return(FALSE) # Some of the tests below trigger
  if(length(X) > 0) return(FALSE) # Some of the tests below trigger
  # warnings when used on functions
  return(
    is.null(X) || # Actually this line is unnecessary since
      length(X) == 0 || # length(NULL) = 0, but I like to be clear
      all(is.na(X)) ||
      all(X=="") ||
      (false.triggers && all(!X)) ||
      grepl("^\\s*$",X)
  )
}
################################################################################
IS_NA <- function(X) {
  suppressWarnings( is.na( X ))
}
################################################################################
IS_NOT_NUM_XV <- function(XV) {
  XV <- na.omit(XV_TEXT_NUMBERS)
  (all( !suppressWarnings( is.na(as.numeric(as.character(XV)))  )))
}
################################################################################
IS_NUM <- function( XV ) {
  (all( !suppressWarnings( is.na(as.numeric(as.character(na.omit(XV))))  )))
}
################################################################################
IS_NUM_XV <- function(XV) {
  XV <- na.omit(XV)
  (all( !suppressWarnings( is.na(as.numeric(as.character(XV)))  )))
}
################################################################################
IS_TABLE <- function( XT ) {
  if ( is.matrix(XT) | is.data.frame(XT) | is.data.table(XT) ) {
    TRUE
  } else {
    FALSE
  }}
################################################################################
JOIN_MAP_TABLE <- function(SHP_FILE_NAME,XT,SHP_COL,TAB_COL) {
  if ( str_to_upper(SUB( SHP_FILE_NAME,-4,-1 ))!= ".SHP" ) { SHP_FILE_NAME <- SHP_FILE_NAME + '.SHP'}
  XT_SHP <- read_sf(SHP_FILE_NAME)
  XT_GEO <- XT_SHP %>% select(NAME = SHP_COL, geometry)
  Encoding(XT_GEO$NAME)  <- 'UTF-8'
  ZT <- left_join(XT_GEO, XT, by = c('NAME' = TAB_COL))
  ZT
}
################################################################################
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
################################################################################
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
################################################################################
LIST_FILES <- function(XS,X_PATH) {
  if (missing(X_PATH)) {X_PATH <- getwd()}
  if (X_PATH=='ALL' || X_PATH==0) {
    X_PATH <- 'C:/'
    XV_ALL_FILES <- list.files(X_PATH,full.names = TRUE,ignore.case = TRUE, recursive = TRUE,include.dirs=TRUE)
  } else {
    XV_ALL_FILES <- list.files(X_PATH,full.names = TRUE,ignore.case = TRUE, recursive = TRUE)
  }
  GREP( XV_ALL_FILES,XS )
}
################################################################################
LIST_FILES_OLD <- function(XS,XB=FALSE) {
  #XB - if true then all files on C
  if (XB) {
    XV_ALL_FILES <-   list.files('C:/',full.names = TRUE,ignore.case = TRUE, recursive = TRUE,include.dirs=TRUE)
  } else {
    XV_ALL_FILES <- list.files(getwd(),full.names = TRUE,ignore.case = TRUE, recursive = TRUE)
  }
  XV_ALL_FILES[stri_detect_regex(XV_ALL_FILES,XS)]
}
################################################################################
LOKATA <- function(X_AMOUNT,X_PCT,X_MONTH =12 ) {
  0.8 * X_AMOUNT *  (X_PCT/100)  * (X_MONTH/12)
}
################################################################################
lower <- function (str, locale = NULL)
{
  .Call(C_stri_trans_tolower, str, locale)
}
################################################################################
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
################################################################################
MIN_AS_NUMBER <- function(XV) {
  xv <- trimws(XV)
  ZV <- XV
  XV <- na.omit(XV)
  if ( IS_NUM(SUB( XV,1,2 )) & IS_NUM(SUB( XV,4,5 )) & all(SUB( XV,3,3 )==':') ) {
    ZV <- 	sapply( ZV, CONVERT_MIN_TO_NUMBER)
  }
  ZV
}
################################################################################
MPG2SEC <- function(XS) {
  XV_SPL <- SPL( XS,':' )
  XMIN <- as.numeric(XV_SPL[1])
  XSEC <- as.numeric(XV_SPL[2])/60
  return( ROUND( XMIN+XSEC ) )
}
################################################################################
MY_TO_SHINY <- function(X_APP_ORG_FILE='APP.R',X_APP_MY_FILE='APP.R') {
  XRL <- suppressWarnings( TRY_CATCH( readLines(X_APP_ORG_FILE)))
  if ( !all(is.na(XRL)) ) {
    XRL <- trimws(XRL)
    XRL <- XRL[XRL!='']
    XT  <- XT_SHINY[NCHAR>3]
    for ( i in 1:nrow( XT ) ) {
      XRL <- 	gsub( "\\b" + XT[i,F_NAME_MY] + "\\b" ,XT[i,F_NAME_ORG],XRL )
    }
    WL( XRL,X_APP_MY_FILE )
  } else {
    return(NULL)
  }
}
################################################################################
NUM_TO_DATE <- function( XV ) {
  XL <- SPL( XV,'\\.' )
  ZV <- vector(  )
  for ( i in seq_along(XL) ) {
    X_DAY <- XL[[i]][2]
    X_MON <- XL[[i]][1]
    if ( nchar( X_DAY ) == 1 ) {X_DAY <- 0+X_DAY}
    if ( nchar( X_MON ) == 1 ) {X_MON <- 0+X_MON}
    ZV[i] <- X_MON + '-' + X_DAY
  }
  ZV
}
################################################################################
o <- function (..., na.last = TRUE, decreasing = FALSE, method = c("auto",
                                                                   "shell", "radix"))
{
  z <- list(...)
  decreasing <- as.logical(decreasing)
  if (length(z) == 1L && is.numeric(x <- z[[1L]]) && !is.object(x) &&
      length(x) > 0) {
    if (.Internal(sorted_fpass(x, decreasing, na.last)))
      return(seq_along(x))
  }
  method <- match.arg(method)
  if (any(vapply(z, is.object, logical(1L)))) {
    z <- lapply(z, function(x) if (is.object(x))
      as.vector(xtfrm(x))
      else x)
    return(do.call("order", c(z, list(na.last = na.last,
                                      decreasing = decreasing, method = method))))
  }
  if (method == "auto") {
    useRadix <- all(vapply(z, function(x) {
      (is.numeric(x) || is.factor(x) || is.logical(x)) &&
        is.integer(length(x))
    }, logical(1L)))
    method <- if (useRadix)
      "radix"
    else "shell"
  }
  if (method != "radix" && !is.na(na.last)) {
    return(.Internal(order(na.last, decreasing, ...)))
  }
  if (method == "radix") {
    decreasing <- rep_len(as.logical(decreasing), length(z))
    return(.Internal(radixsort(na.last, decreasing, FALSE,
                               TRUE, ...)))
  }
  if (any(diff((l.z <- lengths(z)) != 0L)))
    stop("argument lengths differ")
  na <- vapply(z, is.na, rep.int(NA, l.z[1L]))
  ok <- if (is.matrix(na))
    rowSums(na) == 0L
  else !any(na)
  if (all(!ok))
    return(integer())
  z[[1L]][!ok] <- NA
  ans <- do.call("order", c(z, list(decreasing = decreasing)))
  ans[ok[ans]]
}
################################################################################
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
################################################################################
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
################################################################################
PLOT <- function(XV,FUN,...) {
  plot( XV, FUN(XV),asp = 1)
  abline(v=SEQ_RANGE( XV ), col="lightgray", lty="dotted")
  abline(h=SEQ_RANGE( XV ), col="lightgray", lty="dotted")
}
################################################################################
PLOT_VEC <- function(XV,YV,...) {
  plot( XV, YV,asp = 1)
  abline(v=SEQ_RANGE( XV ), col="lightgray", lty="dotted")
  abline(h=SEQ_RANGE( XV ), col="lightgray", lty="dotted")
}
################################################################################
PLUS_AS_NUMBER <- function(XV) {
  #XV <- unname(unlist(XT[,'+/-']))
  XV <- trimws(XV)
  ZV <- XV
  XV <- na.omit(XV)
  if ( all(SUB( XV,1,1 )=='+' | SUB( XV,1,1 )=='-') & ( IS_NUM(SUB( XV,2,-1 ))) ) {
    ZV <- 	as.numeric(gsub( '\\+','',ZV ))
  }
  ZV
}
################################################################################
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
################################################################################
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
      }}#</IF>#
    Z_MAP <- Z_MAP + geom_text(data = STATE_coords, aes(X, Y, label = Y_TEXT), colour = "white")
  }
  Z_MAP
}
################################################################################
POWER_TRANS <- function( XV, Y ) {
  (XV^Y - 1)/Y
}
################################################################################
q <- function (x, ...)
  UseMethod("quantile")
################################################################################
Q <- function(  ) {
  fn$sqldf(GET_SQL(  ))
}
################################################################################
qu <- function (x, ...)
  UseMethod("quantile")
################################################################################
QU <- function( XV,XN=3 ){
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
################################################################################
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
  return(ZV)
}
################################################################################
RBIND <- function(XT1,XT2) {
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
################################################################################
READ_HTML <- function( XU,READ_HTML_VER=T) {
  if ( all(class(XU) == c("xml_document","xml_node"))) {
    Z <- XU
  } else {
    if ( READ_HTML_VER ) {
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
      Z <-  read_html( XU )
    }}
  return( Z )
}
################################################################################
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
################################################################################
READ_HTML_2 <- function( XU,AS_CHAR=FALSE ) {
  XRH <- (as.character(read_html( XU)))
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
  writeLines(XRL,'XRH.HTML',useBytes = TRUE)
  if( AS_CHAR ) {
    Z <- readLines('XRH.HTML',encoding = 'UTF-8')
  } else {
    Z <- read_html('XRH.HTML')
  }
  return( Z )
}
################################################################################
READ_HTML_BR <- function( XU ) {
  XRH <- UP(as.character(read_html( XU )))
  XRH <- unlist(strsplit(XRH, "(?<=>)", perl=TRUE))
  XRH <-   unlist(strsplit(XRH, "<(?=<)", perl = T))
  writeLines(XRH,'XRH.HTML',useBytes = TRUE)
  XRL <- readLines('XRH.HTML',encoding = 'UTF-8')
  XRL <- XRL[XRL!='']
  writeLines(XRL,'XRH.HTML',useBytes = TRUE)
  XRL <- gsub( '<!-- ','<',  XRL)
  writeLines(XRL,'XRH2.HTML',useBytes = TRUE)
  XRL <- gsub( '<!--','<',  XRL)
  XRL <- gsub( ' -->','>',  XRL)
  XRL <- gsub( '-->','>',  XRL)
  writeLines(XRL,'XRH3.HTML',useBytes = TRUE)
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
  writeLines(XRL,'XRH4.HTML',useBytes = TRUE)
  read_html('XRH4.HTML')
}
################################################################################
REMOVE_COLNAMES_AS_ROWS <- function(XT) {
  XCOLNAMES <- colnames(XT)
  for ( i in 1:nrow( XT ) ) {
    if	(SIMILAR_VECTORS(XCOLNAMES,unname(unlist( XT[i] )))) {
      XT <- XT[-XV_DUP_ROWS_NUMBER]
    }
  }
  XT
}
################################################################################
RITE_HTML <- function( XU,X_FILE='TEMP.HTML') {
  XRH <- (as.character(READ_HTML( XU )))
  XRH <- unlist(strsplit(XRH, "(?<=>)", perl=TRUE))
  XRH <- unlist(strsplit(XRH, "<(?=<)", perl = T))
  XRH <- gsub("[\n\r\t\v\f]", "",XRH)
  XRH <- XRH[XRH!='']
  writeLines(XRH,X_FILE,useBytes = TRUE)
}
################################################################################
RL <- function (con = stdin(), n = -1L, ok = TRUE, warn = TRUE, encoding = "unknown",
                skipNul = FALSE)
{
  if (is.character(con)) {
    con <- file(con, "r")
    on.exit(close(con))
  }
  .Internal(readLines(con, n, ok, warn, encoding, skipNul))
}
################################################################################
ROUND <- function(X,XN=2) {
  if (is.vector(X)) {
    round(X,XN)
  } else {
    for(i in which(sapply(X, class) == "numeric")) {X[[i]] = round(X[[i]],XN) }
    X
  }
}
################################################################################
ROW_COLNAMES <- function(XT){
  colnames( XT )  <- unname(unlist(XT[1,]))
  XT <- XT[-1,]
  XT
}
################################################################################
RUN_APP <- function(XS_PATH) {
  XAPP <- XS_PATH + '\\XAPP.R'
  APP <- XS_PATH + '\\APP.R'
  XAPP_TO_APP( XAPP,APP )
  APP_TO_XAPP( APP,XAPP )
  runApp(XS_PATH)
}
################################################################################
SAME_LENGTH <- function( ... ) {
  XL <- list(...)
  all(diff(sapply(XL,length))==0)
}
################################################################################
SEL_COL <- function(XT,XS) {
  as.data.table(select(XT,matches(XS)))
}
################################################################################
SEQ_RANGE <- function(XV, STEP=1 ) {
  seq( floor(min( XV )) , ceiling( max( XV ) ),STEP )
}
################################################################################
server <- function(input, output, session) {
  ###___X_ONLY_WYBRANY_KRAJ___###
  X_ONLY_WYBRANY_KRAJ <- reactive({
    ZT[ZT$COUNTRY_REGION == input$X_WYBRANY_KRAJ, ]
  })
  ###___output$X_Lista_Odcinkow___###
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
}
################################################################################
server0 <- function(input, output, session) {
  library( data.table )
  output$table <- renderDataTable({
    invalidateLater(60000, session)
    XT <- fread( 'PTE.csv' )
    return( XT )
  })
}
################################################################################
SHINY_TO_MY <- function(APP='APP.R',XAPP='XAPP.R') {
  XRL <- readLines(APP)
  XRL <- trimws(XRL)
  XRL <- XRL[XRL!='']
  XRL <- XRL[SUB( XRL,1,1 )!='#']
  XT  <- XT_SHINY[NCHAR>3]
  for ( i in 1:nrow( XT ) ) {
    XRL <- 	gsub( "\\b" + XT[i,F_NAME_ORG] + "\\b" ,XT[i,F_NAME_MY],XRL )
  }
  WL( XRL,XAPP )
}
################################################################################
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
################################################################################
SIMPLIFY_DOM <- function(XV,DOM) {
  DOM_ORG <- "<" + DOM + ".*>"
  DOM_SHORT <- "<" + DOM + ">"
  trimws(gsub(DOM_ORG, DOM_SHORT, XV, ignore.case = TRUE))
}
################################################################################
SPL <- function(XV,XS=' ') {
  if( length(XV)==1 ) {
    trimws(unlist( str_split(unlist(XV),XS) ))
  } else {
    lapply(XV, function( X )	trimws(unlist( str_split(unlist(X),XS) )))
  }
}
################################################################################
SPL_OLD <- function(X,XS=' ') {
  trimws(unlist( str_split(unlist(X),XS) ))
}
################################################################################
SPL_WHICH <- function(XV,XS=' ',XN=1) {
  XL <- SPL(XV,XS)
  sapply(XL, function( X ) X[[XN]])
}
################################################################################
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
################################################################################
SUB <- function(XS,X_START,X_END) {
  unname(sapply(XS,function(X) SUB0(X,X_START,X_END)))
}
################################################################################
SUB_LIKE <- function( XV,XS ) {
  XV[which(XV %like% XS)] <- XS
  XV
}
################################################################################
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
################################################################################
TRY_CATCH <- function(X) {
  TRY_CATCH_0 <- tryCatch( X , error = function(e) NULL)
  if ( IS_BLANK( TRY_CATCH_0 ) ) {
    Z <- NA
  } else {
    Z <- TRY_CATCH_0
  }
  return( Z )
}
################################################################################
TRY_CATCH_ERROR <- function(X) {
  tryCatch(X, error = function(XER) list(OK=NULL,ERROR=as.character( XER )))
}
################################################################################
UA <- function(XU,xall=FALSE) {
  if ( all(class(XU) == c("xml_document","xml_node"))) {
    XRH <- XU
  } else {
    XRH <- READ_HTML(XU)
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
  if (!xall) {
    ZT <- ZT[,c('XI','TEXT','HREF')]
    colnames(ZT) <- c('XI','XT','XH')
    ZT[ , `:=`(XC=.N) , by = .(XT,XH) ]
  }
  closeAllConnections()
  as.data.table(ZT)
}
################################################################################
UA_0 <- function(XU,xall=FALSE) {
  if ( all(class(XU) == c("xml_document","xml_node"))) {
    XRH <- XU
  } else {
    XRH <- READ_HTML(XU)
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
  if (!xall) {
    ZT <- ZT[,c('XI','TEXT','HREF')]
    colnames(ZT) <- c('XI','XT','XH')
    ZT[ , `:=`(XC=.N) , by = .(XT,XH) ]
  }
  closeAllConnections()
  as.data.table(ZT)
}
################################################################################
UA_NODES <- function(XU,XS_ATR = 'table') {
  XRH <- READ_HTML(XU)
  XL_ATTRS_ALL <- html_nodes(XRH,XS_ATR) %>% html_attrs()
  XV_ATTRS <- unique(unlist(lapply(XL_ATTRS_ALL,names)))
  X_LEN <- length(XL_ATTRS_ALL)
  ZT <- as.data.frame(t(unlist(XL_ATTRS_ALL[[1]])))
  if ( X_LEN>1 ) {
    for ( i in 2:X_LEN ) {
      ZT <- bind_rows( ZT , as.data.frame(t(unlist(XL_ATTRS_ALL[[i]]))) )
    }
  }
  ZT <- setnames(ZT,gsub("\\bXI\\b", "XI_ORG",colnames(ZT)))
  #ZT[, XI := .I]
  #colnames(ZT) <- stri_trans_toupper(colnames(ZT))
  closeAllConnections()
  as.data.table(ZT)
}
################################################################################
UA_OLD <- function(XU,xall=FALSE) {
  if ( all(class(XU) == c("xml_document","xml_node"))) {
    XRH <- XU
  } else {
    XRH <- read_html(XU)
  }
  xl_attrs_all <- html_nodes(XRH,'a') %>% html_attrs()
  xv_text <- stri_trans_toupper(html_nodes(XRH,'a') %>% html_text())
  xv_attrs <- unique(unlist(lapply(xl_attrs_all,names)))
  x_len <- length(xl_attrs_all)
  ZT <- as.data.frame(t(unlist(xl_attrs_all[[1]])))
  for ( i in 2:x_len ) {
    ZT <- bind_rows( ZT , as.data.frame(t(unlist(xl_attrs_all[[i]]))) )
  }
  ZT <- data.table(text=xv_text,ZT)
  ZT <- setnames(ZT,gsub("\\bXI\\b", "XI_ORG",colnames(ZT)))
  ZT[, XI := .I]
  colnames(ZT) <- stri_trans_toupper(colnames(ZT))
  if (!xall) {
    ZT <- ZT[,c('XI','TEXT','HREF')]
    colnames(ZT) <- c('XI','XT','XH')
    ZT[ , `:=`(XC=.N) , by = .(XT,XH) ]
  }
  closeAllConnections()
  as.data.table(ZT)
}
################################################################################
UNCAMEL <- function(x) {
  toupper(gsub("([a-z0-9])([A-Z])", "\\1_\\2", gsub("(.)([A-Z][a-z]+)", "\\1_\\2", x)))
}
################################################################################
UP <- function (str, locale = NULL)
{
  .Call(C_stri_trans_toupper, str, locale)
}
################################################################################
UPD_ROW <- function( XT , XN_ROW_NUM ,XV ) {
  XT[XN_ROW_NUM, (colnames(XT)):= as.list(XV)]
}
################################################################################
URL_ATR <- function(XU,xall=FALSE) {
  if ( all(class(XU) == c("xml_document","xml_node"))) {
    XRH <- XU
  } else {
    XRH <- read_html(XU)
  }
  xl_attrs_all <- html_nodes(XRH,'a') %>% html_attrs()
  xv_text <- stri_trans_toupper(html_nodes(XRH,'a') %>% html_text())
  xv_attrs <- unique(unlist(lapply(xl_attrs_all,names)))
  x_len <- length(xl_attrs_all)
  ZT <- as.data.frame(t(unlist(xl_attrs_all[[1]])))
  for ( i in 2:x_len ) {
    ZT <- bind_rows( ZT , as.data.frame(t(unlist(xl_attrs_all[[i]]))) )
  }
  ZT <- data.table(text=xv_text,ZT)
  ZT[, ID := .I]
  colnames(ZT) <- stri_trans_toupper(colnames(ZT))
  if (!xall) {
    ZT <- ZT[,c('ID','TEXT','HREF')]
    colnames(ZT) <- c('XI','XT','XH')
    ZT[ , `:=`(XC=.N) , by = .(XT,XH) ]
  }
  as.data.table(ZT)
}
################################################################################
URL_TAB <- function(XU,xall=FALSE) {
  if ( all(class(XU) == c("xml_document","xml_node"))) {
    XRH <- XU
  } else {
    XRH <- read_html(XU)
  }
  ZL <- html_nodes(XRH, 'table') %>% html_table(fill = TRUE)
  ZL <- lapply(ZL, as.data.table)
  if (!xall) {
    XNROW <- sapply(ZL, nrow)
    XW <- min(which(XNROW==max(XNROW)))
    Z <- ZL[[XW]]
  } else {
    Z <- ZL
  }
  Z
}
################################################################################
UT <- function(XU,xall=FALSE) {
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
  Z
}
################################################################################
VEC_BEL <- function(XV1,XV2) {
  if (length(XV1)==length(XV1)) {
    Z <- TRUE
    for ( i in 1:length(XV1) ) {
      if (!(grepl( XV1[i],XV2[i] ) | grepl( XV2[i],XV1[i] ) )) {
        Z <- FALSE
        break
      }
    }
  } else {
    Z <- 'vectors different length'
  }
  Z
}
################################################################################
WIKI <- function( XS ) {
  paste0('https://en.wikipedia.org/wiki/',XS)
}
################################################################################
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
################################################################################
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
################################################################################
WL <- function( XV,X_FILE_NAME = 'XWL.R' ) {
  writeLines(XV,X_FILE_NAME,useBytes = TRUE)
}
################################################################################
word <- function(C, k) paste(rep.int(C, k), collapse = "")
################################################################################
WRITE_HTML <- function( XU,X_FILE='TEMP.HTML') {
  XRH <- (as.character(READ_HTML( XU )))
  XRH <- unlist(strsplit(XRH, "(?<=>)", perl=TRUE))
  XRH <- unlist(strsplit(XRH, "<(?=<)", perl = T))
  XRH <- gsub("[\n\r\t\v\f]", "",XRH)
  XRH <- XRH[XRH!='']
  writeLines(XRH,X_FILE,useBytes = TRUE)
}
################################################################################
WRITE_HTML_ORG <- function( XU,X_FILE='TEMP.HTML') {
  XRH <- (as.character(read_html( XU )))
  XRH <- unlist(strsplit(XRH, "(?<=>)", perl=TRUE))
  XRH <- unlist(strsplit(XRH, "<(?=<)", perl = T))
  XRH <- gsub("[\n\r\t\v\f]", "",XRH)
  XRH <- XRH[XRH!='']
  writeLines(XRH,X_FILE,useBytes = TRUE)
}
################################################################################
WT <- function(XT,XS_FILE,XS_SEP=';') {
  if ( missing(XS_FILE) )
  {
    XS_FILE = deparse(substitute(XT))
    XS_FILE = paste0(XS_FILE,'.csv')
  }
  write.table(XT,XS_FILE,sep=XS_SEP,quote = FALSE,row.names = FALSE,col.names = TRUE)
}
################################################################################
XAPP_TO_APP <- function(XAPP='XAPP.R',APP='APP.R') {
  XRL <- suppressWarnings( TRY_CATCH( readLines(XAPP)))
  if ( !all(is.na(XRL)) ) {
    #XRL <- trimws(XRL)
    #XRL <- XRL[XRL!='']
    XT  <- XT_SHINY[NCHAR>3]
    for ( i in 1:nrow( XT ) ) {
      XRL <- 	gsub( "\\b" + XT[i,F_NAME_MY] + "\\b" ,XT[i,F_NAME_ORG],XRL )
    }
    WL( XRL,APP )
  } else {
    return(NULL)
  }
}
################################################################################
