################################################################################
`+` <- function(x, y) {
if(is.character(x) || is.character(y)) {
return(stri_c(x, y))
} else {
.Primitive("+")(x, y)
}
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
if (all( !suppressWarnings( is.na(as.numeric(as.character(Z)))  ))) {
Z <- as.numeric( as.character(Z) )
} else {
Z <- X
}
return( Z )
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
CHECK_ID <- function( XT ) {
any(stri_detect_regex(colnames(XT),'^ID$'))
}
################################################################################
CLEAN_STRING <- function( XS,UPPER_CASE=TRUE ) {
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
DATA_TABLE <- function( XT , XV=c(5, 30, 50)) {
DT::datatable(XT, options = list(lengthMenu = XV, pageLength = nrow( XT )))
}
################################################################################
f.lib <- function() {
options(stringsAsFactors=FALSE)
library(stringr)
library(stringi)
require(xlsx)
library(readxl)
library(sqldf)
library(dplyr)
library(data.table)
library(rvest)
library( janitor )
library(shiny)
library(rsconnect)
#source('connections.r')
}
################################################################################
F_CONVERT_MIN <- function(XS) {
XV_SPL <- SPL( XS,':' )
XMIN <- as.numeric(XV_SPL[1])
XSEC <- as.numeric(XV_SPL[2])/60
return( ROUND( XMIN+XSEC ) )
}
################################################################################
F_FUNCTION_DUMP <- function() {
dump(lsf.str(),file=XS_FILE_NAME)
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
writeLines(na.omit(ZV[ZV!='']),XS_FILE_NAME)
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
IS_BLANK <- function(X, false.triggers=FALSE){
if(is.function(X)) return(FALSE) # Some of the tests below trigger
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
IS_NUM <- function( X ) {
sapply(X,IS_NUM_0)
}
################################################################################
IS_NUM_0 <- function( X ) {
is.numeric(AS_NUM_0( X ) )
}
################################################################################
IS_TABLE <- function( XT ) {
if ( is.matrix(XT) | is.data.frame(XT) | is.data.table(XT) ) {
TRUE
} else {
FALSE
}}
################################################################################
flike <- function(XV,XS) {
XV[grepl(XS, XV, ignore.case=TRUE)]
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
LIST_FILES <- function(XS,XB=FALSE) {
#XB - if true then all files on C
if (XB) {
XV_ALL_FILES <-   list.files('C:/',full.names = TRUE,ignore.case = TRUE, recursive = TRUE,include.dirs=TRUE)
} else {
XV_ALL_FILES <- list.files(getwd(),full.names = TRUE,ignore.case = TRUE, recursive = TRUE)
}
XV_ALL_FILES[stri_detect_regex(XV_ALL_FILES,XS)]
}
################################################################################
MPG2SEC <- function(XS) {
XV_SPL <- SPL( XS,':' )
XMIN <- as.numeric(XV_SPL[1])
XSEC <- as.numeric(XV_SPL[2])/60
return( ROUND( XMIN+XSEC ) )
}
################################################################################
o <- order
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
SPL <- function(X,XS=' ') {
trimws(unlist( str_split(unlist(X),XS) ))
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
UPD_ROW <- function( XT , XN_ROW_NUM ,XV ) {
XT[XN_ROW_NUM, (colnames(XT)):= as.list(XV)]
}
################################################################################
UT <- function(XU,xall=FALSE) {
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
WT <- function(XT,XS_FILE,XS_SEP=';') {
if ( missing(XS_FILE) )
{
XS_FILE = deparse(substitute(XT))
XS_FILE = paste0(XS_FILE,'.csv')
}
write.table(XT,XS_FILE,sep=XS_SEP,quote = FALSE,row.names = FALSE,col.names = TRUE)
}
