XT_CORONA_ALL <- fread( 'https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv' )
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