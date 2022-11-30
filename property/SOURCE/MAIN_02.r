XT <- fread('SOURCE/XT_OTODOM_SPRZEDAZ_ALL.CSV',sep=';',encoding = 'UTF-8' )
XT <- XT[ !is.na(PRICE) & !is.na(AREA) ]
XT[,PRICE_PER_AREA:=ROUND(PRICE/AREA)]
XT <- XT[ (AREA > 20 & PROPERTY_TYPE == 'HOUSE') | ( PROPERTY_TYPE != 'HOUSE' )]
XT <- XT[ (AREA > 6 & PROPERTY_TYPE == 'APARTMENT') | ( PROPERTY_TYPE != 'APARTMENT' )]
XT <- XT[ (AREA > 2 & PROPERTY_TYPE == 'COMMERCIAL') | ( PROPERTY_TYPE != 'COMMERCIAL' )]
XT <- XT[ (AREA > 2 & PROPERTY_TYPE == 'WAREHOUSE') | ( PROPERTY_TYPE != 'WAREHOUSE' )]
XT <- XT[ (AREA > 30 & PROPERTY_TYPE == 'LAND') | ( PROPERTY_TYPE != 'LAND' )]
XT <- XT[ (!(AREA < 100 & PRICE > 1000000 & PROPERTY_TYPE == 'LAND')) | ( PROPERTY_TYPE != 'LAND' )]


SHP_WOJ <- read_sf('SOURCE/WOJ.shp')

SHP_POW <- read_sf('SOURCE/Powiaty.shp')
SHP_POW$ID_WOJ <- SUB(SHP_POW$JPT_KOD_JE,1,2)

XT_WOJ <- as.data.table( as.data.frame( SHP_WOJ ) )

SHP_POW <- left_join(SHP_POW, XT_WOJ[,.( WOJ=JPT_NAZWA_,JPT_KOD_JE )],
          by = c('ID_WOJ' = 'JPT_KOD_JE'))

XV_STAT  <- c( 'mean','median','sd','quantile','mode','interquartile range')
XV_STAT_SAME_NAME  <- c( 'mean','median','sd')

XV_FILL_HOUSE  <- stri_trans_tolower( c ("PRICE PER AREA","PRICE","AREA","LAND AREA","ROOMS","VOLUME" ))
XV_FILL_APARTMENT <- stri_trans_tolower( c ("PRICE PER AREA","PRICE","AREA","ROOMS","VOLUME" ))
XV_FILL_LAND <- stri_trans_tolower( c ("PRICE PER AREA","PRICE","AREA","VOLUME" ))
XV_FILL_COMMERCIAL <- stri_trans_tolower( c ("PRICE PER AREA","PRICE","AREA","VOLUME" ))
XV_FILL_GARAGE <- stri_trans_tolower( c ("PRICE PER AREA","PRICE","AREA","VOLUME" ))
XV_FILL_WAREHOUSE <- stri_trans_tolower( c ("PRICE PER AREA","PRICE","AREA","VOLUME" ))

XV_COL_VIR  <- c( 'viridis','magma','plasma','inferno','cividis')
XV_COLORS  <-  c( XV_COL_VIR,'terrain','gray')

XV_STATE <- SHP_WOJ$JPT_NAZWA_
XV_COUNTY <- SHP_POW$JPT_NAZWA_
XV_REGION_TYPE_FOR_COUNTY  <- c( 'Poland', XV_STATE )

#XT[ (!(AREA < 100 & PRICE > 1000000 & PROPERTY_TYPE == 'LAND')) | ( PROPERTY_TYPE != 'LAND' )]