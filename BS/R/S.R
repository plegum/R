#' S family functions search for matches to argument pattern.
#' 
#' B is vectorised version of B0
#' First argument - X, a character vector where matches are sought, or an object which can be coerced by as.character to a character vector.
#' Second arguments is elipsis.
#' S allow to search by all given pattern.
#' There is option to exclude pattern by using operator:`!` or `-`
#' For example:
#' XV <- names(sort(islands,decreasing = T))
#' S(XV,-'A','b') #matching B and not A.
#' S(XV,-'^A') #matching not started by A.
#' Default option is ignoring cases.
#' S return logical values
#' SV return values
#' SW return which values

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
####################################___SV___####################################
SV <- function(XV, ... ,ignore.case = T, perl = T,fixed = F,useBytes = F) {
	XL <- list(...)
	YV <- unlist(XL)
	ZV <- vector()
	YL <- list()
	for ( i in seq_along(XV) ) {
		YL[[i]] <- L()
		for ( j in seq_along( YV ) ) {
			YL[[i]][j] <-  grepl(YV[j], XV[i],ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
		}
	}
XV[unlist(lapply(YL, function( X ) all( unlist(X ))))]
}
####################################___SW___####################################
SW <- function(XV, ... ,ignore.case = T, perl = T,fixed = F,useBytes = F) {
	XL <- list(...)
	YV <- unlist( XL )
	ZV <- vector(  )
	YL <- list()
	for ( i in seq_along(XV) ) {
		YL[[i]] <- L()
		for ( j in seq_along( YV ) ) {
			YL[[i]][j] <-  grepl(YV[j], XV[i],ignore.case = ignore.case,perl = perl,fixed=fixed,useBytes=useBytes)
		}
	}
unname(which(unlist(lapply( YL,  function( X ) all( unlist(X ))))))
}