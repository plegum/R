#' B family functions extracts the specified substrings in the given index ranges based on numbers or characters.
#' If a character is given, the function finds its location and treats its origin as a boundary.
#' B is vectorised version of B0

#####################################___B___####################################
B <- function(XS,X_START,X_END) {
	unname(sapply(XS,function(X) B0(X,X_START,X_END)))
}
####################################___B0___####################################
B0 <- function(XS,X_START,X_END) {
	if (is.numeric(X_START) & is.numeric(X_END) ) {
		ZS <- stringi::stri_sub(XS,X_START,X_END)
	} else {
		if ( is.numeric(X_START)) {
			XN_START <- X_START
		} else {
			XN_START <- as.numeric(stringi::stri_locate_first_regex(XS,X_START)[1,2]) + 1
		}
		XS <- stringi::stri_sub(XS,XN_START,-1)
		if ( is.numeric(X_END)) {
			XN_END <- X_END
		} else {
			XN_END <- as.numeric( stringi::stri_locate_first_regex(XS,X_END)[1,1]) - 1
		}
		ZS <- stringi::stri_sub(XS,1,XN_END)
	}
	ZS
}