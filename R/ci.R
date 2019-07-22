
#' Confidence interval structure
#' 
#' Create a `confidence-interval` object.
#' 
#' @param estimate The Estimate
#' @param lower Lower bound
#' @param upper Upper bound.
#' @param confidence confidence level
#' @param ... other information such as 
#' 
#' @export
ci <-
function( estimate  #< Estimate
		, lower		#< Lower bound
		, upper		#< Upper bound
		, confidence = 0.95 #< confidence level
		, ...
		){
	structure( estimate, class=c("confidence-interval", class(estimate))
		     , bounds = data.frame(lower, upper) 
		     , confidence = confidence
		     , ...)
}

#' @export
`format.confidence-interval` <- 
function( x, justify="right", width=NULL
        , digits = NULL
        , ci.digits = NULL
        , ...){
    if(is.null(digits))
        digits <- attr(x, 'digits')
	s <- NextMethod(width=NULL, digits = digits)
    
    if(is.null(ci.digits))
        ci.digits <- attr(x, 'ci.digits')
    if(is.null(ci.digits) && !is.null(digits))
        ci.digits <- digits
	format(
        ifelse( is.na(x), NA_character_
		      , sprintf( "%s (%s\u2013%s)"
		               , s
		               , format(attr(x, 'bounds')$lower, digits = ci.digits, ...)
		               , format(attr(x, 'bounds')$upper, digits = ci.digits, ...)
		               )
              )
	, justify=justify, width=width, ...)
}

#' @export
`print.confidence-interval` <- 
function(x		#< Object
		, ...	#< arguments to format/print.
		){
	print(format(x, ...), quote=FALSE, ...)
	invisible(x)
}
#' @export
`c.confidence-interval` <- 
function( x, ...){
    .list <- list(...)
    stopifnot(all(sapply(.list, inherits, "confidence-interval")))
    structure( NextMethod()
             , bounds = do.call(rbind, c( list(attr(x, 'bounds'))
                                        , lapply(.list, attr, 'bounds')
                                        ))
             , class = 'confidence-interval'
             ) 
}


if(FALSE){
x <- 
need_data(performance, "Performance") %$%
	ci( percent(Sensitivity.mean)
      , percent(Sensitivity.lower)
      , percent(Sensitivity.upper)
	  )
nchar(format(x, width=30))
	  
	  
need_data(performance, "Performance") %>%
    mutate( Sensitivity  = ci( percent(Sensitivity.mean)
                             , percent(Sensitivity.lower)
                             , percent(Sensitivity.upper)
							 )
		  )
          
          
a <- rnorm(4)
x <- ci(a, a-runif(4), a+runif(4))
y <- ci(1, 0, 2)
c(y, x) %>% unclass


data.frame(x) %>% multiline_table
          
          
          
          
}
