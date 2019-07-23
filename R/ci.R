
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
if(FALSE){#@testing
    # taken from confint example
    fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
    bounds <- confint(fit)


    val <- ci(coef(fit), bounds[,1], bounds[,2])
    expect_is(val, 'confidence-interval')
    expect_equal( attr(val, 'bounds')$lower
                , unname(bounds[,1])
                )
    expect_equal( attr(val, 'bounds')$upper
                , unname(bounds[,2])
                )
    expect_equal(attr(val, 'confidence'), 0.95)
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
if(FALSE){#@testing
    fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
    bounds <- confint(fit)
    x <- ci(coef(fit), bounds[,1], bounds[,2])
    val <- format(x, digits = 2, width =50)

    expect_is(val, 'character')
    expect_match(val, "( |-|)(\\d+\\.\\d+) \\(( |-|)(\\d+\\.\\d+).( |-|)(\\d+\\.\\d+)\\)")
    expect_true(all(nchar(val)==50))
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
if(FALSE){#@testing
    a <- ci(0, -1, 1)
    b <- ci(0, -2, 2)

    val <-c(a,b)
    expect_is(val, 'confidence-interval')
    expect_length(val, 2)


    fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
    bounds <- confint(fit)
    df <- tibble( variable = names(coef(fit))
                , estimate = coef(fit)
                , lower = bounds[,1]
                , upper = bounds[,1]
                )
    expect_silent(val2 <- group_by(df, variable) %>% mutate(ci=ci(estimate, lower, upper)))
}
