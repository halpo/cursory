
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
    assert_that( length(estimate) == length(lower)
               , length(lower) == length(upper)
               )
    mapply( data.frame
          , estimate=estimate
          , lower=lower
          , upper=upper
          , SIMPLIFY=FALSE) %>%
        lapply(`attr<-`, 'confidence', confidence) %>%
        lapply(`class<-`, 'confidence-interval') %>%
        lapply(structure, ...) %>%
        `class<-`("list<confidence-interval>")
}
if(FALSE){#@testing
    # taken from confint example
    fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
    bounds <- confint(fit)

    val <- ci(coef(fit), bounds[,1], bounds[,2])
    expect_is(val, 'list<confidence-interval>')
    expect_true(is.list(val))
    testextra::expect_all_inherit(val, 'confidence-interval')
}

#' @export
`format.list<confidence-interval>`<-function(x, ...){
    I(purrr::map_chr(x, format, ...))
}

#' @export
`format.confidence-interval` <-
function( x, justify="right", width=NULL
        , digits = attr(x, 'digits') %||% getOption('digits')
        , ci.digits = attr(x, 'ci.digits') %||% digits
        , span = attr(x, "span") %||% "\u2013"
        , ...){
	format(
        ifelse( is.na(x$estimate), NA_character_
		      , sprintf( "%s (%s%s%s)"
		               , format(x$estimate, digits = digits, ...)
		               , format(x$lower, digits = ci.digits,...)
		               , span
		               , format(x$upper, digits = ci.digits, ...)
		               )
              )
	, justify=justify, width=width, ...)
}
if(FALSE){#@testing
    fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
    bounds <- confint(fit)
    x <- ci(coef(fit), bounds[,1], bounds[,2])
    format(x[[1]])

    val <- format(x)

    expect_is(val, 'AsIs')
    expect_match(val, "( |-|)(\\d+\\.\\d+) \\(( |-|)(\\d+\\.\\d+).( |-|)(\\d+\\.\\d+)\\)")


    val <- format(x, width=50)
    expect_true(all(nchar(val)==50))
}

#' @export
`print.confidence-interval` <-
function(x		#< Object
		, ...	#< arguments to format/print.
		){ # nocov start
	print(format(x, ...), quote=FALSE, ...)
	invisible(x)
} # nocov end

#' @export
`c.list<confidence-interval>` <-
function( x, ...){
    .list <- list(...)
    stopifnot(all(sapply(.list, inherits, "list<confidence-interval>")))
    structure( NextMethod()
             , bounds = do.call(rbind, c( list(attr(x, 'bounds'))
                                        , lapply(.list, attr, 'bounds')
                                        ))
             , class = 'list<confidence-interval>'
             )
}
if(FALSE){#@testing
    a <- ci(0, -1, 1)
    b <- ci(0, -2, 2)

    val <-c(a,b)
    expect_is(val, 'list<confidence-interval>')
    expect_length(val, 2)
}
if(FALSE){#@testing confidence intervals in grouped data frame operations.
    fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
    bounds <- confint(fit)
    df <- tibble( variable = names(coef(fit))
                , estimate = coef(fit)
                , lower = bounds[,1]
                , upper = bounds[,1]
                )
    expect_silent(val2 <- group_by(df, variable) %>% mutate(ci=ci(estimate, lower, upper)))
    expect_is(val2$ci, 'list<confidence-interval>')
    expect_length(val2$ci, 5)
}
