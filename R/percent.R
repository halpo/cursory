#' Designate a numeric vector as a percent.
#'
#' The percentage is stored as a formatted string with the original
#' value as an attribute.  The formatted value is what will most often
#' be needed but but allows for the original value to be recovered
#' when the attribute is not stripped off.
#'
#' @param x a numeric object indicating a percentage.
#' @param places Places to show after the decimal point.
#' @param threshold The minimum absolute percentage to show.
#' @param ... additional formatting arguments.
#' @export
percent <-
function( x, ...){
    structure( pct(x,...)
             , raw = x
             , class = "percent")
}
if(FALSE){#@testing
    val <- percent(1/3)
    expect_is(val, 'percent')
    expect_true(is.character(val))
    expect_equal(as.character(val), "33.33%")
    expect_identical(attr(val, 'raw'), 1/3)
}

#' @describeIn percent Format a number as a percent.
#' @export
pct <-
function( x
        , places    = attr(x, 'places') %||% getOption("percent::places", 2)  #< Places to show after period
        , threshold = attr(x, 'threshold') %||% getOption("percent::threshold", 1*10^-places)  #< minimum percent to show.
        , ...               #< ignored
        ){
    assert_that(is.numeric(x))
    fmt <- paste0( "%2.", places, "f%%")
    str <- ifelse( abs(x) < threshold
                 , sprintf("< %s", sprintf(fmt, sign(x)*threshold))
                 , sprintf(fmt, x*100)
                 )
}
if(FALSE){#@testing
    val <- pct(1/3, places=3)
    expect_equal(val, "33.333%")

    val <- pct(0.009, places=2)
    expect_equal(val, "< 0.01%")
}

#' @export
format.percent <-
function( x, ...){
    if(is.numeric(x)) x <- pct(x, ...)
    format(x, ...)
}
if(FALSE){#@testing
    val <- format.percent(1/3)
    expect_identical(val, "33.33%")
}


#' @export
print.percent <- function(x,...){# nocov start
    print(noquote(format.percent(x,...)), ...)
    invisible(x)
} # nocov end
