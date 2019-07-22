#' Designate a numeric vector as a percent.
#' @param x a numeric object.
#' @param ... additional formatting arguments.
#' @export
percent <- 
function( x     #< Object
        , ...   #< attributes
        ){
    "Designate a numeric vector as a percent."
    stopifnot(is.numeric(x))
    structure(x, class = c("percent", class(x)), ...)
}

#' @describeIn percent Convert to character.
#' @export
as.character.percent <- 
function( x
        , places    = NULL  #< Places to show after period
        , threshold = NULL  #< minimum percent to show.
        , ...               #< ignored
        ){
    if (is.null(places))
        places <- attr(x, "places") %||% getOption("percent::places") %||%  2 
    if (is.null(threshold))
        threshold <- attr(x, 'threshold') %||% getOption("percent::threshold") %||% -Inf
    fmt <- paste0("%2.", places, "f%%")
    str <- ifelse( x < threshold
                 , sprintf("< %s", sprintf(fmt, threshold))
                 , sprintf(fmt, x*100)
    )
}

#' @describeIn percent format the percent.
#' @param places    Number of places after decimal to show.
#' @param threshold Minimum percent to show.
#' 
#' @export
format.percent <- 
function( x
        , places    = NULL  #< Places to show after period
        , threshold = NULL  #< minimum percent to show.
        , ... #< currently ignored
        ){
    format(as.character.percent(x, places=places, threshold = threshold), ...)
}

#' @export
print.percent <- function(x,...){
    print(noquote(format.percent(x,...)), ...)
    invisible(x)
}



if(F){# Testing

    percent(1/3)
    data.frame(x=percent(1/3:4))


    class(percent(matrix(1:4, 2,2)/5))


}
