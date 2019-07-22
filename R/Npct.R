#' Combine count with a percent
#'
#' @param x count or object to count
#' @param n see methods.
#' @param ... formatting arguments for formatting the percent. See format.percent.
#' @export
Npct <- function(x, n, ...){stop("Not implimented")}
setGeneric('Npct')

#' @describeIn Npct Count and give percent of `TRUE` from a logical vector.
setMethod('Npct', signature('logical', 'missing'), function(x, n, ...){
    sprintf('%d (%s)', sum(x), format.percent(mean(x), ...))
})
#' @describeIn Npct Count and percent of a logical filtered by a second logical.
setMethod('Npct', signature('logical', 'logical'), function(x, n, ...){
    assert_that(length(x) == length(n))
    sprintf('%d (%s)', sum(x[n]), format.percent(mean(x[n]), ...))
})
#' @describeIn Npct Provided with count(s) of cases and total(s)
setMethod('Npct', signature('integer', 'integer'), function(x, n, ...){
    assert_that( all(x <  n)
               , all(x >= 0)
               , all(n >  0)
    )
    sprintf("%d (%s)", x, format.percent(x/n, ...))
})
#' @describeIn Npct Provided the actual count and the percent.
setMethod('Npct', signature('numeric', 'numeric'), function(x, n, ...){
    assert_that( rlang::is_integerish(x)
               , all(x >= 0L)
               , all(0 <= n & n <= 1)
               , length(x) == length(n)
               )
    sprintf("%d (%s)", x, format.percent(n, ...))
})

Npct_sql <- function(n, pct){
    assert_that(requireNamespace("dbplyr"))
    dbplyr::build_sql("FORMAT(", n, ", 'D') + '(' + ", "FORMAT(", pct, ",'P') + ')'")
}
