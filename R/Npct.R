#' Combine count with a percent
#'
#' @param x count or object to count
#' @param n see methods.
#' @param ... formatting arguments for formatting the percent. See format.percent.
#' @export
Npct <- function(x, n, ...){pkg_error("Not implimented")}
setGeneric('Npct')
if(FALSE){#@testing
    expect_error(Npct("not", "valid"))
}



#' @describeIn Npct Count and give percent of `TRUE` from a logical vector.
setMethod('Npct', signature('logical', 'missing'), function(x, n, ...){
    count <- sum(x)
    percent <- mean(x)
    structure( sprintf('%d (%s)', count, pct(percent, ...))
             , count = count, percent=percent)
})
if(FALSE){#@testing
    val <- Npct(c(T, F, T))
    expect_equal(as.character(val), '2 (66.67%)')
    expect_identical(attr(val, 'count'), 2L)
    expect_identical(attr(val, 'percent'), 2/3)
}

#' @describeIn Npct Count and percent of a logical filtered by a second logical.
setMethod('Npct', signature('logical', 'logical'), function(x, n, ...){
    assert_that(length(x) == length(n))
    count <- sum(x[n])
    percent <- mean(x[n])
    structure( sprintf('%d (%s)', count, pct(percent, ...))
             , count = count, percent=percent)
})
if(FALSE){#@testing
    val <- Npct( c(T,F,T,F,T)
               , c(T,T,T,F,F)
               )
    expect_equal(as.character(val), '2 (66.67%)')
    expect_identical(attr(val, 'count'), 2L)
    expect_identical(attr(val, 'percent'), 2/3)
}

#' @describeIn Npct Provided with count(s) of cases and total(s)
setMethod('Npct', signature('integer', 'integer'), function(x, n, ...){
    assert_that( all(x <  n)
               , all(x >= 0)
               , all(n >  0)
    )
    count <- x
    percent <- x/n
    structure( sprintf('%d (%s)', count, pct(percent, ...))
             , count = count, percent=percent)
})
if(FALSE){#@testing
    val <- Npct(2L, 3L)
    expect_equal(as.character(val), '2 (66.67%)')
    expect_identical(attr(val, 'count'), 2L)
    expect_identical(attr(val, 'percent'), 2/3)
}


#' @describeIn Npct Provided the actual count and the percent.
setMethod('Npct', signature('numeric', 'numeric'), function(x, n, ...){
    assert_that( rlang::is_integerish(x)
               , all(x >= 0L)
               , all(0 <= n & n <= 1)
               , length(x) == length(n)
               )
    count <- as.integer(x)
    percent <- n
    structure( sprintf('%d (%s)', count, pct(percent, ...))
             , count = count, percent=percent)
})
if(FALSE){#@testing
    val <- Npct( 2, 2/3)
    expect_equal(as.character(val), '2 (66.67%)')
    expect_identical(attr(val, 'count'), 2L)
    expect_identical(attr(val, 'percent'), 2/3)
}
