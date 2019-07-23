#' Indicate that when printing repeat values should be hidden.
#'
#' @param x a vector
#' @param replace.with what to replace the value with.
#' @export
dontrepeat <- function(x, replace.with='')
    structure(x, class=c('dontrepeat', oldClass(x)), replace.with=replace.with)

format.dontrepeat <- function(x, ..., replace.with = attr(x, 'replace.with') %||% ''){
    repeats <- c(FALSE, head(x, -1) == tail(x, -1))
    ifelse(repeats, format(replace.with, ...), NextMethod())
}
if(FALSE){#@testing
    x <- dontrepeat(c('a','a', 'b', 'b', 'b'))
    val <- format(x)
    expect_identical(val, c('a', '', 'b', '', ''))

    x <- dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
    val <- format(x)
    expect_identical(val, c('a', '.', 'b', '.', '.'))

    x <- dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
    val <- format(x, replace.with='-')
    expect_identical(val, c('a', '-', 'b', '-', '-'))

    x <- dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
    val <- format(x, replace.with='-', width=5, justify='right')
    expect_identical(val, c('    a', '    -', '    b', '    -', '    -'))
}

`[.dontrepeat` <- function(x,...)carry_forward(NextMethod())
`c.dontrepeat` <- function(x,...)carry_forward(NextMethod())
if(FALSE){
    x <- dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
    val <- x[TRUE]

    expect_is(val, 'dontrepeat')
    expect_equal(attr(val, 'replace.with'), '.')
}

if(FALSE){#@testing dontrepeat in a tbl
    x <- tibble( x = dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
               , y = 1:5
               )
    expect_is(x$x, 'dontrepeat')
    expect_is(head(x, 5)$x, 'dontrepeat')


    expect_equal( format(as.data.frame(x))$x
                , I(c('a', '.', 'b', '.', '.')))
}

