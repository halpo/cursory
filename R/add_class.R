#' Add to object helpers
#'
#' @param x   object to alter
#' @param new new characteristic
#'
#' @description
#' These function make using [magrittr] functions easier when altering
#' an object.
#'
#' @export
add_class <- function(x, new)structure(x, class = c(new, oldClass(x)))
if(FALSE){#@testing
    expect_is(add_class(1, 'test'), 'test')

    val <- add_class(add_class(1, 'class1'), 'class2')
    expect_is(val, 'class2')
    expect_is(val, 'class1')
    expect_is_not(val, 'class3')
}

#' @describeIn add_class Overwrite the class
#' @export
set_class <- function(x, new)structure(x, class = new)
if(FALSE){#@testing
    expect_is(set_class(1, 'test'), 'test')

    val <- set_class(set_class(1, 'class1'), 'class2')
    expect_is(val, 'class2')
    expect_is_not(val, 'class1')
    expect_is_not(val, 'class3')
}

#' @describeIn add_class Add a comment
#' @export
add_comment<- function(x, new) structure(x, comment = c(comment(x), new))
if(FALSE){#@testing
    val <- add_comment(list(), "a test comment")
    expect_equal(comment(val), "a test comment")

    val <- add_comment(val, "another comment")
    expect_equal(comment(val), c("a test comment", "another comment"))
}

#' @describeIn add_class Overwrite the comment
#' @export
set_comment<- function(x, new) structure(x, comment = new)
if(FALSE){#@testing
    val <- set_comment(list(), "a test comment")
    expect_equal(comment(val), "a test comment")

    val <- set_comment(val, "another comment")
    expect_equal(comment(val), "another comment")
}


carry_forward <-
function( value
        , original
        , which
        ){
    if (missing(original) && sys.nframe() > 1L){
        original <- eval(sys.call(which=sys.parent())[[2]], parent.frame())
    }
    attributes <- base::attributes(original)
    if(missing(which)){
        which <- names(attributes)
    } else {
        assert_that(all(which %in% names(attributes)))
    }
    mostattributes(value) <- attributes[which]
    return(value)
}
if(FALSE){#@testing
    x <- dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
    y <- carry_forward(c('c', 'd', 'd'), x)
    expect_identical(attributes(x), attributes(y))
}


