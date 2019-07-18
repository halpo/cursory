#' @import dplyr
#' @import purrr
#' @importFrom tidyselect everything
#' @importFrom rlang :=
NULL

# Generics --------------------------------------------------------

#' Cursory Functions
#'
#' Cursory functions act like the dplyr `summarise_(all|at|if)`
#' functions with an important difference, they put the variable name
#' in a column and for each function passed in it puts the value in
#' it's own column.
#'
#' * `cursory_all()` is the analog of [dplyr::summarise_all()]
#' * `cursory_at()` is the analog of [dplyr::summarise_at()]
#' * `cursory_if()` is the analog of [dplyr::summarise_if()]
#'
#' @inheritParams dplyr::summarise_all
#' @param var.name Name of the column with variable names.
#' @export
cursory_all <- function (.tbl, .funs, ..., var.name="Variable") UseMethod("cursory_all")

#' @rdname cursory_all
#' @export
cursory_at <- function (.tbl, .vars, .funs, ..., var.name="Variable") UseMethod("cursory_at")

#' @rdname cursory_all
#' @export
cursory_if <- function (.tbl, .predicate, .funs, ..., var.name="Variable") UseMethod("cursory_if")


# Methods ---------------------------------------------------------

#' @export
cursory_at.tbl <-
function (.tbl, .vars, .funs, ...)
{
    parts <- purrr::map( tidyselect::vars_select(tbl_vars(.tbl), !!!.vars)
              , cursory_1, .tbl=.tbl, .funs=.funs
              , ...)
    purrr::reduce(parts, union_all)
}
if(FALSE){#@testing
    library(dbplyr)
    library(RSQLite)
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    suppress_warnings( .tbl <- group_by(copy_to(con, iris, 'iris', overwrite=TRUE), Species)
                     , "partial argument match")
    .vars <- setdiff(tbl_vars(.tbl), group_vars(.tbl))
    .funs <- lst(mean, sum)
    val <- cursory_at( .tbl, .vars, .funs)
}

cursory_1 <- function(var, .tbl, .funs, ..., var.name="Variable")
{
    select( mutate( summarise_at(.tbl, var, .funs, ...)
                  , !!var.name := var)
          , !!var.name, everything()
          )
}

#' @export
cursory_at.tbl_df <-
function (.tbl, .vars, .funs, ...)
{
    purrr::map_dfr( tidyselect::vars_select(tbl_vars(.tbl), !!!.vars)
                  , cursory_1, .tbl=.tbl, .funs=.funs
                  , ...)
}

#' @export
cursory_at.grouped_df <-
function(.tbl, .vars, .funs, ...)
{
    group_by( NextMethod("cursory_at", .tbl, .vars=.vars, .funs=.funs, ...)
            , !!!groups(.tbl)
            )
}

#' @export
cursory_at.data.frame <- function(.tbl, ...) cursory_at(tbl_df(.tbl), ...)


#' @export
cursory_all.tbl <-
function(.tbl, .funs, ...)
{
    cursory_at( .tbl = .tbl
              , .vars = setdiff(tbl_vars(.tbl), group_vars(.tbl))
              , .funs = .funs
              , ...)
}
if(FALSE){#@testing
    .tbl <- group_by(as_tibble(iris), Species)
    .funs <- lst(Missing = . %>% is.na %>% sum(na.rm = TRUE)
                , mean, sd )

    val <- cursory_all(.tbl, .funs)
    expect_is(val, 'tbl_df')
    expect_identical( as.character(tbl_vars(val))
                    , c('Variable', 'Species'
                       , 'Missing', 'mean', 'sd'))
    expect_identical(dim(val), c(12L, 5L))

    expect_is(val, 'grouped_df')
    expect_equal(group_vars(val), 'Species')
}

#' @export
cursory_if.data.frame <-
function(.tbl, .predicate, .funs, ...)
{
    cursory_all(select_if(.tbl, .predicate), .funs, ...)
}
