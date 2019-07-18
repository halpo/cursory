#' @import pkgcond
#' @import dplyr
NULL

#' Cursory Functions
#'
#' look Cursory functions act like the dplyr `summarise_(all|at|if)`
#' functions with an important difference, they put the variable name
#' in a column and for each function passed in it puts the value in
#' it's own column.
#'
#' * `cursory_all()` is the analog of [dplyr::summarise_all()]
#' * `cursory_at()` is the analog of [dplyr::summarise_at()]
#' * `cursory_if()` is the analog of [dplyr::summarise_if()]
#'
#' @inheritParams dplyr::summarise_all
#' @export
cursory_all <- function (.tbl, .funs, ..., var.name="Variable") UseMethod("cursory_all")

#' @describeIn cursory_all
#' @export
cursory_at <- function (.tbl, .vars, .funs, ..., var.name="Variable") UseMethod("cursory_at")

#' @describeIn cursory_all
#' @export
cursory_if <- function (.tbl, .predicate, .funs, ..., var.name="Variable") UseMethod("cursory_if")

cursory_all.tbl_lazy <-
function (.tbl, .funs, ..., var.name="Variable")
{
    assert_that(.tbl %inherits% "tbl_lazy", is.function(.funs) ||
        .funs %inherits% "fun_list")
    vars <- setdiff(tbl_vars(.tbl), group_vars(.tbl))
    funs <- dplyr:::as_fun_list(.funs, enquo(.funs), ...)
    cenv <- rlang::caller_env()
    do1 <- function(fun, name, ...) {
        s1 <- dplyr:::manip_all(.tbl, fun, fun, cenv, ...)
        dplyr::summarise(.tbl, !!!s1) %>% dplyr::group_by(!!!dplyr::groups(.tbl)) %>%
            pivot::unpivot(Variable, !!name, everything())
    }
    purrr::reduce( purrr::imap(funs, do1, ...)
                 , dplyr::left_join
                 , by = c(group_vars(.tbl), "Variable"))
}
if(FALSE){#@testing


}



cursory_at.tbl_df <-
function (.tbl, .vars, .funs, ...)
{
    .l <- purrr::imap(.funs, function(fun, name) tidyr::gather(summarise_at(.tbl,
        .vars, fun, ...), Variable, !!name))
    purrr::reduce(.l, full_join, by = c(group_vars(.tbl), "Variable"))
}

cursory_at.tbl_lazy <-
function (.tbl, .vars, .funs, ...)
{
    vars <- select_vars(tbl_vars(.tbl), !!!.vars, exclude = group_vars(.tbl))
    info.df <- expand.grid(Variable = vars, ._METRIC_. = names(.funs)) %>%
        mutate(._X_. = paste(Variable, ._METRIC_., sep = "_")) %>%
        copy_to(.tbl$src, df = ., name = paste("##CURSORY_INFO_DF",
            digest::digest(.), digest::digest(.tbl, "crc32"),
            digest::digest(.funs, "crc32"), sep = "_"),
            overwrite = TRUE)
    summarise_at(.tbl, .vars, .funs, ...) %>% group_by(!!!groups(.tbl)) %>%
        pivot::unpivot(._X_., ._VALUE_., everything()) %>% dplyr::full_join(info.df,
        by = "._X_.") %>% select(-._X_.) %>% pivot::pivot(._METRIC_.,
        ._VALUE_., !!!(as_quos(names(.funs))))
}

cursory_if.tbl_lazy <-
function (.tbl, .predicate, .funs, ...)
{
    vars <- tbl_vars(select_if(ungroup(.tbl), .predicate))
    info.df <- expand.grid(Variable = vars, ._METRIC_. = names(.funs)) %>%
        dplyr::mutate(._X_. = paste(Variable, ._METRIC_., sep = "_")) %>%
        dplyr::copy_to(.tbl$src, df = ., name = paste("##CURSORY_INFO_DF",
            digest::digest(.), digest::digest(.tbl, "crc32"),
            digest::digest(.funs, "crc32"), sep = "_"),
            overwrite = TRUE)
    summarise_if(.tbl, .predicate, .funs, ...) %>% group_by(!!!groups(.tbl)) %>%
        pivot::unpivot(._X_., ._VALUE_., everything(), -one_of(group_vars(.tbl))) %>%
        dplyr::full_join(info.df, by = "._X_.") %>% select(-._X_.) %>%
        pivot::pivot(._METRIC_., ._VALUE_., !!!(as_quos(names(.funs))))
}

