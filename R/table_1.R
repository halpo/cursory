#' @importFrom tidymargins with_margins
#' @importFrom pkgcond assert_that
NULL
utils::globalVariables('.')

#' Create a Table 1
#'
#' This helps creates the demographics table,
#' the eponymous "table 1".
#' Given a data set a key for columns,
#' describe the differences across the provided
#' factor variables between the levels of key.
#'
#' `table_1_summarise` and `table_1_dispatcher` dispatch on the data type of the
#' variable identified by `var` in `.data`
#'
#' @param .data a dataset
#' @param key the comparison variable, such as case/control.
#' @param ... a lazy list of variables to include in the description.
#'
#' @export
table_1 <-
function( .data, key
        , ...
        ){
    key <- rlang::enquo(key)
    dots <- rlang::quos(...)
    vv <- group_by_prepare(.data, !!!dots)
    purrr::imap( vv$groups %>% rlang::exprs_auto_name()
               , table_1_dispatcher
               , key = key, .data = vv$data
               ) %>%
        purrr::reduce(dplyr::union_all)
}

#' @rdname table_1
#' @param var  Variable identifier, used to dispatch
#' @param name name of the variable
#' @export
table_1_dispatcher <- function(.data, var, name, key)
    UseMethod("table_1_dispatcher", var)

#' @method table_1_dispatcher quosure
#' @export
table_1_dispatcher.quosure <- function(.data, var, name, key){
    UseMethod("table_1_dispatcher", rlang::quo_squash(var))
}

#' @method table_1_dispatcher call
#' @export
#' @importFrom rlang :=
table_1_dispatcher.call <- function(.data, var, name, ...){
    test <- .data  %>% utils::head() %>% transmute(!!name := !!var) %>% pull(!!name)
    UseMethod("table_1_summarise", test)
}
if(FALSE){#@Testing
    .data <- dplyr::mutate(iris, Size = ifelse(Sepal.Length > median(Sepal.Length), 'Big', 'Little'))
    var <- rlang::expr(toupper(Species))
    name <- 'SPECIES'
    key  <- rlang::parse_quo('Size', env=globalenv())
    result <- table_1_dispatcher(.data, var, 'SPECIES', key)
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'Big', 'Little'))
    expect_equal(nrow(result), 3)
}


#' @method table_1_dispatcher name
#' @export
table_1_dispatcher.name <- function(.data, var, name, ...){
    test <- .data  %>% utils::head() %>% pull(!!var)
    UseMethod("table_1_summarise", test)
}
if(FALSE){#@Testing
    .data <- dplyr::mutate(iris, Size = ifelse(Sepal.Length > median(Sepal.Length), 'Big', 'Little'))
    key  <- rlang::parse_quo('Size', env=globalenv())
    result <- table_1_dispatcher(.data, as.name('Species'), 'SPECIES', key)
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'Big', 'Little'))
    expect_equal(nrow(result), 3)
}

#' @method table_1_dispatcher character
#' @export
table_1_dispatcher.character <- function(.data, var, name, ...){
    test <- .data %>% utils::head() %>% pull(!!var)
    var  <- rlang::as_quosure(as.name(var))
    UseMethod("table_1_summarise", test)
}
if(FALSE){#@Testing
    .data <- dplyr::mutate(iris, Size = ifelse(Sepal.Length > median(Sepal.Length), 'Big', 'Little'))
    result <- table_1_dispatcher( .data
                                , quo(Species)
                                , 'SPECIES'
                                , rlang::parse_quo('Size', env=globalenv())
                                )
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'Big', 'Little'))
    expect_equal(nrow(result), 3)
    expect_equal(unique(result$Variable), 'SPECIES')
}

#' @rdname table_1
#' @export
table_1_summarise <-
    function(.data, var, name, key)
        UseMethod("table_1_dispatcher", var)

#' @rdname table_1
#' @export
table_1_summarize <- table_1_summarise

#' @method table_1_summarise logical
#' @export
table_1_summarise.logical <- function(.data, var, name, key, all.name = "(All)"){
    n <- pct <- NULL
    .data  %>%
        dplyr::group_by(!!key, add=TRUE) %>%
        with_margins(summarise, all.name=all.name)( Variable = !!name
                    , Level = 'Yes'
                    , n        = sum(as.integer(!!var))
                    , pct      = mean(as.numeric(!!var))
                    ) %>%
        mutate(VALUE=Npct(n,pct)) %>%
        select('Variable', 'Level', !!key, 'VALUE') %>%
        tidyr::spread(!!key, 'VALUE') %>%
        dplyr::select('Variable', 'Level', !!all.name, tidyselect::everything())
}
if(FALSE){#@Testing
    result <- iris %>%
        dplyr::mutate(Big = Sepal.Length > median(Sepal.Length)) %>%
        table_1_summarise.logical( var  = rlang::as_quosure(as.name('Big'))
                                 , name = 'Is Big?'
                                 , rlang::parse_quo('Species', env=globalenv())
                                 )
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'setosa', 'versicolor', 'virginica'))
    expect_equal(nrow(result), 1)
}

#' @method table_1_summarise character
#' @export
table_1_summarise.character <- function(.data, var, name, key, all.name = "(All)"){
    if (inherits(.data, 'tbl_sql')){
        if (!exists('Npct', dbplyr::base_odbc_scalar))
            assign('Npct', value = Npct_sql, envir = dbplyr::base_odbc_scalar)
    }
    pct <- rlang::sym('._PERCENT_.')
    .data %>%
        dplyr::group_by(!!key, add=TRUE) %>%
        with_margins(dplyr::count, all.name=all.name)(Level = !!var) %>%
        dplyr::group_by(Variable = !!name, !!key, add=FALSE) %>%
        dplyr::mutate( !!pct   := as.numeric(n)/sum(as.numeric(n), na.rm=TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select('Variable', 'Level', !!key, 'n', !!pct) %>%
        dplyr::mutate(VALUE = Npct(n, !!pct)) %>%
        dplyr::select('Variable', 'Level', !!key, 'VALUE') %>%
        tidyr::spread(!!key, 'VALUE') %>%
        dplyr::select('Variable', 'Level', !!all.name, tidyselect::everything())
}
if(FALSE){#@Testing
    .data <- dplyr::mutate(iris, Size = ifelse(Sepal.Length > median(Sepal.Length), 'Big', 'Little'))
    result <- table_1_summarise.character( .data
                                         , var = quo(Species)
                                         , name='SPECIES'
                                         , rlang::parse_quo('Size', env=globalenv())
                                         )
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'Big', 'Little'))
    expect_equal(result$Level, factor(c('setosa', 'versicolor', 'virginica')))
    expect_equal(nrow(result), 3)
}

#' @method table_1_summarise factor
#' @export
table_1_summarise.factor <- function(.data, var, name, key, all.name = "(All)"){
    .data <- mutate_at(.data, vars(!!var), as.character)
    table_1_summarise.character(.data, var, name, key, all.name = all.name)
}
if(FALSE){#@Testing
    .data <- dplyr::mutate(iris, Size = cut(Sepal.Length, c(-Inf, 5, 6.4, Inf), c('Small', 'Medium', 'Large'))
                                    %>% ordered(c('Small', 'Medium', 'Large')))
    expect_true(is.factor(.data$Size))
    result <- table_1_summarise.character( .data
                                         , var = quo(Size)
                                         , name='SPECIES'
                                         , rlang::parse_quo('Species', env=globalenv())
                                         )
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'setosa', 'versicolor', 'virginica'))
    expect_equal(nrow(result), 3)
    expect_equal(result$Level, forcats::fct_inorder(result$Level))
}

#' @method table_1_summarise numeric
#' @export
table_1_summarise.numeric <- function(.data, var, name, key, all.name = "(All)"){
    Variable <- Level <- NULL
    .data  %>%
        dplyr::group_by( !!key) %>%
        with_margins(dplyr::summarise_at, all.name = all.name)(.data = .
            , .vars = dplyr::vars(!!var)
            , .funs = tibble::lst( Min    = ~min(., na.rm=TRUE)
                         , Median = ~dplyr::nth(.,  n() %/% 2L, .)
                         , Mean   = ~mean(., na.rm=TRUE)
                         , Max    = ~max(., na.rm=TRUE)
                         , SD     = ~stats::sd(., na.rm=TRUE)
                         )
            ) %>%
        tidyr::gather( 'Level', 'VALUE', 'Min', 'Median', 'Mean', 'Max', 'SD') %>%
        dplyr::mutate_at('VALUE', format, digits = 3) %>%
        dplyr::mutate( Variable = !!name
                     , Level    = ordered(Level, levels=c('Min', 'Median', 'Mean', 'Max', 'SD'))
                     ) %>%
        tidyr::spread(!!key, 'VALUE') %>%
        dplyr::arrange(Variable, Level) %>%
        dplyr::mutate_at('Level', as.character) %>%
        dplyr::select('Variable', 'Level', !!all.name, tidyselect::everything())
}
if(F){#@testing
    result <- table_1_summarise.numeric(iris, rlang::quo(Petal.Length), 'Petal Length', rlang::quo(Species))
    expect_is(result , 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'setosa', 'versicolor', 'virginica'))
    expect_equal(nrow(result), 5)
    expect_equal(result$Level, c('Min', 'Median', 'Mean', 'Max', 'SD'))
}

