#' Calculate grand mean collar height from dates of collar changes only
#'
#' @description Helper function that takes a data frame of data by sampling unit
#'   and date, finds dates on which collar heights change, and uses those dates
#'   to calculate a grand mean of collar height.
#'
#' @param data A data frame
#' @param collar_var <[`data-masked`][rlang::args_data_masking]> Name of collar
#'   height variable in `data`
#' @param SU_var <[`data-masked`][rlang::args_data_masking]> Name of sampling
#'   unit variable in `data`
#' @param date_var <[`data-masked`][rlang::args_data_masking]> Name of date
#'   variable in `data`
#'
#' @returns A length 1 numeric
#' @export
mean_collar_height <- function(data, collar_var = collar_height_cm, SU_var = SU,
                               date_var = Date) {
  df <- data %>%
    dplyr::arrange({{ SU_var }}, {{ date_var }}) %>%
    dplyr::mutate(
      change = dplyr::row_number({{ collar_var }}) == 1 |
        {{ collar_var }} != dplyr::lag({{ collar_var }}),
      .by = {{ SU_var }}
    ) %>%
    dplyr::filter(change) %>%
    dplyr::pull({{ collar_var }}) %>%
    mean(na.rm = TRUE)
}
