magrittr::`%>%`

.vars2ListCol <- function(data, id, vars, var_name) {

  id <- dplyr::enquo(id)
  vars <- dplyr::enquo(vars)

  data %>%
    dplyr::select(!! id, !! vars) %>%
    tidyr::gather("variable", "response", -!! id) %>%
    dplyr::group_by(!! id) %>%
    dplyr::mutate(!! var_name := list(na.omit(response))) %>%
    dplyr::select(!! id, var_name) %>%
    dplyr::distinct(!! id, .keep_all = TRUE)

}

.vars2ListCol2 <- function(data, id, vars, var_name, cond) {

  id <- dplyr::enquo(id)
  vars <- dplyr::enquo(vars)

  data %>%
    dplyr::select(!! id, !! vars) %>%
    tidyr::gather("variable", "response", -!! id) %>%
    dplyr::group_by(!! id) %>%
    dplyr::mutate(response = ifelse(response == cond, variable, NA)) %>%
    dplyr::mutate(!! var_name := list(na.omit(response))) %>%
    dplyr::select(!! id, var_name) %>%
    dplyr::distinct(!! id, .keep_all = TRUE)

}
