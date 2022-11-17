#' Convert codebook variable to generic format
#'
#' @param var_raw the raw codebook variable. This often contains higher level information
#' such as year, gender or other attributes. We o
#'
#' @return A character value which is just the base variable name.
#' @export
#'
#' @examples
#' sanitize_codebook_var(var_raw = "OBESITY_L1AD_YYYY")

# FUNCTIONS
sanitize_codebook_var = function(var_raw){
  var_raw %>%
    stringr::str_to_upper() %>%
    stringr::str_remove("L1AD") %>%
    # str_remove("L2_5") %>%
    stringr::str_remove("L3") %>%
    stringr::str_remove("L2") %>%
    stringr::str_remove("L1UX") %>%
    stringr::str_remove("L1XS") %>%
    stringr::str_remove("YYYY")%>%
    stringr::str_remove("YYY")%>%
    stringr::str_remove("LLLL")%>%
    stringr::str_remove("XX") %>%
    stringr::str_remove("<LEVEL>") %>%
    stringr::str_remove_all("_")
}
