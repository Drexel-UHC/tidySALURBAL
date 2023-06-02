#' Tidy year for input dataframe
#'
#' Takes in a dataframe and tidies or unpacks the year field. So any ranges or seperateotrs will be pivot longer.
#'
#' @param df the input data frame
#'
#' @return returns dataframe
#' @export
#'
#' @examples
#' unpack_year(df = df_raw__codebook)


unpack_year = function(df){

  unpack_year_string = function(string_tmp){

    if (string_tmp == '_all'){
      years = 1970:2030 %>% as.character()
    } else {
      years  = string_tmp %>%
        str_split_1(";") %>%
        str_trim() %>%
        map(~{
          range__vec = .x %>%
            str_split("-") %>%
            unlist() %>%
            as.numeric()
          if (length(range__vec)==1){
            list_years = list(as.character(range__vec))
          } else {
            list_years = list(as.character(range__vec[1]:range__vec[2]))
          }
          return(list_years)
        }) %>%
        unlist()
    }


    list_years = list(years)


    return(list_years)
  }

  if (!'year'%in%names(df)) return(df)

  df_year_unpacked = df %>%
    rowwise() %>%
    mutate(year = unpack_year_string(year)) %>%
    unnest(year)

  return(df_year_unpacked)
}
