#' Imports a SALURBAL formated excel codebook
#'
#' This will operationalize various sheets as a list of dataframes. These will serve as data
#' structures that will go through QC and ultimate dernoamlziationa nd export.
#'
#' @param xlsx_path: the path to the .xslx to import
#' @param i: dataset import object
#' @param src: optinoal parapmeter to indicate if this is a source dictionary to import
#'
#' @return returns a named list of dataframes.
#' @export
#'
#' @examples
#' import_salurbal_xslx_cdbk()


import_salurbal_xslx_cdbk = function(xlsx_path, i = NULL, src = F){

  sheets = excel_sheets(xlsx_path)
  generate_summary_sheets = c('Summary', 'Codebook')
  headless_sheets = c('Summary')

  codebooks = sheets %>%
    map(~{

      # .x = 'Summary'
      # .x = 'by_key_iso2'

      sheet_import_raw = xlsx::read.xlsx2( xlsx_path,
                                           sheetName = .x,
                                           head = ifelse(.x%in%headless_sheets,F,T)) %>%
        as_tibble() %>%
        mutate_all(~as.character(.x)) %>%
        filter(.[[1]] != '') %>%
        select(-any_of(c("X.")))

      ## Detect redundancy due to excel merges
      if (.x!='Summary'){
        df_rename =  tibble(raw = names(sheet_import_raw)) %>%
          mutate(cleaned = str_replace(raw, "\\..*", "")) %>%
          group_by(cleaned) %>%
          filter(str_detect(raw,".1")) %>%
          ungroup()

        sheet_import = sheet_import_raw %>%
          select(-df_rename$cleaned) %>%
          rename_with(~str_remove(., "\\.1$"))
      } else {
        sheet_import = sheet_import_raw
      }



      if (src == T){
        sheet_content = sheet_import
      } else if(.x=='Summary'){
        sheet_content = sheet_import
      } else if (.x=='Codebook'){
        sheet_content = sheet_import %>%
          rename("Variable Label" = "Variable.Label",
                 "Variable Name" = "Variable.Name")
      } else if (.x=='source_by_iso2_year'){
        sheet_content = sheet_import %>%
          select(source_key, iso2, year, source_value) %>%
          filter(source_key!='')

      } else if (.x=='source'){
        sheet_content = sheet_import %>%
          select(source_key, source_value) %>%
          filter(source_key!='')
      } else if (is.null(i)){
        sheet_content = sheet_import
      } else {
        sheet_content = sheet_import  %>%
          select( names(sheet_import) %>% keep(~.x%in%i$denormalized_fields) )

      }

      return(sheet_content)

    }) %>%
    set_names(sheets)

  return(codebooks)
}
