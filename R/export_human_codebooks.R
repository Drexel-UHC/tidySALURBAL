#' Exports human friendly codebook excell sheet
#'
#' WOrked wihtDMC to develop internal facing human accessible codebooks. THis is run in rpoduction for all fully renovated codebooks.
#'
#'
#'
#' @param df_denorm dernoamlized table
#' @param df_data_details supplemental details about data.
#' @param i dataset sepcific improt object
#'
#'
#' @return side effect. will write human readable codebook.
#' @export
#'
#' @examples
#' export_human_codebooks()

export_human_codebooks = function(df_denorm, df_data_details, i){



  { # setup -------------------------------------------------------------------

    summary_file = glue("{i$dataset_id_tmp}_codebook.xlsx")
    summary_file_path_repo = glue("datasets/{i$dataset_id_tmp}/{summary_file}")
    summary_file_path_db = glue("{i$db_etl_path}/{summary_file}")


    ## dynamic fields
    df__dynamic_tables =  i$df_schema_tidy %>%
      filter(column%in%c("acknowledgements",'source','limitations')) %>%
      select(column, table) %>%
      rowwise() %>%
      mutate(linkage = ifelse(check_by_dataset(df_denorm, column)&(column!='limitations'),
                              'by_dataset',
                              table))
  }

  { # summary sheet -----------------------------------------------------------------

    { ## operationalize  -----------------------------------------------------------------
      summary_dynamic_fields = df__dynamic_tables %>%
        filter(linkage == 'by_dataset') %>%
        pull(column)



      df__summary = df_denorm  %>%
        slice(1) %>%
        mutate(version = i$version_tmp,
               codebook_date = get_date_from_data_file_name(file_codebook) %>% mdy() %>% format("%m-%d-%Y"),
               nrow = df_data_details$nrow,
               ncol = df_denorm$var_name %>% unique() %>% length() + 2,
               data_files_cell = df_denorm %>% pull(file_data) %>% unique() %>% sort() %>%
                 paste(collapse = ', \n ')) %>%
        mutate_all(~as.character(.x)) %>%
        select(`Dataset:` = dataset_id,
               `Version:` = version,
               `Data file:` = data_files_cell,
               # `Codebook file:` = file_codebook,
               `Codebook date:` = codebook_date,
               `Number of rows:` = nrow,
               `Number of columns (variables):` = ncol,
               any_of(summary_dynamic_fields)) %>%
        rename_all(~str_to_title(.x)) %>%
        mutate(r = row_number()) %>%
        pivot_longer(cols = -r,
                     names_to = 'X1',
                     values_to = 'X2') %>%
        select(-r)
    }
  }

  { ## codebook sheet  -----------------------------------------------------------------

    { ## op. ID  -----------------------------------------------------------------
      iso2_coding = "AR = Argentina \n BR = Brazil \n CL = Chile \n CO = Colombia \n CR = Costa Rica \n GT = Guatemala \n MX = Mexico \n NI = Nicaragua \n PA = Panama \n PE = Peru \n SV = El Salvador"

      id_rows = tribble(
        ~Domain, ~Subdomain, ~"Variable Name", ~Type,      ~Coding,  ~Definition,
        "ID",   "ID",       "ISO2",           "character", '',       "2-letter ISO2 country code",
        "ID",   "ID",       "SALID1",          "discrete", '',       "Level 1AD identification number"
      ) %>%
        mutate(Coding = ifelse(`Variable Name`=="ISO2",
                               iso2_coding,
                               ''))
    }

    { ## op. codebooks  -----------------------------------------------------------------
      codebook_rows  =  df_denorm  %>%
        select(Domain = domain,
               Subdomain = subdomain,
               `Variable Name` = var_name,
               `Variable Label` = var_label,
               Type = value_type,
               Units = units,
               Coding = coding,
               Definition = var_def,
               Limitations = limitations) %>%
        distinct()
    }

    df__codebook = bind_rows(id_rows,codebook_rows) %>%
      select(Domain ,
             Subdomain ,
             `Variable Name` ,
             `Variable Label`,
             Type ,
             Units ,
             Coding,
             Definition,
             Limitations)
  }

  { ## Compile   -----------------------------------------------------------------
    list_current = list(
      "Summary" = df__summary,
      "Codebook" = df__codebook
    )
  }

  { # Write  -------------------------------------------------------------------

    if (!identical_cdbk_vs_xslx(list_current,summary_file_path_repo, i)){

      { ## Setup xlsx -------------------------------------------------------------------
        wb = createWorkbook()

        cs_text_bold <- CellStyle(wb) +
          Font(wb, isBold=TRUE) +
          Border(color="black",
                 position=c("BOTTOM", "LEFT", "TOP", "RIGHT"),
                 pen=c("BORDER_THIN"))+
          Alignment(wrapText=TRUE)

        cs_text <- CellStyle(wb) +
          Border(color="black",
                 position=c("BOTTOM", "LEFT", "TOP", "RIGHT"),
                 pen=c("BORDER_THIN"))+
          Alignment(wrapText=TRUE)
      }

      { ## Add Summary sheet -----------------------------------------------------------------
        sheet__summary  <- createSheet(wb, sheetName="Summary")


        setColumnWidth(sheet__summary, 1, 35)
        setColumnWidth(sheet__summary, 2, 50)

        addDataFrame(as.data.frame(df__summary),
                     sheet__summary,
                     startRow=1,
                     startColumn=1,
                     row.names = F,
                     col.names = F,
                     colnamesStyle = cs_text_bold,
                     colStyle=list(`1`= cs_text_bold, `2`= cs_text))
      }
      { ## Add Codebook Sheet-----------------------------------------------------------------
        sheet__codebook  <- createSheet(wb, sheetName="Codebook")
        x = 20
        y = 60
        setColumnWidth(sheet__codebook, 1, x)
        setColumnWidth(sheet__codebook, 2, x)
        setColumnWidth(sheet__codebook, 3, x)
        setColumnWidth(sheet__codebook, 4, x)
        setColumnWidth(sheet__codebook, 5, x)
        # setColumnWidth(sheet__codebook, 6, y)
        setColumnWidth(sheet__codebook, 7, x)
        setColumnWidth(sheet__codebook, 8, y)
        setColumnWidth(sheet__codebook, 9, y)

        addDataFrame(as.data.frame(df__codebook),
                     sheet__codebook,
                     startRow=1,
                     startColumn=1,
                     row.names = F,
                     col.names = T,
                     colnamesStyle = cs_text_bold,
                     colStyle=list(`1`= cs_text,
                                   `2`= cs_text,
                                   `3`= cs_text,
                                   `4`= cs_text,
                                   `5`= cs_text,
                                   `6`= cs_text,
                                   `7`= cs_text,
                                   `8`= cs_text,
                                   `9`= cs_text))
      }


      {## Save xlsx -------------------------------------------------------------------
        saveWorkbook(wb,summary_file_path_repo)
        saveWorkbook(wb,summary_file_path_db)
        cli_alert_success(paste("Wrote human codebooks:",summary_file_path_repo), .envir = globalenv())
        cli_alert_success(paste("Wrote human codebooks:",summary_file_path_db), .envir = globalenv())
      }

    }

  }
}
