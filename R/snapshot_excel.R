#' Creates a diff accessible snapshot of .xlsx file
#'
#' this function will store codebooks in .xlsx in .json for a particular dataset.This will allow us to track changes between versions much better with git.
#'
#' @param dataset_id_tmp the dataset id of interests
#' @param path a specific path if desired
#' @param quiet boolean to indicate if CLI messages are to be generated
#' @param folder which folde rin the repositroy should we scan
#'
#' @return nothing, just side effects
#' @export
#'
#' @examples
#' snapshot_excel("DTH")



snapshot_excel = function(dataset_id_tmp = NULL, path = NULL, quiet = T, folder = "datasets"){

  ## setup
  pattern = case_when(
    folder == 'datasets' ~ 'codebook|src',
    TRUE ~ '.*'
  )

  ## repository level snapshot
  excel_files = list.files(folder, recursive = T, full.names = T) %>%
    keep(~str_detect(.x, '.xlsx')) %>%
    keep( ~ str_detect(.x, pattern)) %>%
    discard( ~ str_detect(.x,  "\\(1\\)")) %>%
    discard( ~ str_detect(.x,  "\\(2\\)")) %>%
    discard( ~ str_detect(.x,  "\\(3\\)")) %>%
    discard( ~ str_detect(.x,  "\\(4\\)")) %>%
    discard( ~ str_detect(.x, 'archive'))


  ## dataset specific snapshot
  if (!is.null(dataset_id_tmp)) {
    excel_files = excel_files %>% keep( ~ str_detect(.x, dataset_id_tmp))
  }

  ## file specific snapshot
  if  (!is.null(path)) {
    excel_files = excel_files %>% keep( ~ .x == path)
  }

  excel_files %>%
    walk( ~ {
      import_salurbal_xslx_cdbk(.x) %>%
        jsonlite::write_json(path = str_replace(.x, 'xlsx', 'json'),
                             pretty = T)
      if (!quiet) {
        cli_alert("snapshot {.x} as .json")
      }

    })


}
