#' Get the UHC server path for a certain file depending on the datasetid
#'
#' This function is useful for generating path names to the UHC server. It will only work if
#' have uploaded your files to the UHC server `\\files.drexel.edu\colleges\SOPH\Shared\UHC\Projects\Wellcome_Trust\Data Methods Core\Dashboards\FAIR Renovations`
#'
#' @param dataset_id the folder name of your dataset in the FAIR Renovation folder.
#' For example `APSL1AD`.
#' @param file the name of file you want. possible options include any of the clean
#' files  (e.g  `var_name.csv`) or raw files (e.g. `APSL1AD_06132022.csv`)
#'
#' @return a path to file you specified
#' @export
#'
#' @examples
#' get_uhc_file_path(dataset_id = "APSL1AD", file = "APSL1AD_06132022.csv")
#' get_uhc_file_path(dataset_id = "APSL1AD", file = "var_name.csv")

# FUNCTIONS
get_uhc_file_path = function(dataset_id,file){
  server_base_dir = "\\\\files.drexel.edu\\colleges\\SOPH\\Shared\\UHC\\Projects\\Wellcome_Trust\\Data Methods Core\\Dashboards\\FAIR Renovations\\"
  server_raw_dir = paste0(server_base_dir,dataset_id,'\\raw-data\\')
  server_clean_dir = paste0(server_base_dir,dataset_id,'\\clean\\')

  if (file %in%c("var_name.csv","codebook.csv","linkages.csv",'strata.csv','data.csv')){
    return(paste0(server_clean_dir,file))
  } else {
    return(paste0(server_raw_dir,file))
  }

}
