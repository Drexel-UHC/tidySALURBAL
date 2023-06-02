#' Checks CSV for UTF8
#'
#' Takes in a CSV path  that CSV is fully UTF-8 encoded
#'
#' @param csv_path thepath to the csv to check
#'
#' @return returns boolean TRUE or FALSE
#' @export
#'
#' @examples
#' validate_csv_utf8(csv_path = "raw__codebook.csv")


validate_csv_utf8 =function(csv_path){

    file_text <- readLines(csv_path, encoding = "UTF-8")

    valid_encoding <- all(utf8::utf8_valid(file_text))

    return(valid_encoding)
  }

