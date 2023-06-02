#' Takes in a codebook object and compiles the public jinja.
#'
#'
#'
#' @param active__codebook: the active codebook to compile
#' @param stage: the stage of codebook being processed
#' @param i: dataset import object
#'
#' @return returns a dataframe object that has the public compiled from our pojrect dictionary.
#' @export
#'
#' @examples
#' export_human_codebooks()

compile_public_jinja <- function(active__codebook, stage, i){


  { # Setup -------------------------------------------------------------------

    ## Table information
    raw_item = active__codebook %>% keep(~any(names(.x)=='public'))
    raw_table_name = names(raw_item)
    raw_table = raw_item[[1]]
    raw_table_pk = names(raw_table) %>% keep(~.x%in%i$xwalk_keys$keys)


    ## Jinja setup
    jinja_pattern = "\\{\\{.*\\}\\}"
    raw_jinja = raw_table %>% filter(str_detect(public,jinja_pattern)) %>% rename(key = public)
    raw_non_jinja = raw_table %>% filter(!str_detect(public,jinja_pattern))
    raw_jinja_pk = names(raw_jinja) %>% keep(~.x%in%i$xwalk_keys$keys) %>% discard(~.x=='dataset_id')
    raw_key_tmp = raw_jinja$key %>% unique()

    ## QC
    if (nrow(raw_jinja)==0|stage!='base')return(active__codebook)
  }




  { # Prep src crosswalk ------------------------------------------------------
    keys_tmp =  c('key',raw_jinja_pk,'public_value')

    public_src_item = i$src %>%
      keep(~'public_value'%in%names(.x)) %>%
      keep(~raw_key_tmp%in%unique(.x$key))
    if (length(public_src_item) != 1){
      cli_alert_danger("ERROR compile_public_jinja: invalid source table for this key/value pair! Either missing or multiple available! Need checking!")
      stop()
    }

    xwalk_public = public_src_item[[1]] %>%
      select(-any_of('source_value')) %>%
      filter(key%in%raw_key_tmp) %>%
      mutate(public_value = paste(key,public_value) %>%
               str_replace( "\\{\\{", "{") %>%
               str_replace( "\\}\\}", "}")) %>%
      distinct() %>%
      unpack_year()

    if (!'year'%in%raw_table_pk&&raw_key_tmp=='{{census}}') xwalk_public = xwalk_public %>% select(-year) %>% distinct()
  }

  { # Compile  ----------------------------------------------------------
    compiled_jinja = raw_jinja %>%
      left_join(xwalk_public) %>%
      select(-key) %>%
      rename(public = public_value)
  }

  { # QC ----------------------------------------------------------------------

    df_missing_upstream = compiled_jinja %>%
      filter(is.na(public)) %>%
      left_join(
        raw_jinja %>%
          select(any_of(c('dataset_id',  raw_jinja_pk, 'key'))))
    if(nrow(df_missing_upstream) > 0){
      cli_alert_danger('Jinja keys missing dictionary values!')
      print(df_missing_upstream)
      stop()
    }
  }





  { # Return ------------------------------------------------------------------
    compiled__src_table = bind_rows(raw_non_jinja,compiled_jinja)%>%
      arrange(across(all_of(raw_table_pk)))

    compiled__codebook = active__codebook
    compiled__codebook[[raw_table_name]] = compiled__src_table
    return(compiled__codebook)
  }

}
