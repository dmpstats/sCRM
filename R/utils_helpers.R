#' Convert variable name to id, suitable for use in coding
#'
#' @description 
#' Converts variable labels (e.g. species or windfarm names) to an id formatted
#' so it can be used in coding (i.e. replace white spaces and dashes with
#' underscores). The function's main intention is to standardize label-to-id
#' conversion
#'
#' @param label a character string, the name/label of the variable
#' 
#' @return character string, the variable's id, to use in coding
label2id <- function(label){
  gsub("\\s|-", "_", label)
}


#' Special concatenation
#' 
#' Concatenate multiple strings, using "," to separate intermediate strings and
#' "and" as the last separator
#' 
#' @param vec a character vector
#' 
#' @return a character string with elements of `vec` concatenated using "," and
#'   "and" separators accordingly
#'   
#' @source <https://stackoverflow.com/questions/42456452/changing-vector-to-string-special-separator-for-last-element>
fPaste <- function(vec) sub(",\\s+([^,]+)$", " and \\1", toString(vec))



#' Generate a tag of current time, e.g. to add to filenames
time_tag <- function(){
  format(Sys.time(), "%d%m%Y_%H%M")
}
