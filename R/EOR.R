#' calculate file sequence 
#'
#' The classic fileseq End of record loading. 
#' This function will create a vector that will identify the row number or file sequence id (as per .
#' By setting creating this, you can always revert to the original order of your data set
#' 
#'
#' @param data a data.frame or data.table
#' @param fileseq  character string of length 1 which will be the name of the created column 
#' @param safe \code{logical} Check whether \code{file.seq}-named column exists prior to overwriting.
#' @return a data.frame updated with column \code{file.seq} or the data.table with fileseq added by reference.
#' @keywords 'End of record loading' EOR sequences check
#' @seealso \code{\link{morbseq}}
#' @export fileseq 
#' @examples
#' DF <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
#' DT <- data.table(DF)
#' fileseq(DF)
#' fileseq(DT)
#' fileseq(DT)
#' # gives error as fileseq has been assigned by reference
#' fileseq(DT, safe = FALSE)
#' 
fileseq <- function(data,fileseq.name, safe){
  UseMethod('fileseq')
}
#'
#' @rdname fileseq
#' @method fileseq data.frame
#' @S3method fileseq data.frame
fileseq.data.frame <- function(data, fileseq.name = 'fileseq', safe = TRUE){
 # `safety` checks
 if(safe){
   check.exists(fileseq.name, data)
 }
  data[[fileseq.name]] <- seq_len(nrow(data))
  return(data)
}
#'
#' @rdname fileseq
#' @method fileseq data.table
#' @S3method fileseq data.table
#' @import data.table
fileseq.data.table <- function(data, fileseq.name = 'fileseq', safe = TRUE){
  if(safe){
    check.exists(fileseq.name, data)
  }
  data[, c(fileseq.name) :=  seq_len(nrow(data)), with = FALSE]
 
 }
#'
#'
#' Create a morbseq end of record loading
#' 
#' A function to create a plain morbseq EOR based on a single id field
#' 
#' @param data a data set (data.table or data.frame)
#' @param id \code{character} length 1, string identifying column in data to create morbseq on
#' @param morbseq.name the name of the created column
#' @param safe \code{logical} check if \code{morbseq.name} column exists already, and return error if TRUE
#' @export morbseq
#' @seealso \code{\link{fileseq}}
#' @examples
#' DF <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
#' DT <- data.table(DF)
#' morbseq(DF, id = 'ppn')
#' morbseq(DT, id = 'ppn')
#'
morbseq <-  function(data, id,morbseq.name = 'morbseq',safe = TRUE,... )
  UseMethod('morbseq')
#'
#'
#'
morbseq.data.frame <- function(data, id, morbseq.name = 'morbseq', safe = TRUE,...){
  
  if(missing(id)) stop('id argument must be specifed')
  if(length(id) > 1) stop('id argument must be length 1')
  if(is.character(id) && !exists(id,data)) stop('id column must exist within data')

  if(safe){
     check.exists(morbseq.name, data)
  }
  
  data[[morbseq.name]]  <- ave(x = data[[id]], data[[id]], FUN = seq_along)
  data 
}
#'
#'
#'
morbseq.data.table <- function(data, id, morbseq.name = 'morbseq', safe = TRUE, ...){
  stop(missing(id), 'id argument must be specified')
  stop(length(id) ==1, 'id argument must be length 1')
  stop(!exists(id, data), 'id column must exit in data')
  if(safe){
    check.exists(morbseq.name, data)
  }
  data[, c(morbseq.name) :=  seq_len(.N), by = id, with = FALSE]
  
}

#' Helper function for error messages
#' 
#' Simple wrapper to return a meaningful error when checking
#' for existing columns prior to overwriting
#' 
#' @param what character vector of columns
#' @param where a data.frame, data.table, list or environment
check.exists <- function(what, where){
  message.text <-'Column%s: %s already exist within %s'
  w <- sapply(what, exists, where = where)
  if(any(w)){
    .w <- paste(what[w], collapse = ', ')
    plural <- ifelse(sum(w)>1, 's', '')
    .where <- deparse(substitute(where))
    stop(sprintf(message.text,plural, .w, .where))
  }
}


