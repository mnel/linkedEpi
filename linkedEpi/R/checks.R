#' Check whether a data set  identifier occurs only once for each identifier
#'
#' This function will check if a identifier within a \code{data.frame} is unique. This
#' is used, for example, to ensure that merges are performed on a one-to-one or one-to-many
#' basis and not a many-to-many basis that might cause problems
#'
#' @param data data frame to check
#' @param id  character string of length 1 defining the column which contains the unique identifer
#' @return logical TRUE if unique, FALSE if not.
#' @keywords merge check
#' @export
#' @examples
#' df.non <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
#' df.unique <- data.frame(ppn = seq_len(3), var2 = seq_len(3))
#' check.unique(df.non, id = 'ppn')
#' check.unique(df.unique, id = 'ppn')
check.unique <- function(data, id = 'ppn'){
  !any(duplicated(data[[id]]))
}
