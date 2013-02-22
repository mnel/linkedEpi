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

#' Merge data sets. Check that each data set being merged has a unique
#' key
#'
#' @param x, y data frames, or objects to be coerced to one
#' @param check.unique logical; if TRUE, x and y are checked to see that
#' the column used for merging is unique
#' @return Merged data set
#' @export
#' @examples
#' df.non <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
#' df.unique <- data.frame(ppn = seq_len(3), var2 = seq_len(3))
#' merge.unique(df.unique, df.non, by = 'ppn')
#' # Error: check.unique(y, id = by) is not TRUE
merge.unique <- function(..., check.unique=TRUE)
{
  args = list(...)
  x = args[[1]]
  y = args[[2]]
  by = args[["by"]]
  stopifnot(check.unique(x, id=by), check.unique(y, id=by))
  return(do.call(merge, args))
}
