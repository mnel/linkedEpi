\name{merge.unique}
\alias{merge.unique}
\title{Merge data sets. Check that each data set being merged has a unique
key}
\usage{
  merge.unique(x, y, by, ..., check.unique = TRUE)
}
\arguments{
  \item{x,}{y data frames, or objects to be coerced to one}

  \item{by}{character indicating which column to merge by
  (and check uniqueness)}

  \item{check.unique}{logical; if TRUE, x and y are checked
  to see that the column used for merging is unique}
}
\value{
  Merged data set
}
\description{
  Merge data sets. Check that each data set being merged
  has a unique key
}
\examples{
df.non <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
df.unique <- data.frame(ppn = seq_len(3), var2 = seq_len(3))
merge.unique(df.unique, df.non, by = 'ppn')
# Error: check.unique(y, id = by) is not TRUE
}

