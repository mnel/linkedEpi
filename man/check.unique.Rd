\name{check.unique}
\alias{check.unique}
\title{Check whether a data set  identifier occurs only once for each identifier}
\usage{
  check.unique(data, id = "ppn")
}
\arguments{
  \item{data}{data frame to check}

  \item{id}{character string of length 1 defining the
  column which contains the unique identifer}
}
\value{
  logical TRUE if unique, FALSE if not.
}
\description{
  This function will check if a identifier within a
  \code{data.frame} is unique. This is used, for example,
  to ensure that merges are performed on a one-to-one or
  one-to-many basis and not a many-to-many basis that might
  cause problems
}
\examples{
df.non <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
df.unique <- data.frame(ppn = seq_len(3), var2 = seq_len(3))
check.unique(df.non, id = 'ppn')
check.unique(df.unique, id = 'ppn')
}
\keyword{check}
\keyword{merge}

