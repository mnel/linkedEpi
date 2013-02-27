\name{fileseq}
\alias{fileseq}
\alias{fileseq.data.frame}
\alias{fileseq.data.table}
\title{calculate file sequence}
\usage{
  fileseq(data, fileseq.name, safe)

  \method{fileseq}{data.frame} (data,
    fileseq.name = "fileseq", safe = TRUE)

  \method{fileseq}{data.table} (data,
    fileseq.name = "fileseq", safe = TRUE)
}
\arguments{
  \item{data}{a data.frame or data.table}

  \item{fileseq}{character string of length 1 which will be
  the name of the created column}

  \item{safe}{\code{logical} Check whether
  \code{file.seq}-named column exists prior to
  overwriting.}
}
\value{
  a data.frame updated with column \code{file.seq} or the
  data.table with fileseq added by reference.
}
\description{
  The classic fileseq End of record loading. This function
  will create a vector that will identify the row number or
  file sequence id (as per . By setting creating this, you
  can always revert to the original order of your data set
}
\examples{
DF <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
DT <- data.table(DF)
fileseq(DF)
fileseq(DT)
fileseq(DT)
# gives error as fileseq has been assigned by reference
fileseq(DT, safe = FALSE)
}
\seealso{
  \code{\link{morbseq}}
}
\keyword{'End}
\keyword{EOR}
\keyword{check}
\keyword{loading'}
\keyword{of}
\keyword{record}
\keyword{sequences}

