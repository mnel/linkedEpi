\name{morbseq}
\alias{morbseq}
\title{Create a morbseq end of record loading}
\usage{
  morbseq(data, id, morbseq.name = "morbseq", safe = TRUE,
    ...)
}
\arguments{
  \item{data}{a data set (data.table or data.frame)}

  \item{id}{\code{character} length 1, string identifying
  column in data to create morbseq on}

  \item{morbseq.name}{the name of the created column}

  \item{safe}{\code{logical} check if \code{morbseq.name}
  column exists already, and return error if TRUE}
}
\description{
  A function to create a plain morbseq EOR based on a
  single id field
}
\examples{
DF <- data.frame(ppn = c(1L,1L,2L), var1 = seq_len(3))
DT <- data.table(DF)
morbseq(DF, id = 'ppn')
morbseq(DT, id = 'ppn')
}
\seealso{
  \code{\link{fileseq}}
}

