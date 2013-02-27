\name{check.exists}
\alias{check.exists}
\title{Helper function for error messages}
\usage{
  check.exists(what, where)
}
\arguments{
  \item{what}{character vector of columns}

  \item{where}{a data.frame, data.table, list or
  environment}
}
\description{
  Simple wrapper to return a meaningful error when checking
  for existing columns prior to overwriting
}

