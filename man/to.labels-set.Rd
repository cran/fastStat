% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to.labels-set.R
\name{to.labels<-}
\alias{to.labels<-}
\title{Give Labels to Factor}
\usage{
to.labels(x) <- value
}
\arguments{
\item{x}{factor or numeric variable}

\item{value}{labels separated by colon}
}
\value{
factor variable with lables, the first lable will be treated as reference.
}
\description{
Give Labels to Factor
}
\examples{
to.labels(x=mtcars$am) <- c('0:Female','1:Man')
}
