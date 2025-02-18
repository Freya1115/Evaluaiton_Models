% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CV.R
\name{CV}
\alias{CV}
\title{CV:Coefficient of variation method}
\usage{
CV(data)
}
\arguments{
\item{data}{a numeric data frame or tibble}
}
\value{
a data frame that contains:mean,std,CV,w
}
\description{
CV is a method that can be used to determine indicator weights.
}
\examples{
data<-iris[,-5]
CV(data)
}
