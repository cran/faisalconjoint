\name{carpet_data}
\alias{carpet_data}
\docType{data}
\title{
Carpet Data
}
\description{
Carper research design (orthogonal design)
}
\usage{data(carpet_data)}
\format{
  A data frame with 16 observations on the following 7 variables.
  \describe{
    \item{\code{Profile}}{Profile no}
    \item{\code{Package}}{package design has three levels; A, B, C}
    \item{\code{Brand}}{brand name has three levels; K2R, Glory, Bissell}
    \item{\code{Price}}{has three levels; $1.19, $1.39, $1.59}
    \item{\code{Seal}}{Good Housekeeping seal has two levels, No, Yes}
    \item{\code{Money}}{money-back guarante has two levels; No, Yes}
    \item{\code{Ranks}}{Rank by respondents, Greatest to Least}
  }
}
\details{
http://www.unileon.es/ficheros/servicios/informatica/spss/english/IBM-SPSS_conjoint.pdf
}
\source{
SPSS 19 
}
\references{
Green, P. E. and Y. Wind (1973), Multi-Attribute Decisions in Marketing. New York: Holt, Rinehart & Winston
}
\examples{
library(faisalconjoint)
data(carpet_data)
carpet_data
}
