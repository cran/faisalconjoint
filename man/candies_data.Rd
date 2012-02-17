\name{candies_data}
\alias{candies_data}
\docType{data}
\title{
Chocolate Candy
}
\description{
Metric Conjoint Analysis Example
}
\usage{data(candies_data)}
\format{
  A data frame with 8 observations on the following 5 variables.
  \describe{
    \item{\code{Profile}}{Profile no}
    \item{\code{Chco}}{Chocolate Type, two levels: Dark, Milk}
    \item{\code{Center}}{Center, two levels: Hard, Soft}
    \item{\code{Nuts}}{Nuts, two levels: Nuts, No nuts}
    \item{\code{Ranks}}{Rank by respondents, Greatest to Least}
  }
}
\details{
SAS Technical Report R-109 was created and written by Warren F. Kuhfeld. Development and support of the TRANSREJG procedure is the responsibility of Warren F. Kuhfeld.
available at : http://support.sas.com/documentation/onlinedoc/v82/techreport_r109.pdf 
}
\source{
SAS
}
\references{
SAS Technical Report R-l 09
}
\examples{
library(faisalconjoint)
data(candies_data)
candies_data
}
