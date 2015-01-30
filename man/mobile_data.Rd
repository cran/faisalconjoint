\name{mobile_data}
\alias{mobile_data}
\docType{data}
\title{
Mobile Phone Set Data
}
\description{
Data Collected through Sample Survey from Karachi, Pakistan from July 1,2007  by July 31, 2007
}
\usage{data("mobile_data")}
\format{
  A data frame with 16 observations on the following 6 variables.
  \describe{
    \item{\code{Profile}}{a numeric vector containing profile number}
    \item{\code{Brand}}{1= Nokia, 2 = Sony Ericsson, 3 = Samsung, 4 = LG, 5 = Motorola }
    \item{\code{FM.Radio}}{1 = yes, 2 = No}
    \item{\code{Camera}}{1 = yes, 2= No}
    \item{\code{Price}}{Different price level from market}
    \item{\code{Rank}}{customer preference to purchase, Greatest to Least}
  }
}

\examples{
library(faisalconjoint)
data(mobile_data)
mobile_data
}
\keyword{datasets}
