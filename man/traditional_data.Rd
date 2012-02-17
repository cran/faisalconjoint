\name{traditional_data}
\alias{traditional_data}
\docType{data}
\title{
Traditional Conjoint Analysis 
}
\description{
Traditional Conjoint Analysis with Excel
}
\usage{data(traditional_data)}
\format{
  A data frame with 18 observations on the following 5 variables.
  \describe{
    \item{\code{Profile}}{Profile no}
    \item{\code{Brand}}{Brand name, three levels: A, B, C}
    \item{\code{Color}}{Color, two level: Red, Blue}
    \item{\code{Price}}{Price, three levels: $50, $100, $150}
    \item{\code{Ranks}}{Rank by respondents, Greatest to Least}
  }
}
\details{
http://www.sawtoothsoftware.com/download/techpap/caexcel.pdf
}
\source{
Orme, B. (2010) Getting Started with Conjoint Analysis: Strategies for Product Design and Pricing Research. Second Edition, Madison, Wis.: Research Publishers LLC.
}

\references{Orme, B. (2010) Getting Started with Conjoint Analysis: Strategies for Product Design and Pricing Research. Second Edition, Madison, Wis.: Research Publishers LLC.}

\examples{
library(faisalconjoint)
data(traditional_data)
traditional_data
}

