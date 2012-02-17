\name{cleanser_data}
\alias{cleanser_data}
\docType{data}
\title{
Cleanser Example
}
\description{
Hypothetical product with three attributes
}
\usage{data(cleanser_data)}
\format{
  A data frame with 8 observations on the following 5 variables.
  \describe{
    \item{\code{Profile}}{Profile no}
    \item{\code{Form}}{Form of cleanser, two levels: Liquid, Powder}
    \item{\code{Ingredients}}{Core Ingredients, two levels: Phosphate-fee, Phosphate-based}
    \item{\code{Brand}}{Brand name, two levels: HBAT, Generic Band}
    \item{\code{Rank}}{Rank by respondents, Greatest to Least}
  }
}
\details{
Hair et al. 2009, Multivariate Data Analysis, 7/ed, Prenhal
}
\source{
Hair et al. 2009, Multivariate Data Analysis, 7/ed, Prenhal
}
\references{
Hair et al. 2009, Multivariate Data Analysis, 7/ed, Prenhal
}
\examples{
library(faisalconjoint)
data(cleanser_data)
cleanser_data
}

