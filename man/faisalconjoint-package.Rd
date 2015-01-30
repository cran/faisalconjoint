\name{faisalconjoint-package}
\alias{faisalconjoint-package}
\alias{faisalconjoint}
\docType{package}
\title{
Faisal Conjoint Model: A New Approach of Conjoint Analysis
}
\description{
Faisal Conjoint Model (FCM) is a conjoint model, developed by Faisal Afzal Siddiqui, Ghulam Hussain, and Mudassiruddin in 2011. Its algorithm was written in R statistical language. It is independent from any design.  It could be used for any research design, i.e., randomization, replication, blocking, orthogonal, factorial, fuzzy, etc. Another important point about FCM is rank procedure. It works for every kind of ranks i.e. unique ranks, percentage ranks, tight ranks, missing ranks etc.
}
\details{
\tabular{ll}{
Package: \tab faisalconjoint\cr
Type: \tab Package\cr
Version: \tab 1.15\cr
Date: \tab 2015-01-30\cr
License: \tab GPL-3\cr
}
Conjoint Analysis is a technique that allows you to work out the hidden rules people use to make trade-offs between different products and services and the values they place on different features. By understanding precisely how people make decisions and what they value in your products and services, you can work out the optimum level of features and services that balance value to the customer against cost to the company.
}
\author{
Faisal Afzal Siddiqui, Ghulam Hussain, Mudassir Uddin

Maintainer: Faisal Afzal Siddiqui <brc.khi@gmail.com>
}


\keyword{conjoint, faisal comjoint model, rank, preferences, buying behavior, factor importance, part-worth utilities, package }

\examples{
# Example 1: Mobile Phone Set, Conjoint Analysis Examples
library(faisalconjoint)
data(mobile_data)
data(mobile_levels)
faisalconjoint(mobile_data,mobile_levels)
}
