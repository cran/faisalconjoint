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
Version: \tab 1.10\cr
Date: \tab 2012-06-03\cr
License: \tab GPL-3\cr
}
It has been tested with many published data (SPSS 19, SAS 9.3, Baheri et al.,  2011, Orme 2010, Hair  et al. 2009, Chan 2005, Green and Wind 1973, Green and Wind 1975, SAS Technical Report 1993, http://www.marketingengineering.com, www.sawtoothsoftware.com,) .  Most of time the results are same with same magnitudes, and some time the rank of results are in same order with different magnitudes.
}
\author{
Faisal Afzal Siddiqui, Ghulam Hussain, Mudassir Uddin

Maintainer: Faisal Afzal Siddiqui <brc.khi@gmail.com>

}
\references{
SPSS 19
SAS 9.3
Baheri et al.,  2011
Orme 2010
Hair  et al. 2009
Chan 2005
Green and Wind 1973
Green and Wind 1975
SAS Technical Report 1993
http://www.marketingengineering.com
www.sawtoothsoftware.com
}

\keyword{ conjoint, conjoint model, conjoint analysis, faisal conjoint, faisal conjoint model }

\examples{
# Example 1: SAS Teclmical Report R-109, Conjoint Analysis Examples
library(faisalconjoint)
data(candies_data)
data(candies_levels)
faisalconjoint(candies_data,candies_levels)

# Example 2: Hair et al. 2009, Multivariate Data Analysis, 5/ed, Prenhall
library(faisalconjoint)
data(cleanser_data)
data(cleanser_levels)
faisalconjoint(cleanser_data,cleanser_levels)

# Example 3: SPSS 19 Conjoint (2011)
library(faisalconjoint)
data(carpet_data)
data(carpet_levels)
faisalconjoint(carpet_data,carpet_levels)

# Example 4: Orme, B. (2010) Getting Started with Conjoint Analysis
library(faisalconjoint)
data(traditional_data)
data(traditional_levels)
faisalconjoint(traditional_data,traditional_levels)
}

