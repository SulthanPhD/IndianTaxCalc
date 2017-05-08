\name{ITI2018}
\alias{ITI2018}
\title{Income Tax-India (FY 2017-2018)}
\description{
Calculate Taxable Income and Income Tax liability for Assessment Year 2018-19 (for income earned during Financial Year 01.04.2017 to 31.03.2018) for Individual resident aged below 60 years,Senior Citizen,Super Senior Citizen, Firm, Local Authority, Any Non Resident Individual / Hindu Undivided Family / Association of Persons /Body of Individuals / Artificial Judicial Person, Co-operative Society.
}
\usage{
ITI2018(income, category)
}
\arguments{
  \item{income}{numeric value > 0, Annual income for the Financial Year (FY) 2017-2018.}
  \item{category}{numeric values between(1-6), Respective category as per their source of Income.}
}
\details{
Calculate Taxable Income and Income Tax liability for Assessment Year 2017-18 (for income earned during Financial Year 01.04.2017 to 31.03.2018) for Individual resident aged below 60 years,Senior Citizen,Super Senior Citizen, Firm, Local Authority, Any Non Resident Individual / Hindu Undivided Family / Association of Persons /Body of Individuals / Artificial Judicial Person, Co-operative Society.

}
\value{
 Categories details:
    \item{1}{Individual resident aged below 60 years (Male and Female)}
    \item{2}{Senior Citizen}
    \item{3}{Super Senior Citizen}
    \item{4}{Any NRI or HUF or AOP or BOI or AJP}
    \item{5}{Co-operative Society}
    \item{6}{Firm,Local Authority}

  A list containing the following components:
  \item{IT}{Income tax}
  \item{TI}{Taxable income}
  \item{EC}{Education Cess}
  \item{SC}{Surcharge}
  \item{MR}{Marginal Relief (if applicable)}
  \item{NSC}{Net Surcharge}
}
\references{
www.incometaxindiaefiling.gov.in
}
\author{
Sulthan
}
\note{
Will be updated with every years Tax policies
}


\examples{
##Income Tax calculation for individual
ITI2018(330000)

##Income Tax calculation for Senior Citizen
ITI2018(580000,2)

##Income Tax calculation for group of individuals
employees<-c(250000,350000,200000,7500000)
ITI2018(employees,1)

##Income Tax calculation for dataframe with mixed category of data
sdata <- data.frame(income = c(300000, 600000, 5000000,15000000),
                  category = c(1, 2, 3, 4))
ITI2018(sdata$income,sdata$category)
}

\keyword{Tax}
\keyword{Finance}
