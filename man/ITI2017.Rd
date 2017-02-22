\name{ITI2017}
\alias{ITI2017}
\title{Income Tax-India (FY 2016-2017)}
\description{
Calculate Taxable Income and Income Tax liability for Assessment Year 2017-18 (for income earned during Financial Year 01.04.2016 to 31.03.2017) for Individual resident aged below 60 years,Senior Citizen,Super Senior Citizen, Firm, Local Authority, Any NRI / HUF / AOP / BOI / AJP, Co-operative Society.
}
\usage{
ITI2017(income, category)
}
\arguments{
  \item{income}{numeric value > 0, Annual income for the Financial Year (FY) 2016-2017.}
  \item{category}{numeric values between(1-6), Respective category as per their source of Income.}
}
\details{
Calculate Taxable Income and Income Tax liability for Assessment Year 2017-18 (for income earned during Financial Year 01.04.2016 to 31.03.2017) for Individual resident aged below 60 years,Senior Citizen,Super Senior Citizen, Firm, Local Authority, Any NRI / HUF / AOP / BOI / AJP, Co-operative Society.

Abbreviations used :

NRI - Non Resident Individual;

HUF - Hindu Undivided Family;

AOP - Association of Persons;

BOI - Body of Individuals;

AJP - Artificial Judicial Person
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
ITI2017(330000,1)
##Income Tax calculation for group of individuals
employees<-c(250000,350000,200000,7500000)
ITI2017(employees,1)
##Income Tax calculation for dataframe with mixed category of data
sdata <- data.frame(income = c(300000, 400000, 5000000,15000000),
                  category = c(1, 2, 3, 4))
ITI2017(sdata$income,sdata$category)
}

\keyword{Tax}
\keyword{Finance}
