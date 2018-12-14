\name{cardata}
\alias{cardata}
\docType{data}
\title{
Second-hand cars
}
\description{
Dataset combining information about all second-hand cars placed as listings on the Paru Vendu French website.
}
\usage{data("cardata")}
\format{
  The format is:
 chr "cardata.rda"
}
\details{
Dataset meant to be used in the shiny app built in the "findyourdreamcar" package.
}
\source{
https://www.paruvendu.fr/
}
\references{
See data cleaning, data scrapind and original scrapped dataset in the data-raw/ folder of the package.
}
\examples{
data(cardata)
head(cardata)
}
\keyword{datasets}
