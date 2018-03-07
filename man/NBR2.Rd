\name{Normalized Burn Ratio 2}
\alias{NBR2}
\title{Normalized Burn Ratio 2}
\description{ The Normalized Burn Ratio 2 (NBR2) is calculated as a ratio between the SWIR values, substituting the SWIR1 band for the NIR band used in NBR to highlight sensitivity to water in vegetation.

NBR2= (SWIR1 - SWIR2) / (SWIR1 + SWIR2)
}
\usage{
NBR2(a = "SWIR1", b = "SWIR2")
}
\arguments{
  \item{a}{SWIR1 satellite band (Wavelength:1.566 - 1.651 micrometer) (format:TIF)
}
  \item{b}{SWIR2 satellite band (Wavelength:2.107 - 2.294 micrometer)(format:TIF)
}
}
\references{[1] https://landsat.usgs.gov/sites/default/files/documents/si_product_guide.pdf
}
\author{Mehdi Sarparast}
\examples{
## The function is currently defined as
function (a = SWIR1, b = SWIR2)
{
name=NBR2(a,b)
areaXY <-c(xmin, xmax, ymin, ymax)
cropXY <- crop(name, areaXY)
plot(cropXY,lwd=4,
     main="NBR2",
     xlab="easting", ylab="northing")
hist(cropXY,
     main="NBR2"  ,
     xlab="NBR2",col="red", ylab="Frequency of Pixels")
     }
}
