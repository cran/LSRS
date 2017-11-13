\name{Modified Soil-Adjusted Vegetation Index}
\alias{MSAVI}
\title{Modified Soil-Adjusted Vegetation Index}
\description{The modified soil-adjusted vegetation index (MSAVI) is the soil adjusted vegetation index that seek to address some of the limitation of NDVI when applied to areas with a high degree of exposed soil surface. The problem with the original soil-adjusted vegetation index (SAVI) is that it required specifying the soil-brightness correction factor (L) through trial-and-error based on the amount of vegetation in the study area. Not only did this lead to the majority of people just using the default L value of 0.5, but it also created a circular logic problem of needing to know what the vegetation amount/cover was before you could apply SAVI which was supposed to give you information on how much vegetation there was. Qi et al. (1994a) developed the MSAVI to more reliably and simply calculate a soil brightness correction factor.

   MSAVI=(NIR-Red)/(NIR+Red+L)*(1+L)

   where RED is the red band reflectance from a sensor, NIR is the near infrared band reflectance, and L is the soil brightness correction factor. The difference between SAVI and MSAVI, however, comes in how L is calculated. In SAVI, L is estimated based on how much vegetation there is (but it's generally left alone at a compromise of 0.5). MSAVI uses the following formula to calculate L:

   L=2*s*(NIR-Red)*(NIR-s* Red)/(NIR+Red)

   where s is the slope of the soil line from a plot of red versus near infrared brightness values.
}
\usage{
MSAVI(a = NIR, b = Red)
}
\arguments{
  \item{a}{NIR satellite band (format:TIF)
}
  \item{b}{Red satellite band (format:TIF)
}
}
\references{[1] https://water.usgs.gov/GIS/metadata/usgswrd/XML/msavi92.xml.
[2]http://wiki.landscapetoolbox.org/doku.php/remote_sensing_methods:modified_soil     -adjusted_vegetation_index.
}
\author{Mehdi Sarparast}
\examples{
## The function is currently defined as
function (a = NIR, b = Red)
{
name=MSAVI(a,b)
areaXY <-c(xmin, xmax, ymin, ymax)
cropXY <- crop(name, areaXY)
plot(cropXY,lwd=4, main="MSAVI",
     xlab="easting", ylab="northing")
hist(cropXY,
     main="Histogram of MSAVI",
     xlab="MSAVI",col="red", ylab="Frequency of Pixels")
  }
}


