\name{Normalized Burn Ratio }
\alias{NBR}
\title{Normalized Burn Ratio}
\description{ The Normalized Burn Ratio (NBR) was designed to highlight burned areas and estimate fire severity. The formula is similar to NDVI, except that it uses near-infrared (NIR) and shortwave-infrared (SWIR) wavelengths.

NBR=(NIR - SWIR) / (NIR + SWIR)
}
\usage{
NBR(a = NIR, b = SWIR)
}
\arguments{
  \item{a}{NIR satellite band (format:TIF)
}
  \item{b}{SWIR satellite band (format:TIF)
}
}
\references{[1] USGS burn severity - Overview of applied remote sensing principles.
[2] Norton, J.M. 2006. The use of remote sensing indices to determine wildland burn severity in semiarid sagebrush steppe rangelands using Landsat ETM+ and SPOT 5. MS Thesis, Idaho State University.
}
\author{Mehdi Sarparast}
\examples{
## The function is currently defined as
function (a = NIR, b = SWIR)
{
name=NBR(a,b)
areaXY <-c(xmin, xmax, ymin, ymax)
cropXY <- crop(name, areaXY)
plot(cropXY,lwd=4,
     main="NBR",
     xlab="easting", ylab="northing")
hist(cropXY,
     main="Histogram of NBR"
     ,
     xlab="NBR",col="red", ylab="Frequency of Pixels")
     }
}


