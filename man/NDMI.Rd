\name{Normalized Difference Moisture (Water) Index }
\alias{NDMI}
\title{Normalized Difference Moisture (Water) Index}
\description{ The Normalized Difference Moisture (Water) Index (NDMI or NDWI) is a satellite-derived index from the Near-Infrared (NIR) and Short Wave Infrared (SWIR) channels. The SWIR reflectance reflects changes in both the vegetation water content and the spongy mesophyll structure in vegetation canopies, while the NIR reflectance is affected by leaf internal structure and leaf dry matter content but not by water content. The combination of the NIR with the SWIR removes variations induced by leaf internal structure and leaf dry matter content, improving the accuracy in retrieving the vegetation water content. The amount of water available in the internal leaf structure largely controls the spectral reflectance in the SWIR interval of the electromagnetic spectrum. SWIR reflectance is therefore negatively related to leaf water content. NDWI is computed using the near infrared (NIR) and the short wave infrared (SWIR) reflectance’s:

NDMI=(NIR - SWIR) / (NIR + SWIR)
}
\usage{
NDMI(a = "NIR", b = "SWIR")
}
\arguments{
  \item{a}{NIR satellite band (format:TIF)
}
  \item{b}{SWIR satellite band (format:TIF)
}
}
\references{[1] Gao, B.C. 1996. NDWI : A normalized difference water index for remote sensing of vegetation liquid water from space. Remote Sensing of Environment 58: 257-266
}
\author{Mehdi Sarparast}
\examples{
## The function is currently defined as
function (a = "NIR", b = "SWIR")
{
   name <- NDMI(a,b)
areaXY <-c(xmin, xmax, ymin, ymax)
cropXY <- crop(name, areaXY)
plot(cropXY,lwd=4,
     main="NDMI",
     xlab="easting", ylab="northing")
hist(cropXY,
     main="NDMI"
     ,
     xlab="NDMI",col="red", ylab="Frequency of Pixels")
  }
}
