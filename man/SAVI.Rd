\name{Soil-Adjusted Vegetation Index}
\alias{SAVI}
\title{Soil-Adjusted Vegetation Index}
\description{ In areas where vegetative cover is low (i.e., < 40 percent)  and     the soil surface is exposed, the reflectance of light in the red and near    -infrared spectra can influence vegetation index values. This is especially     problematic when comparisons are being made across different soil types       that may reflect different amounts of light in the red and near infrared     wavelengths (i.e., soils with different brightness values). The soil          -adjusted vegetation index was developed as a modification of the Normalize    d Difference Vegetation Index to correct for the influence of soil            brightness when vegetative cover is low. The SAVI is structured similar to    the NDVI but with the addition of a “soil brightness correction factor,”

       SAVI=(NIR-Red)/(NIR+Red+L)*(1+L)

    The SAVI is structured similar to the NDVI but with the addition of a         “soil brightness correction factor,” where NIR is the reflectance value of       the near infrared band, RED is reflectance of the red band, and L is the        soil brightness correction factor. The value of L varies by the amount or      cover of green vegetation: in very high vegetation regions, L=0; and in         areas with no green vegetation, L=1. Generally, an L=0.5 works well in             most     situations and is the default value used. When L=0, then SAVI =      NDVI.
}
\usage{
SAVI(a="NIR",b="Red")
}
\arguments{
  \item{a}{NIR satellite band (format:TIF)
}
  \item{b}{Red satellite band (format:TIF)
}
}
\references{[1] http://wiki.landscapetoolbox.org/doku.php/remote_sensing_methods:soil-adjusted_vegetation_index.
}
\author{Mehdi Sarparast}
\examples{
## The function is currently defined as
function (a="NIR",b="Red")
{
name <- SAVI(a,b)
areaXY <-c(xmin, xmax, ymin, ymax)
cropXY <- crop(name, areaXY)
plot(cropXY,lwd=4,
     main="SAVI",
     xlab="easting", ylab="northing")
hist(cropXY,
     main="SAVI"
     ,
     xlab="SAVI",col="red", ylab="Frequency of Pixels")
  }
}
