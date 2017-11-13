\name{Normalized Difference Vegetation Index}
\alias{NDVI}
\title{Normalized Difference Vegetation Index}
\description{ Normalized Difference Vegetation Index (NDVI) quantifies            vegetation by measuring the difference between near-infrared (which           vegetation strongly reflects) and red light (which vegetation absorbs).       NDVI   always ranges from -1 to +1. But there isn’t a distinct boundary       for each type of land cover. For example, when you have negative values,      it’s highly likely that it’s water. On the other hand, if you have a NDVI      value close to +1, there’s a high possibility that it’s dense green          leaves. But when NDVI is close to zero, there isn’t green leaves and          it could even be an urbanized area.As shown below, Normalized Difference      Vegetation Index (NDVI) uses the NIR and red channels in its formula.
        NDVI=NIR-Red/NIR+Red

        The result of this formula generates a value between -1 and +1. If you      have low reflectance (or low values) in the red channel and high             reflectance in the NIR channel, this will yield a high NDVI value. And        vice versa.Overall, NDVI is a standardized way to measure healthy             vegetation. When you have high NDVI values, you have healthier vegetation.     When you have low NDVI, you have less or no vegetation. satellites like       Sentinel-2, Landsat and SPOT produce red and near infrared images.
}
\usage{
NDVI(a = NIR, b = Red)
}
\arguments{
  \item{a}{NIR satellite band (format:TIF)
}
  \item{b}{Red satellite band (format:TIF)
}
}
\references{[1] http://gisgeography.com/ndvi-normalized-difference-vegetation-index/
}
\author{ Mehdi Sarparast}
\examples{
## The function is currently defined as
function (a = NIR, b = Red)
{
name <- NDVI(a,b)
areaXY <-c(xmin, xmax, ymin, ymax)
cropXY <- crop(name, areaXY)
plot(cropXY,lwd=4,
     main="NDVI",
     xlab="easting", ylab="northing")
hist(cropXY,
     main="Histogram of NDVI"
     ,
     xlab="NDVI",col="red", ylab="Frequency of Pixels")
     }
}
