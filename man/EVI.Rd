\name{Enhanced Vegetation Index}
\alias{EVI}
\title{Enhanced Vegetation Index}
\description{In areas of dense canopy where the leaf area index (LAI) is high, the NDVI values can be improved by leveraging information in the blue wavelength. Information in this portion of the spectrum can help correct for soil background signals and atmospheric influences. The enhanced vegetation index (EVI) is an 'optimized' vegetation index designed to enhance the vegetation signal with improved sensitivity in high biomass regions and improved vegetation monitoring through a de-coupling of the canopy background signal and a reduction in atmosphere influences. EVI is computed following this equation:

VALUE = 2.5*(NIR - Red) / (NIR + 6*Red - 7.5*Blue + 1)
}
\usage{
EVI(a="NIR",b="Red",c="Blue",Pixel.Depth)
}
\arguments{
  \item{a}{NIR satellite band (format:TIF)
}
  \item{b}{Red satellite band (format:TIF)
}
  \item{c}{Blue satellite band (format:TIF)
}
  \item{Pixel.Depth}{for satellite image with digital numbers (DN) of 0 to 255,Pixel.Depth=null and for the larger DN, Pixel.Depth=1
  }
}
\note{in some cases for satellite image with very large digital numbers it need     to be multiplied with 0.1,0.01,.001,...
}
\references{ [1] A. Huete, K. Didan, T. Miura, E. P. Rodriguez, X. Gao, L. G. Ferreira. Overview of the radiometric and biophysical performance of the MODIS vegetation indices. Remote Sensing of Environment 83(2002) 195-213.
}
\author{Mehdi Sarparast}
\examples{
## The function is currently defined as
function (a = NIR, b = Red, c = Blue,Pixel.Depth)
{
name=EVI(a,b,c,Pixel.Depth=null)
areaXY <-c(xmin, xmax, ymin, ymax)
cropXY <- crop(name, areaXY)
plot(cropXY,lwd=4,  main="EVI",xlab="easting", ylab="northing")
hist(cropXY,   main="EVI",
     xlab="EVI",col="red", ylab="Frequency of Pixels")
  }
}
