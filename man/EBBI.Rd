\name{Enhanced Built-Up and Bareness Index}
\alias{EBBI}
\title{ Enhanced Built-Up and Bareness Index }
\description{ Enhanced Built-Up and Bareness Index (EBBI):
   The EBBI is a remote sensing index that applies bands of NIR, SWIR, and TIR.    The NIR and SWIR bands are associated with a high contrast level for            detecting built-up and bare land areas In addition, in these bands, there is    an inverse reflectance ratio with respect to detecting built-up or bare land    areas compared to vegetation. Vegetation has a high reflectance in NIR band,    but the reflectance of built-up or bare land in NIR band is low. subtraction    of NIR band  from SWIR band  will result in positive values for built-up and    barren pixels and will result in negative values for vegetation.
   In addition, a summation of SWIR band  and TIR band  will result in higher      values pixel for built-up and bare land than for vegetation. The difference     between the subtraction of NIR band  from SWIR band  and the summation of       SWIR band  and TIR band  will result in virtually 0 water pixels as well as     negative values for vegetation and positive values for built-up and barren      pixels. This outcome allows for easy distinguishing between built-up and bare    land areas.EBBI can be calculated as:

   EBBI=SWIR-Red/10*sqrt(SWIR+TIR)

    }
\usage{
EBBI(x = "NIR", y = "SWIR", z = "TIR", Pixel.Depth)
}
\arguments{
  \item{x}{Red satellite band (format:TIF)
}
  \item{y}{SWIR satellite band (format:TIF)
}
  \item{z}{TIR satellite band (format:TIF)
}
  \item{Pixel.Depth}{for satellite image with digital numbers (DN) of 0 to 255,Pixel.Depth=null and for the larger DN, Pixel.Depth=1
}
}
\references{[1] As syakur, A.R., Adnyana, I.W.S., Arthana, I.W., Nuarsa, I.W., 2012. Enhanced built-UP and bareness index (EBBI) for mapping built-UP and bare land in an urban area. Remote Sens. 4, 2957-2970. doi:10.3390/rs4102957
}
\author{Mehdi Sarparast}
\examples{
## The function is currently defined as
function (x = "NIR", y = "SWIR", z = "TIR", Pixel.Depth)
{
name <- EBBI (a,b,c,Pixel.Depth=1)
areaXY <-c(xmin, xmax, ymin, ymax)
cropXY <- crop(name, areaXY)
plot(cropXY,lwd=4,main=" EBBI ",
     xlab="easting", ylab="northing")
hist(cropXY,main="EBBI "
     ,
     xlab=" EBBI ",col="red", ylab="Frequency of Pixels")
  }
}

