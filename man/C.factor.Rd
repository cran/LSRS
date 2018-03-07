\name{cover management factor in RUSLE model}
\alias{C.factor}
\title{ cover management factor in RUSLE model }
\description{ cover management factor in RUSLE model (C factor): Many people in     the erosion control industry use the Revised Universal Soil Loss Equation       (RUSLE) to estimate soil loss caused by rainfall erosion. The equation          combines the interrelated physical and management parameters of climate,        soils, topography, and land use. The full equation is:

    A = R*K*LS*C*P

    C factor : Cover Management
    Cover management is considered by RUSLE through the C factor. The C factor      represents the effect of surface cover and roughness on soil erosion. The       cover factor is the most common factor used to assess the impact of BMPs on     reducing erosion because the C factor represents the effect of land use on      soil erosion (Renard et al., 1997). Erosion control blankets and surface       applied BMPs such as blown straw are represented as C factors within RUSLE.     By definition, C = 1 under standard fallow conditions. As surface cover is      added to the soil, the C factor value approaches zero.
    The unitless cover management factor can be determined in different ways.       The     remote sensing data, useful for reconstructing the temporal             variation of C factor. The relationship between the C factor and NDVI is the     same applied to the regional scale that is the following (Knijff et al.,        2000):

    C=exp(2.5* NDVI/1-NDVI)

   And  another equation (Durigon et al., 2014):

    c=[(-1*NDVI+0.5)]
}
\usage{
C.factor(a = "NIR", b = "Red", method, na.rm = TRUE)
}
\arguments{
  \item{a}{NIR satellite band (format:TIF)
}
  \item{b}{Red satellite band (format:TIF)
}
  \item{method}{based on Knijff or Durigon equation
}
  \item{na.rm}{remove missing values
}
}
\references{[1] Dudigon,v.l., Carvalho, D.F., Antunes, M.A.H., Oliveira, P.T.S., Fernandes, M.M. NDVI time series for monitoring RUSLE cover management    factor in a tropical watershed. International Journal of Remote Sensing, v.35, p.441-453, 2014. DOI: 0.1080/01431161.2013.871081. [2] Knijff, van der J.M., Jones R.J.A. and Montanarella L., 2000. Soil Erosion Risk Assessment in Italy. European Soil Bureau, JRC, Space Applications Institute.
}
\author{Mehdi Sarparast}
\examples{
## The function is currently defined as
function (a = "NIR", b = "Red", method, na.rm = TRUE)
{
    name <- C.factor (a,b, method="Knijff",na.rm = TRUE)
areaXY <-c(xmin, xmax, ymin, ymax)
cropXY <- crop(name, areaXY)
plot(cropXY,lwd=4,main=" C.factor ",
     xlab="easting", ylab="northing")
hist(cropXY,main=" C.factor "
     ,
     xlab=" C.factor ",col="red", ylab="Frequency of Pixels")
  }
}
