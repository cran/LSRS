\name{Topsoil Grain Size Index }
\alias{TGSI}
\title{Topsoil Grain Size Index}
\description{Topsoil Grain Size Index (TGSI): Topsoil Grain Size Index (TGSI) was developed based on the field survey of soil surface spectral reflectance and laboratory analyses of soil grain composition. Even one rainfall can significantly increase the vegetation cover. Thus, NDVI is misinterpret the actual degree of desertification (Xiao et al., 2006). To overcome this problem, Xiao et al. (2006) proposed a new index, topsoil grain size index (TGSI), which is associated with the mechanical composition of topsoil. It indicates the coarsening of topsoil grain size which has a positive correlation with fine sand content (this class of sand is dominated by the finer sizes of sand particle, and somewhat less coarse than either sand or coarse sand) of surface soil texture, as a manifestation of undergoing desertification. The more severe the desertification, the coarser the topsoil grain size composition (grain size refers to the mean or effective diameter of individual mineral grains or particles. A high TGSI value corresponds to an area with high content of fine sand in the topsoil or low content of clay-silt grains. The TGSI can be calculated as:

    TGSI= (Red-Blue)/(Red+Blue+green)

    where R, B, and G are the red, blue, and green bands of the remote sensing data, respectively. TGSI is an index used to detect the texture of the topsoil layer, or grain size. Negative values or those near 0 repre- sent zones with vegetation or water bodies and values near 0.20 indicate high contents of fine sand.
}
\usage{
TGSI(a = Red, b = Blue, c = green)
}
\arguments{
  \item{a}{Red satellite band (format:TIF)
}
  \item{b}{Blue satellite band (format:TIF)
}
  \item{c}{green satellite band (format:TIF)
}
}
\references{[1] Xiao, J., Shen, Y., Tateishi, R., et al., 2006. Development of topsoil grain size index for monitoring desertification in arid land using remote sensing. Int J. Remote Sens. 27, 2411–2422.
[2] Lamchin, M., Lee, J.Y., Lee, W.K., Lee, E.J., Kim, M., Lim, C.H., Choi, H.A. and Kim, S.R., 2016. Assessment of land cover change and desertification using remote sensing technology in a local region of Mongolia. Advances in Space Research, 57(1), pp.64-77.
}
\author{Mehdi Sarparast}
\examples{
## The function is currently defined as
function (a = Red, b = Blue, c = green)
{
name <- TGSI(a,b,c)
areaXY <-c(xmin, xmax, ymin, ymax)
cropXY <- crop(name, areaXY)
plot(cropXY,lwd=4,main="TGSI",
     xlab="easting", ylab="northing")
hist(cropXY,main="Histogram of TGSI"
     ,
     xlab="TGSI",col="red", ylab="Frequency of Pixels")
  }
}
