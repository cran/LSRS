NDVI<-function(a = "NIR", b = "Red") {
  result <- (a-b) / (a+b)
  return(result)
}

SAVI<-function(a="NIR",b="Red"){
  results<- (a-b) / (a+b+.5)*(1+.5)
  return((results))
}

TGSI<-function(a="Red",b="Blue",c="green"){
  result1<- (a-b) / (a+b+c)
  return((result1))
}



NDMI <- function(a="NIR",b="SWIR"){
  resultnd <- (a - b) / (a + b)
  return(resultnd)
}

NBR2=function(a="SWIR1",b="SWIR2"){
  resultnbr <- (a - b) / (a + b)
  return(resultnbr)
}


MSAVI =function(a="NIR",b="Red"){
  result21 <- 2 * (a)+1
  result22 <- sqrt(4 * result21)
  result3 <- 8*(a - b)
  result4 <- result21-result22-result3/2
  result5 <- result4*.0025
  return(result5)
}

EVI =function(a="NIR",b="Red",c="Blue"){
  h<- a-b
  h0<- 2.5*h
  h1<- a+6
  h2<- b-7.5
  h3<- c+1
  enhanc <- h0/h1*h2*h3
  evif= enhanc*0.0001
  return(evif)
}


NBR=function(a="NIR",b="SWIR"){
  resultnbrr <- (a - b) / (a + b)
  return(resultnbrr)
}
