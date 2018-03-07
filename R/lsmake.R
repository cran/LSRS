
NDVI= function(a = "NIR", b = "Red") {
  result <- (a-b) / (a+b)
  return(result)
}




SAVI= function(a="NIR",b="Red"){
  results<- (a-b) / (a+b+.5)*(1+.5)
  return(results)
}




EBBI=function(x="NIR", y="SWIR",z="TIR",Pixel.Depth){
  if(is.null(Pixel.Depth)){
    test <- y-x
    trau <- y+z
    root <- sqrt(trau)
    mroot <- 10*root
    final <- test/mroot
    return(final)
  }
  else{
    test <- y-x
    trau <- y+z
    root <- sqrt(trau)
    mroot <- 10*root
    final <- test/mroot
    finally=final*0.1
    return(finally)
  }
}




C.factor<-function(a = "NIR", b = "Red",method,na.rm = TRUE) {
  if(method=="Knijff"){
    x <- (a-b)
    y <- (a+b)
    ndvi <- x/y
    top= 2.5*(ndvi)
    bel= 1-ndvi
    fin= top/bel
    finalc= exp(fin)
    finalg=finalc*0.1
    return(finalg)}
  if (method=="Durigon"){
    x <- (a-b)
    y <- (a+b)
    ndvi <- x/y
    g=abs(-1*ndvi+.5)
    return(g)
  }
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



MSAVI =function(a="NIR",b="Red",Pixel.Depth){
  if(!is.null(Pixel.Depth)){
    result21 <- 2 * (a)+1
    result22 <- sqrt(4 * result21)
    result3 <- 8*(a - b)
    result4 <- result21-result22-result3/2
    result5 <- result4*.0025
    return(result5)
  }
  else{
    result21 <- 2 * (a)+1
    result22 <- sqrt(4 * result21)
    result3 <- 8*(a - b)
    result4 <- result21-result22-result3/2
    result5 <- result4*.0025
    result6 <- result5* 0.01
    return(result6)
  }
}



EVI =function(a="NIR",b="Red",c="Blue",Pixel.Depth){
  if(is.null(Pixel.Depth)){
    h<- a-b
    h0<- 2.5*h
    h1<- a+6
    h2<- b-7.5
    h3<- c+1
    enhanc <- h0/h1*h2*h3
    evif= enhanc*0.0001
    hac=evif/2
    return(hac)
  }
  else{
    h<- a-b
    h0<- 2.5*h
    h1<- a+6
    h2<- b-7.5
    h3<- c+1
    enhanc <- h0/h1*h2*h3
    evif= enhanc*0.0001
    fil= evif*0.0001
    return(fil)
  }
}



NBR=function(a="NIR",b="SWIR"){
  resultnbrr <- (a - b) / (a + b)
  return(resultnbrr)
}

