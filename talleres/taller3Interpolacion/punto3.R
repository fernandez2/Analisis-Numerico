
library(PolynomF)

fx<-function(x) exp(x)

puntosx<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
puntosy<-c()
i<-1
cat(puntosx[2])
while(i<12){
  y<-fx(puntosx[i])
  puntosy <- c(puntosy, y)
  i<-i+1
}
cat(puntosy)

DatosX = puntosx[1:11]; DatosY = puntosy[1:11]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio
plot(puntosx,puntosy, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Punto3 ")
points(DatosX,DatosY, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Punto3")
curve(Ajuste_Polinomio,add=T,from =0,to =1)