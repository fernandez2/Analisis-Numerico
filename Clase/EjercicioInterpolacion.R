##Analisis numerico 

install.packages("Matrix")#instalar paquete
library(Matrix)
install.packages("PolynomF")#instalar paquete
library(PolynomF)

#Puntos
hora=c(6,8,10,12,14,16,18,20)                                                                                                       
grados=c(7,9,12,18,21,19,15,10)     


plot(hora,grados, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama ")

#Realizamos el ajuste 

DatosX = hora[1:8]; DatosY = grados[1:8]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio

#ingresamos los ajustes y los puntos en la grafica
plot(hora,grados, pch=19, cex=1, col = "red", asp=1,xlab="hora", ylab="grados", main="Mano derecha")
points(DatosX,DatosY, pch=19, cex=1, col = "red", asp=1,xlab="hora", ylab="grados", main="Mano derecha")
curve(Ajuste_Polinomio,add=T,from =6,to =20)

i=1
cat(hora[i])
while(i<8){
  medio=((hora[i+1])-hora[i])
  cat(hora[i]," ")
  par(new=TRUE)
  points(medio,Ajuste_Polinomio(medio),col="blue")
  i=i+1
}


