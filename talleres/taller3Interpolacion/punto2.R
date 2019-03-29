 

x<-c(100,200,300,400,500,600)
y<-c(-160,-35,-4.2,9,16.9,21.3)

#USANDO SPLINES
splines<-splinefun(x,y,method="monoH.FC",ties=mean)
splines
plot(x,y,xlab="temperatura", ylab="segundo coeficiente virial", main="Punto2 ")
points(450,splines(450),col = "red")
curve(splines(x),add=T,from =100,to =600)
cat("Segundo coeficiente virial a 450K es aproximadamente: ",splines(450), "cm^3/mol"," con splines")

#USANDO LAGRANGE
lagrange<-poly.calc(x,y)
lagrange
#plot(x,y,xlab="temperatura", ylab="segundo coeficiente virial", main="Punto2 ")
points(450,lagrange(450),col = "blue")
curve(lagrange(x),add=T,from =100,to =600)
cat("Segundo coeficiente virial a 450K es aproximadamente: ",lagrange(450), "cm^3/mol"," con lagrange")

#Como podemos observar lagrange no se ajusta de manera apropiada a los puntos y por lo tanto da un numero equivocado