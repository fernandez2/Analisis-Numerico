#Rango de Notas 30-40 40-50 50-60 60-70 70-80
#No Estudiantes 35 48 70 40 22
#Se crea la tabla con frecuaencias acumuladas para encontras el total de estudiantes que cumplen el criterio
#Se interpola con lagrange 

x<-c(40,50,60,70,80)
y<-c(35,83,153,193,215)

Ajuste_Polinomio = poly.calc(x,y)
Ajuste_Polinomio
plot(x,y,xlab="notas", ylab="cantidad estudiantes", main="Punto4 ")
points(55,Ajuste_Polinomio(55),col = "red")
curve(Ajuste_Polinomio,add=T,from =40,to =80)
cat("Estudiantes con nota menor a 55: ",Ajuste_Polinomio(55))