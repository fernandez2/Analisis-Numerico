
f1 <- function(x) exp(x)
f2 <- function(x) 1/x
p1 <- taylor(f1, 0, 4) #sumatoria(n=0,inf) x^n/n!
p1=signif(p1,digits=5)


p2 <- taylor(f2, 1, 4)#1-(x-1)+(x-1)^2-(x-1)^3....
p2=signif(p2,digits=5)

par(mfrow=c(1,2))
x <- seq(-1.0, 1.0, length.out=100)
yf <- f1(x)
yp <- polyval(p1, x)
plot(x, yf, type = "l", col = "blue", lwd = 3, main="e^x ")
lines(x, yp, col = "red")
grid()

x2 <- seq(-1.0, 1.0, length.out=100)
yf2 <- f2(x2)
yp2 <- polyval(p2, x2)
plot(x2, yf2, type = "l", col = "blue", lwd = 3,main="1/x ")
lines(x2, yp2, col = "red")
grid()

#El polinomio de taylor es buen interpolador para cierto tipo de ecuaciones. Cuanto estas no son continuas como es el caso 
#de 1/x el polinomio solo interpola correctamente a los puntos cercanos del x0 seleccionado.