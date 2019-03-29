#El polinomio interpolante de n numero de nodos distintos
#es siempre de grado <= n-1. Por esta misma razon si asumimos 
#que existe un unico poliniomio interpolante entonces podriamos
#reemplazar cada punto en una matriz de vandermonde de grado <=n-1
#     [,1]    [,2]        [,3]
#[1,]   1     c1*x0      c2*x0^2
#[2,]   1     c1*x1      c2*x1^2 
#[3,]   1     c1*x3      c2*x3^2
#Ejemplo en el caso de 3 (xi) puntos necesitamos un polinomio de grado 2.
#Si el determinante de este polinomio es diferente a 0 entonces existe una unica solucion 
#a la matriz y por lo tanto un unico polinomio interpolante.


vandermonde.matrix <- function( vec, n )
{

  
  m <- length( vec )
  V <- matrix( 0, nrow=m, ncol=n )
  V[,1] <- rep( 1, m )
  j <- 2
  while ( j <= n ) {
    x <- vec ^ ( j - 1 )
    V[,j] <- x
    j <- j + 1
  }
  return( V )
}

alpha <- c( 6, 5, .9, .5,3)
beta<-c( 9, 2, -5)
gamma<- c(-9,8,6,1,5,4,1.5,2,2.4)
V <- vandermonde.matrix( alpha, 5 )
print( V )
cat("El determinante es: ",det(V))
V <- vandermonde.matrix( beta, 3 )
print( V )
cat ("El determinante es: ", det(V))
V <- vandermonde.matrix( gamma, 9 )
print( V )
cat ("El determinante es: ", det(V))

