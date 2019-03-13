#PUNTO 2

fx <- function(x)  tan(pi*x)
gx <- function(x)  sin(pi*x)

curve(fx,-2,2, col="red");abline(h=-0,v=0,lty=2)
par(new=TRUE)
curve(gx,-2,2, col="blue");abline(h=-0,v=0,lty=2)
par(new=TRUE)
points(2,0,col="darkgreen")
par(new=TRUE)
points(1,0,col="darkgreen")
par(new=TRUE)
points(-1,0,col="darkgreen")

#Se igualan las ecuaciones para encontrar la interseccion
Fx<- function(x) tan(pi*x)-sin(pi*x)

#Funcion 2A
funcionraiz<-function(x1,x2,err)
{
  ERROR=1
  x0=x1-((Fx(x1)*(x1-x2))/(Fx(x1)-Fx(x2)))
  x1=x2
  x2=x0
  contador=0
  while(ERROR>err)
  {
    x3=x1-((Fx(x1)*(x1-x2))/(Fx(x1)-Fx(x2)))
    ERROR=abs((x3-x2)/x3)*100
    cat("iter: ",contador,"\t x=",x3,"\t Error:",ERROR,"\n")
    x1=x2
    x2=x3
    contador=contador+1
  }
  cat("Interseccion en: ",x3,"\n")
}

#Derivada de F(x) para implementar el metodo newton
Dx <- function(x) (((1/cos(pi*x))^2)*pi)-((pi*cos(pi*x))*pi)
#Funcion  2B Mejorado
newtonMejorado<-function(x,err){
  ERROR=1
  contador=0
  #Aceleracion de Atkins
  while(contador<4 && ERROR>err){
    if(x!=0){ 
      xanterior=x
      x=x-((Fx(x))/Dx(x))
      ERROR=(abs(xanterior-x))/abs(xanterior)
      cat("iter: ",contador,"\t x=",x,"\t Error:",ERROR,"\n")
      if(contador==1)
      {
        x0=x
      }
      else
      {
        if(contador==2)
        {
          x1=x
        } 
        else
        {
          x2=x
        }
      }
    }
    contador=contador+1
  }
  
  while(ERROR>err){
    if(x!=0){
      x3=x2-(((x2-x1)^2)/(x2-(2*x1)+x0))
      x0=x1
      x1=x2
      ERROR=(abs(x2-x3))/abs(x2)
      cat("iter: ",contador,"\t x=",x3,"\t Error:",ERROR,"\n")
      x=x-((Fx(x))/Dx(x))
      x2=x
    }
    contador=contador+1
  }
  cat("Interseccion en: ",x3,"\n")
}


funcionraiz(0.9,1.3,1e-9)
funcionraiz(1.9,2.2,1e-9)

newtonMejorado(0.9,1e-9)
newtonMejorado(1.9,1e-9)
