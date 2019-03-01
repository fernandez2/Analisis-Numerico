
#Se igualan las ecuaciones para encontrar la interseccion
Fx <- function(x) tan(x*pi)-sin(x*pi)
fx<-function(x) tan(x*pi)
fx1<-function(x) sin(x*pi)

funcionraiz <- function(a,err) {
  v<-vector()
  x<-a
  c<-3
  x1<-Fx(x)
  cat(x1)
  cat("Iter=",1,"\tX=",x1*pi,"\tE="," ","\n")
  v<-c(v,x1)
  x2<-((1)-(((x1)*((1)-(0)))/((x1)-(0))))
  cat("Iter=",2,"\tX=",x2*pi,"\tE=",abs(x2-x1),"\n")
  v<-c(v,x2)
  repeat {
    x<-((c-1)-(((v[c-1])*((c-1)-(c-2)))/((v[c-1])-(v[c-2]))))
    error<-abs(Fx(x)-v[c-1])
    v<-c(v,x)
    cat("Iter=",c,"\tX=",x,"\tE=",error,"\n")
    c<-c+1
    if(error< err) break;
    
  }
  #La raiz de la ecuacion es 2pi y las siguientes raices son 2pi+1
  cat("Se encontro la raiz ",x*pi," en ",c," iteraciones","\n")
  
  cat("La siguiente raiz es",(x*pi)+1)
}
funcionraiz((251515/5151),1e-9)
x = seq(-4,2,0.1)
plot(x,fx(x),type="p",xlim=c(0,5),ylim=c(-pi,pi),col="orange",xlab="t",ylab="r(t)")
par(new=TRUE)
plot(x,fx1(x),type="p",xlim=c(0,5*pi),ylim=c(-pi,pi),col="green",xlab="t",ylab="r(t)")


