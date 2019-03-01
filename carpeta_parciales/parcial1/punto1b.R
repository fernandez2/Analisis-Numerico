

gauss = function(A, b)
  op = 0
  n = nrow(A) 
  
  entrada = n*n+length(b)
  cat(entrada," ")

  Ab = cbind(A,b)

  for (k in 1:(n-1)){ 
    if(Ab[k,k]==0){ 
      fila = which(Ab[k, ]!=0)[1]
      Ab[c(k, fila), ] = Ab[c(fila, k), ]
    }
    

    for (i in (k+1):n){
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
      op = op + 3*(n-i)
    }
  }

  x = rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n] 
  op = op + n
  
  for(i in (n-1):1 ){
    x[i]= (Ab[i, n+1] - sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
    op = op +3
  }
  cat(op, "\n")
  return(x)
  



A = matrix(c( 4, -1, -0.9, -1,
              -1, 4, -1, -1,
              -1, -1, 4, -1,
              -1, -1, -1, 4), nrow=4, byrow=TRUE)
b = c(-exp(1),5,6,0)

C = matrix(c( 4, -1, -1, -1,
              -1, 4, -1, -1,
              -1, -1, 4, -1,
              -1, -1, -1, 4), nrow=4, byrow=TRUE)
d = c(-exp(1),5,6,0)



cat(solve(A,b), "\n") 
cat("\n")

cat(solve(C,d), "\n")


