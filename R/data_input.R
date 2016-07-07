data_input <- function(data, method, pen.value, costfunc, minseglen, nquantiles = 10){
  if (costfunc == "nonparametric.ed" | costfunc == "nonparametric.ed.mbic" ){
    nonparametric.ed.sumstat = function(data,K=nquantiles){ # This now takes into account the integral transformation
      ##USE K points in integral
      n <- length(data)
      if(K>n) K=n
      Q <- matrix(0,K,n+1)
      x=sort(data)
      yK= -1 + (2*(1:K)/K-1/K)
      c=-log(2*n-1)
      pK=(1+exp(c*yK))^-1
      for (i in 1:K){
        j=as.integer((n-1)*pK[i] + 1)
        Q[i,-1] <- cumsum(data<x[j])+0.5*cumsum(data==x[j])
      }
      return(Q)
    }
    sumstat <- nonparametric.ed.sumstat(data, K = nquantiles)
  }

  if(method == "PELT"){
    out=NPPELT(sumstat,pen=pen.value,cost_func = costfunc,minseglen=minseglen, nquantiles = nquantiles)
  }

  return(out)
}
