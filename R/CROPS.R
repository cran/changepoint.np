CROPS <- function(data, penalty="CROPS", pen.value, method="PELT", test.stat="empirical_distribution", class=TRUE, param.est=TRUE, minseglen, nquantiles, func,verbose){
  if(method != "PELT"){stop('CROPS is a valid penalty choice only if method="PELT", please change your method or your penalty.')}
  if (test.stat == "empirical_distribution"){
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

  switch(test.stat,
    "empirical_distribution" = {stat="ed"},
    {stop("Only empirical_distribution is a valid test statistics")}
  )
  costfunc = paste0(func, ".", stat)

  out = range_of_penalties(sumstat, cost=costfunc, min_pen=pen.value[1], max_pen=pen.value[2], minseglen=minseglen, nquantiles = nquantiles,verbose = verbose)

  if(func=="nonparametric"){
    cpttype="nonparametric"
  }

  if(class==TRUE){
      ans = class_input(data=data,cpttype=cpttype, method=method, test.stat=test.stat, penalty=penalty, pen.value=pen.value, minseglen=minseglen, out=out)
    return(ans)
  }

  else{return(out)}
}
