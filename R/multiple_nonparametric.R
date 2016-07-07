multiple.nonparametric.ed=function(data,mul.method="PELT",penalty="MBIC",pen.value=0,class=TRUE,minseglen, nquantiles=10){
  if(!(mul.method=="PELT")){
    stop("Multiple Method is not recognised")
  }
  costfunc = "nonparametric.ed"
  if(penalty=="MBIC"){
    costfunc = "nonparametric.ed.mbic"
  }
  diffparam=1
  if(is.null(ncol(data))==TRUE || is.na(ncol(data)) == TRUE){
    # single dataset
    n=length(data) # still works if data is of class ts
  }
  else{
    n=ncol(data)
  }

  pen.value = penalty_decision(penalty, pen.value, n, diffparam, method=mul.method)

  if(is.null(ncol(data))==TRUE || is.na(ncol(data)) == TRUE){
    # single dataset
    out = data_input(data=data,method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen, nquantiles = nquantiles)

    if(class==TRUE){
      return(class_input(data, cpttype="nonparametric", method=mul.method, test.stat="empirical_distribution", penalty=penalty, pen.value=pen.value, minseglen=minseglen, out=out))
    }
    else{
      return(out[[2]])
    }
  }
  else{
    rep=nrow(data)
    out=list()
    if(class==TRUE){cpts=list()}
    for(i in 1:rep){
      out[[i]]=data_input(data[i,],method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen, nquantiles = nquantiles)
    }

    cps=lapply(out, '[[', 2)

    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=class_input(data[i,], cpttype="nonparametric", method=mul.method, test.stat="empirical_distribution", penalty=penalty, pen.value=pen.value, minseglen=minseglen, out=out[[i]])
      }
      return(ans)
    }
    else{return(cps)}
  }
}
