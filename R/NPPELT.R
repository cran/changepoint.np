NPPELT = function(sumstat,pen=0, cost_func = "norm.mean", minseglen = 1, nquantiles = 100){
  # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
  n=dim(sumstat)[2]-1
  if(n<2){stop('Data must have atleast 2 observations to fit a changepoint model.')}

  storage.mode(sumstat) = 'double'
  error=0

  lastchangelike = array(0,dim = n+1)
  lastchangecpts = array(0,dim = n+1)
  numchangecpts = array(0,dim = n+1)

  cptsout=rep(0,n) # sets up null vector for changepoint answer
  storage.mode(cptsout)='integer'

  answer=list()
  answer[[6]]=1
  on.exit(.C("FreePELT",answer[[6]]))

  storage.mode(lastchangelike) = 'double'
  storage.mode(lastchangecpts) = 'integer'
  storage.mode(numchangecpts) = 'integer'

  answer=.C('PELT',cost_func, sumstat,as.integer(n),as.double(pen),cptsout,as.integer(error), as.integer(minseglen), as.integer(nquantiles), lastchangelike, lastchangecpts,numchangecpts)
  if(answer[[6]]>0){
    stop("C code error:",answer[[6]],call.=F)
  }
  return(list(answer[[10]],sort(answer[[5]][answer[[5]]>0]), answer[[9]], answer[[11]]))

}
