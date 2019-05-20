#' Identifying Changes using a Nonparametric Cost Function
#'
#' @description Calculates the optimal positioning and number of changepoints for data given a user specified cost function and penalty.
#' @param data A vector, ts object or matrix containing the data within which you wish to find a changepoint.  If the data is a matrix, each row is considered as a separate dataset.
#' @param penalty Choice of "None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Manual" and "CROPS" penalties.  If Manual is specified, the manual penalty is contained in the pen.value parameter. If CROPS is specified, the penalty range is contained in the pen.value parameter; note this is a vector of length 2 which contains the minimum and maximum penalty value.  Note CROPS can only be used if the method is "PELT". The predefined penalties listed DO count the changepoint as a parameter, postfix a 0 e.g."SIC0" to NOT count the changepoint as a parameter.
#' @param pen.value The value of the penalty when using the Manual penalty option.  A vector of length 2 (min,max) if using the CROPS penalty.
#' @param method Currently the only method is "PELT".
#' @param test.stat The assumed test statistic/distribution of the data. Currently only "empirical_distribution".
#' @param class Logical. If TRUE then an object of class cpt is returned.
#' @param minseglen Positive integer giving the minimum segment length (number of observations between changes), default is the minimum allowed by theory.
#' @param nquantiles The number of quantiles to calculate when test.stat = "empirical_distribution".
#'
#' @details This function is used to find multiple changes in a data set using the changepoint algorithm PELT with a nonparametric cost function based on the empirical distribution.  A changepoint is denoted as the first observation of the new segment.
#' @return  If \code{class=TRUE} then an object of S4 class "cpt" is returned.  The slot \code{cpts} contains the changepoints that are returned.  For \code{class=FALSE} the structure is as follows.
#'
#' If data is a vector (single dataset) then a vector/list is returned depending on the value of method.  If data is a matrix (multiple datasets) then a list is returned where each element in the list is either a vector or list depending on the value of method.
#'
#' If method is PELT then a vector is returned containing the changepoint locations for the penalty supplied. If the penalty is CROPS then a list is returned with the elements:
#'
#' \item{cpt.out}{A data frame containing the value of the penalty value where the number of segmentations changes, the number of segmentations and the value of the cost at that penalty value.}
#' \item{changepoints}{The optimal changepoints for the different penalty values starting with the lowest penalty value.}
#'
#' @author Kaylea Haynes
#' 
#' @references \insertRef{Haynes2017}{changepoint.np}
#' @references \insertRef{Killick2012}{changepoint.np}
#' @references \insertRef{Haynes2015}{changepoint.np}
#' 
#' @seealso PELT in parametric settings: \code{\link[changepoint]{cpt.mean}} for changes in the mean, \code{\link[changepoint]{cpt.var}} for changes in the variance and \code{\link[changepoint]{cpt.meanvar}} for changes in the mean and variance.
#' 
#' @examples
#'
#' #Example of a data set of length 1000 with changes in location
#' #(model 1 of Haynes, K et al. (2016)) with the empirical distribution cost function.
#'
#' set.seed(12)
#'
#' J <- function(x){
#'    (1+sign(x))/2
#'  }
#'
#' n <- 1000
#' tau <- c(0.1,0.13,0.15,0.23,0.25,0.4,0.44,0.65,0.76,0.78,0.81)*n
#' h <- c(2.01, -2.51, 1.51, -2.01, 2.51, -2.11, 1.05, 2.16, -1.56, 2.56, -2.11)
#' sigma <- 0.5
#' t <- seq(0,1,length.out = n)
#' data <- array()
#' for (i in 1:n){
#'    data[i] <- sum(h*J(n*t[i] - tau)) + (sigma * rnorm(1))
#' }
#'
#' out <- cpt.np(data, penalty = "SIC",method="PELT",test.stat="empirical_distribution",
#'               class=TRUE,minseglen=2, nquantiles =4*log(length(data)))
#' cpts(out)
#' #returns 100 130 150 230 250 400 440 650 760 780 810 as the changepoint locations.
#' plot(out)
#' \donttest{
#' #Example 2 uses the heart rate data . 
#'
#' data(HeartRate)
#' cptHeartRate <- cpt.np(HeartRate, penalty = "CROPS", pen.value = c(5,200), 
#'                        method="PELT", test.stat="empirical_distribution",
#'                        class=TRUE,minseglen=2, 
#'                        nquantiles =4*log(length(HeartRate)))
#' plot(cptHeartRate, diagnostic = TRUE)
#' plot(cptHeartRate, ncpts = 11)
#' }
#' 
#' @useDynLib changepoint.np
#' @import changepoint zoo
#' @importFrom graphics abline lines segments
#' @importFrom methods as new
#' @importFrom stats is.ts ts var
#' @importFrom utils packageVersion
#' @export

cpt.np=function(data,penalty="MBIC",pen.value=0,method="PELT",test.stat="empirical_distribution",class=TRUE,minseglen=1, nquantiles = 10){
  # checkData(data)
  if(minseglen<1){minseglen=1;warning('Minimum segment length cannot be less than 1, automatically changed to be 1.')}
  if((method == "PELT")&&(test.stat!="empirical_distribution")){ stop("Invalid test statistic, must be empirical_distribution")}

  if(penalty == "CROPS"){
    if(is.numeric(pen.value)){
      if(length(pen.value) == 2){
        if(pen.value[2] < pen.value[1]){
          pen.value = rev(pen.value)
        }
        #run range of penalties
        return(CROPS(data=data, method=method, pen.value=pen.value, test.stat=test.stat, class=class, minseglen=minseglen, nquantiles=nquantiles, func="nonparametric"))
      }else{
        stop('The length of pen.value must be 2')
      }
    }else{
      stop('For CROPS, pen.value must be supplied as a numeric vector and must be of length 2')
    }
  }
  else{
    if(method == "PELT"){
      return(multiple.nonparametric.ed(data,mul.method=method,penalty,pen.value,class,minseglen, nquantiles))
    }
    else{
      stop("Invalid Method, must be PELT")
    }
  }
}

