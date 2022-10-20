penalty_decision = function(penalty, pen.value, n, diffparam, method){

  if((penalty=="SIC0") || (penalty=="BIC0")){
    pen.return=diffparam*log(n)
  }
  else if((penalty=="SIC") || (penalty=="BIC")){
    pen.return=(diffparam+1)*log(n)
  }
  else if(penalty=="MBIC"){
    pen.return=(diffparam+2)*log(n)
  }
  else if(penalty=="AIC0"){
    pen.return=2*diffparam
  }
  else if(penalty=="AIC"){
    pen.return=2*(diffparam+1)
  }
  else if(penalty=="Hannan-Quinn0"){
    pen.return=2*diffparam*log(log(n))
  }
  else if(penalty=="Hannan-Quinn"){
    pen.return=2*(diffparam+1)*log(log(n))
  }
  else if(penalty=="None"){
    pen.return=0
  }
  else if((penalty!="Manual")&&(penalty!="Asymptotic")){
    stop('Unknown Penalty')
  }


  if((penalty=="Manual")&&(is.numeric(pen.value)==FALSE)){
      tryCatch(
      {
         pen.value <- eval(parse(text=paste(pen.value)))  
      }, error = function(e) stop('Your manual penalty cannot be evaluated')
      )
      pen.return=pen.value
    }


  if((penalty=="Manual")&&(is.numeric(pen.value)==TRUE)){
    pen.return=pen.value
  }

  if(penalty=="Asymptotic"){
    stop('Asymptotic penalties are not available for nonparametric approaches, please choose an alternative penalty type')
  }

  if(pen.return < 0){
    stop('pen.value cannot be negative, please change your penalty value')
  }
  else{
    return(pen.return)
  }
}


