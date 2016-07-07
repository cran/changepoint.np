class_input <- function(data, cpttype, method, test.stat, penalty, pen.value, minseglen, out=list()){
  if(penalty=="CROPS"){
    ans=new("cpt.range")
  }
  else{
    ans=new("cpt")
  }
  data.set(ans)=data;cpttype(ans)=paste(cpttype," (",test.stat,")", sep = "");method(ans)=method; test.stat(ans)=test.stat;pen.type(ans)=penalty;pen.value(ans)=pen.value;minseglen(ans)=minseglen;
  if(penalty!="CROPS"){ # crops is only one that doesn't give a single set of cpts
    cpts(ans)=out[[2]]
  }

  if(penalty=="CROPS"){
    m = t(sapply(out[[2]], '[', 1:max(sapply(out[[2]], length))))

    cpts.full(ans) = m
    pen.value.full(ans) = out[[1]][1,]
  }
  return(ans)
}



