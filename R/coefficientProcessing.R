generateCoefficients = function(coefficientStatements){
  toRet = list()
  for(s in coefficientStatements){
    toRet[[length(toRet)+1]]=processCoefficientStatement(s)
  }

  f=str2lang('function(data)return(data)')
  w=str2lang('within(data,{})')
  for(tr in toRet){
    w[[3]][[length(w[[3]])+1]]=str2lang(tr)
  }

  f[[3]][[2]]=w

  return(eval(f))
}
