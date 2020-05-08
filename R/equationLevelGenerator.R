generateEquationLevels = function(statements) {
  toRet = list('"/" <- function(x,y) ifelse(y==0,0,base:::"/"(x,y))
')
  for (s in statements) {
    if (s$class == 'equation') {
      toRet[[length(toRet) + 1]] = processEquationSetupStatement(s)
      toRet[[length(toRet) + 1]] = processEquationStatement(s)
    }
  }

  f = str2lang('function(data)return(data)')
  w = str2lang('within(data,{})')
  for (tr in toRet) {
    w[[3]][[length(w[[3]]) + 1]] = str2lang(paste(tr, collapse = ''))
  }

  f[[3]][[2]] = w

  return(eval(f))
}
