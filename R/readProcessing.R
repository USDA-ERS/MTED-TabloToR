generateReads = function(readStatements) {
  toRet = list()
  for (s in readStatements) {
    toRet[[length(toRet) + 1]] = processReadStatement(s)
  }

  f = str2lang('function(data)return(data)')
  w = str2lang('within(data,{})')
  for (tr in toRet) {
    w[[3]][[length(w[[3]]) + 1]] = str2lang(tr)
  }

  f[[3]][[2]] = w

  return(eval(f))
}
