generateSkeleton = function(statements) {
  toRet = list()
  for (s in statements) {
    if (s$class == 'set')
      toRet[[length(toRet) + 1]] = processSetStatement(s)
    else if (s$class == 'formula') {
      toRet[[length(toRet) + 1]] = processFormulaStatement(s)
    }
    else if (s$class == 'coefficient') {
      toRet[[length(toRet) + 1]] = processCoefficientStatement(s)
    }
    else if (s$class == 'read') {
      toRet[[length(toRet) + 1]] = processReadStatement(s)
    }
  }

  f = str2lang('function(data)return(data)')
  w = str2lang('within(data,{})')
  for (tr in toRet) {
    w[[3]][[length(w[[3]]) + 1]] = str2lang(paste(tr,collapse=''))
  }

  f[[3]][[2]] = w

  return(eval(f))
}
