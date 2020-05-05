removeFunctions = function(exp) {
  return(str2lang(gsub('\\)', ']', gsub(
    '\\(', '[', deparse(exp)
  ))))
}
correctFormula = function(formulaText) {
  formulaText = str2lang(gsub('\\]', ')', gsub('\\[', '(', deparse(formulaText))))
  exp = str2lang(formulaText)

  exp = functionToData(exp)
  return(exp)
}

functionToData = function(exp) {
  dataNames = c('sum',
                'exp',
                'loge',
                '=',
                '-',
                '+',
                '/',
                '*',
                '(',
                '==',
                '!=',
                '<',
                '>')
  if (length(exp) == 1) {
    return(exp)
  } else    if (!(as.character(exp[[1]]) %in% dataNames)) {
    dataName = exp[[1]]
    exp[[1]] = as.name('[')

    for (c2 in length(exp):2) {
      exp[[c2 + 1]] = exp[[c2]]
    }

    exp[[2]] = dataName
    return(exp)
  }

  else{
    for (c1 in 1:length(exp)) {
      exp[[c1]] = functionToData(exp[[c1]])
    }
    return(exp)
  }
}


generateSets = function(statements) {
  toRet = list()
  for (s in statements) {
    toRet[[length(toRet) + 1]] = processSetStatement(s)

  }

  f = str2lang('function(data)return(data)')
  w = str2lang('within(data,{})')
  for (tr in toRet) {
    w[[3]][[length(w[[3]]) + 1]] = str2lang(tr)
  }

  f[[3]][[2]] = w

  return(eval(f))
}
