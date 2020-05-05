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
  dataNames=c('sum','exp','loge','=','-','+','/','*','(','==','!=','<','>')
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


generateSets = function(statements){
  toRet = list()
  for(s in statements){
  # SET READ
  if (grepl(".* maximum size .* read elements from file .* header \"*\"",
            s$command)) {
    words = strsplit(s$command, " ")[[1]]
    #toRet[[words[1]]] = files[[words[9]]][[gsub("\"", "", words[11])]]
    toRet[[length(toRet)+1]] = sprintf('%s=%s$%s',words[[1]],words[[9]],gsub("\"", "", words[11]))
  }
  # SET DIFFERENCE
  else if (grepl(".* = .* - .*", s$command)) {
    command = str2lang(s$command)
    command[[3]][[1]] = as.name('setdiff')
    #toRet[[deparse(command[[2]])]] = eval(command[[3]], toRet)
    toRet[[length(toRet)+1]] = sprintf('%s=%s',deparse(command[[2]]),deparse(command[[3]]))
  }
  # SET UNION
  else if (grepl(".* = .* union .*", s$command)) {
    command = str2lang(gsub('union', '+', s$command))
    command[[3]][[1]] = as.name('union')
    #toRet[[deparse(command[[2]])]] = eval(command[[3]], toRet)
    toRet[[length(toRet)+1]] = sprintf('%s=%s',deparse(command[[2]]),deparse(command[[3]]))
  }
  # SET FORMULA
  else if (grepl(".* = \\(all,.*,.*\\)", s$command)) {
    preCommand = str2lang(gsub(":", ",", gsub("\\(all,", "all(", s$command)))

    setName = deparse(preCommand[[3]][[3]])
    standIn = deparse(preCommand[[3]][[2]])
    preCommand[[3]][[4]] = str2lang(gsub(
      paste0('\\b', standIn, '\\b'),
      setName ,
      deparse(preCommand[[3]][[4]])
    ))

    preCommand[[3]][[1]] = as.name('[')
    preCommand[[3]][[2]] = NULL

    preCommand[[3]][[3]] = removeFunctions(preCommand[[3]][[3]])

    #toRet[[deparse(preCommand[[2]])]] = eval(preCommand[[3]], toRet)
    toRet[[length(toRet)+1]] = sprintf('%s=%s',deparse(preCommand[[2]]),deparse(preCommand[[3]]))
    #eval(str2lang('SLUG[ENDW_COMM]'), toRet)
    #eval(quote(SLUG[ENDW_COMM]),toRet)
  }
  # SET SPECIFIED
  else if (grepl(".* \\(.*\\)", s$command)) {
    from = regexpr('\\(', s$command)
    to = regexpr('\\)', s$command)
    elements = strsplit(substr(s$command, from + 1, to - 1), ',')[[1]]

    #toRet[[trimws(substr(s$command, 1, from - 1))]] = elements
    toRet[[length(toRet)+1]] = sprintf('%s=c(%s)',trimws(substr(s$command, 1, from - 1)),paste('"',elements,'"',sep='',collapse=','))
  }
  }

  f=str2lang('function(data)return(data)')
  w=str2lang('within(data,{})')
  for(tr in toRet){
    w[[3]][[length(w[[3]])+1]]=str2lang(tr)
  }

  f[[3]][[2]]=w

  return(eval(f))
}
