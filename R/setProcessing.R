removeFunctions = function(exp) {
  return(str2lang(gsub('\\)', ']', gsub(
    '\\(', '[', deparse1(exp)
  ))))
}

correctFormula = function(formulaText) {

  if (grepl('IF\\(', formulaText)) {
    
    temp = strsplit(formulaText, split = "IF\\(")
    if (length(temp[[1]] > 1)) {
      for (f in 2:length(temp[[1]])) {
        temp[[1]][[f]] <- paste0("IF(", sub(",(?![^()]*\\))", ") {", temp[[1]][[f]], perl = TRUE))
        #Replace unopened parentheses with "}"
        temp[[1]][[f]] <- gsub('(?:(\\((?:[^()]++|(?1))*\\))|(\\[(?:[^][]++|(?2))*]))(*SKIP)(*F)|[][()]', "}", temp[[1]][[f]], perl=TRUE)
        # If there is a '}}" in the end, replace it by "})" (reason: gtapv7, equation:E_CNTalleffr)
        temp[[1]][[f]] <- sub("}}(?!(.|\n)*}})", "}\\)", temp[[1]][[f]], perl=TRUE)
        # Replace "=" inside parentheses with "=="
        temp[[1]][[f]] <- sub("(?:\\G(?!^)|\\()[^()]*?\\K\\=", "==", temp[[1]][[f]], perl=TRUE)
      }
    }
    formulaText = paste(temp[[1]],collapse="") 
    # Replace "IF(" with "if(" 
    formulaText = gsub('IF\\(', 'if\\(', formulaText)
    # Replace " in " with " %in% " 
    formulaText = gsub(' in ', ' %in% ', formulaText)
    
  }
  
  formulaText = gsub('\\$pos', 'match', formulaText)
  
  #formulaText = str2lang(gsub('\\]', ')', gsub('\\[', '(', deparse(gsub(":", "%:%", gsub("\\(all,", "all(", gsub('>==','>=',gsub('<==','<=',gsub('=','==',formulaText)))))))))
  formulaText = str2lang(gsub('\\]', ')', gsub('\\[', '(', deparse1(formulaText))))
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
