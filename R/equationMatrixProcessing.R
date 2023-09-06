generateEquationCoefficientMatrix = function(variableStatements, equationStatements) {
  toRet = list('equations=c()', 'variables=c()')
  for (s in equationStatements) {
    # Get the formula for each variable

    dimensions = s$parsed$elements[grep('\\(all,', s$parsed$elements)]
    qualifiers = gsub('<>', '!=', gsub(':', ",", gsub(
      '=', '==', gsub('\\(all,', 'all(', dimensions)
    )))


    equationName = s$parsed$equationName
    equationIndices = Map(function(f)
      str2lang(f)[[2]], qualifiers)


    if (length(equationIndices) > 0) {
      expr = sprintf(
        "sprintf('%s[%s]',%s)",
        equationName,
        paste(rep('\"%s\"', length(
          equationIndices
        )), collapse = ','),
        paste(unlist(equationIndices), collapse = ',')
      )

    } else {
      expr = sprintf("'%s[%s]'", equationName,
                     paste(rep('\"%s\"', length(
                       equationIndices
                     )), collapse = ','))
    }

    for (qualifier in c(qualifiers)) {
      q = str2lang(qualifier)
      expr = sprintf(#'for(%s in %s){%s}',
        'Map(function(%s)%s,%s)',
        deparse1(q[[2]]),
        expr,
        deparse1(q[[3]]))
    }
    if (length(qualifiers) == 0) {
      toRet[[length(toRet) + 1]] = sprintf('equations = c(equations,%s)' , expr)

    } else{
      toRet[[length(toRet) + 1]] = sprintf('equations = c(equations,unlist(do.call(base:::c,%s)))' , expr)

    }
  }


  for (s in variableStatements) {
    dimensions = s$parsed$elements[grep('\\(all,', s$parsed$elements)]
    qualifiers = gsub('<>', '!=', gsub(':', ",", gsub(
      '=', '==', gsub('\\(all,', 'all(', dimensions)
    )))


    variableDefinition = correctFormula(s$parsed$equation)

    if (length(variableDefinition) == 1) {
      variableName = deparse1(variableDefinition)
      variableIndices = NULL
    } else {
      variableName = deparse1(variableDefinition[[2]])
      
      variableIndices= c()
      for (i in 3:length(variableDefinition)){
        variableIndices = c(variableIndices, variableDefinition[[i]])
      }
      
      qualifiers_named <- c()
      for (q in c(qualifiers)) {
        name <- as.character(str2lang(q)[[2]])
        qualifiers_named[name] = q
      }
      qualifiers = qualifiers_named[order(match(unlist(variableIndices), names(qualifiers_named)))]
    }

    variableIndices = Map(function(f)
      str2lang(f)[[2]], qualifiers)
    
    if (length(variableIndices) > 0) {
      expr = sprintf(
        "sprintf('%s[%s]',%s)",
        variableName,
        paste(rep('\"%s\"', length(
          variableIndices
        )), collapse = ','),
        paste(unlist(variableIndices), collapse = ',')
      )
    } else {
      expr = sprintf("'%s[%s]'",
                     variableName,
                     paste(rep('\"%s\"', length(
                       variableIndices
                     )), collapse = ','))
    }

    
    for (qualifier in c(qualifiers)) {
      q = str2lang(qualifier)
      expr = sprintf(#        'for(%s in %s){%s}',
        'Map(function(%s)%s,%s)',
        deparse1(q[[2]]),
        expr,
        deparse1(q[[3]]))
    }
    #    toRet[[length(toRet) + 1]] = expr
    if (length(qualifiers) == 0) {
      toRet[[length(toRet) + 1]] = sprintf('variables = c(variables,%s)' , expr)

    } else{
      toRet[[length(toRet) + 1]] = sprintf('variables = c(variables,unlist(do.call(base:::c,%s)))' , expr)

    }
  }

  toRet[[length(toRet) + 1]] = 'equations = unname(equations)'
  toRet[[length(toRet) + 1]] = 'variables = unname(variables)'

  toRet[[length(toRet) + 1]] = 'equationNumbers = 1:length(equations)'
  toRet[[length(toRet) + 1]] = 'names(equationNumbers)=equations'

  toRet[[length(toRet) + 1]] = 'variableNumbers = 1:length(variables)'
  toRet[[length(toRet) + 1]] = 'names(variableNumbers)=variables'


  f = str2lang('function(data)return(data)')
  w = str2lang('within(data,{})')

  for (tr in toRet) {
    w[[3]][[length(w[[3]]) + 1]] = str2lang(tr)
  }

  f[[3]][[2]] = w

  return(eval(f))


}
