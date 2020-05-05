generateEquationCoefficientMatrix = function(variableStatements, equationStatements) {
  toRet = list('equations=c()','variables=c()')
  for (s in equationStatements) {
    # Get the formula for each variable

    dimensions = s$parsed$elements[grep('\\(all,', s$parsed$elements)]
    qualifiers = gsub('<>', '!=', gsub(':', ",", gsub(
      '=', '==', gsub('\\(all,', 'all(', dimensions)
    )))


    equationName = s$parsed$equationName
    equationIndices=Map(function(f)str2lang(f)[[2]],qualifiers)


    #Loop throuch each variable mentioned in this equation
    expr = sprintf(
      "equations = c(equations, %s)",
      sprintf(
        ifelse(length(equationIndices)>0,"sprintf('%s[%s]',%s)","'%s[%s]'"),
        equationName,
        paste(rep('\"%s\"', length(
          equationIndices
        )), collapse = ','),
        paste(unlist(equationIndices), collapse = ',')
      )
    )
    for (qualifier in c(qualifiers)) {
      q = str2lang(qualifier)
      expr = sprintf(
        'for(%s in %s){%s}',
        deparse(q[[2]], width.cutoff = 500),
        deparse(q[[3]], width.cutoff = 500),
        expr
      )
    }
    toRet[[length(toRet) + 1]] = expr
  }


  for (s in variableStatements) {

    dimensions = s$parsed$elements[grep('\\(all,', s$parsed$elements)]
    qualifiers = gsub('<>', '!=', gsub(':', ",", gsub(
      '=', '==', gsub('\\(all,', 'all(', dimensions)
    )))


    variableDefinition = correctFormula(s$parsed$equation)

    if(length(variableDefinition)==1){
      variableName=deparse(variableDefinition)
    } else {
      variableName=deparse(variableDefinition[[2]])
    }
    variableIndices=Map(function(f)str2lang(f)[[2]],qualifiers)

    #Loop throuch each variable mentioned in this equation
    expr = sprintf(
      "variables = c(variables, %s)",
      sprintf(
        ifelse(length(variableIndices)>0,"sprintf('%s[%s]',%s)","'%s[%s]'"),
        variableName,
        paste(rep('\"%s\"', length(
          variableIndices
        )), collapse = ','),
        paste(unlist(variableIndices), collapse = ',')
      )
    )
    for (qualifier in c(qualifiers)) {
      q = str2lang(qualifier)
      expr = sprintf(
        'for(%s in %s){%s}',
        deparse(q[[2]], width.cutoff = 500),
        deparse(q[[3]], width.cutoff = 500),
        expr
      )
    }
    toRet[[length(toRet) + 1]] = expr
  }


  toRet[[length(toRet)+1]]=sprintf('eqcoeff = Matrix::sparseMatrix(i=c(),j=c(),x=as.numeric(c()),dim=c(length(equations), length(variables)), dimnames=list(equations, variables))')

  f = str2lang('function(data)return(data)')
  w = str2lang('within(data,{})')
  c1=0
  for (tr in toRet) {
    c1=c1+1
    if(c1/5==round(c1/5)){
      w[[3]][[length(w[[3]]) + 1]] = str2lang(sprintf('message("%s/%s")',c1, length(toRet)))
    }
    w[[3]][[length(w[[3]]) + 1]] = str2lang(tr)
  }

  f[[3]][[2]] = w

  return(eval(f))


}
