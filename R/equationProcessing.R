# getVarCoefsEquation = function(expr){
#   expr[[1]]=as.name('-')
#
#   print(expr)
#
#   toRet= list()
#   toRet= getVarCoefs(expr,toRet)
#   return(toRet)
# }
#
# getVarCoefs=function(expr,ret, sign=1){
#   if(expr[[1]]=='*'){
#     nmult=length(expr)
#     if(sign==1){
#       ret[[length(ret)+1]] = list(coef=expr[[2]], var=expr[[3]])
#     }else{
#       ret[[length(ret)+1]] = list(coef=call('*',sign,expr[[2]]), var=expr[[3]])
#     }
#
#   } else if (expr[[1]]=='+'){
#     ret=getVarCoefs(expr[[2]],ret)
#     ret=getVarCoefs(expr[[3]],ret)
#   } else if (expr[[1]]=='-'){
#     ret=getVarCoefs(expr[[2]],ret)
#     ret=getVarCoefs(expr[[3]],ret, sign=-1)
#   } else if (expr[[1]]=='['){
#     ret[[length(ret)+1]] = list(coef=sign*1, var=expr)
#   } else if (expr[[1]]=='('){
#     ret[[length(ret)+1]] = list(coef=sign*1, var=expr)
#   }
#   return(ret)
# }
#
#
# getVarCoefsEquation(str2lang('a[r]=U[r]*b[r]-PP[r]*c[r]'))
# getVarCoefsEquation(str2lang('a[r]=U[r]*b[r]'))
#
getVarCoef = function(expr,
                      sets = list(),
                      coefficient = 1) {
  toRet = list()
  if (length(expr) == 1) {
    toRet[[length(toRet) + 1]] = list(variable = expr,
                                      sets = sets,
                                      coefficient = coefficient)
  }
  # If the top operator is sum: add to set/index
  else if (expr[[1]] == 'sum') {
    sets[[length(sets) + 1]] = list(index = expr[[2]], set = expr[[3]])

    toRet[[length(toRet) + 1]] = getVarCoef(expr[[4]], sets, coefficient = coefficient)

  } else if (expr[[1]] == '[') {
    toRet[[length(toRet) + 1]] = list(variable = expr,
                                      sets = sets,
                                      coefficient = coefficient)
  } else if (expr[[1]] == '(') {
    toRet[[length(toRet) + 1]] = getVarCoef(expr[[2]], sets, coefficient = coefficient)
  } else if (expr[[1]] == '+') {
    toRet[[length(toRet) + 1]] = getVarCoef(expr[[2]], sets, coefficient = coefficient)
    toRet[[length(toRet) + 1]] = getVarCoef(expr[[3]], sets, coefficient = coefficient)
  } else if (expr[[1]] == '-' & length(expr)==3) {
    toRet[[length(toRet) + 1]] = getVarCoef(expr[[2]], sets, coefficient = coefficient)
    toRet[[length(toRet) + 1]] = getVarCoef(expr[[3]], sets, coefficient = call('-',coefficient))
  } else if (expr[[1]] == '-' & length(expr)==2) {
    toRet[[length(toRet) + 1]] = getVarCoef(expr[[2]], sets, coefficient = call('-',coefficient))
  } else if (expr[[1]] == '*') {
    toRet[[length(toRet) + 1]] = getVarCoef(expr[[3]], sets, coefficient = call('*', coefficient, expr[[2]]))
  } else if (expr[[1]] == '=') {
    toRet[[length(toRet) + 1]] = getVarCoef(expr[[2]], sets, coefficient = coefficient)
    toRet[[length(toRet) + 1]] = getVarCoef(expr[[3]], sets, coefficient = call('*', coefficient,-1))

  }
  return(toRet)
}

#getVarCoef(str2lang(s$parsed$equation))

unlistVarCoef = function(obj) {
  toRet = list()
  for (e in obj) {
    if (!is.null(e$variable)) {
      toRet[[length(toRet) + 1]] = e
    } else{
      getResults = unlistVarCoef(e)
      for (ge in getResults) {
        toRet[[length(toRet) + 1]] = ge
      }
    }
  }
  return(toRet)
}

generateEquationCoefficients = function(equationStatements) {
  toRet = list('equationMatrixList=list()')
  for (s in equationStatements) {
    # Get the formula for each variable
    frm = correctFormula(s$parsed$equation)

    variables = unlistVarCoef(getVarCoef(frm))

    for (v in 1:length(variables)) {
      if (length(variables[[v]]$variable) == 1) {
        variables[[v]]$indices = list()
        variables[[v]]$varname = variables[[v]]$variable
      } else{
        variables[[v]]$indices =  as.list(variables[[v]]$variable[3:length(variables[[v]]$variable)])
        variables[[v]]$varname = variables[[v]]$variable[[2]]
      }
      variables[[v]]$qualifiers = Map(function(f)
        sprintf('all(%s,%s)', deparse(f$index), deparse(f$set)),
        variables[[v]]$sets)
    }

    dimensions = s$parsed$elements[grep('\\(all,', s$parsed$elements)]
    qualifiers = gsub('<>', '!=', gsub(':', ",", gsub(
      '=', '==', gsub('\\(all,', 'all(', dimensions)
    )))


    equationName = s$parsed$equationName
    equationIndices=Map(function(f)str2lang(f)[[2]],qualifiers)

    #Loop throuch each variable mentioned in this equation
    for (v in variables) {
      #Loop through the qualifiers (zeroth is the initial equation)
      expr = sprintf(
        #        "eqcoeff[%s,%s]= %s",
        "list(list(equation=%s,variable = %s, expression= %s))",
        sprintf(
          ifelse(length(equationIndices)>0,"sprintf('%s[%s]',%s)",'"%s[%s]"'),
          equationName,
          paste(rep('\"%s\"', length(equationIndices)), collapse = ','),
          paste(unlist(equationIndices), collapse = ',')
        ),
        sprintf(
          ifelse(length(v$indices)>0,"sprintf('%s[%s]',%s)",'"%s[%s]"'),
          deparse(v$varname),
          paste(rep('\"%s\"', length(v$indices)), collapse = ','),
          paste(unlist(v$indices), collapse = ',')
        ),
        #deparse(v$variable),
        deparse(sumToMap(v$coefficient),width.cutoff = 500)
      )
      for (qualifier in c(qualifiers, v$qualifiers)) {
        q = str2lang(qualifier)
        expr = sprintf(
          #          'for(%s in %s){%s}',
          'unlist(Map(function(%s)%s,%s),recursive=F,use.names=F)',
          deparse(q[[2]], width.cutoff = 500),
          expr,
          deparse(q[[3]], width.cutoff = 500)
        )
      }
      #toRet[[length(toRet) + 1]] = expr
      toRet[[length(toRet) + 1]] = sprintf('equationMatrixList=c(equationMatrixList,%s)',expr)
    }


  }

  f = str2lang('function(data)return(data)')
  w = str2lang('within(data,{})')
  c1=0
  for (tr in toRet) {
    c1=c1+1
    w[[3]][[length(w[[3]]) + 1]] = str2lang(sprintf("message(%s)",c1))
    w[[3]][[length(w[[3]]) + 1]] = str2lang(tr)
  }

  f[[3]][[2]] = w

  return(eval(f))


}
