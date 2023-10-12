getVarCoef <- function(expr,
                       sets = list(),
                       ifStatement = list(),
                       coefficient = 1) {
  toRet <- list()
  if (length(expr) == 1) {
    toRet[[length(toRet) + 1]] <- list(
      variable = expr,
      sets = sets,
      ifStatement = ifStatement,
      coefficient = coefficient
    )
  } else if (expr[[1]] == "if") {
      if (is.numeric(expr[[2]][[3]])){
        coefficient = str2lang(sprintf("(if(%s){%s}else{0})", deparse1(expr[[2]]), deparse1(coefficient)))
      }
      if(length(expr[[4]]) < 3){
      #ifStatement[[length(ifStatement) + 1]] <- list(filter = expr[[2]])
      toRet[[length(toRet) + 1]] <- getVarCoef(expr[[3]], sets, ifStatement = expr[[2]], coefficient = coefficient)
      # Criar aqui uma lista que carregue o filtro para aplicar como com %in% marg nos mappings
      } else {
        #ifStatement[[length(ifStatement) + 1]] <- list(filter = expr[[2]])
        toRet[[length(toRet) + 1]] <- getVarCoef(expr[[3]], sets, ifStatement = expr[[2]], coefficient = coefficient)
        toRet[[length(toRet) + 1]] <- getVarCoef(expr[[4]][[3]], sets, ifStatement, coefficient = coefficient)
      }
  } else if (expr[[1]] == "sum") {
    # If the top operator is sum: add to set/index
    sets[[length(sets) + 1]] <- list(index = expr[[2]], set = expr[[3]])
    # browser()
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[4]], sets, ifStatement, coefficient = coefficient)
  } else if (expr[[1]] == "[") {
    toRet[[length(toRet) + 1]] <- list(
      variable = expr,
      sets = sets,
      ifStatement = ifStatement,
      coefficient = coefficient
    )
  } else if (expr[[1]] == "{") {
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[2]], sets, ifStatement, coefficient = coefficient)
  } else if (expr[[1]] == "(") {
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[2]], sets, ifStatement, coefficient = coefficient)
  } else if (expr[[1]] == "+") {
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[2]], sets, ifStatement, coefficient = coefficient)
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[3]], sets, ifStatement, coefficient = coefficient)
  } else if (expr[[1]] == "-" & length(expr) == 3) {
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[2]], sets, ifStatement, coefficient = coefficient)
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[3]], sets, ifStatement, coefficient = call("-", coefficient))
  } else if (expr[[1]] == "-" & length(expr) == 2) {
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[2]], sets, ifStatement, coefficient = call("-", coefficient))
  } else if (expr[[1]] == "*") {
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[3]], sets, ifStatement, coefficient = call("*", coefficient, expr[[2]]))
  } else if (expr[[1]] == "=") {
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[2]], sets, ifStatement, coefficient = coefficient)
    toRet[[length(toRet) + 1]] <- getVarCoef(expr[[3]], sets, ifStatement, coefficient = call("*", coefficient, -1))
  }
  return(toRet)
}


unlistVarCoef <- function(obj) {
  toRet <- list()
  for (e in obj) {
    if (!is.null(e$variable)) {
      toRet[[length(toRet) + 1]] <- e
    } else {
      getResults <- unlistVarCoef(e)
      for (ge in getResults) {
        toRet[[length(toRet) + 1]] <- ge
      }
    }
  }
  return(toRet)
}



generateEquationCoefficients <- function(equationStatements) {
  # toRet = list('equationMatrixList=list()')
  toRet <- list()
  for (s in equationStatements) {
    # Get the formula for each variable
    frm <- correctFormula(s$parsed$equation)

    variables <- unlistVarCoef(getVarCoef(frm))
    
    for (v in 1:length(variables)) {
      if (length(variables[[v]]$variable) == 1) {
        variables[[v]]$indices <- list()
        variables[[v]]$varname <- variables[[v]]$variable
      } else {
        variables[[v]]$indices <- as.list(variables[[v]]$variable[3:length(variables[[v]]$variable)])
        variables[[v]]$indices <- lapply(variables[[v]]$indices, deparse)
        variables[[v]]$varname <- variables[[v]]$variable[[2]]
      }
      variables[[v]]$qualifiers <- Map(
        function(f) {
          sprintf("all(%s,%s)", deparse(f$index), deparse1(f$set))
        },
        variables[[v]]$sets
      )
    }

    dimensions <- s$parsed$elements[grep("\\(all,", s$parsed$elements)]
    qualifiers <- gsub("<>", "!=", gsub(":", ",", gsub(
      "=", "==", gsub("\\(all,", "all(", dimensions)
    )))


    equationName <- s$parsed$equationName
    equationIndices <- Map(function(f) str2lang(f)[[2]], qualifiers)

    # Loop throuch each variable mentioned in this equation
    for (v in variables) {
      # Loop through the qualifiers (zeroth is the initial equation)
      expr <- sprintf(
        "list(list(equation=%s,variable = %s, expression= %s))",
        ifelse(length(equationIndices) > 0, sprintf(
          "sprintf('%s[%s]',%s)",
          equationName,
          paste(rep('\"%s\"', length(equationIndices)), collapse = ","),
          paste(unlist(equationIndices), collapse = ",")
        ), sprintf(
          '"%s[%s]"',
          equationName,
          paste(rep('\"%s\"', length(equationIndices)), collapse = ",")
        )),
        ifelse(
          length(v$indices) > 0,
          sprintf(
            "sprintf('%s[%s]',%s)",
            deparse1(v$varname),
            paste(rep('\"%s\"', length(v$indices)), collapse = ","),
            paste(unlist(v$indices), collapse = ",")
          ),
          sprintf(
            '"%s[%s]"',
            deparse1(v$varname),
            paste(rep('\"%s\"', length(v$indices)), collapse = ",")
          )
        ),
        deparse1(sumToMap(v$coefficient))
      )
      for (qualifier in c(qualifiers, v$qualifiers)) {
        q <- str2lang(qualifier)
        set = q[[3]]
        
        if (length(v$ifStatement)>0) { 
          if (q[[2]] == v$ifStatement[[2]]) {
            v$ifStatement[[2]] = str2lang(deparse1(set))
            set = str2lang(sprintf("%s[%s]", deparse1(set), deparse1(v$ifStatement)))
          }
        }
        
        expr <- sprintf(
          "unlist(Map(function(%s)%s,%s),recursive=F,use.names=F)",
          deparse1(q[[2]]),
          expr,
          deparse1(set)
        )
      }
      toRet[[length(toRet) + 1]] <- sprintf("%s", expr)
    }
  }

  combinedExpression <- sprintf("equationMatrixList=unlist(list(%s),recursive=FALSE)", paste(toRet, collapse = ","))


  f <- str2lang("function(data)return(data)")
  w <- str2lang("within(data,{})")
  # for (tr in toRet) {
  #   w[[3]][[length(w[[3]]) + 1]] = str2lang(tr)
  # }

  w[[3]][[length(w[[3]]) + 1]] <- parse(text = combinedExpression)[[1]]

  f[[3]][[2]] <- w

  return(eval(f))
}

# v = variables[[4]]

# qualifier = qualifiers[[1]]

# qualifier = qualifiers[[1]]


# q <- str2lang(qualifier)
# if (length(v$ifStatement)>0) {
#   if (q[[2]] == v$ifStatement[[1]]$filter[[2]]) {
#   v$ifStatement[[1]]$filter[[2]] = str2lang(deparse1(q[[3]]))
#   expr <- sprintf(
#     "unlist(Map(function(%s)%s,%s),recursive=F,use.names=F)",
#     deparse1(q[[2]]),
#     expr,
#     deparse1(v$ifStatement[[1]]$filter)
#   )
#   }
#
# }









