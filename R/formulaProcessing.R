sumToMap = function(expr) {
    if (class(expr) == 'name') {
    return(expr)
  }

  if (expr[[1]] == 'sum') {
    index = expr[[2]]
    set = expr[[3]]
    expression = expr[[4]]

    expr[[2]] = str2lang('unlist()')
    expr[[4]] = NULL
    expr[[3]] = NULL
    expr[[2]][[2]] = str2lang(sprintf(
      'Map(function(%s)%s,%s)',
      deparse(index),
      deparse(sumToMap(expression), width.cutoff = 500),
      deparse(set)
    ))

  }else  if (length(expr) > 1) {
    for (i in 2:length(expr)) {
      expr[[i]] = sumToMap(expr[[i]])
    }

  }
  return(expr)
}



generateFormulas = function(formulaStatements) {
  toRet = list()
  for (s in formulaStatements) {
    frm = correctFormula(s$parsed$equation)

    dimensions = s$parsed$elements[grep('\\(all,', s$parsed$elements)]
    qualifiers = gsub('<>', '!=', gsub(':', ",", gsub(
      '=', '==', gsub('\\(all,', 'all(', dimensions)
    )))

    if (length(qualifiers) > 0) {
      condition = list()
      for (l in 1:length(qualifiers)) {
        q = str2lang(qualifiers[[l]])
        if (length(q) == 4) {
          condition[[length(condition) + 1]] = deparse(correctFormula(deparse(q[[4]])), width.cutoff = 500)
        }
      }

      for (l in 1:length(qualifiers)) {
        q = str2lang(qualifiers[[l]])
        if (l == 1) {
          expr = sprintf(
            'for(%s in %s){%s}',
            deparse(q[[2]]),
            deparse(q[[3]]),
            ifelse(
              length(condition) == 0,
              deparse(frm, width.cutoff = 500),
              sprintf(
                'if(%s){%s}',
                paste(condition, collapse = '&'),
                deparse(frm, width.cutoff = 500)
              )
            )
          )
        } else {
          expr = sprintf(
            'for(%s in %s){%s}',
            deparse(q[[2]], width.cutoff = 500),
            deparse(q[[3]], width.cutoff = 500),
            expr
          )
        }
      }
    } else{
      expr = deparse(frm, width.cutoff = 500)
    }
    toRet[[length(toRet) + 1]] = expr


  }

  f = str2lang('function(data)return(data)')
  w = str2lang('within(data,{})')
  for (tr in toRet) {
    w[[3]][[length(w[[3]]) + 1]] = sumToMap(str2lang(tr))
  }

  f[[3]][[2]] = w

  return(eval(f))
}

#
# toRet[[55]]
#
# sum(r, REG, VST[l, r])
#
#
# x = str2lang('sum(r,REG,POP[r])')
# y = str2lang(
#   sprintf(
#     '%s(unlist(Map(function(%s)%s,%s)))',
#     deparse(x[[1]]),
#     deparse(x[[2]]),
#     deparse(x[[4]]),
#     deparse(x[[3]])
#   )
# )
#
# REG = c('A', 'B')
# PROD = c('P','R','S')
#
# VPA = array(c(1, 2, 3, 4), dim = c(3,2), dimnames = list(PROD,REG))
# VGA = array(c(1, 2, 3, 4), dim = c(2), dimnames = list(REG))
#
# eval(sumToMap(str2lang('sum(r,REG,VGA[r]+sum(j,PROD,VPA[j,r]))')))
#
#
# POP = array(c(1, 2, 3, 4), dim = c(4), dimnames = list(REG))
# eval(y)
#
# sumToMap(str2lang('sum(r,REG,VGA[r]+sum(j,PROD,VPA[j,r]))'))
#
#
# sum(VST[l, REG])
#
# sum(Map(function(r)
#   , VXWD[i, r, s]))
#
# sum(r, REG, sum(s, REG, VXWD[i, r, s])) / (sum(k,
#                                                TRAD_COMM, sum(r, REG, sum(s, REG, VXWD[k, r, s]))) +
#                                              sum(l, MARG_COMM, sum(r, REG, VST[l, r])))
