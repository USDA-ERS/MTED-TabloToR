processEquationStatement = function(s){
  frm = correctFormula(s$parsed$equation)
  frm[[1]]=as.name('-')


  dimensions = s$parsed$elements[grep('\\(all,', s$parsed$elements)]
  qualifiers = gsub('<>', '!=', gsub(':', ",", gsub(
    '=', '==', gsub('\\(all,', 'all(', dimensions)
  )))

  equationName = s$parsed$equationName
  equationIndices = paste(Map(function(f)str2lang(f)[[2]],qualifiers),collapse=',')
  if(is.null(equationIndices)){
    equationIndices=''
  }

  frm = str2lang(sprintf('%s = %s', sprintf('%s[%s]',equationName, equationIndices), paste(deparse(frm,width.cutoff = 500),collapse='')))

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
            paste(deparse(frm, width.cutoff = 500),collapse=''),
            sprintf(
              'if(%s){%s}',
              paste(condition, collapse = '&'),
              paste(deparse(frm, width.cutoff = 500),collapse='')
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
  return(deparse(sumToMap(str2lang(expr)), width.cutoff = 500))
}
