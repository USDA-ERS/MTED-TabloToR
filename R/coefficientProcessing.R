generateCoefficients = function(coefficientStatements){
  toRet = list()
  for(s in coefficientStatements){
    dimensions = s$parsed$elements[grep('\\(all,', s$parsed$elements)]
    if (length(dimensions) == 0) {
      #toRet[[s$parsed$equation]] = NA
      toRet[[length(toRet)+1]]=sprintf('%s=NA',s$parsed$equation)
    } else {
      qualifiers = gsub('\\(all,', 'all(', dimensions)

      dn = list()
      for (q in qualifiers) {
        dn[[str2lang(q)[[2]]]] = str2lang(q)[[3]]
      }


#      toRet[[str2lang(s$parsed$equation)[[1]]]] = array(NA, dim = unlist(Map(function(f)length(f), dn)), dimnames = dn)
      pe=str2lang(s$parsed$equation)
      toRet[[length(toRet)+1]]=sprintf('%s = array(NA, dim = c(%s), dimnames = list(%s))',as.character(pe[[1]]),paste('length(',Map(function(f)dn[[pe[[f]]]],2:length(pe)),')',sep='',collapse=','),paste(Map(function(f)sprintf('%s=%s',as.character(dn[[pe[[f]]]]),as.character(dn[[pe[[f]]]])),2:length(pe)),sep='',collapse=','))
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
