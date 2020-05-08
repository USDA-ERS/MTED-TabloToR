processEquationSetupStatement = function(s) {
  dimensions = s$parsed$elements[grep('\\(all,', s$parsed$elements)]
  equationName = s$parsed$equationName

  if (length(dimensions) == 0) {
    #toRet[[s$parsed$equation]] = NA
    return(sprintf('%s=NA', equationName))
  } else {
    qualifiers = gsub('<>', '!=', gsub(':', ",", gsub(
      '=', '==', gsub('\\(all,', 'all(', dimensions)
    )))

    dn = list()
    for (q in qualifiers) {
      dn[[str2lang(q)[[2]]]] = str2lang(q)[[3]]
    }

    equationIndices = paste(Map(function(f)
      str2lang(f)[[2]], qualifiers), collapse = ',')
    if (is.null(equationIndices)) {
      equationIndices = ''
    }



    return(
      sprintf(
        '%s = array(NA, dim = c(%s), dimnames = list(%s))',
        equationName,
        paste(
          'length(',
          unlist(Map(function(f)
            deparse(f), dn)),
          ')',
          sep = '',
          collapse = ','
        ),
        paste(
          Map(function(f)
            sprintf('%s=%s', deparse(f), deparse(f)),
            dn),
          sep = '',
          collapse = ','
        )
      )
    )
  }
}
