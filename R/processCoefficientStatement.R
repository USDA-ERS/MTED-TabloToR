processCoefficientStatement <- function(s) {
  dimensions <- s$parsed$elements[grep("\\(all,", s$parsed$elements)]
  if (length(dimensions) == 0) {
    # toRet[[s$parsed$equation]] = NA
    return(sprintf("%s=NA", s$parsed$equation))
  } else {
    qualifiers <- gsub("\\(all,", "all(", dimensions)

    dn <- list()
    for (q in qualifiers) {
      dn[[str2lang(q)[[2]]]] <- str2lang(q)[[3]]
    }


    #      toRet[[str2lang(s$parsed$equation)[[1]]]] = array(NA, dim = unlist(Map(function(f)length(f), dn)), dimnames = dn)
    pe <- str2lang(s$parsed$equation)
    return(
      sprintf(
        "%s = array(NA, dim = c(%s), dimnames = list(%s))",
        as.character(pe[[1]]),
        paste(
          "length(",
          Map(function(f) {
            dn[[pe[[f]]]]
          }, 2:length(pe)),
          ")",
          sep = "",
          collapse = ","
        ),
        paste(
          Map(
            function(f) {
              sprintf("%s=%s", as.character(dn[[pe[[f]]]]), as.character(dn[[pe[[f]]]]))
            },
            2:length(pe)
          ),
          sep = "",
          collapse = ","
        )
      )
    )
  }
}
