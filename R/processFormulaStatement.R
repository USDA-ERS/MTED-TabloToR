  replacePos <- function(frm, qualifiers) {
    if (any(grepl("match\\(", frm))) {
      #   l =  grep("match", frm)
      #   toReplace <- frm[[l]]

      for (f in seq_along(frm)) {
        if (length(frm[[f]]) > 1) {
          if (!grepl("^match", frm[[f]][[1]])) {
            if (is.character(frm[[f]])) {
              f <- str2lang(frm[[f]])
            }
            f <- replacePos(frm[[f]])
          } else {
            if (length(f) < 3) {
              qualifiersList <- lapply(qualifiers, str2lang)

              for (i in seq_along(qualifiersList)) {
                if (qualifiersList[[i]][[2]] == frm[[f]][[2]]) {
                  set <- qualifiersList[[i]][[3]]
                }
              }
              set_ref <- frm[[2]][[2]]

              expr <- sprintf(
                "%s[match(%s, %s)]",
                paste0(deparse1(set_ref), collapse = " "),
                paste0(deparse1(frm[[f]][[2]]), collapse = " "),
                paste0(deparse1(set), collapse = " ")
              )

              # toReplace = gsub("match\\([A-Za-z]+\\)", expr, toReplace)
              frm[[f]] <- str2lang(expr)

              return(frm)
            }
          }
        }
      }
    } else {
      return(frm)
    }
  }


  processFormulaStatement <- function(s) {
    frm <- correctFormula(s$parsed$equation)

    # frm = correctFormula(gsub(":","%:%",gsub('>==','>=',gsub('<==','<=',gsub('=','=',s$parsed$equation)))))

    dimensions <- s$parsed$elements[grep("\\(all,", s$parsed$elements)]
    qualifiers <- gsub("<>", "!=", gsub(":", ",", gsub(
      "=", "==", gsub("\\(all,", "all(", dimensions)
    )))

    frm <- replacePos(frm, qualifiers)

    if (length(qualifiers) > 0) {
      condition <- list()
      for (l in 1:length(qualifiers)) {
        q <- str2lang(qualifiers[[l]])
        if (length(q) == 4) {
          condition[[length(condition) + 1]] <- paste0(deparse1(correctFormula(deparse1(q[[4]]))), collapse = " ")
        }
      }

      for (l in 1:length(qualifiers)) {
        q <- str2lang(qualifiers[[l]])
        if (l == 1) {
          expr <- sprintf(
            "for(%s in %s){%s}",
            paste0(deparse1(q[[2]]), collapse = " "),
            paste0(deparse1(q[[3]]), collapse = " "),
            ifelse(
              length(condition) == 0,
              paste0(deparse1(frm), collapse = " "),
              sprintf(
                "if(%s){%s}",
                paste(condition, collapse = "&"),
                paste0(deparse1(frm), collapse = " ")
              )
            )
          )
        } else {
          expr <- sprintf(
            "for(%s in %s){%s}",
            paste0(deparse1(q[[2]]), collapse = " "),
            paste0(deparse1(q[[3]]), collapse = " "),
            expr
          )
        }
      }
    } else {
      expr <- paste0(deparse1(frm), collapse = " ")
    }




    return(paste0(deparse1(sumToMap(str2lang(expr))), collapse = " "))
  }
