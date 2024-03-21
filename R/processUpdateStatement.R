processUpdateStatement <- function(s) {
  dimensions <- s$parsed$elements[grep("\\(all,", s$parsed$elements)]
  qualifiers <- gsub("<>", "!=", gsub(":", ",", gsub(
    "=", "==", gsub("\\(all,", "all(", dimensions)
  )))



  frm <- correctFormula(s$parsed$equation)

  if (s$class == "update") {
    if (any(grepl("\\(change\\)", s$parsed$elements, ignore.case = T)) == T) {
      frm[[3]] <- call("+", frm[[2]], frm[[3]])
    } else if (length(frm[[3]]) == 1) {
      frm[[3]] <- call("*", frm[[2]], call("+", 1, call("/", frm[[3]], 100)))
    } else {
      if (frm[[3]][[1]] == "*") {
        # frm[[3]] = call('*', frm[[2]] , call('+', 1, call('+', call(
        #   '/', frm[[3]][[2]], 100
        # ),  call('/', frm[[3]][[3]], 100))))
        frm[[3]] <- call("*", frm[[2]], call("*", call("+", 1, call(
          "/", frm[[3]][[2]], 100
        )), call("+", 1, call("/", frm[[3]][[3]], 100))))
      } else {
        frm[[3]] <- call("*", frm[[2]], call("+", 1, call("/", frm[[3]], 100)))
      }
    }
  }



  if (length(qualifiers) > 0) {
    condition <- list()
    for (l in 1:length(qualifiers)) {
      q <- str2lang(qualifiers[[l]])
      if (length(q) == 4) {
        condition[[length(condition) + 1]] <- deparse1(correctFormula(deparse(q[[4]])))
      }
    }

    for (l in 1:length(qualifiers)) {
      q <- str2lang(qualifiers[[l]])
      if (l == 1) {
        expr <- sprintf(
          "for(%s in %s){%s}",
          deparse1(q[[2]]),
          deparse1(q[[3]]),
          ifelse(
            length(condition) == 0,
            deparse1(frm),
            sprintf(
              "if(%s){%s}",
              paste(condition, collapse = "&"),
              deparse1(frm)
            )
          )
        )
      } else {
        expr <- sprintf(
          "for(%s in %s){%s}",
          deparse1(q[[2]]),
          deparse1(q[[3]]),
          expr
        )
      }
    }
  } else {
    expr <- deparse1(frm)
  }
  return(deparse1(sumToMap(str2lang(expr))))
}
