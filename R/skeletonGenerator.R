generateSkeleton <- function(statements) {
  toRet <- list('"/" <- function(x,y) ifelse(y==0,0,base:::"/"(x,y))
')
  for (s in statements) {
    if (s$class == "set") {
      toRet[[length(toRet) + 1]] <- processSetStatement(s)
    } else if (s$class == "formula") {
      toRet[[length(toRet) + 1]] <- processFormulaStatement(s)
    #} else if (s$class  %in% c("coefficient", "variable")) {
    } else if (s$class  %in% c("coefficient")) {
      toRet[[length(toRet) + 1]] <- processCoefficientStatement(s)
    } else if (s$class == "read") {
      toRet[[length(toRet) + 1]] <- processReadStatement(s)
    } else if (s$class == "mapping") {
      toRet[[length(toRet) + 1]] <- processmappingStatement(s)
    }
  }
  
  f <- str2lang("function(data)return(data)")
  w <- str2lang("within(data,{})")
  
  for (tr in toRet) {
    w[[3]][[length(w[[3]]) + 1]] <- str2lang(paste(tr, collapse = ""))
  }
  
  f[[3]][[2]] <- w
  
  return(eval(f))
}
