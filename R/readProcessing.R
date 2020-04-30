generateReads = function(readStatements) {
  toRet = list()
  for (s in readStatements) {
    if (grepl(".* from file .* header \"*\"",
              s$command)) {
      words = strsplit(s$command, " ")[[1]]

      toRet[[length(toRet) + 1]] = sprintf('%s[] = %s$%s', words[1], words[4], gsub("\"", "", words[6]))
    }
  }

  f = str2lang('function(data)return(data)')
  w = str2lang('within(data,{})')
  for (tr in toRet) {
    w[[3]][[length(w[[3]]) + 1]] = str2lang(tr)
  }

  f[[3]][[2]] = w

  return(eval(f))
}
