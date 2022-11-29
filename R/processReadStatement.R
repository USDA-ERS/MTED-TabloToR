processReadStatement = function(s) {
  if (grepl(".* from file .* header \"*\"",
            s$command)) {
    words = strsplit(s$command, " ")[[1]]

    return(sprintf('%s[] = %s$`%s`', words[1], words[4], gsub("\"", "", words[6])))
  }

}
