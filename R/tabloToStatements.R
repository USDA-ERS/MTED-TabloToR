removeComments <- function(linn) {
  # Remove line comments

  # linn = linn[substr(linn, 1, 1) != "!"]
  linn <- linn[nchar(linn) > 0]

  # Remove block comments

  inComment <- F
  include <- c()
  for (i in 1:length(linn)) {
    if (substr(linn[i], 1, 2) == "/*" & inComment == F) {
      inComment <- T
    } else if (substr(linn[i], 1, 2) == "*/" & inComment == T) {
      inComment <- F
    } else if (!inComment) {
      include <- c(include, i)
    }
  }

  linn <- linn[include]
  return(linn)
}


breakLine <- function(line, definitions) {
  return(Map(function(d) {
    trimws(substr(
      line,
      ifelse(is.null(d$from), 1, d$from),
      ifelse(is.null(d$to), nchar(line), d$to)
    ))
  }, definitions))
}
readFirstWord <- function(statement) {
  statement <- trimws(statement)

  if (grepl("^equation", statement, ignore.case = TRUE)) {
    firstWord <- "equation"
  } else if (grepl("^variable", statement, ignore.case = TRUE)) {
    firstWord <- "variable"
  } else if (grepl("^read", statement, ignore.case = TRUE)) {
    firstWord <- "read"
  } else if (grepl("^write", statement, ignore.case = TRUE)) {
    firstWord <- "write"
  } else if (grepl("^file", statement, ignore.case = TRUE)) {
    firstWord <- "file"
  } else if (grepl("^assertion", statement, ignore.case = TRUE)) {
    firstWord <- "assertion"
  } else if (grepl("^set", statement, ignore.case = TRUE)) {
    firstWord <- "set"
  } else if (grepl("^subset", statement, ignore.case = TRUE)) {
    firstWord <- "subset"
  } else if (grepl("^coefficient", statement, ignore.case = TRUE)) {
    firstWord <- "coefficient"
  } else if (grepl("^update", statement, ignore.case = TRUE)) {
    firstWord <- "update"
  } else if (grepl("^formula", statement, ignore.case = TRUE)) {
    firstWord <- "formula"
  } else if (grepl("^zerodivide", statement, ignore.case = TRUE)) {
    firstWord <- "zerodivide"
  } else if (grepl("^mapping", statement, ignore.case = TRUE)) {
    firstWord <- "mapping"
  } else {
    firstWord <- ""
  }

  return(list(firstWord = firstWord, rest = trimws(substr(statement, nchar(firstWord) + 1, nchar(statement)))))
}

readEquationName <- function(statement) {
  statement <- trimws(statement)

  findName <- gregexpr("^[a-z]{1,}[a-z0-9_]{1,}", statement, ignore.case = TRUE)[[1]]

  firstWord <- substr(statement, findName, attributes(findName)$match.length)

  return(list(firstWord = firstWord, rest = trimws(substr(statement, attributes(findName)$match.length + 1, nchar(statement)))))
}

cleanLine <- function(line) {
  inComment <- F
  lineClean <- ""
  comment <- ""
  for (i in 1:nchar(line)) {
    if (substr(line, i, i) == "#") {
      inComment <- !inComment
      #      comment = paste(comment, substr(line, i, i), sep = '')
    } else if (!inComment) {
      if (!(substr(line, i, i) == " " &
        substr(lineClean, nchar(lineClean), nchar(lineClean)) == " ")) {
        lineClean <- paste(lineClean, substr(line, i, i), sep = "")
      }
    } else {
      comment <- paste(comment, substr(line, i, i), sep = "")
    }
  }

  comment <- trimws(comment)
  lineClean <- trimws(lineClean)
  lineClean <- gsub("\\[", "(", lineClean)
  lineClean <- gsub("\\]", ")", lineClean)
  lineClean <- gsub("\\{", "(", lineClean)
  lineClean <- gsub("\\}", ")", lineClean)
  lineClean <- gsub("\\bif\\b", "IF", lineClean)
  return(list(
    comment = comment,
    statement = lineClean
  ))
}

# Take a file name, read the file, remove comments and return a vector of tablo lines
fileToLines <- function(fileName) {
  # browser()
  file <- tolower(readChar(fileName, file.info(fileName)$size))


  beginStrongComment <- gregexpr("!\\[\\[!", file, )[[1]]
  endStrongComment <- gregexpr("!\\]\\]!", file, )[[1]]
  if (beginStrongComment[1] > 0) {
    strongCommentDepthBegin <- unlist(Map(
      function(f) {
        sum(endStrongComment <= f) - sum(beginStrongComment < f)
      },
      beginStrongComment
    ))
    strongCommentDepthEnd <- unlist(Map(
      function(f) {
        sum(beginStrongComment < f) - sum(endStrongComment <= f)
      },
      endStrongComment
    ))

    beginStrongComment <- beginStrongComment[strongCommentDepthBegin == 0]
    endStrongComment <- endStrongComment[strongCommentDepthEnd == 0]

    fileClean <- substr(file, 1, beginStrongComment[1] - 1)

    for (nn in 1:length(endStrongComment)) {
      if (nn < length(endStrongComment)) {
        fileClean <- paste0(
          fileClean,
          substr(file, endStrongComment[nn] + 5, beginStrongComment[nn + 1] - 1)
        )
      } else {
        fileClean <- paste0(fileClean, substr(file, endStrongComment[nn] + 5, nchar(file)))
      }
    }
  } else {
    fileClean <- file
  }


  comments <- gregexpr("!", fileClean, )[[1]]


  if (comments[1] > 0) {
    beginComment <- comments[c(TRUE, FALSE)]
    endComment <- comments[c(FALSE, TRUE)]


    fileClean2 <- substr(fileClean, 1, beginComment[1] - 1)

    for (nn in 1:length(beginComment)) {
      if (nn < length(beginComment)) {
        fileClean2 <- paste0(
          fileClean2,
          substr(fileClean, endComment[nn] + 1, beginComment[nn + 1] - 1)
        )
      } else {
        fileClean2 <- paste0(fileClean2, substr(fileClean, endComment[nn] + 1, nchar(fileClean)))
      }
    }
  } else {
    fileClean2 <- fileClean
  }

  smallComment <- gregexpr("#", fileClean2, )[[1]]


  beginSmallComment <- smallComment[c(TRUE, FALSE)]
  endSmallComment <- smallComment[c(FALSE, TRUE)]


  exclamations <- gregexpr(";", fileClean2, )[[1]]

  if (beginSmallComment[1] == -1) {
    breakLine <- exclamations
  } else {
    breakLine <- Filter(function(f) {
      !any(f > beginSmallComment & f < endSmallComment)
    }, exclamations)
  }


  lineBeginnings <- c(1, breakLine + 1)
  lineEnds <- c(breakLine - 1, nchar(fileClean2))

  toReturn <- unlist(Map(function(f) {
    trimws(gsub("\\n", " ", gsub("\\r", " ", substr(fileClean2, lineBeginnings[f], lineEnds[f]))))
  }, 1:length(lineBeginnings)))

  return(toReturn[nchar(toReturn) > 0])

  # fileClean2
  #
  # inComment = F
  # strongComment = 0
  # fileClean = ''
  #
  # i=1
  # while(i<=nchar(file)){
  #   #for (i in 1:nchar(file)) {
  #
  #   if(substr(file,i,i+3)=='![[!' & !inComment){
  #     strongComment = strongComment + 1
  #     i = i + 4
  #   } else if (substr(file,i,i+3)=='!]]!' & !inComment){
  #     strongComment = strongComment - 1
  #     i = i + 4
  #   } else if (substr(file, i, i) == '!' & strongComment==0) {
  #     inComment = !inComment
  #   } else if (!inComment & strongComment==0) {
  #     if (!is.element(substr(file, i, i) , c('\r', '\n'))) {
  #       fileClean = paste(fileClean, substr(file, i, i), sep = '')
  #     }
  #   }
  #   i=i+1
  # }

  # return(strsplit(fileClean, ';', fixed = T)[[1]])
}

generateParsedInput <- function(statement) {
  # Pattern ()()expression

  inParenthesis <- 0
  element <- ""
  elements <- list()

  for (i in 1:nchar(statement)) {
    if (substr(statement, i, i) == "(" & inParenthesis == 0) {
      element <- paste(element, substr(statement, i, i), sep = "")
      inParenthesis <- inParenthesis + 1
    } else if (inParenthesis == 0 & !substr(statement, i, i) %in% c(" ")) {
      break
    } else if (inParenthesis > 0 & substr(statement, i, i) == ")") {
      element <- paste(element, substr(statement, i, i), sep = "")
      if (inParenthesis == 1) {
        elements[[length(elements) + 1]] <- element
        element <- ""
      }
      inParenthesis <- inParenthesis - 1
    } else {
      element <- paste(element, substr(statement, i, i), sep = "")
      if (substr(statement, i, i) == "(") {
        inParenthesis <- inParenthesis + 1
      }
    }
  }

  equation <- substr(statement, i, nchar(statement))

  return(list(elements = elements, equation = equation))
}

generateParsedInputEquation <- function(statement) {
  # statement = "(all,i,IND)(all,o,OCC)x1lab[i,o] = x1lab_o[i] - SIGMA1LAB[i]*(p1lab[i,o] - p1lab_o[i])"
  # statement="Equation E_SalesDecompA(all,c,COM)(all,d,DEST) INITSALES(c)*SalesDecomp(c,d) = 100*delSale(c,\"dom\",d)"

  # Find all valid elements
  # In equation, you can only specify (all,X,Y)
  foundElements <- gregexpr(
    "\\(\\s*all\\s*,\\s*[a-z]{1,}[a-z0-9_]{0,}\\s*,\\s*[a-z]{1,}[a-z0-9_]{0,}\\s*\\)",
    statement,
    ignore.case = TRUE
  )


  elements <- Map(function(f) {
    substr(
      statement,
      foundElements[[1]][f],
      foundElements[[1]][f] + attributes(foundElements[[1]])$match.length[f] - 1
    )
  }, 1:length(foundElements[[1]]))

  equation <- substr(
    statement,
    foundElements[[1]][length(foundElements[[1]])] + attributes(foundElements[[1]])$match.length[length(foundElements[[1]])],
    nchar(statement)
  )

  return(list(elements = elements, equation = equation))
}

postsimSplit <- function(input) {
  # input=cleanLines



  beginPostsim <- grep(tolower('"postsim\\s*\\(begin\\)"'), input)
  endPostsim <- grep(tolower('"postsim\\s*\\(end\\)"'), input)
  # beginPostsim = which(grepl("postsim\\s*\\(begin\\)", cleanLines))
  # endPostsim = which(grepl("postsim\\s*\\(end\\)", cleanLines))
  # beginPostsim = gregexpr("postsim\\s*\\(begin\\)", file,)
  # endPostsim = gregexpr("postsim\\s*\\(end\\)", file,)[[1]]

  for (i in seq_along(input)) {
    input[[i]][["postsim"]] <- F
  }

  if (length(beginPostsim) > 0) {
    # filePostsim = c()
    # fileSim = input
    for (p in seq_along(beginPostsim)) {
      lineChunk <- beginPostsim[p]:endPostsim[p]

      # filePostsim[p] = substr(file, beginPostsim[p], endPostsim[p])
      # filePostsimTemp = input[lineChunk]
      # filePostsim = c(filePostsim, filePostsimTemp)
      for (l in lineChunk) {
        input[[l]][["postsim"]] <- T
      }
      # input[lineChunk]$PostSim = T
      #
      # fileSim = fileSim[-lineChunk]
    }
    # filePostsim = paste(filePostsim, collapse = "\n")

    # fileSim <-
    #   gsub("postsim\\s*\\(begin\\).*?postsim\\s*\\(end\\)",
    #        "",
    #        file)

    output <- input[-c(beginPostsim, endPostsim)]
  } else {
    output <- input
  }



  return(output)
}


# tablo = here::here("data/gtapv7/gtapv7.tab")
# This takes as input a filename for a tablo file and returns a list of statements
tabloToStatements <- function(tablo) {
  # browser()

  # filename <- 'd:/temp/gtap.tab'
  lines <- fileToLines(tablo)

  lines <- gsub(
    " lt ", " < ",
    gsub(
      " le ", " <= ",
      gsub(
        " gt ", " > ",
        gsub(
          " ge ", " >= ",
          gsub(
            " eq ", " == ",
            gsub(
              " ne ", " != ",
              lines
            )
          )
        )
      )
    )
  )

  # Get a set of lines wiht comments out
  cleanLines <- Map(cleanLine, lines)
  names(cleanLines) <- NULL

  cleanLines <- Map(
    function(f) {
      temp <- readFirstWord(f$statement)
      f$class <- tolower(temp$firstWord)
      f$command <- temp$rest
      # class(f$command)=f$class
      return(f)
    },
    cleanLines
  )

  # If there is no statement, then use the statment before
  for (n in 2:length(cleanLines)) {
    if (cleanLines[[n]]$class == "") {
      cleanLines[[n]]$class <- cleanLines[[n - 1]]$class
    }
  }

  cleanLinesParsed <- Map(function(f) {
    if (f$class == "equation") {
      # Equations are recorded very differently from the rest of the objects in TABLO
      getEquationName <- readEquationName(f$command)
      temp <- generateParsedInputEquation(getEquationName$rest)
      temp$equationName <- getEquationName$firstWord
    } else {
      temp <- generateParsedInput(f$command)
    }
    f$parsed <- temp
    return(f)
  }, cleanLines)

  cleanLinesParsed <- postsimSplit(cleanLinesParsed)

  return(cleanLinesParsed)
}
