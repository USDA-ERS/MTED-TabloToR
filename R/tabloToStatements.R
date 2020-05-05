
removeComments <- function(linn) {
  # Remove line comments

  #linn = linn[substr(linn, 1, 1) != "!"]
  linn = linn[nchar(linn) > 0]

  # Remove block comments

  inComment = F
  include = c()
  for (i in 1:length(linn)) {
    if (substr(linn[i], 1, 2) == "/*" & inComment == F) {
      inComment = T
    } else if (substr(linn[i], 1, 2) == "*/" & inComment == T) {
      inComment = F
    } else if (!inComment) {
      include = c(include, i)
    }
  }

  linn = linn[include]
  return(linn)
}


breakLine <- function(line, definitions) {
  return(Map(function(d)
    trimws(substr(
      line,
      ifelse(is.null(d$from), 1, d$from),
      ifelse(is.null(d$to), nchar(line), d$to)
    )), definitions))

}
readFirstWord = function(statement){
  firstWord = ''
  for(i in 1:nchar(statement)){
    curLetter = substr(statement,i,i)
    if(grepl('[A-Za-z0-9_]',curLetter)){
      firstWord = paste(firstWord,curLetter,sep='')
    } else {
      return(list(firstWord = firstWord, rest = trimws(substr(statement,i,nchar(statement)))))
    }
  }
  return(list(firstWord = firstWord, rest = trimws(substr(statement,i,nchar(statement)))))
}
cleanLine = function(line) {
  inComment = F
  lineClean = ''
  comment = ''
  for (i in 1:nchar(line)) {
    if (substr(line, i, i) == '#') {
      inComment = !inComment
      #      comment = paste(comment, substr(line, i, i), sep = '')
    } else if (!inComment) {
      if (!(substr(line, i, i) == ' ' &
            substr(lineClean, nchar(lineClean), nchar(lineClean)) == ' ')) {
        lineClean = paste(lineClean, substr(line, i, i), sep = '')
      }
    } else {
      comment = paste(comment, substr(line, i, i), sep = '')
    }
  }

  comment = trimws(comment)
  lineClean = trimws(lineClean)
  lineClean=gsub('\\[','(',lineClean)
  lineClean=gsub('\\]',')',lineClean)
  return(list(
    comment = comment,
    statement = lineClean
  ))
}


# Take a file name, read the file, remove comments and return a vector of tablo lines
fileToLines = function(fileName){

  file = readChar(fileName, file.info(fileName)$size)

  inComment = F
  fileClean = ''
  for (i in 1:nchar(file)) {
    if (substr(file, i, i) == '!') {
      inComment = !inComment
    } else if (!inComment) {
      if (!is.element(substr(file, i, i) , c('\r', '\n'))) {
        fileClean = paste(fileClean, substr(file, i, i), sep = '')
      }
    }
  }

  return(strsplit(fileClean, ';', fixed = T)[[1]])

}

generateParsedInput = function(statement){
  # Pattern ()()expression

  inParenthesis=0
  element = ''
  elements=list()

  for(i in 1:nchar(statement)){
    if(substr(statement,i,i)=='(' & inParenthesis==0){
      element = paste(element,substr(statement,i,i),sep='')
      inParenthesis = inParenthesis+1
    }
    else if(inParenthesis==0 & ! substr(statement,i,i) %in% c(' ')){
      break
    } else if (inParenthesis>0 & substr(statement,i,i)==')'){
      element = paste(element,substr(statement,i,i),sep='')
      if(inParenthesis==1){
        elements[[length(elements)+1]]=element
        element=''
      }
      inParenthesis=inParenthesis-1
    }
    else{
      element = paste(element,substr(statement,i,i),sep='')
      if(substr(statement,i,i)=='('){
        inParenthesis = inParenthesis+1
      }
    }
  }

  equation = substr(statement, i,nchar(statement))

  return(list(elements=elements, equation = equation))
}


# This takes as input a filename for a tablo file and returns a list of statements
tabloToStatements = function(tablo){

  #filename <- 'd:/temp/gtap.tab'
  lines = fileToLines(filename)

  # Get a set of lines wiht comments out
  cleanLines = Map(cleanLine, lines)
  names(cleanLines)=NULL

  cleanLines = Map(function(f){
    temp = readFirstWord(f$statement)
    f$class = tolower(temp$firstWord)
    f$command = temp$rest
    #class(f$command)=f$class
    return(f)
  }
  , cleanLines)


  cleanLinesParsed = Map(function(f){
    if(f$class=='equation'){
      # Equations are recorded very differently from the rest of the objects in TABLO
      getEquationName=readFirstWord(f$command)
      temp =generateParsedInput(getEquationName$rest)
      temp$equationName=getEquationName$firstWord
    }else {
      temp = generateParsedInput(f$command)
    }
    f$parsed = temp
    return(f)
  },cleanLines)


}
