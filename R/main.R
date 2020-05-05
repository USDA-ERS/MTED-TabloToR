#' @export
#' @param tabloFileName Path to the tablo file
#' @param filePaths A list of paths to all files mentioned in the tablo file
tabloToR = function(tabloFileName, filePaths) {

  # Read all definitions from the tablo file and return a series of R objects
  # all coefficients
  tablo = tabloToObjects(tabloFileName, filePaths)

  # generate sets (we only supply set definitions and file data; we get back sets)
  sets = generateSets(Filter(function(f)f$class=='set',tablo$definitions), fileData)

  # generate variables
  variables = generateVariables(tablo$variableDefinitions, sets)

  return(list(
    variables = variables,
    equations = equations,
    modelMatrix = modelMatrix
  ))
}
