#' @export
tabloToR = function(tabloFileName, fileData) {
  # Read all definitions from the tablo file
  tablo = processTablo(tabloFileName)

  # generate sets (we only supply set definitions and file data; we get back sets)
  sets = generateSets(Filter(function(f)f$class=='set',table$definitions), fileData)

  # generate variables
  variables = generateVariables(tablo$variableDefinitions, sets)

  return(list(
    variables = variables,
    equations = equations,
    modelMatrix = modelMatrix
  ))
}
