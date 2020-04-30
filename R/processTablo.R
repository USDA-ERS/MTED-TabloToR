# This function is to take a TABLO file (text) and return:
# a function to set initial coefficient values
# a function to update coefficients
# a function to formulas to update coefficients, a model matrix to allow for solution

processTablo = function(tablo) {
  tablo = 'd:/temp/gtap.tab'
  statements = tabloToStatements(tablo)

  gs = generateSets(Filter(function(f)
    f$class == 'set', statements))
  gc = generateCoefficients(Filter(function(f)
    f$class == 'coefficient', statements))
  gr = generateReads(Filter(function(f)
    f$class == 'read', statements))
  gf = generateFormulas(Filter(function(f)
    f$class == 'formula', statements))

  return(
    setGenerator = gs,
    coefficientGenerator = gc,
    readGenerator = gr,
    formulaGenerator = gf
  )
}
