# This function is to take a TABLO file (text) and return:
# a function to set initial coefficient values
# a function to update coefficients
# a function to formulas to update coefficients, a model matrix to allow for solution

processTablo = function(tablo) {
  statements = tabloToStatements(tablo)

  gs = generateSets(Filter(function(f)
    f$class == 'set', statements))
  gc = generateCoefficients(Filter(function(f)
    f$class == 'coefficient', statements))
  gr = generateReads(Filter(function(f)
    f$class == 'read', statements))
  gf = generateFormulas(Filter(function(f)
    f$class == 'formula', statements))
  gem = generateEquationCoefficientMatrix(Filter(function(f)
    f$class == 'variable', statements), Filter(function(f)
    f$class == 'equation', statements))
  gec = generateEquationCoefficients(Filter(function(f)
    f$class == 'equation', statements))

  return(
    list(
      setGenerator = gs,
      coefficientGenerator = gc,
      readGenerator = gr,
      formulaGenerator = gf,
      equationCoefficientMatrixGenerator = gem,
      equationCoefficientGenerator = gec
    )
  )
}

pt = processTablo('d:/temp/gtap.tab')

require(HARr)
data=list(
  GTAPSETS=read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\sets.har'),
  GTAPPARM=read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\default.prm'),
  GTAPDATA=read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\basedata.har')
)

data=pt$setGenerator(data)
data=pt$coefficientGenerator(data)
data=pt$readGenerator(data)
data=pt$formulaGenerator(data)
data=pt$equationCoefficientMatrixGenerator(data)
data=pt$equationCoefficientGenerator(data)

data$VVA

GTAPSETS=read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\sets.har')
