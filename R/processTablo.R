# This function is to take a TABLO file (text) and return:
# a function to set initial coefficient values
# a function to update coefficients
# a function to formulas to update coefficients, a model matrix to allow for solution

processTablo = function(tablo) {
  require(Matrix)
  statements = tabloToStatements(tablo)

  generator = generateSkeleton(Filter(
    function(f)
      f$class %in% c('set', 'coefficient', 'read', 'formula'),
    statements
  ))

  # gs = generateSets(Filter(function(f)
  #   f$class == 'set', statements))
  # gc = generateCoefficients(Filter(function(f)
  #   f$class == 'coefficient', statements))
  # gr = generateReads(Filter(function(f)
  #   f$class == 'read', statements))
  # gf = generateFormulas(Filter(function(f)
  #   f$class == 'formula', statements))
  gem = generateEquationCoefficientMatrix(
    Filter(function(f)
      f$class == 'variable', statements),
    Filter(function(f)
      f$class == 'equation', statements)
  )
  gec = generateEquationCoefficients(Filter(function(f)
    f$class == 'equation', statements))

  return(
    list(
      # setGenerator = gs,
      # coefficientGenerator = gc,
      # readGenerator = gr,
      # formulaGenerator = gf,
      skeletonGenerator = generator,
      equationCoefficientMatrixGenerator = gem,
      equationCoefficientGenerator = gec
    )
  )
}
#
# require(HARr)
#


# origData = list(
#   GTAPSETS = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\sets.har'),
#   GTAPPARM = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\default.prm'),
#   GTAPDATA = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\basedata.har')
# )
#



#
#
# with(data,
#      for (r in REG) {
#        for (j in PROD_COMM) {
#          for (i in ENDWS_COMM) {
#            print(sprintf("SPFACTPRICE[\"%s\",\"%s\",\"%s\"]",
#                          i, j, r))
#            print(sprintf("pfe[\"%s\",\"%s\",\"%s\"]",
#                          i, j, r))
#          }
#        }
#      })
#
# dimnames(data$eqcoeff)[[1]][grep('SPFACTPRICE', dimnames(data$eqcoeff)[[1]])]
# dimnames(data$eqcoeff)[[2]][grep('pfe', dimnames(data$eqcoeff)[[2]])]
#
# data$VVA
#
# GTAPSETS = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\sets.har')
