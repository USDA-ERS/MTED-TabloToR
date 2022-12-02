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

  gem = generateEquationCoefficientMatrix(
    Filter(function(f)
      f$class == 'variable', statements),
    Filter(function(f)
      f$class == 'equation', statements)
  )
  gec = generateEquationCoefficients(Filter(function(f)
    f$class == 'equation', statements))

  gev = generateVariables(Filter(function(f)
    f$class == 'variable', statements))
  geq = generateEquationLevels(Filter(function(f)
    f$class == 'equation', statements))
  gup = generateUpdates(Filter(function(f)
    f$class %in% c('update','formula'), statements))

  return(
    list(
      skeletonGenerator = generator,
      equationCoefficientMatrixGenerator = gem,
      equationCoefficientGenerator = gec,
      changeVariables = unlist(Map(
        function(f){
          paren = regexpr('\\(', f$parsed$equation)
          if(paren==-1) {
            return(f$parsed$equation)
          } else {
            return(substr(f$parsed$equation,1,paren-1))
          }
        },
        Filter(
          function(f)
            #!'(change)' %in% f$parsed$element,
            any(grepl('change',f$parsed$element)),
          Filter(function(f)
            f$class == 'variable', statements)
        )
      )),
      generateEquationLevelValues = geq,
      generateVariables = gev,
      generateUpdates = gup
    )
  )
}
