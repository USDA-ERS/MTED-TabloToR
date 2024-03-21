# This function is to take a TABLO file (text) and return:
# a function to set initial coefficient values
# a function to update coefficients
# a function to formulas to update coefficients, a model matrix to allow for solution

processTablo <- function(tablo) {
  require(Matrix)
  statements <- tabloToStatements(tablo)

  # statements = Filter(function(f)
  #   f$postsim == F, statements)

  generator <- generateSkeleton(Filter(
    function(f) {
      f$class %in% c("set", "mapping", "coefficient", "variable", "read", "formula", "formulaandequation") &
      #f$class %in% c("set", "mapping", "coefficient", "read", "formula") &
        f$postsim == F
    },
    statements
  ))

  generatorPostsim <- generateSkeleton(Filter(
    function(f) {
      f$class %in% c("set", "mapping", "coefficient", "variable", "read", "formula") |
        f$postsim == T
    },
    statements
  ))

  gem <- generateEquationCoefficientMatrix(
    Filter(function(f) {
      f$class == "variable"
    }, statements),
    Filter(function(f) {
      f$class %in% c("equation")
    }, statements)
  )
  gec <- generateEquationCoefficients(Filter(function(f) {
    f$class %in% c("equation")
  }, statements))

  gev <- generateVariables(Filter(function(f) {
    f$class == "variable"
  }, statements))
  geq <- generateEquationLevels(Filter(function(f) {
    f$class  %in% c("equation", "formulaandequation")
   # f$class == "equation"
  }, statements))
  gup <- generateUpdates(Filter(
    function(f) {
      f$class %in% c("update", "formula") &
        f$postsim == F
    },
    statements
  ))

  return(
    list(
      skeletonGenerator = generator,
      skeletonGeneratorPostsim = generatorPostsim,
      equationCoefficientMatrixGenerator = gem,
      equationCoefficientGenerator = gec,
      changeVariables = unlist(Map(
        function(f) {
          paren <- regexpr("\\(", f$parsed$equation)
          if (paren == -1) {
            return(f$parsed$equation)
          } else {
            return(substr(f$parsed$equation, 1, paren - 1))
          }
        },
        Filter(
          function(f) {
            # !'(change)' %in% f$parsed$element,
            any(grepl("change", f$parsed$element))
          },
          Filter(function(f) {
            f$class == "variable"
          }, statements)
        )
      )),
      variables = unlist(Map(
        function(f) {
          paren <- regexpr("\\(", f$parsed$equation)
          if (paren == -1) {
            return(f$parsed$equation)
          } else {
            return(substr(f$parsed$equation, 1, paren - 1))
          }
        },
        Filter(function(f) {
          f$class == "variable"
        }, statements)
      )),
      generateEquationLevelValues = geq,
      generateVariables = gev,
      generateUpdates = gup
    )
  )
}
