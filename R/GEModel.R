# properties:
#   TABLO as a recipe
#   data files as data
#   exogenous variables as a definition
# methods:
#   solve the model (for the given coefficients)
#   update data (execute all updates/formulas always)

setRefClass(
  "GEModel",
  fields = list(
    TABLO = "character",
    exogenousVariables = "list",
    formulas = "list",
    coefficients = "list",
    coefficientGenerator = 'function'
  ),
  methods = list(
    processTablo = function() {
      results = processTablo(TABLO)
      coefficientGenerator<<-results$coefficientGenerator
    }
  )
)
