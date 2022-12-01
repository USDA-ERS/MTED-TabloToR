# properties:
#   TABLO as a recipe
#   data files as data
#   exogenous variables as a definition
# methods:
#   solve the model (for the given coefficients)
#   update data (execute all updates/formulas always)

GEModel = setRefClass(
  "GEModel",
  fields = list(
    shocks = "numeric",
    skeletonGenerator = 'function',
    equationCoefficientMatrixGenerator = 'function',
    equationCoefficientGenerator = 'function',
    generateVariables = 'function',
    generateUpdates = 'function',
    data = 'list',
    solution = 'numeric',
    changeVariables = 'character',
    basicChangeVariables = 'character'
  ),
  methods = list(
    # Loads a tablo without any data (only produces generic functions to genrate coefficients/equation coefficients etc.)
    loadTablo = function(tabloPath) {
      results = processTablo(tabloPath)
      skeletonGenerator <<- results$skeletonGenerator
      equationCoefficientMatrixGenerator <<-
        results$equationCoefficientMatrixGenerator
      equationCoefficientGenerator <<- results$equationCoefficientGenerator
      generateVariables <<- results$generateVariables
      generateUpdates <<- results$generateUpdates
      if(!is.null(results$changeVariables)){
      basicChangeVariables <<- results$changeVariables
      }
    },
    loadData = function(inputData) {
      #browser()
      data <<- skeletonGenerator(inputData)
      data <<- equationCoefficientMatrixGenerator(data)
      data <<- generateVariables(data)
      changeVariables <<- data$variables[data$variables %loosein% basicChangeVariables]
    },
    setShocks = function(shocks) {
      shocks <<- shocks
    },
    generateSolution = function(subShocks){
      iNames = unlist(Map(function(i)
        i$equation, data$equationMatrixList))
      iNumbers = data$equationNumbers[iNames]
      jNames = unlist(Map(function(i)
        i$variable, data$equationMatrixList))
      jNumbers = data$variableNumbers[jNames]

      # tictoc::tic()
      # xValues = unlist(Map(
      #   function(i)
      #     ifelse(
      #       #substr(i$variable, 1, regexpr('\\[', i$variable) - 1) %in% changeVariables,
      #       i$variable %in% changeVariables,
      #       0.01,
      #       1
      #     ) * i$expression,
      #   data$equationMatrixList
      # ))
      # tictoc::toc()

      tictoc::tic()
      xValues = unlist(Map(
        function(i)
          i$expression,
        data$equationMatrixList
      ))

      names(xValues) = unlist(Map(
        function(i)
          i$variable,
        data$equationMatrixList
      ))

      relChangeVariables = intersect(changeVariables, names(xValues))
      toChange = which(names(xValues) %in% relChangeVariables)
      xValues[toChange] = xValues[toChange] * 0.01
      #xValues2[relChangeVariables] = xValues2[relChangeVariables] * 0.01

      tictoc::toc()

      #browser()

      data$eqcoeff = sparseMatrix(
        i = iNumbers,
        j = jNumbers,
        x = xValues,
        dims = c(length(data$equations), length(data$variables)),
        dimnames = list(
          equations = data$equations,
          variables = data$variables
        )
      )


      bigMatrix = data$eqcoeff[, setdiff(colnames(data$eqcoeff), names(shocks))]

      smallMatrix = data$eqcoeff[, names(shocks)]

      ### Do backsolving first
      bigMatrix2 = as(bigMatrix, 'TsparseMatrix')
      tt=table(bigMatrix2@j)
      removeJ=bigMatrix2@j[which(bigMatrix2@j %in% as.numeric(names(tt)[tt==1]))]
      removeI=bigMatrix2@i[which(bigMatrix2@j %in% as.numeric(names(tt)[tt==1]))]

      keepI=setdiff(1:dim(bigMatrix)[1] ,removeI+1)
      keepJ=setdiff(1:dim(bigMatrix)[1] ,removeJ+1)

      backSolveMatrixLeft = bigMatrix[removeI+1,keepJ]
      backSolveMatrixRight = bigMatrix[removeI+1,removeJ+1]
      bigMatrixReduced=bigMatrix[keepI,keepJ]

      exoVector=-smallMatrix %*% subShocks

      exoVectorReduced = exoVector[keepI,,drop=F]

      solutionReduced = SparseM::solve(bigMatrixReduced,exoVectorReduced,sparse=T,tol=1e-40)

      solutionExtra = SparseM::solve(backSolveMatrixRight,-backSolveMatrixLeft%*%solutionReduced,sparse=T,tol=1e-40)

      iterationSolution =c(solutionExtra,solutionReduced) [colnames(bigMatrix)]

      return(iterationSolution)
    },
    solveModel = function(iter = 3, type='linear') {

      # subShocks = ifelse(names(shocks) %in% changeVariables, shocks/iter, 100 * (exp(log(1 + shocks / 100) / iter) - 1))

      subShocks = shocks/iter

      names(subShocks)=names(shocks)

      #message(subShocks[subShocks!=0])

      solution <<- as.numeric(c())

      for (it in 1:iter) {
        message(sprintf('Iteration %s', it))
        tictoc::tic()
        data <<- equationCoefficientGenerator(data)
        tictoc::toc()
        if(type =='linear'){
          iterationSolution = generateSolution(subShocks)
        }else if (type=='quadratic'){
          message('A')
          iterationSolution1 = generateSolution(subShocks*0.001)
          oldData <<- data
          message('B')

          tempSolution <<- as.numeric(c())
          names(subShocks)=names(shocks)

          if (length(tempSolution)==0) {
            tempSolution <<- c(iterationSolution1, subShocks*0.001)
          } else{

            namesToUse = names(tempSolution)
            intermediateSolution = ifelse(names(c(iterationSolution1, subShocks)) %in% changeVariables, tempSolution + c(iterationSolution1, subShocks*0.001), ((1 + tempSolution / 100) * (1 + c(iterationSolution1, subShocks*0.001) / 100) - 1) * 100)
            names(intermediateSolution1)=namesToUse
            tempSolution<<-intermediateSolution
          }

          gTempSolution <<- tempSolution
          gSubShocks <<- subShocks

          for (n in names(tempSolution)) {
            data <<- within(data, {
              eval(str2lang(sprintf('%s = %f', n, tempSolution[n])))
            })
          }

          for (n in names(subShocks)) {
            data <<-  within(data, {
              eval(str2lang(sprintf('%s = %f', n, subShocks[n]*0.001)))
            })
          }



          data <<- generateUpdates(data)
          data <<- equationCoefficientGenerator(data)

          newData<<-data
          iterationSolution2 = generateSolution(subShocks*0.001)
          message('C')

          data <<- oldData
          changeIterationSolution = (iterationSolution1-iterationSolution2)/0.001

          gIterationSolution1<<-iterationSolution1
          gIterationSolution2<<-iterationSolution2
          gChangeIterationSolution <<- changeIterationSolution

          iterationSolution = iterationSolution1 / 0.001 + 1/2 * changeIterationSolution/0.001 * 1^2
        }

        # iterationSolution = SparseM::solve(bigMatrix,-smallMatrix %*% subShocks,
        #                  sparse = T,
        #                  tol = 1e-20)

        # for (n in names(iterationSolution)) {
        #   data = within(data, {
        #     eval(str2lang(sprintf('%s = %f', n, iterationSolution[n])))
        #   })
        # }
        #
        # for (n in names(shocks)) {
        #   data =  within(data, {
        #     eval(str2lang(sprintf('%s = %f', n, shocks[n])))
        #   })
        # }
        #message(iterationSolution['qgdp["omn"]'])
        #message(shocks[shocks!=0])

        if (length(solution)==0) {
          solution <<- c(iterationSolution, shocks)
        } else{

          namesToUse = names(solution)
          intermediateSolution = ifelse(names(c(iterationSolution, shocks)) %in% changeVariables, solution + c(iterationSolution, shocks), ((1 + solution / 100) * (1 + c(iterationSolution, shocks) / 100) - 1) * 100)
          names(intermediateSolution)=namesToUse
          solution<<-intermediateSolution
        }

        #browser()
        tictoc::tic()
        # for (n in names(solution)) {
        #   data <<- within(data, {
        #     #eval(str2lang(sprintf('%s = %f', n, solution[n])))
        #     eval(str2lang(sprintf('%s = %f', n, iterationSolution[n])))
        #   })
        # }
        data <<- within(data,{
          eval(parse(text=sprintf("%s=%s;", names(solution), iterationSolution[names(solution)])))
        })
        tictoc::toc()


        tictoc::tic()
        # for (n in names(shocks)) {
        #   data <<-  within(data, {
        #     #            eval(str2lang(sprintf('%s = %f', n, shocks[n])))
        #                 eval(str2lang(sprintf('%s = %f', n, subShocks[n])))
        #   })
        # }
        data <<- within(data,{
          eval(parse(text=sprintf("%s=%s;", names(shocks), subShocks[names(shocks)])))
        })
        tictoc::toc()
        data <<- generateUpdates(data)
      }

      tictoc::tic()
      # for (n in names(solution)) {
      #   data <<- within(data, {
      #     eval(str2lang(sprintf('%s = %f', n, solution[n])))
      #   })
      # }
      data <<- within(data,{
        eval(parse(text=sprintf("%s=%s;", names(solution), solution[names(solution)])))
      })
      tictoc::toc()

      tictoc::tic()
      # for (n in names(shocks)) {
      #   data <<-  within(data, {
      #     eval(str2lang(sprintf('%s = %f', n, shocks[n])))
      #   })
      # }
      data <<- within(data,{
        eval(parse(text=sprintf("%s=%s;", names(shocks), shocks[names(shocks)])))
      })

      tictoc::toc()

    }
  )
)
