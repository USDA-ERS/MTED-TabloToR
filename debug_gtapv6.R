model = tabloToR::GEModel$new()


source(here::here("R/tabloToStatements.R"))
statements = tabloToStatements(here::here("data/gtapv6/gtapv6.tab")) # OK





source(here::here("R/tabloToStatements.R"))
source(here::here("R/processCoefficientStatement.R"))
source(here::here("R/processReadStatement.R"))
source(here::here("R/processSetStatement.R"))
source(here::here("R/processFormulaStatement.R"))
source(here::here("R/setProcessing.R"))
source(here::here("R/processTablo.R"))
source(here::here("R/skeletonGenerator.R"))
source(here::here("R/formulaprocessing.R"))
source(here::here("R/equationMatrixProcessing.R"))
source(here::here("R/equationProcessing.R"))
source(here::here("R/variableGenerator.R"))
source(here::here("R/equationlevelGenerator.R"))
source(here::here("R/processEquationSetupStatement.R"))
source(here::here("R/processEquationStatement.R"))
source(here::here("R/updateGenerator.R"))
source(here::here("R/processUpdateStatement.R"))
source(here::here("R/tabloToStatements.R"))
source(here::here("R/helpers.R"))



tablo = here::here("data/gtapv6/gtapv6.tab")
results <- processTablo(here::here("data/gtapv6/gtapv6.tab"))


as.data.frame.table(txt)


write.table(txt, file = "tablo_r.txt", sep = "\t",
            row.names = TRUE, col.names = NA)



class(txt)

# Read the model

model = tabloToR::GEModel$new()
tablo <- here::here("data/TablotoR/gtap.tab")
model$loadTablo(here::here("data/TablotoR/gtap.tab"))

# Read the databases

data = list(
  gtapsets = HARr::read_har(here::here("data/TablotoR/sets.har")),
  gtapparm = HARr::read_har(here::here("data/TablotoR/default.prm")),
  gtapdata = HARr::read_har(here::here("data/TablotoR/basedata.har"))
)

statements = tabloToStatements(tablo) # OK
generator <- generateSkeleton(Filter(
  function(f)
    f$class %in% c('set', 'coefficient', 'read', 'formula'),
  statements
))

model$loadData(data)

variables <- results$variables
basicChangeVariables <- results$changeVariables
inputData = data
loadData = function(inputData) {
  #browser()
  data <- results$skeletonGenerator(inputData)
  data <- results$equationCoefficientMatrixGenerator(data)
  data <- results$generateVariables(data)
  variableValues <- data[variables]
  changeVariables <- data$variables[substr(data$variables,1,regexpr('\\[',data$variables)-1) %in% basicChangeVariables]
}

a <- loadData(inputData)



#### Run the model #####






generateSolution = function(subShocks){
  #browser()
  iNames = unlist(Map(function(i)
    i$equation, data$equationMatrixList))
  iNumbers = data$equationNumbers[iNames]
  jNames = unlist(Map(function(i)
    i$variable, data$equationMatrixList))
  jNumbers = data$variableNumbers[jNames]
  
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
  
  #pctChanges = setdiff(names(xValues),changeVariables)
  #toChange = which(names(xValues) %in% relChangeVariables)
  
  #browser()
  
  #xValues[pctChanges] = xValues[pctChanges] * 0.01
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
  
  a <- data$eqcoeff
  
  equations |> write.csv2("euations.csv")
  
  bigMatrix = data$eqcoeff[, setdiff(colnames(data$eqcoeff), names(shocks)), drop = FALSE]
  
  #browser()
  
  smallMatrix = data$eqcoeff[, names(shocks), drop  = FALSE]
  
  # ### Do backsolving first
  # bigMatrix2 = as(bigMatrix, 'TsparseMatrix')
  # tt=table(bigMatrix2@j)
  # removeJ=bigMatrix2@j[which(bigMatrix2@j %in% as.numeric(names(tt)[tt==1]))]
  # removeI=bigMatrix2@i[which(bigMatrix2@j %in% as.numeric(names(tt)[tt==1]))]
  #
  # keepI=setdiff(1:dim(bigMatrix)[1] ,removeI+1)
  # keepJ=setdiff(1:dim(bigMatrix)[1] ,removeJ+1)
  #
  # backSolveMatrixLeft = bigMatrix[removeI+1,keepJ, drop = FALSE]
  # backSolveMatrixRight = bigMatrix[removeI+1,removeJ+1, drop = FALSE]
  # bigMatrixReduced=bigMatrix[keepI,keepJ, drop = FALSE]
  
  exoVector=-smallMatrix %*% subShocks
  
  # exoVectorReduced = exoVector[keepI,,drop=FALSE]
  #
  # tictoc::tic()
  # solutionReduced = SparseM::solve(bigMatrixReduced,exoVectorReduced,sparse=T,tol=1e-40)
  # tictoc::toc()
  #
  # #browser()
  #
  # solutionExtra = SparseM::solve(backSolveMatrixRight,-backSolveMatrixLeft%*%solutionReduced,sparse=T,tol=1e-40)
  #
  # iterationSolution =c(solutionExtra,solutionReduced) [colnames(bigMatrix)]
  
  iterationSolution=SparseM::solve(bigMatrix,exoVector,sparse=T,tol=1e-40)
  
  return(iterationSolution)
}




solveModel = function(iter = 1, steps = 1) {
  
  # Create a shock variable
  
  #browser()
  
  shocks <- do.call(c,unname(Map(function(f){
    toVector(variableValues[[f]],f)
  }, names(variableValues))))
  
  # shocks |> class() |> write.csv2("shocks.csv")
  # shocks[500:600]
  
  shocks<-shocks[!is.na(shocks)]
  
  #browser()
  
  # shocks for change variables are not compounded
  #subShocks = shocks/iter
  
  # # list of relevant change variables in shocks
  # pctChangeShocks = setdiff(names(subShocks), changeVariables)
  #
  # # shocks need to be split for each subinterval
  # subShocks[pctChangeShocks] = (exp(log(1+shocks[pctChangeShocks]/100)/iter)-1)*100
  
  
  #names(subShocks)=names(shocks)
  
  solution <<- as.numeric(c())
  
  iterationSolution = list()
  
  appliedShocks = shocks
  appliedShocks[] = 0
  
  it=1
  iter=1
  step = 1
  steps = 1
  currentStep = 1
  # Go through each iteration (subinterval)
  for (it in 1:iter) {
    message(sprintf('Iteration %s/%s', it, iter))
    
    remainingShocks = ((1+shocks/100)/(1+appliedShocks/100)-1)*100
    subShocks = remainingShocks/(iter-it+1)
    
    appliedShocks = ((1+ appliedShocks/100) * (1+subShocks/100)-1)*100
    
    # Within each iteration (subinterval) do steps
    
    # Save the state of the model
    originalData = data
    
    stepSolution = list()
    
    for(step in 1:length(steps)){
      
      message(sprintf('Step set %s/%s', step,length(steps)))
      
      # In each step set start from the original state of data
      data <- originalData
      
      # Except for change variables...
      # stepShocks = subShocks/steps[step]
      
      # .... step shocks are compunded
      #stepShocks[pctChangeShocks] =  (exp(log(1+subShocks[pctChangeShocks]/100)/steps[step])-1)*100
      
      subStepSolution=list()
      
      appliedSubShocks = subShocks
      appliedSubShocks[] = 0
      
      
      for(currentStep in 1:steps[step]){
        
        remainingSubShocks = ((1+subShocks/100)/(1+appliedSubShocks/100)-1)*100
        
        stepShocks = remainingSubShocks/(steps[step]-currentStep+1)
        
        appliedSubShocks = ((1+ appliedSubShocks/100) * (1+stepShocks/100)-1)*100
        
        
        data <- results$equationCoefficientGenerator(data)
        message(sprintf('Step %s/%s', currentStep,steps[step]))
        #browser()
        # Solve the model for this shock
        subStepSolution[[currentStep]] = generateSolution(stepShocks)
        
        # Update the variables
        data <<- within(data,{
          eval(parse(text=sprintf("%s=%s;", names(subStepSolution[[currentStep]]), subStepSolution[[currentStep]][names(subStepSolution[[currentStep]])])))
        })
        
        # Update the shocked variables
        data <<- within(data,{
          eval(parse(text=sprintf("%s=%s;", names(stepShocks), stepShocks[names(stepShocks)])))
        })
        
        # Update the data
        data <<- generateUpdates(data)
        
      }
      #browser()
      
      
      stepSolution[[step]] = rowSums(do.call(cbind,subStepSolution))
      
      solutionPctChangeVariables = setdiff(names(stepSolution[[step]]), changeVariables)
      
      stepSolution[[step]][solutionPctChangeVariables] = ((apply(do.call(cbind,subStepSolution)/100+1, MARGIN=1, FUN = prod)-1)*100)[solutionPctChangeVariables]
      #browser()
      # If any step <-100 we have to treat it as a change variable (like GEMPACK)
      # sols = apply(do.call(cbind,subStepSolution)<=-100,MARGIN = 1, any)
      #
      # solutionPctChangeVariables=setdiff(solutionPctChangeVariables, names(sols[sols]))
      
      #stepSolution[[step]][solutionPctChangeVariables] = (exp(rowSums(log(1+do.call(cbind,subStepSolution)[solutionPctChangeVariables,, drop = FALSE]/100)))-1)*100
    }
    
    #browser()
    
    if(length(steps)==1){
      
      # We only have one set of steps--this is the solution
      iterationSolution[[it]] = stepSolution[[1]]
      
    } else if(length(steps)==2) {
      
      # We have two sets of steps and so we can extrapolate
      #browser()
      iterationSolution[[it]] = colSums(t(do.call(cbind,stepSolution)) * (steps[c(1,2)] * c(1,-1))) / (steps[1]-steps[2])
      
      
      
    } else if(length(steps)==3) {
      
      # We have three sets of steps and so we can extrapolate and provide accuracy
      iterationSolution[[it]] = colSums(t(do.call(cbind,stepSolution)) * (steps[c(2,3)] * c(1,-1))) / (steps[2]-steps[3])
      
    }
    
    # tictoc::tic()
    # data <<- equationCoefficientGenerator(data)
    # tictoc::toc()
    #
    # iterationSolution = generateSolution(subShocks)
    #
    # if (length(solution)==0) {
    #   solution <<- c(iterationSolution, shocks)
    # } else{
    #
    #   namesToUse = names(solution)
    #   intermediateSolution = ifelse(names(c(iterationSolution, shocks)) %in% changeVariables, solution + c(iterationSolution, shocks), ((1 + solution / 100) * (1 + c(iterationSolution, shocks) / 100) - 1) * 100)
    #   names(intermediateSolution)=namesToUse
    #   solution<<-intermediateSolution
    # }
    
    #browser()
    
    data <<-originalData
    
    tictoc::tic()
    data <<- within(data,{
      eval(parse(text=sprintf("%s=%s;", names(iterationSolution[[it]]), iterationSolution[[it]][names(iterationSolution[[it]])])))
    })
    tictoc::toc()
    
    
    tictoc::tic()
    data <<- within(data,{
      eval(parse(text=sprintf("%s=%s;", names(shocks), subShocks[names(shocks)])))
    })
    tictoc::toc()
    
    #browser()
    data <<- generateUpdates(data)
  }
  
  #browser()
  
  if(length(iterationSolution)==1){
    solution <<- iterationSolution[[1]]
  } else {
    solution <<- rowSums(do.call(cbind,iterationSolution))
    solutionPctChangeVariables = setdiff(names(solution), changeVariables)
    solution[solutionPctChangeVariables] <<- ((apply(1+do.call(cbind,iterationSolution)/100, MARGIN = 1, FUN = prod)-1)*100)[solutionPctChangeVariables]
  }
  
  #solution[solutionPctChangeVariables]<<- (exp(rowSums(log(1+do.call(cbind,iterationSolution)[solutionPctChangeVariables,, drop = FALSE]/100)))-1)*100
  
  tictoc::tic()
  data <<- within(data,{
    eval(parse(text=sprintf("%s=%s;", names(solution), solution[names(solution)])))
  })
  tictoc::toc()
  
  tictoc::tic()
  data <<- within(data,{
    eval(parse(text=sprintf("%s=%s;", names(shocks), shocks[names(shocks)])))
  })
  
  tictoc::toc()
  
}











x <- "  Some text. "
x
trimws(x)
trimws(x, "l")










