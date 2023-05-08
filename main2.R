## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Install required R packages 

install.packages('devtools')
devtools::install_github('https://github.com/USDA-ERS/MTED-TabloToR.git',
                         auth_token = "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")
devtools::install_github('https://github.com/USDA-ERS/MTED-HARr.git',
                         auth_token = "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")



# Read the model

model = tabloToR::GEModel$new()

model$loadTablo(here::here("data/TablotoR/gtap.tab"))

# Read the databases

data = list(
  gtapsets = HARr::read_har(here::here("data/TablotoR/sets.har")),
  gtapparm = HARr::read_har(here::here("data/TablotoR/default.prm")),
  gtapdata = HARr::read_har(here::here("data/TablotoR/basedata.har"))
)

model$loadData(data)

# Set up the closure

for (var in c(
  "afall",
  "afcom",
  "afeall",
  "afecom",
  "afereg",
  "afesec",
  "afreg",
  "afsec",
  "ams",
  "aoall",
  "aoreg",
  #' qgdp',
  "aosec",
  "atall",
  "atd",
  "atf",
  "atm",
  "ats",
  "au",
  "avaall",
  "avareg",
  "avasec",
  "cgdslack",
  "dpgov",
  "dppriv",
  "dpsave",
  "endwslack",
  "incomeslack",
  "pfactwld",
  "pop",
  "profitslack",
  "psaveslack",
  # "qo(ENDW_COMM,REG)",
  "tf",
  "tfd",
  "tfm",
  "tgd",
  "tgm",
  "tm",
  "tms",
  "to",
  "tpd",
  "tpm",
  "tp",
  "tradslack",
  "tx",
  "txs"
)) {
  model$variableValues[[var]][] <- 0
}

model$variableValues$qo[model$data$endw_comm,] = 0

# Set up the shocks

ag= c("grainscrops")

model$variableValues$aoall[ag,c("eu_25")] = 1

agFood = c("meatlstk", "grainscrops")

model$variableValues$tms[agFood,,c("eu_25")] = (model$data$viws[agFood,,c("eu_25")] / model$data$vims[agFood,,c("eu_25")] -1)*100

# Run the simulation

tictoc::tic()

model$solveModel(iter = 3,steps = c(1,3))

tictoc::toc()








##### GTAPv7 ####

# Read the model

model = tabloToR::GEModel$new()

model$loadTablo(here::here("data/gtapv7/gtapv7.tab"))

# Read the databases

data = list(
  gtapsets = HARr::read_har(here::here("data\\gtapv7\\gtapV7_condensed\\gtapV7_condensed\\sets.har")),
  gtapparm = HARr::read_har(here::here("data\\gtapv7\\gtapV7_condensed\\gtapV7_condensed\\default.prm")),
  gtapdata = HARr::read_har(here::here("data\\gtapv7\\gtapV7_condensed\\gtapV7_condensed\\basedata.har"))
)

loadData(data)

model$loadData(data)


tabloPath = here::here("data/gtapv7/gtapv7.tab")

inputData = list(
  gtapsets = HARr::read_har(here::here("data/TablotoR/sets.har")),
  gtapparm = HARr::read_har(here::here("data/TablotoR/default.prm")),
  gtapdata = HARr::read_har(here::here("data/TablotoR/basedata.har"))
)







x <- c("a", "b", "c")
match("a",x)



lapply(model$variableValues, class)

model$solveModel(iter = 3,steps = c(1,3))

knitr::purl(here::here("main.rmd"))



as.numeric("jfjf")




























# Filter out all model variables that match the exogenous variable pattern

exogenousModelVariables=allVariables[(Reduce(function
                                             (a,f)
  c(a,grep(sprintf('^%s\\[',f),allVariables)),
  exogenousVariables,c()
))]

#Only select some of the qo variables that are exogenous (for factors)
for(r in model$data$REG)for( e in model$data$ENDW_COMM)
  exogenousModelVariables=c(exogenousModelVariables,
                            sprintf('qo["%s","%s"]',e,r))

shocks=array(0,dim=length(exogenousModelVariables),
             dimnames=list(exogenousModelVariables))

#Specify shocks
shocks['qo["UnSkLab","SSA"]']= -20

#Specify shocks (a vector of values for all exogenous variables)
model$setShocks(shocks)

#Solve the model in a single iteration(=Johansen)
model$solveModel(iter=1)





model$variableValues





solveModel = function(iter = 3, steps = c(1,3)) {
  
  # Create a shock variable
  
  #browser()
  
  shocks <<- do.call(c,unname(Map(function(f){
    toVector(model$variableValues[[f]],f)
  }, names(model$variableValues))))
  
  shocks<<-shocks[!is.na(shocks)]
  
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
      data <<- originalData
      
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
        
        
        data <<- equationCoefficientGenerator(data)
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













rm(list=ls())



model = tabloToR::GEModel$new()



model$loadTablo('gtap.tab')



data = list(
  
  gtapsets = HARr::read_har('gsdgset.har'),
  
  gtapparm = HARr::read_har('gsdgpar.har'),
  
  gtapdata = HARr::read_har('gsdgdat.har')
  
)





model$loadData(data)



for (var in c(
  "afall",
  "afcom",
  "afeall",
  "afecom",
  "afereg",
  "afesec",
  "afreg",
  "afsec",
  "ams",
  "aoall",
  "aoreg",
  #' qgdp',
  "aosec",
  "atall",
  "atd",
  "atf",
  "atm",
  "ats",
  "au",
  "avaall",
  "avareg",
  "avasec",
  "cgdslack",
  "dpgov",
  "dppriv",
  "dpsave",
  "endwslack",
  "incomeslack",
  "pfactwld",
  "pop",
  "profitslack",
  "psaveslack",
  # "qo(ENDW_COMM,REG)",
  "tf",
  "tfd",
  "tfm",
  "tgd",
  "tgm",
  "tm",
  "tms",
  "to",
  "tpd",
  "tpm",
  "tp",
  "tradslack",
  "tx",
  "txs"
)) {
  model$variableValues[[var]][] <- 0
}


model$variableValues$qo[model$data$endw_comm,] = 0

ag= c("grainscrops")

model$variableValues$aoall[ag,c("eu_25")] = 1



agFood = c("meatlstk", "grainscrops")



model$variableValues$tms[agFood,,c("eu_25")] = (model$data$viws[agFood,,c("eu_25")] / model$data$vims[agFood,,c("eu_25")] -1)*100



tictoc::tic()

model$solveModel(iter = 3,steps = c(1,3))

tictoc::toc()






