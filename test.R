origData = list(
  GTAPSETS = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\sets.har'),
  GTAPPARM = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\default.prm'),
  GTAPDATA = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\basedata.har')
)

pt = processTablo('d:/temp/gtap.tab')
data = origData
data = pt$skeletonGenerator(data)
data = pt$equationCoefficientMatrixGenerator(data)
data = pt$equationCoefficientGenerator(data)

length(dimnames(data$eqcoeff)[[1]])
length(dimnames(data$eqcoeff)[[2]])
allVariables = dimnames(data$eqcoeff)[[2]]

exogenousVariables = c("afall",
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
                       #"qo(ENDW_COMM,REG)",
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
                       "txs")


excludedVariables = dimnames(data$eqcoeff)[[2]][ (Reduce(function(a,f)
  c(a,grep(sprintf('^%s\\[',f), allVariables)),exogenousVariables,c()
))]

for(r in data$REG)for(e in data$ENDW_COMM)excludedVariables=c(excludedVariables,sprintf('qo["%s","%s"]',e,r))


endogenousVariables =  setdiff(dimnames(data$eqcoeff)[[2]],excludedVariables)

length(endogenousVariables)


bigMatrix = data$eqcoeff[,endogenousVariables]

length(colnames(bigMatrix)[colSums(bigMatrix!=0)==1])

tictoc::tic()
solvedBigMatrix = solve(bigMatrix)
tictoc::toc()



data$equations =c('')
require(tictoc)
data=with(data,{
tic()
for (r in REG) {
  for (j in PROD_COMM) {
    equations = c(equations, sprintf("AVAWORLD[\"%s\",\"%s\"]",
                                     j, r))
  }
}
  toc()
  tic()

  for (r in REG) {
    for (j in PROD_COMM) {
      equations = c(equations, sprintf("VADEMAND[\"%s\",\"%s\"]",
                                       j, r))
    }
  }
  toc()
  tic()
  for (r in REG) {
    for (j in PROD_COMM) {
      for (i in TRAD_COMM) {
        equations = c(equations, sprintf("AFWORLD[\"%s\",\"%s\",\"%s\"]",
                                         i, j, r))
      }
    }
  }
  toc()
  tic()
  for (r in REG) {
    for (j in PROD_COMM) {
      for (i in TRAD_COMM) {
        equations = c(equations, sprintf("INTDEMAND[\"%s\",\"%s\",\"%s\"]",
                                         i, j, r))
      }
    }
  }
  toc()
  tic()
  for (r in REG) {
    for (j in PROD_COMM) {
      for (i in TRAD_COMM) {
        equations = c(equations, sprintf("DMNDDPRICE[\"%s\",\"%s\",\"%s\"]",
                                         i, j, r))
      }
    }
  }
  toc()
})
