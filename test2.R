GTAP = GEModel$new()
GTAP$loadTablo('d:/temp/gtap.tab')

data = list(
  GTAPSETS = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\sets.har'),
  GTAPPARM = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\default.prm'),
  GTAPDATA = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\basedata.har')
)


GTAP$loadData(data)
allVariables = GTAP$data$variables
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




excludedVariables = allVariables[ (Reduce(function(a,f)
  c(a,grep(sprintf('^%s\\[',f), allVariables)),exogenousVariables,c()
))]

for(r in GTAP$data$REG)for(e in GTAP$data$ENDW_COMM)excludedVariables=c(excludedVariables,sprintf('qo["%s","%s"]',e,r))

# shocks= array(0, dim=length(excludedVariables), dimnames=list(excludedVariables))
# shocks['tms["Food","NAmerica","LatinAmer"]']=-5
#
# GTAP$setShocks(shocks)


# GTAP$solveModel(iter = 3)
#
# solution1 = GTAP$solution
# solution1['pm["Food","LatinAmer"]']
#
# GTAP$solveModel(iter = 3)
# solution2 = GTAP$solution
# solution2['pm["Food","LatinAmer"]']
# solution2['walraslack[]']

shocks= array(0, dim=length(excludedVariables), dimnames=list(excludedVariables))
shocks['pop["LatinAmer"]']=15

require(tictoc)
GTAP$setShocks(shocks)


tic()
GTAP$solveModel(iter = 1)
toc()

solution3 = GTAP$solution
solution3['WEV[]']
solution3['walraslack[]']

solution3['pm["Food","LatinAmer"]']

solution3['qp["Food","LatinAmer"]']
solution3['u["LatinAmer"]']
solution3['EV["LatinAmer"]']
GTAP$changeVariables



