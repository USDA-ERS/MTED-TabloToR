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
# for(r in GTAP$data$REG){
#   shocks[sprintf('qo["UnSkLab","%s"]',r)]=-30
#}

shocks['qo["UnSkLab","LatinAmer"]']=-20
shocks['qo["UnSkLab","NAmerica"]']=-20
shocks['qo["UnSkLab","EU_28"]']=-20

# shocks['pop["LatinAmer"]']=-5
# shocks['pop["NAmerica"]']=-5
# shocks['pop["EU_28"]']=-5


require(tictoc)
GTAP$setShocks(shocks)




require(tictoc)
tic()
GTAP$solveModel(iter = 6)
toc()


oneiteration = GTAP$data

GTAP$data$EV

GTAP$data$EVOA[,'NAmerica']

GTAP$data$EV

GEMPACK = HARr::read_har("C:\\Users\\MAROS.IVANIC\\Documents\\covid\\init.sl4")

GPResults = list()
for(v in 1:length(GEMPACK$VARS)){
  GPResults[[GEMPACK$VARS[v]]] = GEMPACK$CUMS[GEMPACK$PCUM[v]:(GEMPACK$PCUM[v]+GEMPACK$ORND[v]-1)]
}

EV_Compare = rbind(
GPResults$EV,GTAP$data$EV
)
rownames(EV_Compare)=c('GEMPACK','R')

xtable(t(EV_Compare))

pm2 = GTAP$data$pm

pm2[]=GPResults$pm

pm2['UnSkLab',]

pm_Compare = rbind(
  pm2['UnSkLab',],GTAP$data$pm['UnSkLab',]
)
rownames(pm_Compare)=c('GEMPACK','R')


xtable(t(pm_Compare),digits=6)

print(xtable(cbind(as.data.frame(GTAP$data$REG[1:5]),as.data.frame(GTAP$data$REG[6:10]))),include.rownames=FALSE)

print(xtable(cbind(as.data.frame(GTAP$data$TRAD_COMM[1:10]),as.data.frame(GTAP$data$TRAD_COMM[11:20]))),include.rownames=FALSE)

print(xtable(as.data.frame(GTAP$data$ENDW_COMM)),include.rownames=FALSE)


EV_Compare

GTAP$solution['EV["Oceania"]']
GTAP$solution['pm["UnSkLab","Oceania"]']


GTAP$data$walras_dem
GTAP$data$walras_sup

savedData = GTAP$data

writeClipboard(as.character(GTAP$data$EV))
writeClipboard(as.character(GTAP$data$pm['UnSkLab',]))
writeClipboard(as.character(names(GTAP$data$EV)))

GEMPACKSolution = HARr::read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\init.sl4')

# GTAP$data$yp
#
# xx=with(GTAP$data,
#      for(s in names(GTAP$solution)){
#       eval(sprintf('%s=%s',s,GTAP$solution[s]))
#      })
#
# GTAP$data$yp
#
#
#
# solution3 = GTAP$solution
# solution3['WEV[]']
# solution3['walraslack[]']
# solution3['EV["LatinAmer"]']
# solution3['EV["EU_28"]']
# solution3['EV["NAmerica"]']
# solution3['EV["SSA"]']
#
# solution3['yp["SSA"]']
# solution3['yp["NAmerica"]']
#
# solution3['pm["Food","LatinAmer"]']
#
# solution3['qp["Food","LatinAmer"]']
# solution3['u["LatinAmer"]']
# solution3['EV["LatinAmer"]']
# GTAP$changeVariables
#
#
#
