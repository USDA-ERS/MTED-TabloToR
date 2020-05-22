require(HARr)
origData = list(
  GTAPSETS = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\sets.har'),
  GTAPPARM = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\default.prm'),
  GTAPDATA = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\basedata.har')
)

# origData = list(
#   GTAPSETS = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\sets.har'),
#   GTAPPARM = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\default.prm'),
#   GTAPDATA = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\GTAP10\\basedata.har')
# )
#

pt = processTablo('d:/temp/gtap.tab')
require(tictoc)
tic()
data = origData
data = pt$skeletonGenerator(data)
data = pt$equationCoefficientMatrixGenerator(data)
data = pt$equationCoefficientGenerator(data)
data = pt$generateVariables(data)
toc()


iNames=unlist(Map(function(i)i$equation, data$equationMatrixList))
iNumbers = data$equationNumbers[iNames]
jNames=unlist(Map(function(i)i$variable, data$equationMatrixList))
jNumbers = data$variableNumbers[jNames]
xValues = unlist(Map(function(i)ifelse(substr(i$variable,1,regexpr('\\[',i$variable)-1) %in% pt$changeVariables,100,1)*i$expression, data$equationMatrixList))

data$eqcoeff = sparseMatrix(i=iNumbers, j= jNumbers, x=xValues, dims=c(length(data$equations), length(data$variables)),dimnames=list(equations=data$equations, variables=data$variables))

# require(SparseM)
# data$eqcoeff2=new("matrix.coo",ra=xValues, ia=as.integer(iNumbers), ja=as.integer(jNumbers), dimension = c(length(data$equations),length(data$variables)))
# data$eqcoeff3=as.matrix.coo(data$eqcoeff2)
#
# data$eqcoeff3[5:6,5:9]
#
# dim(data$eqcoeff2)

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

#
# changeVariables = dimnames(data$eqcoeff)[[2]][ (Reduce(function(a,f)
#   c(a,grep(sprintf('^%s\\[',f), allVariables)),pt$changVariables,c()
# ))]

# variableMultiplier = array(1,dim=length(data$variables), dimnames=list(data$variables))
# variableMultiplier[changeVariables]=100

#variableMultiplierMatrix = do.call(rbind,Map(function(f)variableMultiplier,data$equations))


for(r in data$REG)for(e in data$ENDW_COMM)excludedVariables=c(excludedVariables,sprintf('qo["%s","%s"]',e,r))


endogenousVariables =  setdiff(dimnames(data$eqcoeff)[[2]],excludedVariables)

length(endogenousVariables)
length(data$equations)


bigMatrix = data$eqcoeff[,endogenousVariables]

smallMatrix = data$eqcoeff[,excludedVariables]

exogenous= array(0, dim=length(excludedVariables), dimnames=list(excludedVariables))
#exogenous['tms["Machinery","LatinAmer","NAmerica"]']=0
#exogenous['tms["AgInd","Usa","RestofWorld"]']=10
exogenous['pfactwld[]']=10
#exogenous['tms["Food","NAmerica","LatinAmer"]']=-5



tictoc::tic()
solution = SparseM::solve(bigMatrix,(-smallMatrix %*% exogenous)[rownames(bigMatrix),],sparse=T,tol=1e-20)
tictoc::toc()


xx=-smallMatrix %*% exogenous


dim(bigMatrix)
dim(smallMatrix)
length(exogenous)

dim(bigMatrix)


removeVariables = colnames(bigMatrix)[colSums(bigMatrix!=0)==1]




length(removeVariables)

cn=colnames(bigMatrix)

removeVariableNumbers=which(cn %in% removeVariables)


rn=rownames(bigMatrix)

pb=txtProgressBar(min=0,max=length(bigMatrix2@i),style=3)
removeEquationNumbers = Reduce(function(a,f){
  setTxtProgressBar(pb,f)
  if((bigMatrix2@j[f]+1) %in% removeVariableNumbers){
    return=c(a,bigMatrix2@i[f]+1)
  }
} ,1:length(bigMatrix2@i),c())


dim(bigMatrix)

bigMatrix2 = as(bigMatrix, 'TsparseMatrix')
tt=table(bigMatrix2@j)
removeJ=bigMatrix2@j[which(bigMatrix2@j %in% as.numeric(names(tt)[tt==1]))]
removeI=bigMatrix2@i[which(bigMatrix2@j %in% as.numeric(names(tt)[tt==1]))]

unique(removeJ)
unique(removeI)

keepI=setdiff(1:dim(bigMatrix)[1] ,removeI+1)
keepJ=setdiff(1:dim(bigMatrix)[1] ,removeJ+1)

length(keepI)
length(keepJ)

bigMatrix=bigMatrix[keepI,keepJ]
print(dim(bigMatrix))

length(removeI)
length(removeJ)

yy = bigMatrix2@i[xx]+1

length(unique(yy))
length(removeVariableNumbers)

pb=txtProgressBar(min=0,max=length(bigMatrix2@i),style=3)
toRemove=c()
for(f in 1:length(bigMatrix2@i)){
  setTxtProgressBar(pb,f)
  if(bigMatrix2@j[f] %in% removeVariableNumbers){
    toRemove=c(toRemove,bigMatrix2@j[f])
  }
}


removeEquations = Map(function(f){
  which(as.vector(bigMatrix[,f])!=0)[1]
  },removeVariableNumbers)

n=2369
m=231501-n


matrixA=bigMatrix[1:n,1:n]
matrixB=bigMatrix[1:n,(n+1):(n+m)]
matrixC=bigMatrix[(n+1):(n+m),1:n]
matrixD=bigMatrix[(n+1):(n+m),(n+1):(n+m)]


vectorE=(-smallMatrix %*% exogenous)[1:n,1,drop=F]
vectorF=(-smallMatrix %*% exogenous)[(n+1):(n+m)]

vectorZ = solve(matrixA,vectorE,sparse=T,tol=1e-35)

which(diag(bigMatrix)!=0)

which(matrixA!=0)

min(matrixA)
class()
class(xx)
dim(xx)
class(vectorE)
class(matrixA)
class(bigMatrix)
det(bigMatrix)

#solution['pm["AgInd","Usa"]']
solution['pm["Food","NAmerica"]']
solution['pm["Food","LatinAmer"]']
solution['psave["LatinAmer"]']
solution['qsave["LatinAmer"]']
solution['walraslack[]']

# Load all variable values

for(n in names(solution)){
  data =within(data, {eval(str2lang(sprintf('%s = %f',n,solution[n])))})
}

for(n in names(exogenous)){
  data =  within(data, {eval(str2lang(sprintf('%s = %f',n,exogenous[n])))})
}


data = pt$generateUpdates(data)



data=pt$generateEquationLevelValues(data)
#
# data$equations
#
# data$GOVDMNDS
#
# data$qg['AgInd','Usa']+data$pop['Usa']
#
# data$ug['Usa']+ (data$pg['AgInd','Usa']+data$pgov['Usa'])
#
#
# dd=data.frame(name=character(), value=numeric(),stringsAsFactors = F)
# for(n in sort(names(solution))){
#   dd[nrow(dd)+1,]=c(n,solution[n])
# }
#
# # tictoc::tic()
# # solvedBigMatrix = solve(bigMatrix,sparse=T)
# # tictoc::toc()
#
# tictoc::tic()
# solvedBigMatrix2 = solve(bigMatrix2)
# tictoc::toc()
#
# bigMatrixSM=as.matrix.csr(bigMatrix)
#
# require(SparseM)
#
#
#
# data$equations =c('')
# require(tictoc)
# with(data,{
# tic()
# for (r in REG) {
#   for (j in PROD_COMM) {
#     equations = c(equations, sprintf("AVAWORLD[\"%s\",\"%s\"]",
#                                      j, r))
#   }
# }
#   toc()
#   tic()
#
#   # for (r in REG) {
#   #   for (j in PROD_COMM) {
#   #     equations = c(equations, sprintf("VADEMAND[\"%s\",\"%s\"]",
#   #                                      j, r))
#   #   }
#   # }
#
#       equations = c(equations, do.call(c,Map(function(j)Map(function(r)sprintf("VADEMAND[\"%s\",\"%s\"]",j, r),REG),PROD_COMM)))
#
#
#   toc()
#   tic()
#   # for (r in REG) {
#   #   for (j in PROD_COMM) {
#   #     for (i in TRAD_COMM) {
#   #       equations = c(equations, sprintf("AFWORLD[\"%s\",\"%s\",\"%s\"]",
#   #                                        i, j, r))
#   #     }
#   #   }
#   # }
#
#   equations = c(equations, do.call(c,Map(function(i)Map(function(j)Map(function(r)sprintf("VADEMAND[\"%s\",\"%s\"]",j, r),REG),PROD_COMM),TRAD_COMM)))
#
#
#     toc()
#   tic()
#   # for (r in REG) {
#   #   for (j in PROD_COMM) {
#   #     for (i in TRAD_COMM) {
#   #       equations = c(equations, sprintf("INTDEMAND[\"%s\",\"%s\",\"%s\"]",
#   #                                        i, j, r))
#   #     }
#   #   }
#   # }
#
#   equations = c(equations, do.call(c,Map(function(i)Map(function(j)Map(function(r)sprintf("INTDEMAND[\"%s\",\"%s\"]",j, r),REG),PROD_COMM),TRAD_COMM)))
#
#
#   toc()
#   tic()
#   # for (r in REG) {
#   #   for (j in PROD_COMM) {
#   #     for (i in TRAD_COMM) {
#   #       equations = c(equations, sprintf("DMNDDPRICE[\"%s\",\"%s\",\"%s\"]",
#   #                                        i, j, r))
#   #     }
#   #   }
#   # }
#
#   equations = c(equations, do.call(c,Map(function(i)Map(function(j)Map(function(r)sprintf("DMNDDPRICE[\"%s\",\"%s\"]",j, r),REG),PROD_COMM),TRAD_COMM)))
#
#   toc()
#
#   print(length(equations))
# })
#
# dimnames(data$eqcoeff)[[1]][grep('AFWORLD',dimnames(data$eqcoeff)[[1]])]
#
# dimnames(data$eqcoeff)[[1]][11788]
#
# with(data,
#      {
#        for (r in REG) {
#          for (j in PROD_COMM) {
#            for (i in TRAD_COMM) {
#              print(sprintf("AFWORLD[\"%s\",\"%s\",\"%s\"]",
#                              i, j, r))
#              print(sprintf("af[\"%s\",\"%s\",\"%s\"]",
#                                                i, j, r))
#            }
#          }
#        }
#
#      })
#
#
# with(data,{
#   ppp=c()
#   ppp=c(ppp,unlist(do.call(c, Map(function(r) Map(function(j) Map(function(i) sprintf("AFWORLD[\"%s\",\"%s\",\"%s\"]",
#                                                                      i, j, r), TRAD_COMM), PROD_COMM), REG))))
#   print(ppp)
# })
#
#
# unlist(Map(function(i)unlist(Map(function(f)list(list(i=i,r=r)),data$REG),recursive=F, use.names = F),data$PROD_COMM),recursive=F, use.names = F)
#
require(HARr)
#INIT = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\init.slc',T)
INIT = read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\init_data.har',T)


INIT$SAVE-data$SAVE


diffs=list()
for(h in names(INIT)){
  if(!is.na(h) & is.numeric(INIT[[h]])){
    diffs[[h]]=max(abs(INIT[[h]]-data[[h]]))
  }
}


eqq=read_har('C:\\Users\\MAROS.IVANIC\\Documents\\covid\\standard.eq4')
