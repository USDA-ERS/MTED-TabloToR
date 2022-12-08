# Package `tabloToR`

A package that can interpret GEMPACK-style TABLO models in R and solve them

# To install, you can try the following: 

```R
install.packages('devtools')
devtools::install_git('https://github.com/mivanic/tabloToR.git')
```

# To perform a simulation, you can try the following:

```R
model = tabloToR::GEModel$new()

# You need to have the model, such as gtap.tab 
model$loadTablo('gtap.tab')

# You need to get the data files .har
data = list(
  gtapsets = HARr::read_har('gsdgset.har'),
  gtapparm = HARr::read_har('gsdgpar.har'),
  gtapdata = HARr::read_har('gsdgdat.har')
)

# Initialize the model object

model$loadData(data)

# Set up the closure

for(var in c("afall",
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
      "txs")){
  model$exogenousVariables[[var]][]=0
}

model$exogenousVariables$qo[model$data$endw_comm,] = 0

# Specify sets for shocks 

ag= c("grains", "v_f", "osd", "c_b", "pfb", "ocr", "ctl", "oap", "rmk", "wol")
model$exogenousVariables$aoall[ag,c("northam")] = 1

agFood = c("grains", "v_f", "osd", "c_b", "pfb", "ocr", "ctl", "oap", "rmk", "wol", "food")

model$exogenousVariables$tms[agFood,,c("northam")] = (model$data$viws[agFood,,c("northam")] / model$data$vims[agFood,,c("northam")] -1)*100

# Run the model
model$solveModel(iter = 3,steps = c(1,3))

# View the results (variable ev--welfare)
model$data$ev

```
