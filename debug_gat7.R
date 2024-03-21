model = tabloToR::GEModel$new()

#model$loadTablo(here::here("data/gtapv7/gtapv7.tab"))


source(here::here("R/tabloToStatements.R"))
results <- processTablo(here::here("data/gtapv7/gtapv7.tab"))
statements = tabloToStatements(here::here("data/gtapv7/gtapv7.tab")) # OK
statements = tabloToStatements(here::here("data/gtapv6/gtapv6.tab")) # OK

formulaStatements = Filter(function(f) f$class == 'formula', statements)
formulaStatements = Filter(function(f) f$class == 'formula', cleanLinesParsed)
equationStatements = Filter(function(f) f$class == 'equation', statements)
coefficientStatements = Filter(function(f) f$class == 'coefficient', statements)
variable = Filter(function(f) f$class == 'variable', statements)
sets = Filter(function(f) f$class == 'set', statements)
#formulaText = formulaStatements[16]
s= equationStatements[[16]]
s = formulaText$parsed$equation

s= formulaStatements[[233]]
formulaText = s$parsed$equation

s= formulaStatements[[46]]
s = formulaText$parsed$equation


s= formulaStatements[[8]]
formulaText = s$parsed$equation


s= formulaStatements[[210]]
formulaText = s$parsed$equation

s= formulaStatements[[213]]
formulaText = s$parsed$equation

s= formulaStatements[[212]]
formulaText = s$parsed$equation

s= equationStatements[[199]]
formulaText = s$parsed$equation

s= formulaStatements[[8]]
formulaText = s$parsed$equation

s= formulaStatements[[133]]
formulaText = s$parsed$equation

s= formulaStatements[[1]]
formulaText = s$parsed$equation

s = statements[[17]]
formulaText = s$parsed$equation

s = statements[[1081]]
formulaText = s$parsed$equation

s = statements[[1079]]
formulaText = s$parsed$equation

s = statements[[906]]
formulaText = s$parsed$equation

s = statements[[1079]]
s
formulaText = s$parsed$equation


s = statements[[17]]
s
formulaText = s$parsed$equation



s = statements[[15]]
s
formulaText = s$parsed$equation

s = statements[[163]]
s
formulaText = s$parsed$equation



s = statements[[1110]]
s
formulaText = s$parsed$equation




s = statements[[969]]
s
formulaText = s$parsed$equation

s = statements[[348]]
s
formulaText = s$parsed$equation


s = statements[[969]]
s
formulaText = s$parsed$equation



my_list_subset <- formulaStatements[sapply(formulaStatements, function(x) "=" %in% x)]  # Subset of list
my_list_subset   

grep(tolower('E_CNTalleffr'), statements)
grep(tolower('qid'), statements)

grep(tolower('ETRQ'), statements)


grep(tolower('\\bx\\b'), statements)



cntalleffr(r,t) = (0.01 * evscalfact(r)) * ( if(t=="prodtax") { sum(c,comm, sum(a,acts, ptax(c,a,r) * (qca(c,a,r) - pop(r))))) 

sum(c,comm, mitax(c,r) * (qim(c,r) - pop(r))) + sum(c,comm, ditax(c,r) * (qid(c,r) - pop(r)))} + if(t=="xtax") { sum(c,comm, sum(s,reg, xtaxd(c,r,s) * (qxs(c,r,s) - pop(r))))} + if(t=="mtax")

  cntalleffr(r,t) = (0.01 * evscalfact(r)) * ( IF(t="prodtax") { sum(c,comm, sum(a,acts, ptax(c,a,r) * (qca(c,a,r) - pop(r))))) + IF(t="inctax") { sum(e,endw, sum(a,acts, inctax(e,a,r) * (qes(e,a,r) - pop(r))))} + IF(t="pfacttax") { sum(e,endw, sum(a,acts, etax(e,a,r) * (qfe(e,a,r) - pop(r))))} + IF(t="inputtax") { sum(c,comm, sum(a,acts, mftax(c,a,r) * (qfm(c,a,r) - pop(r)))) + sum(c,comm, sum(a,acts, dftax(c,a,r) * (qfd(c,a,r) - pop(r))))} + IF(t="contax") { sum(c,comm, mptax(c,r) * (qpm(c,r) - pop(r))) + sum(c,comm, dptax(c,r) * (qpd(c,r) - pop(r)))} + IF(t="govtax") { sum(c,comm, mgtax(c,r) * (qgm(c,r) - pop(r))) + sum(c,comm, dgtax(c,r) * (qgd(c,r) - pop(r)))} + IF(t="invtax") { sum(c,comm, mitax(c,r) * (qim(c,r) - pop(r))) + sum(c,comm, ditax(c,r) * (qid(c,r) - pop(r)))} + IF(t="xtax") { sum(c,comm, sum(s,reg, xtaxd(c,r,s) * (qxs(c,r,s) - pop(r))))} + IF(t="mtax") { sum(c,comm, sum(s,reg, mtax(c,s,r) * (qxs(c,s,r) - p
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           op(r))))})
  IF(t="mtax") { sum(c,comm, sum(s,reg, mtax(c,s,r) * (qxs(c,s,r) - pop(r))))

tablo <- here::here("data/gtapv7/gtapv7.tab")
a <- processTablo(tablo)
statements = tabloToStatements(tablo)

statements = Filter(
  function(f)
    f$class %in% c('set', 'coefficient', 'read', 'formula'),
  statements
)
s <- statements[statements$class == "set,"]

s <-  Filter(function(f) f$class == 'set', statements)[[1]]

sum(t,endwt: endowflag(e,t) != 0, 1) = 1


tabloPath = here::here("data/gtapv7/gtapv7.tab")
results = processTablo(tabloPath)
statements = tabloToStatements(here::here("data/gtapv7/gtapv7.tab")) # OK
generator <- generateSkeleton(Filter(
  function(f)
    f$class %in% c('set', 'coefficient', 'read', 'formula'),
  statements
))
set <- Filter(
  function(f)
    f$class %in% c('set', 'coefficient', 'read', 'formula'),
  statements
)

model = tabloToR::GEModel$new()
model$loadTablo(here::here("data/gtapv7/gtapv7.tab"))


inputData = list(
  gtapsets = HARr::read_har(here::here("data\\gtapv7\\gtapV7_condensed\\gtapV7_condensed\\sets.har")),
  gtapparm = HARr::read_har(here::here("data\\gtapv7\\gtapV7_condensed\\gtapV7_condensed\\default.prm")),
  gtapdata = HARr::read_har(here::here("data\\gtapv7\\gtapV7_condensed\\gtapV7_condensed\\basedata.har"))
)

model$loadData(inputData)
gdpex = Reduce(union, list("household,investment,government,exports,intnlmargins,imports"))


list2env(inputData,globalenv())

list("household","investment","government","exports","intnlmargins","imports") |> length()

endw[endowflag[endw, "fixed"] != 0]

endowflag[endw, "fixed"]

endowflag |> class()

endw = data$gtapsets$endw

data$gtapsets






Formula (initial) (all,r,REG)
UTILELASEV(r) = UTILELAS(r);
Update (all,r,REG)
UTILELASEV(r) = uelasev(r);




