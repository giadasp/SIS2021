#read sav file
setwd("C:\\Users\\giada.spaccapanico2\\Desktop\\repos\\SIS2021\\TIMSS\\2015\\data")
library(foreign)
library(mirt)
library(plyr)
#data_2011 <- read.spss("bsaitam5.sav",to.data.frame=TRUE)

data_2015 <- read.spss("bsaitam6.sav",to.data.frame=TRUE)
data_2019 <- read.spss("bsaitam7.sav",to.data.frame=TRUE)

#data_SCI_2011 <- data_2011[names(data_2011)[startsWith(names(data_2011),"S")]]
data_SCI_2015 <- data_2015[names(data_2015)[startsWith(names(data_2015),"S")]]
data_SCI_e_2019 <- data_2019[names(data_2019)[startsWith(names(data_2019),"SE")]]
names(data_SCI_e_2019) <- gsub("SE", "S0", names(data_SCI_e_2019))

data_SCI_e_2019 <- data_SCI_e_2019[names(data_SCI_e_2019)[!endsWith(names(data_SCI_e_2019),"_S")]]
data_SCI_e_2019 <- data_SCI_e_2019[names(data_SCI_e_2019)[!endsWith(names(data_SCI_e_2019),"_F")]]


#data_merged <- merge(data_SCI_2011, data_SCI_2015, all=T)
data_2015_no_factor <- data.frame(lapply(data_SCI_2015, as.character), stringsAsFactors=FALSE)
data_2019_no_factor <- data.frame(lapply(data_SCI_e_2019, as.character), stringsAsFactors=FALSE)
rm(data_2019)
rm(data_2015)

write.table(data_2019_no_factor, "data_2019.csv",sep = ";", row.names=F)
write.table(data_2015_no_factor, "data_2015.csv",sep = ";", row.names=F)

data_no_factor <- rbind.fill(data_2015_no_factor,data_2019_no_factor)

#remove derived 

exclude_2019 <- c("SE52015Z",
                  "SE52095Z",
                  "SE62006",
                  "SE62018",
                  "SE62022",
                  "SE62042",
                  "SE62047",
                  "SE62101",
                  "SE62173A",
                  "SE62242",
                  "SE62243",
                  "SE72000",
                  "SE72016",
                  "SE72033",
                  "SE72086",
                  "SE72130",
                  "SE72143",
                  "SE72232",
                  "SE72260",
                  "SE72261",
                  "SE72265",
                  "SE72345",
                  "SE72400",
                  "SE72403",
                  "SE72905",
                  "SE72906")
exclude_2019 <- gsub("SE","S0",exclude_2019)

responses_e <-
  data_no_factor[,
                 !(names(data_no_factor) %in% c("S062189",
                                                "S062010",
                                                "S052092Z",
                                                "S052043Z",
                                                "S062018",
                                                "S062173A",
                                                "S052015Z",
                                                "S062242",
                                                "S052095Z",
                                                "S062047",
                                                "S062022",
                                                "S032530Z",
                                                "S042173Z",
                                                "S032650Z", exclude_2019))]


responses_e_dich <- responses_e
responses_e_dich[responses_e_dich=="CORRECT RESPONSE"] <- "CORRECT"
responses_e_dich[responses_e_dich  == "CORRECT RESPONSE_duplicated_11"] <- "CORRECT"
responses_e_dich[responses_e_dich  == "CORRECT RESPONSE_duplicated_19"] <- "CORRECT"
responses_e_dich[responses_e_dich  == "CORRECT RESPONSE_duplicated_12"] <- "CORRECT"

responses_e_dich[responses_e_dich=="A*"] <- "CORRECT"
responses_e_dich[responses_e_dich=="B*"] <- "CORRECT"
responses_e_dich[responses_e_dich=="C*"] <- "CORRECT"
responses_e_dich[responses_e_dich=="D*"] <- "CORRECT"
responses_e_dich[responses_e_dich=="E*"] <- "CORRECT"
responses_e_dich[responses_e_dich=="F*"] <- "CORRECT"
responses_e_dich[responses_e_dich=="G*"] <- "CORRECT"
responses_e_dich[responses_e_dich=="A"] <- "INCORRECT"
responses_e_dich[responses_e_dich=="B"] <- "INCORRECT"
responses_e_dich[responses_e_dich=="C"] <- "INCORRECT"
responses_e_dich[responses_e_dich=="D"] <- "INCORRECT"
responses_e_dich[responses_e_dich=="E"] <- "INCORRECT"
responses_e_dich[responses_e_dich=="F"] <- "INCORRECT"
responses_e_dich[responses_e_dich=="G"] <- "INCORRECT"
responses_e_dich[responses_e_dich == "PARTIALLY CORRECT RESPONSE"] <- "NOT DICH"
responses_e_dich[responses_e_dich == "PARTIALLY CORRECT RESPONSE_duplicated_11"] <- "NOT DICH"
responses_e_dich[responses_e_dich == "INCORRECT RESPONSE"] <- "INCORRECT"
responses_e_dich[responses_e_dich == "INCORRECT RESPONSE_duplicated_79"] <- "INCORRECT"
responses_e_dich[responses_e_dich == "INCORRECT RESPONSE_duplicated_72"] <- "INCORRECT"
responses_e_dich[responses_e_dich == "INCORRECT RESPONSE_duplicated_71"] <- "INCORRECT"

is_dich <- function(x){
  if(any(x == "NOT DICH", na.rm = TRUE)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}
responses_e_dich <- responses_e_dich[,sapply(responses_e_dich, is_dich, simplify = "array")]

#filter columns with 0 levels

responses_e_dich_refactor <- data.frame(lapply(responses_e_dich, as.factor), stringsAsFactors=TRUE)
lapply(responses_e_dich_refactor[(names(responses_e_dich_refactor)[lapply(responses_e_dich_refactor, nlevels)!=2])], levels)
responses_e_dich <- responses_e_dich[names(responses_e_dich_refactor)[lapply(responses_e_dich_refactor, nlevels)==2]]
responses_e_dich <- responses_e_dich[sort(names(responses_e_dich))]


#transform to int
responses_e_dich[responses_e_dich=="CORRECT"] <- 1
responses_e_dich[responses_e_dich=="INCORRECT"] <- 0
responses_e_dich_int <- data.frame(lapply(responses_e_dich, as.numeric))
weights <- responses_e_dich$TOTWGT[apply(responses_e_dich_int, 1, function(y) !all(is.na(y)))]
responses_e_dich_int <- responses_e_dich_int[apply(responses_e_dich, 1, function(y) !all(is.na(y))),]
model_2pl<-mirt(responses_e_dich_int, 1, '2PL', method = "EM", technical = list(NCYCLES = 100))
model <- 'F1 = 1-386
      LBOUND = (1-386,a1,0.0)
      LBOUND = (1-386,d,-4.0)
      UBOUND = (1-386,a1,4.0)
      UBOUND = (1-386,d,4.0)
      LBOUND = (1-386,g,0.0)
      UBOUND = (1-386,g,0.5)
      ' 
model_3pl <- mirt(responses_e_dich_int, mirt.model(model), '3PL', method = "EM", optimizer = 'nlminb')#, pars = mod2values(model_2pl) )
a <- coef(model_3pl, simplify=T, printSE=T, IRTpars=TRUE)$items[,1]
b <- coef(model_3pl, simplify=T, printSE=T, IRTpars=TRUE)$items[,2]
c <- coef(model_3pl, simplify=T, printSE=T, IRTpars=TRUE)$items[,3]
summary(coef(model_3pl, simplify=T, printSE=T, IRTpars=TRUE)$items)
pars <- coef(model_3pl, simplify=T, printSE=T, IRTpars=TRUE)$items
sum(b < -3.0 | b > 3 | a > 3 | c == 0.5)
summary(pars_ok)
pars_ok <- pars[!(b < -3.0 | b > 3 | a > 3 | c == 0.5),]

summary(coef(model_2pl, simplify=T, printSE=T)$items)

coef(model_3pl, simplify=F,printSE=T)
write.table(pars_ok,"TIMSS2015_2019_SCI_WORLD_3PL_IRTpars.csv", row.names=TRUE, sep=";")
responses_paper <- data_no_factor[names(data_no_factor)[startsWith(names(data_no_factor),"SP")]]
responses_paper <- data[names(data)[startsWith(names(data),"SP")]]

responses_paper <- data_no_factor[names(responses_paper)[!endsWith(names(responses_paper),"_S")]]
responses_paper <- data_no_factor[names(responses_paper)[!endsWith(names(responses_paper),"_F")]]
responses_paper_dich <- responses_paper

responses_paper_dich[responses_paper_dich=="CORRECT RESPONSE"] <- "CORRECT"
responses_paper_dich[responses_paper_dich=="CORRECT RESPONSE_duplicated_11"] <- "CORRECT"
responses_paper_dich[responses_paper_dich=="A*"] <- "CORRECT"
responses_paper_dich[responses_paper_dich=="B*"] <- "CORRECT"
responses_paper_dich[responses_paper_dich=="C*"] <- "CORRECT"
responses_paper_dich[responses_paper_dich=="D*"] <- "CORRECT"
responses_paper_dich[responses_paper_dich=="E*"] <- "CORRECT"
responses_paper_dich[responses_paper_dich=="F*"] <- "CORRECT"
responses_paper_dich[responses_paper_dich=="G*"] <- "CORRECT"
responses_paper_dich[responses_paper_dich=="A"] <- "INCORRECT"
responses_paper_dich[responses_paper_dich=="B"] <- "INCORRECT"
responses_paper_dich[responses_paper_dich=="C"] <- "INCORRECT"
responses_paper_dich[responses_paper_dich=="D"] <- "INCORRECT"
responses_paper_dich[responses_paper_dich=="E"] <- "INCORRECT"
responses_paper_dich[responses_paper_dich=="F"] <- "INCORRECT"
responses_paper_dich[responses_paper_dich=="G"] <- "INCORRECT"
responses_paper_dich[responses_paper_dich == "PARTIALLY CORRECT RESPONSE"] <- "NOT DICH"
responses_paper_dich[responses_paper_dich == "PARTIALLY CORRECT RESPONSE_duplicated_11"] <- "NOT DICH"
responses_paper_dich[responses_paper_dich == "INCORRECT RESPONSE"] <- "INCORRECT"
responses_paper_dich[responses_paper_dich == "INCORRECT RESPONSE_duplicated_79"] <- "INCORRECT"
responses_paper_dich[responses_paper_dich == "INCORRECT RESPONSE_duplicated_71"] <- "INCORRECT"
is_dich <- function(x){
  if(any(x == "NOT DICH", na.rm = TRUE)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}
responses_paper_dich <- responses_paper_dich[,sapply(responses_paper_dich, is_dich, simplify = "array")]

#filter columns with 0 levels

responses_paper_dich_refactor <- data.frame(lapply(responses_paper_dich, as.factor), stringsAsFactors=TRUE)
responses_paper_dich <- responses_paper_dich[names(responses_paper_dich_refactor)[lapply(responses_paper_dich_refactor, nlevels)==2]]
responses_paper_dich <- responses_paper_dich[sort(names(responses_paper_dich))]

#remove derived 
responses_paper_dich <- responses_paper_dich[,
                                     !(names(responses_paper_dich) %in% c("SP52015Z",
                                                                          "SP52095Z",
                                                                          "SP62018",
                                                                          "SP62022",
                                                                          "SP62047",
                                                                          "SP62173A",
                                                                          "SP62242",
                                                                          "SP72016",
                                                                          "SP72130",
                                                                          "SP72232",
                                                                          "SP72261",
                                                                          "SP72265",
                                                                          "SP72345",
                                                                          "SP72906"))]
#transform to int
responses_paper_dich[responses_paper_dich=="CORRECT"] <- 1
responses_paper_dich[responses_paper_dich=="INCORRECT"] <- 0
responses_paper_dich_int <- data.frame(lapply(responses_paper_dich, as.numeric))
weights <- weights[apply(responses_paper_dich_int, 1, function(y) !all(is.na(y)))]
responses_paper_dich_int <- responses_paper_dich_int[apply(responses_paper_dich, 1, function(y) !all(is.na(y))),]
model_2pl<-mirt(responses_paper_dich_int, 1, '2PL', method = "EM",survey.weights=weights)
model <- 'F1 = 1-284
      LBOUND = (1-284,a,0.0)
      LBOUND = (1-284,d,-3.0)
      UBOUND = (1-284,a,2.0)
      UBOUND = (1-284,d,3.0)
      LBOUND = (1-284,g,0.0)
      UBOUND = (1-284,g,0.5)
      '
model_3pl <-mirt(responses_paper_dich_int, mirt.model(model), '3PL', method = "EM", optimizer = 'nlminb')#, pars=mod2values(model_2pl) )
coef(model_3pl, simplify=T)

