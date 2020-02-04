eftest = read.csv(choose.files(), stringsAsFactors = FALSE)

str(eftest)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
#####################  direction
bacidirhalf=list()
bacidirthird=list()
badirhalf=list()
badirthird=list()
cidirhalf=list()
cidirthird=list()
badir2d=list()
cidir2d=list()
for(i in 1:length(unique(eftest$datasetID))){
  bacidirhalf[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Direction" & eftest$sample_size=="50%" & eftest$design=="BACI")],na.rm=TRUE)
  bacidirthird[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Direction" & eftest$sample_size=="33%" & eftest$design=="BACI")],na.rm=TRUE)
  
  badirhalf[[i]] = mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Direction" & eftest$sample_size=="50%" & eftest$design=="BA")],na.rm=TRUE)
  badirthird[[i]]= mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Direction" & eftest$sample_size=="33%" & eftest$design=="BA")],na.rm=TRUE)


  cidirhalf[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Direction" & eftest$sample_size=="50%" & eftest$design=="CI")],na.rm=TRUE)
  cidirthird[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Direction" & eftest$sample_size=="33%" & eftest$design=="CI")],na.rm=TRUE)

  badir2d[[i]] = mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Direction" & eftest$sample_size=="100%" & eftest$design=="BA")],na.rm=TRUE)
  cidir2d[[i]] = mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Direction" & eftest$sample_size=="100%" & eftest$design=="CI")],na.rm=TRUE)
  
}

bacidirhalfd=unlist(bacidirhalf)
bacidirthirdd=unlist(bacidirthird)

rbacidirhalfd=unlist(bacidirhalf)
rbacidirthirdd=unlist(bacidirthird)

badirhalfd=unlist(badirhalf)
badirthirdd=unlist(badirthird)

cidirhalfd=unlist(cidirhalf)
cidirthirdd=unlist(cidirthird)

rctdirhalfd=unlist(cidirhalf)
rctdirthirdd=unlist(cidirthird)

cidirhalfd[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
             which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA
cidirthirdd[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
              which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

rctdirhalfd[setdiff(1:length(rctdirhalfd),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                            which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rctdirthirdd[setdiff(1:length(rctdirthirdd),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                              which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA

rbacidirhalfd[setdiff(1:length(rctdirhalfd),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                              which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rbacidirthirdd[setdiff(1:length(rctdirthirdd),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA

badir2d=unlist(badir2d)
cidir2d=unlist(cidir2d)

rctdir2d=cidir2d

badir2d[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
          which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

cidir2d[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
          which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

rctdir2d[setdiff(1:length(rctdir2d),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                      which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA


summstatsdev5direc = 1-c(mean(bacidirhalfd,na.rm=TRUE),
                         mean(bacidirthirdd,na.rm=TRUE),
                         mean(badir2d,na.rm=TRUE),
                         mean(badirhalfd,na.rm=TRUE),
                         mean(badirthirdd,na.rm=TRUE),
                         mean(cidir2d,na.rm=TRUE),
                         mean(cidirhalfd,na.rm=TRUE),
                         mean(cidirthirdd,na.rm=TRUE),
                         mean(rctdir2d,na.rm=TRUE),
                         mean(rctdirhalfd,na.rm=TRUE),
                         mean(rctdirthirdd,na.rm=TRUE),
                         mean(rbacidirhalfd,na.rm=TRUE),
                         mean(rbacidirthirdd,na.rm=TRUE))

###
summstatsdev5direcb = 1-c(bacidirhalfd,bacidirthirdd,badir2d,badirhalfd,badirthirdd,cidir2d,cidirhalfd,cidirthirdd,
                          rctdir2d,rctdirhalfd,rctdirthirdd,
                          rbacidirhalfd,rbacidirthirdd)

summstatsdev5direcbsamp = c(rep("50%",length(bacidirhalfd)),rep("33%",length(bacidirthirdd)),rep("100%",length(badir2d)),
                            rep("50%",length(badirhalfd)),rep("33%",length(badirthirdd)), rep("100%",length(cidir2d)),
                            rep("50%",length(cidirhalfd)),rep("33%",length(cidirthirdd)),
                            rep("100%",length(rctdir2d)),
                            rep("50%",length(rctdirhalfd)),rep("33%",length(rctdirthirdd)),
                            rep("50%",length(rbacidirhalfd)),rep("33%",length(rbacidirthirdd)))

summstatsdev5direcsd = c(             qnorm(0.975)*(sd(bacidirhalfd,na.rm=TRUE)/sqrt(length(which(is.na(bacidirhalfd)==FALSE)))),
                                      qnorm(0.975)*(sd(bacidirthirdd,na.rm=TRUE)/sqrt(length(which(is.na(bacidirthirdd)==FALSE)))),
                                      qnorm(0.975)*(sd(badir2d,na.rm=TRUE)/sqrt(length(which(is.na(badir2d)==FALSE)))),
                                      qnorm(0.975)*(sd(badirhalfd,na.rm=TRUE)/sqrt(length(which(is.na(badirhalfd)==FALSE)))),
                                      qnorm(0.975)*(sd(badirthirdd,na.rm=TRUE)/sqrt(length(which(is.na(badirthirdd)==FALSE)))),
                                      qnorm(0.975)*(sd(cidir2d,na.rm=TRUE)/sqrt(length(which(is.na(cidir2d)==FALSE)))),
                                      qnorm(0.975)*(sd(cidirhalfd,na.rm=TRUE)/sqrt(length(which(is.na(cidirhalfd)==FALSE)))),
                                      qnorm(0.975)*(sd(cidirthirdd,na.rm=TRUE)/sqrt(length(which(is.na(cidirthirdd)==FALSE)))),
                                      qnorm(0.975)*(sd(rctdir2d,na.rm=TRUE)/sqrt(length(which(is.na(rctdir2d)==FALSE)))),
                                      qnorm(0.975)*(sd(rctdirhalfd,na.rm=TRUE)/sqrt(length(which(is.na(rctdirhalfd)==FALSE)))),
                                      qnorm(0.975)*(sd(rctdirthirdd,na.rm=TRUE)/sqrt(length(which(is.na(rctdirthirdd)==FALSE)))),
                                      qnorm(0.975)*(sd(rbacidirhalfd,na.rm=TRUE)/sqrt(length(which(is.na(rbacidirhalfd)==FALSE)))),
                                      qnorm(0.975)*(sd(rbacidirthirdd,na.rm=TRUE)/sqrt(length(which(is.na(rbacidirthirdd)==FALSE)))))


#####################  deviation

bacidirhalf=list()
bacidirthird=list()
badirhalf=list()
badirthird=list()
cidirhalf=list()
cidirthird=list()
badir2=list()
cidir2=list()
for(i in 1:length(unique(eftest$datasetID))){
  bacidirhalf[[i]]  =  abs((mean(abs(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Deviation" & eftest$sample_size=="50%" & eftest$design=="BACI" & is.infinite(eftest$result)==FALSE)]),na.rm=TRUE)))
  bacidirthird[[i]] =  abs((mean(abs(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Deviation" & eftest$sample_size=="33%" & eftest$design=="BACI" & is.infinite(eftest$result)==FALSE)]),na.rm=TRUE)))
  
  badirhalf[[i]]  =  abs((mean(abs(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Deviation" & eftest$sample_size=="50%" & eftest$design=="BA" & is.infinite(eftest$result)==FALSE)]),na.rm=TRUE)))
  badirthird[[i]] =  abs((mean(abs(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Deviation" & eftest$sample_size=="33%" & eftest$design=="BA" & is.infinite(eftest$result)==FALSE)]),na.rm=TRUE)))
  
  cidirhalf[[i]]  =  abs((mean(abs(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Deviation" & eftest$sample_size=="50%" & eftest$design=="CI" & is.infinite(eftest$result)==FALSE)]),na.rm=TRUE)))
  cidirthird[[i]] =  abs((mean(abs(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Deviation" & eftest$sample_size=="33%" & eftest$design=="CI" & is.infinite(eftest$result)==FALSE)]),na.rm=TRUE)))
  
  badir2[[i]] = abs((mean(abs(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Deviation" & eftest$sample_size=="100%" & eftest$design=="BA" & is.infinite(eftest$result)==FALSE)]),na.rm=TRUE)))
  cidir2[[i]] = abs((mean(abs(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Deviation" & eftest$sample_size=="100%" & eftest$design=="CI" & is.infinite(eftest$result)==FALSE)]),na.rm=TRUE)))
  
}

bacidirhalf=unlist(bacidirhalf)[which(is.infinite(unlist(bacidirhalf))==FALSE)]
bacidirthird=unlist(bacidirthird)[which(is.infinite(unlist(bacidirthird))==FALSE)]
rbacidirhalf=unlist(bacidirhalf)[which(is.infinite(unlist(bacidirhalf))==FALSE)]
rbacidirthird=unlist(bacidirthird)[which(is.infinite(unlist(bacidirthird))==FALSE)]

badirhalf=unlist(badirhalf)[which(is.infinite(unlist(badirhalf))==FALSE)]
badirthird=unlist(badirthird)[which(is.infinite(unlist(badirthird))==FALSE)]

cidirhalf=unlist(cidirhalf)
cidirthird=unlist(cidirthird)

rctdirhalf=unlist(cidirhalf)
rctdirthird=unlist(cidirthird)


badirhalf[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
            which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA
badirthird[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
             which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

cidirhalf[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
            which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA
cidirthird[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
             which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

rctdirhalf[setdiff(1:length(rctdirhalf),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                          which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rctdirthird[setdiff(1:length(rctdirthird),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                            which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA

rbacidirhalf[setdiff(1:length(rctdirhalf),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                            which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rbacidirthird[setdiff(1:length(rctdirthird),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                              which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA

cidirhalf=cidirhalf[which(is.infinite(cidirhalf)==FALSE)]
cidirthird=cidirthird[which(is.infinite(cidirthird)==FALSE)]


badir2=unlist(badir2)
cidir2=unlist(cidir2)

rctdir2=cidir2


badir2[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
         which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

cidir2[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
         which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

rctdir2[setdiff(1:length(rctdir2),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                    which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA


summstatsdev1 = c( mean(bacidirhalf,na.rm=TRUE),
                   mean(bacidirthird,na.rm=TRUE),
                   mean(badir2,na.rm=TRUE),
                   mean(badirhalf,na.rm=TRUE),
                   mean(badirthird,na.rm=TRUE),
                   mean(cidir2,na.rm=TRUE),
                   mean(cidirhalf,na.rm=TRUE),
                   mean(cidirthird,na.rm=TRUE),
                   mean(rctdir2,na.rm=TRUE),
                   mean(rctdirhalf,na.rm=TRUE),
                   mean(rctdirthird,na.rm=TRUE),
                   mean(rbacidirhalf,na.rm=TRUE),
                   mean(rbacidirthird,na.rm=TRUE))

summstatsdev1b = c(bacidirhalf,bacidirthird,badir2,badirhalf,badirthird,cidir2,cidirhalf,cidirthird,rctdir2,rctdirhalf,rctdirthird,rbacidirhalf,rbacidirthird)
summstatsdev1bsamp = c(rep("50%",length(bacidirhalf)),rep("33%",length(bacidirthird)),rep("100%",length(badir2)),
                       rep("50%",length(badirhalf)),rep("33%",length(badirthird)), rep("100%",length(cidir2)),
                       rep("50%",length(cidirhalf)),rep("33%",length(cidirthird)), rep("100%",length(rctdir2)),
                       rep("50%",length(rctdirhalf)),rep("33%",length(rctdirthird)),
                       rep("50%",length(rbacidirhalf)),rep("33%",length(rbacidirthird)))


summstatsdev1sd = c(qnorm(0.975)*(sd(bacidirhalf,na.rm=TRUE)/sqrt(length(which(is.na(bacidirhalf)==FALSE)))),
                    qnorm(0.975)*(sd(bacidirthird,na.rm=TRUE)/sqrt(length(which(is.na(bacidirthird)==FALSE)))),
                    qnorm(0.975)*(sd(badir2,na.rm=TRUE)/sqrt(length(which(is.na(badir2)==FALSE)))),
                    qnorm(0.975)*(sd(badirhalf,na.rm=TRUE)/sqrt(length(which(is.na(badirhalf)==FALSE)))),
                    qnorm(0.975)*(sd(badirthird,na.rm=TRUE)/sqrt(length(which(is.na(badirthird)==FALSE)))),
                    qnorm(0.975)*(sd(cidir2,na.rm=TRUE)/sqrt(length(which(is.na(cidir2)==FALSE)))),
                    qnorm(0.975)*(sd(cidirhalf,na.rm=TRUE)/sqrt(length(which(is.na(cidirhalf)==FALSE)))),
                    qnorm(0.975)*(sd(cidirthird,na.rm=TRUE)/sqrt(length(which(is.na(cidirthird)==FALSE)))),
                    qnorm(0.975)*(sd(rctdir2,na.rm=TRUE)/sqrt(length(which(is.na(rctdir2)==FALSE)))),
                    qnorm(0.975)*(sd(rctdirhalf,na.rm=TRUE)/sqrt(length(which(is.na(rctdirhalf)==FALSE)))),
                    qnorm(0.975)*(sd(rctdirthird,na.rm=TRUE)/sqrt(length(which(is.na(rctdirthird)==FALSE)))),
                    qnorm(0.975)*(sd(rbacidirhalf,na.rm=TRUE)/sqrt(length(which(is.na(rbacidirhalf)==FALSE)))),
                    qnorm(0.975)*(sd(rbacidirthird,na.rm=TRUE)/sqrt(length(which(is.na(rbacidirthird)==FALSE)))))


######## Non-coverage, overestimation and underestimation probabilities

bacidirhalfund=list()
bacidirthirdund=list()
badirhalfund=list()
badirthirdund=list()
cidirhalfund=list()
cidirthirdund=list()

bacidirhalfover=list()
bacidirthirdover=list()
badirhalfover=list()
badirthirdover=list()
cidirhalfover=list()
cidirthirdover=list()

bacidirhalfcorr=list()
bacidirthirdcorr=list()
badirhalfcorr=list()
badirthirdcorr=list()
cidirhalfcorr=list()
cidirthirdcorr=list()

badir2und=list()
cidir2und=list()
badir2over=list()
cidir2over=list()
badir2corr=list()
cidir2corr=list()

for(i in 1:length(unique(eftest$datasetID))){
  bacidirhalfund[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Underestimation" & eftest$sample_size=="50%" & eftest$design=="BACI")],na.rm=TRUE)
  bacidirthirdund[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Underestimation" & eftest$sample_size=="33%" & eftest$design=="BACI")],na.rm=TRUE)

  badirhalfund[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Underestimation" & eftest$sample_size=="50%" & eftest$design=="BA")],na.rm=TRUE)
  badirthirdund[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Underestimation" & eftest$sample_size=="33%" & eftest$design=="BA")],na.rm=TRUE)
  
  cidirhalfund[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Underestimation" & eftest$sample_size=="50%" & eftest$design=="CI")],na.rm=TRUE)
  cidirthirdund[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Underestimation" & eftest$sample_size=="33%" & eftest$design=="CI")],na.rm=TRUE)
  
  
  bacidirhalfover[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Overestimation" & eftest$sample_size=="50%" & eftest$design=="BACI")],na.rm=TRUE)
  bacidirthirdover[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Overestimation" & eftest$sample_size=="33%" & eftest$design=="BACI")],na.rm=TRUE)
  
  badirhalfover[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Overestimation" & eftest$sample_size=="50%" & eftest$design=="BA")],na.rm=TRUE)
  badirthirdover[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Overestimation" & eftest$sample_size=="33%" & eftest$design=="BA")],na.rm=TRUE)
  
  cidirhalfover[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Overestimation" & eftest$sample_size=="50%" & eftest$design=="CI")],na.rm=TRUE)
  cidirthirdover[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Overestimation" & eftest$sample_size=="33%" & eftest$design=="CI")],na.rm=TRUE)
  
  
  bacidirhalfcorr[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Non-coverage" & eftest$sample_size=="50%" & eftest$design=="BACI")],na.rm=TRUE)
  bacidirthirdcorr[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Non-coverage" & eftest$sample_size=="33%" & eftest$design=="BACI")],na.rm=TRUE)
  
  badirhalfcorr[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Non-coverage" & eftest$sample_size=="50%" & eftest$design=="BA")],na.rm=TRUE)
  badirthirdcorr[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Non-coverage" & eftest$sample_size=="33%" & eftest$design=="BA")],na.rm=TRUE)
  
  cidirhalfcorr[[i]]  =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Non-coverage" & eftest$sample_size=="50%" & eftest$design=="CI")],na.rm=TRUE)
  cidirthirdcorr[[i]] =  mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Non-coverage" & eftest$sample_size=="33%" & eftest$design=="CI")],na.rm=TRUE)
  
  
  badir2und[[i]] = mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Underestimation" & eftest$sample_size=="100%" & eftest$design=="BA")],na.rm=TRUE)
  cidir2und[[i]] = mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Underestimation" & eftest$sample_size=="100%" & eftest$design=="CI")],na.rm=TRUE)
  
  badir2over[[i]] = mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Overestimation" & eftest$sample_size=="100%" & eftest$design=="BA")],na.rm=TRUE)
  cidir2over[[i]] = mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Overestimation" & eftest$sample_size=="100%" & eftest$design=="CI")],na.rm=TRUE)
  
  badir2corr[[i]] = mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Non-coverage" & eftest$sample_size=="100%" & eftest$design=="BA")],na.rm=TRUE)
  cidir2corr[[i]] = mean(eftest$result[which(eftest$datasetID==unique(eftest$datasetID)[i] & eftest$measure=="Non-coverage" & eftest$sample_size=="100%" & eftest$design=="CI")],na.rm=TRUE)
  
  
}

bacidirhalfund=unlist(bacidirhalfund)
bacidirthirdund=unlist(bacidirthirdund)

rbacidirhalfund=unlist(bacidirhalfund)
rbacidirthirdund=unlist(bacidirthirdund)

badirhalfund=unlist(badirhalfund)
badirthirdund=unlist(badirthirdund)

cidirhalfund=unlist(cidirhalfund)
cidirthirdund=unlist(cidirthirdund)

bacidirhalfover=unlist(bacidirhalfover)
bacidirthirdover=unlist(bacidirthirdover)

rbacidirhalfover=unlist(bacidirhalfover)
rbacidirthirdover=unlist(bacidirthirdover)

badirhalfover=unlist(badirhalfover)
badirthirdover=unlist(badirthirdover)

cidirhalfover=unlist(cidirhalfover)
cidirthirdover=unlist(cidirthirdover)

bacidirhalfcorr=unlist(bacidirhalfcorr)
bacidirthirdcorr=unlist(bacidirthirdcorr)

rbacidirhalfcorr=unlist(bacidirhalfcorr)
rbacidirthirdcorr=unlist(bacidirthirdcorr)

badirhalfcorr=unlist(badirhalfcorr)
badirthirdcorr=unlist(badirthirdcorr)

cidirhalfcorr=unlist(cidirhalfcorr)
cidirthirdcorr=unlist(cidirthirdcorr)

rctdirhalfund=unlist(cidirhalfund)
rctdirthirdund=unlist(cidirthirdund)

badirhalfund[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
               which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA
badirthirdund[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

cidirhalfund[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
               which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA
cidirthirdund[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

rctdirhalfund[setdiff(1:length(rctdirhalfund),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rctdirthirdund[setdiff(1:length(rctdirthirdund),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                  which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rbacidirhalfund[setdiff(1:length(rctdirhalfund),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                  which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rbacidirthirdund[setdiff(1:length(rctdirthirdund),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                    which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA

cidirhalfund=cidirhalfund[which(is.infinite(cidirhalfund)==FALSE)]
cidirthirdund=cidirthirdund[which(is.infinite(cidirthirdund)==FALSE)]
badirhalfund=badirhalfund[which(is.infinite(badirhalfund)==FALSE)]
badirthirdund=badirthirdund[which(is.infinite(badirthirdund)==FALSE)]

rctdirhalfover=unlist(cidirhalfover)
rctdirthirdover=unlist(cidirthirdover)

badirhalfover[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA
badirthirdover[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                 which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

cidirhalfover[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA
cidirthirdover[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                 which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

rctdirhalfover[setdiff(1:length(rctdirhalfover),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                  which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rctdirthirdover[setdiff(1:length(rctdirthirdover),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                    which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rbacidirhalfover[setdiff(1:length(rctdirhalfover),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                    which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rbacidirthirdover[setdiff(1:length(rctdirthirdover),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                      which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA

cidirhalfover=cidirhalfover[which(is.infinite(cidirhalfover)==FALSE)]
cidirthirdover=cidirthirdover[which(is.infinite(cidirthirdover)==FALSE)]
badirhalfover=badirhalfover[which(is.infinite(badirhalfover)==FALSE)]
badirthirdover=badirthirdover[which(is.infinite(badirthirdover)==FALSE)]


rctdirhalfcorr=unlist(cidirhalfcorr)
rctdirthirdcorr=unlist(cidirthirdcorr)

badirhalfcorr[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA
badirthirdcorr[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                 which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

cidirhalfcorr[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA
cidirthirdcorr[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                 which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

rctdirhalfcorr[setdiff(1:length(rctdirhalfcorr),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                  which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rctdirthirdcorr[setdiff(1:length(rctdirthirdcorr),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                    which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rbacidirhalfcorr[setdiff(1:length(rctdirhalfcorr),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                    which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rbacidirthirdcorr[setdiff(1:length(rctdirthirdcorr),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                                      which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA

cidirhalfcorr=cidirhalfcorr[which(is.infinite(cidirhalfcorr)==FALSE)]
cidirthirdcorr=cidirthirdcorr[which(is.infinite(cidirthirdcorr)==FALSE)]
badirhalfcorr=badirhalfcorr[which(is.infinite(badirhalfcorr)==FALSE)]
badirthirdcorr=badirthirdcorr[which(is.infinite(badirthirdcorr)==FALSE)]


badir2und=unlist(badir2und)
cidir2und=unlist(cidir2und)
badir2over=unlist(badir2over)
cidir2over=unlist(cidir2over)
badir2corr=unlist(badir2corr)
cidir2corr=unlist(cidir2corr)

rctdir2und=cidir2und

cidir2und[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
            which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

rctdir2und[setdiff(1:length(rctdir2und),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                          which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA


rctdir2over=cidir2over

cidir2over[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
             which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

rctdir2over[setdiff(1:length(rctdir2over),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                            which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA
rctdir2corr=cidir2corr

cidir2corr[c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
             which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53))] = NA

rctdir2corr[setdiff(1:length(rctdir2corr),c(which(unique(eftest$datasetID)==4),which(unique(eftest$datasetID)==36),which(unique(eftest$datasetID)==37),
                                            which(unique(eftest$datasetID)==51),which(unique(eftest$datasetID)==53)))] = NA



summstatsdev2und = c(              mean(bacidirhalfund,na.rm=TRUE),
                                   mean(bacidirthirdund,na.rm=TRUE),
                                   mean(badir2und,na.rm=TRUE),
                                   mean(badirhalfund,na.rm=TRUE),
                                   mean(badirthirdund,na.rm=TRUE),
                                   mean(cidir2und,na.rm=TRUE),
                                   mean(cidirhalfund,na.rm=TRUE),
                                   mean(cidirthirdund,na.rm=TRUE),
                                   mean(rctdir2und,na.rm=TRUE),
                                   mean(rctdirhalfund,na.rm=TRUE),
                                   mean(rctdirthirdund,na.rm=TRUE),
                                   mean(rbacidirhalfund,na.rm=TRUE),
                                   mean(rbacidirthirdund,na.rm=TRUE))

summstatsdev2undb = c(bacidirhalfund,bacidirthirdund,badir2und,badirhalfund,badirthirdund,cidir2und,cidirhalfund,cidirthirdund,rctdir2und,rctdirhalfund,rctdirthirdund,rbacidirhalfund,rbacidirthirdund)
summstatsdev2undbsamp = c(rep("50%",length(bacidirhalfund)),rep("33%",length(bacidirthirdund)),rep("100%",length(badir2und)),
                          rep("50%",length(badirhalfund)),rep("33%",length(badirthirdund)), rep("100%",length(cidir2und)),
                          rep("50%",length(cidirhalfund)),rep("33%",length(cidirthirdund)), rep("100%",length(rctdir2und)),
                          rep("50%",length(rctdirhalfund)),rep("33%",length(rctdirthirdund)),
                          rep("50%",length(rbacidirhalfund)),rep("33%",length(rbacidirthirdund)))



summstatsdev2undsd = c(             qnorm(0.975)*(sd(bacidirhalfund,na.rm=TRUE)/sqrt(length(which(is.na(bacidirhalfund)==FALSE)))),
                                    qnorm(0.975)*(sd(bacidirthirdund,na.rm=TRUE)/sqrt(length(which(is.na(bacidirthirdund)==FALSE)))),
                                    qnorm(0.975)*(sd(badir2und,na.rm=TRUE)/sqrt(length(which(is.na(badir2und)==FALSE)))),
                                    qnorm(0.975)*(sd(badirhalfund,na.rm=TRUE)/sqrt(length(which(is.na(badirhalfund)==FALSE)))),
                                    qnorm(0.975)*(sd(badirthirdund,na.rm=TRUE)/sqrt(length(which(is.na(badirthirdund)==FALSE)))),
                                    qnorm(0.975)*(sd(cidir2und,na.rm=TRUE)/sqrt(length(which(is.na(cidir2und)==FALSE)))),
                                    qnorm(0.975)*(sd(cidirhalfund,na.rm=TRUE)/sqrt(length(which(is.na(cidirhalfund)==FALSE)))),
                                    qnorm(0.975)*(sd(cidirthirdund,na.rm=TRUE)/sqrt(length(which(is.na(cidirthirdund)==FALSE)))),
                                    qnorm(0.975)*(sd(rctdir2und,na.rm=TRUE)/sqrt(length(which(is.na(rctdir2und)==FALSE)))),
                                    qnorm(0.975)*(sd(rctdirhalfund,na.rm=TRUE)/sqrt(length(which(is.na(rctdirhalfund)==FALSE)))),
                                    qnorm(0.975)*(sd(rctdirthirdund,na.rm=TRUE)/sqrt(length(which(is.na(rctdirthirdund)==FALSE)))),
                                    qnorm(0.975)*(sd(rbacidirhalfund,na.rm=TRUE)/sqrt(length(which(is.na(rbacidirhalfund)==FALSE)))),
                                    qnorm(0.975)*(sd(rbacidirthirdund,na.rm=TRUE)/sqrt(length(which(is.na(rbacidirthirdund)==FALSE)))))

summstatsdev3over = c(mean(bacidirhalfover,na.rm=TRUE),
                      mean(bacidirthirdover,na.rm=TRUE),
                      mean(badir2over,na.rm=TRUE),
                      mean(badirhalfover,na.rm=TRUE),
                      mean(badirthirdover,na.rm=TRUE),
                      mean(cidir2over,na.rm=TRUE),
                      mean(cidirhalfover,na.rm=TRUE),
                      mean(cidirthirdover,na.rm=TRUE),
                      mean(rctdir2over,na.rm=TRUE),
                      mean(rctdirhalfover,na.rm=TRUE),
                      mean(rctdirthirdover,na.rm=TRUE),
                      mean(rbacidirhalfover,na.rm=TRUE),
                      mean(rbacidirthirdover,na.rm=TRUE))


summstatsdev3overb =c(bacidirhalfover,bacidirthirdover,badir2over,badirhalfover,badirthirdover,cidir2over,cidirhalfover,cidirthirdover,rctdir2over,rctdirhalfover,rctdirthirdover,rbacidirhalfover,rbacidirthirdover)
summstatsdev3overbsamp = c(rep("50%",length(bacidirhalfover)),rep("33%",length(bacidirthirdover)),rep("100%",length(badir2over)),
                           rep("50%",length(badirhalfover)),rep("33%",length(badirthirdover)), rep("100%",length(cidir2over)),
                           rep("50%",length(cidirhalfover)),rep("33%",length(cidirthirdover)), rep("100%",length(rctdir2over)),
                           rep("50%",length(rctdirhalfover)),rep("33%",length(rctdirthirdover)),
                           rep("50%",length(rbacidirhalfover)),rep("33%",length(rbacidirthirdover)))

summstatsdev3oversd = c(             qnorm(0.975)*(sd(bacidirhalfover,na.rm=TRUE)/sqrt(length(which(is.na(bacidirhalfover)==FALSE)))),
                                     qnorm(0.975)*(sd(bacidirthirdover,na.rm=TRUE)/sqrt(length(which(is.na(bacidirthirdover)==FALSE)))),
                                     qnorm(0.975)*(sd(badir2over,na.rm=TRUE)/sqrt(length(which(is.na(badir2over)==FALSE)))),
                                     qnorm(0.975)*(sd(badirhalfover,na.rm=TRUE)/sqrt(length(which(is.na(badirhalfover)==FALSE)))),
                                     qnorm(0.975)*(sd(badirthirdover,na.rm=TRUE)/sqrt(length(which(is.na(badirthirdover)==FALSE)))),
                                     qnorm(0.975)*(sd(cidir2over,na.rm=TRUE)/sqrt(length(which(is.na(cidir2over)==FALSE)))),
                                     qnorm(0.975)*(sd(cidirhalfover,na.rm=TRUE)/sqrt(length(which(is.na(cidirhalfover)==FALSE)))),
                                     qnorm(0.975)*(sd(cidirthirdover,na.rm=TRUE)/sqrt(length(which(is.na(cidirthirdover)==FALSE)))),
                                     qnorm(0.975)*(sd(rctdir2over,na.rm=TRUE)/sqrt(length(which(is.na(rctdir2over)==FALSE)))),
                                     qnorm(0.975)*(sd(rctdirhalfover,na.rm=TRUE)/sqrt(length(which(is.na(rctdirhalfover)==FALSE)))),
                                     qnorm(0.975)*(sd(rctdirthirdover,na.rm=TRUE)/sqrt(length(which(is.na(rctdirthirdover)==FALSE)))),
                                     qnorm(0.975)*(sd(rbacidirhalfover,na.rm=TRUE)/sqrt(length(which(is.na(rbacidirhalfover)==FALSE)))),
                                     qnorm(0.975)*(sd(rbacidirthirdover,na.rm=TRUE)/sqrt(length(which(is.na(rbacidirthirdover)==FALSE)))))

summstatsdev4corr = c(mean(bacidirhalfcorr,na.rm=TRUE),
                      mean(bacidirthirdcorr,na.rm=TRUE),
                      mean(badir2corr,na.rm=TRUE),
                      mean(badirhalfcorr,na.rm=TRUE),
                      mean(badirthirdcorr,na.rm=TRUE),
                      mean(cidir2corr,na.rm=TRUE),
                      mean(cidirhalfcorr,na.rm=TRUE),
                      mean(cidirthirdcorr,na.rm=TRUE),
                      mean(rctdir2corr,na.rm=TRUE),
                      mean(rctdirhalfcorr,na.rm=TRUE),
                      mean(rctdirthirdcorr,na.rm=TRUE),
                      mean(rbacidirhalfcorr,na.rm=TRUE),
                      mean(rbacidirthirdcorr,na.rm=TRUE))

summstatsdev4corrb = c(bacidirhalfcorr,bacidirthirdcorr,badir2corr,badirhalfcorr,badirthirdcorr,cidir2corr,cidirhalfcorr,cidirthirdcorr,rctdir2corr,rctdirhalfcorr,rctdirthirdcorr,rbacidirhalfcorr,rbacidirthirdcorr)
summstatsdev4corrbsamp = c(rep("50%",length(bacidirhalfcorr)),rep("33%",length(bacidirthirdcorr)),rep("100%",length(badir2corr)),
                           rep("50%",length(badirhalfcorr)),rep("33%",length(badirthirdcorr)), rep("100%",length(cidir2corr)),
                           rep("50%",length(cidirhalfcorr)),rep("33%",length(cidirthirdcorr)), rep("100%",length(rctdir2corr)),
                           rep("50%",length(rctdirhalfcorr)),rep("33%",length(rctdirthirdcorr)),
                           rep("50%",length(rbacidirhalfcorr)),rep("33%",length(rbacidirthirdcorr)))

summstatsdev4corrsd = c(             qnorm(0.975)*(sd(bacidirhalfcorr,na.rm=TRUE)/sqrt(length(which(is.na(bacidirhalfcorr)==FALSE)))),
                                     qnorm(0.975)*(sd(bacidirthirdcorr,na.rm=TRUE)/sqrt(length(which(is.na(bacidirthirdcorr)==FALSE)))),
                                     qnorm(0.975)*(sd(badir2corr,na.rm=TRUE)/sqrt(length(which(is.na(badir2corr)==FALSE)))),
                                     qnorm(0.975)*(sd(badirhalfcorr,na.rm=TRUE)/sqrt(length(which(is.na(badirhalfcorr)==FALSE)))),
                                     qnorm(0.975)*(sd(badirthirdcorr,na.rm=TRUE)/sqrt(length(which(is.na(badirthirdcorr)==FALSE)))),
                                     qnorm(0.975)*(sd(cidir2corr,na.rm=TRUE)/sqrt(length(which(is.na(cidir2corr)==FALSE)))),
                                     qnorm(0.975)*(sd(cidirhalfcorr,na.rm=TRUE)/sqrt(length(which(is.na(cidirhalfcorr)==FALSE)))),
                                     qnorm(0.975)*(sd(cidirthirdcorr,na.rm=TRUE)/sqrt(length(which(is.na(cidirthirdcorr)==FALSE)))),
                                     qnorm(0.975)*(sd(rctdir2corr,na.rm=TRUE)/sqrt(length(which(is.na(rctdir2corr)==FALSE)))),
                                     qnorm(0.975)*(sd(rctdirhalfcorr,na.rm=TRUE)/sqrt(length(which(is.na(rctdirhalfcorr)==FALSE)))),
                                     qnorm(0.975)*(sd(rctdirthirdcorr,na.rm=TRUE)/sqrt(length(which(is.na(rctdirthirdcorr)==FALSE)))),
                                     qnorm(0.975)*(sd(rbacidirhalfcorr,na.rm=TRUE)/sqrt(length(which(is.na(rbacidirhalfcorr)==FALSE)))),
                                     qnorm(0.975)*(sd(rbacidirthirdcorr,na.rm=TRUE)/sqrt(length(which(is.na(rbacidirthirdcorr)==FALSE)))))


######
cover = data.frame(val = c(1-summstatsdev4corr, summstatsdev5direc,
                           abs(1-exp(summstatsdev1))),
                   type = factor(c(rep("Original BACI ES outside of 95% CIs",13),
                                   rep("Different direction to original BACI ES",13),
                                   rep("Difference to original BACI ES",13)),levels=c(
                                     "Difference to original BACI ES","Different direction to original BACI ES","Original BACI ES outside of 95% CIs"
                                   )),
                   
                   size = factor(rep(c("50%","33%","100%","50%","33%","100%","50%","33%","100%","50%","33%","50%","33%"),3),levels=c("100%","50%","33%")),
                   size1=as.numeric(rep(c(10,8,12,10,8,12,10,8,12,10,8,10,8),3)),
                   
                   
                   
                   des = rep(c(5,5,3,3,3,1,1,1,8,8,8,10,10),3),
                   adj = rep(c(0,-0.55,0.55,0,-0.55,0.55,0,-0.55,0.55,0,-0.55,0,-0.55),3))



coverb = data.frame(val = c(1-summstatsdev4corrb, summstatsdev5direcb,
                            abs(1-exp(summstatsdev1b))),
                    type = factor(c(rep("Original BACI ES outside of 95% CIs",length(summstatsdev4corrb)),
                                    rep("Different direction to original BACI ES",length(summstatsdev5direcb)),
                                    rep("Difference to original BACI ES",length(summstatsdev1b))),levels=c(
                                      "Difference to original BACI ES","Different direction to original BACI ES","Original BACI ES outside of 95% CIs"
                                    )),
                    
                    size = factor(c(summstatsdev4corrbsamp,summstatsdev5direcbsamp,summstatsdev1bsamp),levels=c("100%","50%","33%")),
                    size1 = rep(c(rep(10,length(bacidirhalfcorr)),rep(8,length(bacidirthirdcorr)),rep(12,length(badir2corr)),rep(10,length(badirhalfcorr)),
                                rep(8,length(badirthirdcorr)),rep(12,length(cidir2corr)),rep(10,length(cidirhalfcorr)),rep(8,length(cidirthirdcorr)),
                                rep(12,length(rctdir2corr)),rep(10,length(rctdirhalfcorr)),rep(8,length(rctdirthirdcorr)),rep(10,length(rbacidirhalfcorr)),rep(8,length(rbacidirthirdcorr))),3),
                    
                    des = rep(c(rep(5,length(c(bacidirhalf,bacidirthird))),rep(3,length(c(badir2,badirhalf,badirthird))),rep(1,length(c(cidir2,cidirhalf,cidirthird))),rep(8,length(c(rctdir2,rctdirhalf,rctdirthird))),rep(10,length(c(rbacidirhalf,rbacidirthird)))),3),
                    adj = rep(c(rep(0,length(bacidirhalf)),rep(-0.55,length(bacidirthird)),rep(0.55,length(badir2)),rep(0,length(badirhalf)),
                              rep(-0.55,length(badirthird)),rep(0.55,length(cidir2)),rep(0,length(cidirhalf)),rep(-0.55,length(cidirthird)),
                              rep(0.55,length(rctdir2)),rep(0,length(rctdirhalf)),rep(-0.55,length(rctdirthird)),rep(0,length(rbacidirhalf)),rep(-0.55,length(rbacidirthird))),3))


cover1 = subset(cover,cover$type=="Original BACI ES outside of 95% CIs")
cover1b = subset(coverb,coverb$type=="Original BACI ES outside of 95% CIs")

cover1b[which(cover1b$des==8),]

cover1p=  ggplot(cover1) +    
  geom_point(data=cover1,aes(y=val,x=des+adj,fill=size,size=size1),alpha=0.8,pch=23)+
  geom_jitter(data=cover1b, aes(y=val,x=des+adj,fill=size),width=0.01,alpha=0.4,size=2,pch=21)+geom_vline(xintercept=6.5,linetype="dashed")+
  coord_flip(xlim=c(0.25,10.75))+
  theme_classic()+ 
  scale_color_manual(name="Sample size",values=rev(brewer.pal(10,"Oranges")[c(3,5,7)]))+
  scale_fill_manual(name="Sample size",values=rev(brewer.pal(10,"Oranges")[c(3,5,7)]))+
  scale_size_identity()+
  theme(aspect.ratio=1,axis.title=element_text(size=16,angle=0),
        legend.text = element_text(size=15),legend.title=element_text(size=16),
        strip.text = element_text(size=17),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17,angle=0,vjust=0.5),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))+
  guides(color=FALSE,shape=FALSE,size=FALSE,fill=guide_legend(override.aes = list(size=c(12,10,8))))+
  scale_x_continuous(name="",breaks=c(1,3,5,8,10),labels=c("Control-Impact","Before-After","Before-After Control-Impact","Randomised Controlled Trial","Randomised Before-After Control-Impact"))+
  scale_y_continuous(name="Non-coverage probability", breaks=seq(0,1,0.2),limits=c(-0.01,1.01))


cover2 = subset(cover,cover$type=="Different direction to original BACI ES")
cover2b = subset(coverb,coverb$type=="Different direction to original BACI ES")

cover2p = ggplot(cover2) +    
  geom_point(data=cover2,aes(y=val,x=des+adj,fill=size,size=size1),alpha=0.8,pch=23)+
  geom_jitter(data=cover2b, aes(y=val,x=des+adj,fill=size),width=0.01,alpha=0.4,size=2,pch=21)+geom_vline(xintercept=6.5,linetype="dashed")+
  coord_flip(xlim=c(0.25,10.75))+
  theme_classic()+
  scale_color_manual(name="Sample size",values=rev(brewer.pal(10,"Oranges")[c(3,5,7)]))+
  scale_fill_manual(name="Sample size",values=rev(brewer.pal(10,"Oranges")[c(3,5,7)]))+
  scale_size_identity()+
  theme(aspect.ratio=1,axis.title=element_text(size=16,angle=0),
        legend.text = element_text(size=15),legend.title=element_text(size=16),
        strip.text = element_text(size=17),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17,angle=0,vjust=0.5),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))+
  guides(color=FALSE,shape=FALSE,size=FALSE,fill=guide_legend(override.aes = list(size=c(12,10,8))))+
  scale_x_continuous(name="",breaks=c(1,3,5,8,10),labels=c("Control-Impact","Before-After","Before-After Control-Impact","Randomised Controlled Trial","Randomised Before-After Control-Impact"))+
  scale_y_continuous(name="Probability", breaks=seq(0,1,0.2),limits=c(-0.01,1.01))


cover3 = subset(cover,cover$type=="Difference to original BACI ES")
cover3b = subset(coverb,coverb$type=="Difference to original BACI ES")

cover3p = ggplot(cover3) + 
  geom_point(data=cover3,aes(y=val*100,x=des+adj,fill=size,size=size1),alpha=0.8,pch=23)+
  geom_jitter(data=cover3b, aes(y=val*100,x=des+adj,fill=size),width=0.01,alpha=0.4,size=2,pch=21)+geom_vline(xintercept=6.5,linetype="dashed")+
  coord_flip(xlim=c(0.25,10.5))+
  theme_classic()+ 
  scale_color_manual(name="Sample size",values=rev(brewer.pal(10,"Oranges")[c(3,5,7)]))+
  scale_fill_manual(name="Sample size",values=rev(brewer.pal(10,"Oranges")[c(3,5,7)]))+
  scale_size_identity()+
  theme(aspect.ratio=1,axis.title=element_text(size=16,angle=0),
        legend.text = element_text(size=15),legend.title=element_text(size=16),
        strip.text = element_text(size=17),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17,angle=0,vjust=0.5),axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15))+
  guides(color=FALSE,shape=FALSE,size=FALSE,fill=guide_legend(override.aes = list(size=c(12,10,8))))+
  scale_x_continuous(name="",breaks=c(1,3,5,8,10),labels=c("Control-Impact","Before-After","Before-After Control-Impact","Randomised Controlled Trial","Randomised Before-After Control-Impact"))+
  scale_y_log10(name="Absolute percentage difference (|%|)") 

ggarrange(cover3p,cover1p,ncol=1,common.legend=TRUE,legend="right",
          align='hv')

#ggsave("desdeggraph1_diff_cov_2yrs.svg",width=30,height=30,units="cm")

#aggregate(cover$val,list(cover$type,cover$size,cover$des),function(x) mean(x,na.rm=TRUE))
#write.table(aggregate(coverb$val,list(coverb$type,coverb$size,coverb$des),function(x) mean(x,na.rm=TRUE)),
#            "clipboard",sep="\t",row.names=FALSE)

###########
cover = data.frame(val = c(summstatsdev3over,summstatsdev2und),
                   type = factor(c(rep("Overestimation",13),
                                   rep("Underestimation",13)),levels=c(
                                     "Overestimation","Underestimation"
                                   )),
                   
                   size = factor(rep(c("50%","33%","100%","50%","33%","100%","50%","33%","100%","50%","33%","50%","33%"),2),levels=c("100%","50%","33%")),
                   size1 = as.numeric(rep(c(10,8,12,10,8,12,10,8,12,10,8,10,8),2)),
                   des = rep(c(5,5,3,3,3,1,1,1,8,8,8,10,10),2),
                   adj = rep(c(0,-0.55,0.55,0,-0.55,0.55,0,-0.55,0.55,0,-0.55,0,-0.55),2))


coverb = data.frame(val = c(summstatsdev3overb,summstatsdev2undb),
                    type = factor(c(rep("Overestimation",length(summstatsdev3overb)),
                                    rep("Underestimation",length(summstatsdev2undb))),levels=c(
                                      "Overestimation","Underestimation"
                                    )),
                    
                    size = factor(c(summstatsdev3overbsamp,summstatsdev2undbsamp),levels=c("100%","50%","33%")),
                    size1 = c(c(rep(10,length(bacidirhalfover)),rep(8,length(bacidirthirdover)),rep(12,length(badir2over)),rep(10,length(badirhalfover)),
                                rep(8,length(badirthirdover)),rep(12,length(cidir2over)),rep(10,length(cidirhalfover)),rep(8,length(cidirthirdover)),
                                rep(12,length(rctdir2over)),rep(10,length(rctdirhalfover)),rep(8,length(rctdirthirdover)),rep(10,length(rbacidirhalfover)),rep(8,length(rbacidirthirdover))),
                              c(rep(10,length(bacidirhalfund)),rep(8,length(bacidirthirdund)),rep(12,length(badir2und)),rep(10,length(badirhalfund)),
                                rep(8,length(badirthirdund)),rep(12,length(cidir2und)),rep(10,length(cidirhalfund)),rep(8,length(cidirthirdund)),
                                rep(12,length(rctdir2und)),rep(10,length(rctdirhalfund)),rep(8,length(rctdirthirdund)),rep(10,length(rbacidirhalfund)),rep(8,length(rbacidirthirdund))
                              )),
                    des = c(c(rep(5,length(c(bacidirhalfover,bacidirthirdover))),rep(3,length(c(badir2over,badirhalfover,badirthirdover))),rep(1,length(c(cidir2over,cidirhalfover,cidirthirdover))),rep(8,length(c(rctdir2over,rctdirhalfover,rctdirthirdover))),rep(10,length(c(rbacidirhalfover,rbacidirthirdover)))),
                            c(rep(5,length(c(bacidirhalfund,bacidirthirdund))),rep(3,length(c(badir2und,badirhalfund,badirthirdund))),rep(1,length(c(cidir2und,cidirhalfund,cidirthirdund))),rep(8,length(c(rctdir2und,rctdirhalfund,rctdirthirdund))),rep(10,length(c(rbacidirhalfund,rbacidirthirdund))))),
                    
                    adj = c(c(rep(0,length(bacidirhalfover)),rep(-0.55,length(bacidirthirdover)),rep(0.55,length(badir2over)),rep(0,length(badirhalfover)),
                              rep(-0.55,length(badirthirdover)),rep(0.55,length(cidir2over)),rep(0,length(cidirhalfover)),rep(-0.55,length(cidirthirdover)),
                              rep(0.55,length(rctdir2over)),rep(0,length(rctdirhalfover)),rep(-0.55,length(rctdirthirdover)),rep(0,length(rbacidirhalfover)),rep(-0.55,length(rbacidirthirdover))),
                            c(rep(0,length(bacidirhalfund)),rep(-0.55,length(bacidirthirdund)),rep(0.55,length(badir2und)),rep(0,length(badirhalfund)),
                              rep(-0.55,length(badirthirdund)),rep(0.55,length(cidir2und)),rep(0,length(cidirhalfund)),rep(-0.55,length(cidirthirdund)),
                              rep(0.55,length(rctdir2und)),rep(0,length(rctdirhalfund)),rep(-0.55,length(rctdirthirdund)),rep(0,length(rbacidirhalfund)),rep(-0.55,length(rbacidirthirdund))))
                    
)

cover1 = subset(cover,cover$type=="Overestimation")
cover1b = subset(coverb,coverb$type=="Overestimation")

cover1po=  ggplot(cover1) +    
  geom_point(data=cover1,aes(y=val,x=des+adj,fill=size,size=size1),alpha=0.8,pch=23)+ 
  geom_jitter(data=cover1b, aes(y=val,x=des+adj,fill=size),width=0.01,alpha=0.4,size=2,pch=21)+geom_vline(xintercept=6.5,linetype="dashed")+
  coord_flip(xlim=c(0.25,10.5))+
  theme_classic()+ 
  scale_color_manual(name="Sample size",values=rev(brewer.pal(10,"Oranges")[c(3,5,7)]))+
  scale_fill_manual(name="Sample size",values=rev(brewer.pal(10,"Oranges")[c(3,5,7)]))+
  scale_size_identity()+
  theme(aspect.ratio=1,axis.title=element_text(size=16,angle=0),
        legend.text = element_text(size=15),legend.title=element_text(size=16),
        strip.text = element_text(size=17),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17,angle=0,vjust=0.5),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))+
  guides(color=FALSE,shape=FALSE,fill=guide_legend(override.aes = list(size=c(12,10,8))))+
  scale_x_continuous(name="",breaks=c(1,3,5,8,10),labels=c("Control-Impact","Before-After","Before-After Control-Impact","Randomised Controlled Trial","Randomised Before-After Control-Impact"))+
  scale_y_continuous(name="Probability of overestimation", breaks=seq(0,1,0.2),limits=c(-0.01,1.01))

cover2 = subset(cover,cover$type=="Underestimation")
cover2b = subset(coverb,coverb$type=="Underestimation")

cover2pu = ggplot(cover2) +   
  geom_point(data=cover2,aes(y=val,x=des+adj,fill=size,size=size1),alpha=0.8,pch=23)+ 
  geom_jitter(data=cover2b, aes(y=val,x=des+adj,fill=size),width=0.01,alpha=0.4,size=2,pch=21)+geom_vline(xintercept=6.5,linetype="dashed")+
  coord_flip(xlim=c(0.25,10.5))+
  theme_classic()+
  scale_color_manual(name="Sample size",values=rev(brewer.pal(10,"Oranges")[c(3,5,7)]))+
  scale_fill_manual(name="Sample size",values=rev(brewer.pal(10,"Oranges")[c(3,5,7)]))+
  scale_size_identity()+
  theme(aspect.ratio=1,axis.title=element_text(size=16,angle=0),
        legend.text = element_text(size=15),legend.title=element_text(size=16),
        strip.text = element_text(size=17),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17,angle=0,vjust=0.5),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))+
  guides(color=FALSE,shape=FALSE,fill=guide_legend(override.aes = list(size=c(12,10,8))))+
  scale_x_continuous(name="",breaks=c(1,3,5,8,10),labels=c("Control-Impact","Before-After","Before-After Control-Impact","Randomised Controlled Trial","Randomised Before-After Control-Impact"))+
  scale_y_continuous(name="Probability of underestimation", breaks=seq(0,1,0.2),limits=c(-0.01,1.01))


ggarrange(cover1po,cover2pu,cover2p,ncol=1,common.legend=TRUE,legend="right",
          align='hv')

#ggsave("desdeggraph_dir_over_und_2yrs.svg",width=30,height=30,units="cm")

#aggregate(coverb$val,list(coverb$type,coverb$size,coverb$des),function(x) mean(x,na.rm=TRUE))

#write.table(aggregate(coverb$val,list(coverb$type,coverb$size,coverb$des),function(x) mean(x,na.rm=TRUE)),
#            "clipboard",sep="\t",row.names=FALSE)
