library(data.table)
library(ggplot2)
library(viridis)
library(svglite)
library(Hmisc)

cedat = fread('ceprevdata.csv',stringsAsFactors = FALSE)

designnames = c("After","BA","CI","BACI","R-CI","R-BACI")

vals = NULL
### After
vals[1] = length(unique(cedat[cedat$before == 0 & cedat$controlled == 0 & cedat$review == 0,pageID]))

### BA
vals[2] = length(unique(cedat[cedat$before == 1 & cedat$controlled == 0 & cedat$review == 0,pageID]))

###CI
vals[3]= length(unique(cedat[cedat$randomised == 0 & cedat$before == 0 & cedat$controlled == 1 & cedat$review == 0,pageID]))

##RCT
vals[5]= length(unique(cedat[cedat$randomised == 1 & cedat$before == 0 & cedat$controlled == 1 & cedat$review == 0,pageID]))

##BACI
vals[4] = length(unique(cedat[cedat$before == 1 & cedat$randomised == 0& cedat$controlled == 1 & cedat$review == 0,pageID]))

##RBACI
vals[6] = length(unique(cedat[cedat$before == 1 & cedat$randomised == 1 & cedat$controlled == 1 & cedat$review == 0,pageID]))

####not including reviews

###calculate percentages
vals=100*vals/sum(vals)

cedatalong <- data.table(prev=vals,design=designnames,lit="Conservation Evidence")

#### campbell collab
campbdat = fread('campprevdata.csv')

str(campbdat)

campbdatalong <- data.table(prev=100*unlist(campbdat[,list(After_prop,BA_prop,CI_prop,BACI_prop,RCI_prop,RBACI_prop)]),design=c(sapply(1:length(designnames),function(x){return(rep(designnames[x],nrow(campbdat)))})),lit="Campbell Collaboration")

####collate data

desprev = rbind(cedatalong,campbdatalong)
desprev$design=factor(desprev$design,levels=c("After","BA","CI","BACI","R-CI","R-BACI"))
str(desprev)

desprev[lit=="Conservation Evidence",barprev:=prev]
desprev[lit=="Campbell Collaboration",statprev:=prev]
desprev[lit=="Conservation Evidence",statprev:=NA]
for(i in 1:length(designnames)){
  desprev[lit=="Campbell Collaboration"&design==designnames[i],barprev:=mean(desprev[lit=="Campbell Collaboration"&design==designnames[i],prev],na.rm=TRUE)]
}

####plot data
clrs=c(rgb(86/255,180/255,233/255),rgb(0,158/255,115/255))


ggplot(desprev,aes(fill=lit,col=lit,group=lit)) + 
  theme_classic()+
  stat_summary(aes(x=design, y=prev),geom="bar",width=0.7,position="dodge",fun = 'mean')+
  stat_summary(aes(x=as.numeric(design)-0.175, y=statprev),geom="linerange",fun.data = 'mean_cl_boot',col="black",pch=18,fill="black",size=0.75)+
  geom_jitter(aes(x=as.numeric(design)-0.175, y=statprev),height=0,width=0.15,col="black",alpha=0.4,pch=21,fill=clrs[1])+
  theme(aspect.ratio=1,axis.title=element_text(size=25,angle=0),axis.title.y=element_text(size=25),axis.text=element_text(size=22))+
  scale_fill_manual(name="Design",values=c(clrs[1],clrs[2]),guide=FALSE)+
  scale_colour_manual(name="Design",values=c("black","black"),guide=FALSE)+
  scale_y_continuous(name="Percentage of studies (%)")+
  scale_x_discrete(name="Study design")


ggsave("Figure2.svg",device='svg',width=30,height=30,unit="cm")

