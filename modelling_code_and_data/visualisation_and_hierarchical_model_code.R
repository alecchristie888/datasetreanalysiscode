library(data.table)

data <- fread("dataset_alldata.csv")

data_summary <- data[, list(randomised = mean(randomised),
                            measure_type = measure_type_group[1],
                            num_site = length(unique(site_id)),
                            num_obs = length(measure)), c("dataset_id", "comparison_id")]
data_summary$randomised <- factor(data_summary$randomised)
levels(data_summary$randomised) <- c("Non-randomised", "Randomised")
data_summary$dataset_id <- factor(data_summary$dataset_id)
data_summary$comparison_id <- factor(data_summary$comparison_id)

load("model_output.rda")

names(output)[1:4] <- c("Est", "SE", "t.stat", "p.value")

output <- output[order(dataset_id, comparison_id)]
head(output)
################################################################################

method.comparison.summary <- function(output1, output2) {
  
  stopifnot(nrow(output1) == 1 && nrow(output2) == 1)
  stopifnot(output1$dataset_id == output2$dataset_id)
  stopifnot(output1$comparison_id == output2$comparison_id)
  stopifnot(output1$method != output2$method)
  
  data.frame(dataset_id = output1$dataset_id,
             comparison_id = output1$comparison_id,
             method1 = output1$method,
             method2 = output2$method,
             est.double = abs(output1$Est - output2$Est) / max(abs(output1$Est), abs(output2$Est)) > 0.5,
             est.diff.sign = output1$Est * output2$Est < - 1e-6,
             ci.no.overlap = abs(output1$Est - output2$Est) > 1.96 * (output1$SE + output2$SE),
             diff.significance = (output1$p.value - 0.05) * (output2$p.value - 0.05) < 0,
             ci.diff.sign = (output1$t.stat < -1.96 & output2$t.stat > 1.96) |
               (output2$t.stat < -1.96 & output1$t.stat > 1.96))
  
}

method.comparison.summary.aggregate <- function(output) { # output for the same dataset & comparison
  
  stopifnot(nrow(output) >= 2)
  
  out <- data.frame()
  
  for (i in 1:(nrow(output) - 1)) {
    for (j in (i+1):nrow(output)) {
      out <- rbind(out, method.comparison.summary(output[i, ], output[j, ]))
    }
  }
  
  out
  
}

output_summary1 <- list()
for (i in 1:(nrow(output) / 4)) {
  output_summary1[[i]] <- method.comparison.summary.aggregate(output[i * 4 - 3:0, ])
}

output_summary1 <- rbindlist(output_summary1)
output_summary1 <- merge(output_summary1, data_summary, c("dataset_id", "comparison_id"))

output_summary2 <- output_summary1[, list(est.double = mean(est.double, na.rm = TRUE),
                                          est.diff.sign = mean(est.diff.sign, na.rm = TRUE),
                                          ci.no.overlap = mean(ci.no.overlap, na.rm = TRUE),
                                          diff.significance = mean(diff.significance, na.rm = TRUE),
                                          ci.diff.sign = mean(ci.diff.sign, na.rm = TRUE)),
                                   by = c("randomised", "method1", "method2")]

knitr::kable(output_summary2, digits = 2)

length(unique(output_summary1$dataset_id))

################################################################################
##################################### Plot #####################################
################################################################################

library(reshape2)
output_long <- dcast(output, dataset_id + comparison_id ~ method, value.var = "t.stat")

output_long <- merge(output_long, data_summary, by = c("dataset_id", "comparison_id"))

## Truncate t-statistics
tmax <- 10
output_long$baci <- pmin(tmax, output_long$baci)
output_long$baci <- pmax(- tmax, output_long$baci)
output_long$ci <- pmin(tmax, output_long$ci)
output_long$ci <- pmax(- tmax, output_long$ci)
output_long$ba <- pmin(tmax, output_long$ba)
output_long$ba <- pmax(- tmax, output_long$ba)
output_long$lagged <- pmin(tmax, output_long$lagged)
output_long$lagged <- pmax(- tmax, output_long$lagged)


designs <- rev(c("ba", "ci", "lagged", "baci"))
col1="grey"
col2="orange"
col3="red"

for(p in 1:3){
  for(j in (p+1):4){
    zonename = as.character(interaction(designs[p],designs[j]))
    output_long[,zonename] = factor(rep("col1",nrow(output_long)),levels=c(col1,col2,col3))
    for(i in 1:nrow(output_long)){
      tab1 = table(cut(output_long[i, designs[p]], c(-Inf,-1.96, 1.96, Inf)),
                   cut(output_long[i,designs[j]], c(-Inf,-1.96, 1.96, Inf)))
      if(tab1[1,1]==1|tab1[2,2]==1|tab1[3,3]==1){output_long[i,zonename]=col1}
      else{if(tab1[1,2]==1|tab1[3,2]==1|tab1[2,1]==1|tab1[2,3]==1){output_long[i,zonename]=col2}
        else{output_long[i,zonename] = col3}}
    }
  }
}

head(output_long)

## Pairwise comparison

get.percent <- function(r, method1, method2) {
  
  output_long <- output_long[output_long$randomised == r, ]
  output_percent <- table(cut(output_long[, method1], c(-Inf,-1.96, 1.96, Inf)),
                          cut(output_long[, method2], c(-Inf,-1.96, 1.96, Inf)))
  rownames(output_percent) <- c(-6, 0, 6)
  colnames(output_percent) <- c(-6, 0, 6)
  output_percent <- data.frame(melt(output_percent))
  colnames(output_percent) <- c(method1, method2, "count")
  output_percent
  
}

pairwise.plot <- function(method1, method2) {
  zone=as.character(interaction(method1,method2))
  output_percent1 <- get.percent("Non-randomised", method1, method2)
  output_percent1$randomised  <- "Non-randomised"
  output_percent2 <- get.percent("Randomised", method1, method2)
  output_percent2$randomised  <- "Randomised"
  output_percent <- rbind(output_percent1, output_percent2)
  
  library(ggplot2)
  ggplot(output_long) + aes_string(x = method1, y = method2, col=zone) + geom_point(alpha = 0.5, position = position_jitter(0.2,0.2),pch=16) + geom_text(aes(label = count), data = output_percent,col="black", size = 5) + theme_classic(base_size = 18) + geom_vline(xintercept = -1.96,col="black") + geom_vline(xintercept = 1.96,col="black") + geom_hline(yintercept = -1.96,col="black") + geom_hline(yintercept = 1.96,col="black") + facet_grid(~ randomised) + xlab(paste(toupper(method1), "t-statistics")) + ylab(paste(toupper(method2), "t-statistics"))+scale_color_manual(values=c(col1,col2,col3))+guides(color=FALSE)+theme(aspect.ratio = 1)
  
}


z=1
p=list()
for (i in 1:3) {
  for (j in (i+1):4) {
    p[[z]] <- pairwise.plot(designs[i], designs[j])
    z=z+1
  }
}

library(gridExtra)
p1=marrangeGrob(p,nrow=2,ncol=3)
p1
#ggsave("alltstatplots_alt.pdf",p1,width=60,height=60,units="cm")
#ggsave("alltstatplots_alt_rev.svg",p1,width=60,height=60,units="cm")

################################################################################
#################### Plot point estimates (not very useful) ####################
################################################################################

##
library(reshape2)
output_long <- dcast(output, dataset_id + comparison_id ~ method, value.var = "Est")

output_long <- merge(output_long, data_summary, by = c("dataset_id", "comparison_id"))

summary(output_long$baci,na.rm=TRUE)
summary(output_long$ci,na.rm=TRUE)
summary(output_long$ba,na.rm=TRUE)
summary(output_long$lagged[which(output_long$randomised=="Randomised")],na.rm=TRUE)
summary(output_long$lagged[which(output_long$randomised=="Non-randomised")],na.rm=TRUE)

##### remove two non-randomised lagged design outliers to allow plotting - mention this clearly in legend in Supp.Info.
output_long[which(output_long$lagged>11),]

output_long_remout <- output_long[which(abs(output_long$lagged)<=11),]

designs <- rev(c("ba", "ci", "lagged", "baci"))

head(output_long_remout)

## Pairwise comparison
pairwise.plot <- function(method1, method2) {
  library(ggplot2)
  ggplot(output_long_remout) + aes_string(x = method1, y = method2) + geom_point(alpha = 0.5, position = position_jitter(0.2,0.2),pch=16,col="grey50") + theme_classic(base_size = 18) + facet_grid(~ randomised) + xlab(paste(toupper(method1), "Estimate")) + ylab(paste(toupper(method2), "Estimate"))+scale_color_manual(values=c(col1,col2,col3))+guides(color=FALSE)+theme(aspect.ratio = 1)+coord_cartesian(xlim=c(-11,11),ylim=c(-11,11))+geom_abline(intercept=0,slope=1,col="red",lty=2)
  
}

z=1
p=list()
for (i in 1:3) {
  for (j in (i+1):4) {
    p[[z]] <- pairwise.plot(designs[i], designs[j])
    z=z+1
  }
}

library(gridExtra)
p2=marrangeGrob(p,nrow=2,ncol=3)
p2
#ggsave("allpointestplots_alt.pdf",p2,width=60,height=60,units="cm")
#ggsave("allpointestplots_alt_rev.svg",p2,width=60,height=60,units="cm")

################################################################################
########################### Variance component model ###########################
################################################################################

tstat <- dcast(output, model_no ~ method, value.var = "t.stat")
na_models <- tstat[!complete.cases(tstat), "model_no"]
## se <- dcast(output, model_no ~ method, value.var = "SE")
## tmp <- se$model_no[apply(se[, 2:5], 1, max) > 100]
## na_models <- c(as.character(na_models), as.character(tmp[!is.na(tmp)]))

est <- dcast(output[! model_no %in% na_models], dataset_id + comparison_id ~ method, value.var = "Est")
se <- dcast(output[! model_no %in% na_models], dataset_id + comparison_id ~ method, value.var = "SE")

est <- merge(est, data_summary, by = c("dataset_id", "comparison_id"))
se <- merge(se, data_summary, by = c("dataset_id", "comparison_id"))

## Randomise the sign
s <- sample(c(-1, 1), nrow(est), replace = TRUE)
est[, c("baci", "ba", "ci", "lagged")] <- est[, c("baci", "ba", "ci", "lagged")] * s

vcm_data1 <- list(n = nrow(est[est$randomised == "Non-randomised",]),
                  p = 4,
                  est = est[est$randomised == "Non-randomised", c("baci", "ba", "ci", "lagged")],
                  se = se[se$randomised == "Non-randomised", c("baci", "ba", "ci", "lagged")])

vcm_data2 <- list(n = nrow(est[est$randomised == "Randomised",]),
                  p = 4,
                  est = est[est$randomised == "Randomised", c("baci", "ba", "ci", "lagged")],
                  se = se[se$randomised == "Randomised", c("baci", "ba", "ci", "lagged")])

library(rstan)

vcm <- stan_model("vcm.stan")
fit1 <- sampling(vcm, data = vcm_data1, cores = 4)
fit2 <- sampling(vcm, data = vcm_data2, cores = 4)

vars <- c("sigma_beta", names(fit1)[!grepl("beta", names(fit1))])
cols <- c("mean", "2.5%", "97.5%")

knitr::kable(summary(fit1)$summary[vars, cols], digits = 3)
knitr::kable(summary(fit2)$summary[vars, cols], digits = 3)

save(fit1, fit2, file = "fit.rda")
