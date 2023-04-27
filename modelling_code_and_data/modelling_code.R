library(data.table)
library(lme4)
library(MASS)
library(stringr)

data <- fread("dataset_alldata.csv")

str(data)
data$site_id<-as.character(data$site_id)
data$subsample<-as.character(data$subsample)
data$season<-as.character(data$season)
################################################################################
############################## Data Augmentation ###############################
################################################################################

data[, treatment_group := factor(treatment_group, c("Control", "Impact"))]
data[, time := factor(time, c("Before", "After"))]
data[, comparison_id := factor(data$comparison_id)] ###note this is an id within each dataset, not between
data[, site_id := factor(site_id)] ###note this is an id within each dataset, not between
data[, dataset_id := factor(dataset_id)]
data[, randomised := factor(data$randomised)] ###is dataset randomised?
data[, season := factor(season)] ###note this is an id for season within each dataset, not between
data[, subsample := factor(subsample)] ###note this is an id for subsamples within sites within each dataset, not between
data[, measure_type := factor(measure_type)] ###specific measure used
data[, measure_type_group := factor(measure_type_group)] ### broader type of measure used (lumping measures that have similar statistical properties)
data[, entry_id := (1:nrow(data))] ####unique id for each row
data[, measure := as.numeric(measure)] #### measured data for each dataset
data[, treatment_status := as.numeric(treatment_group == "Impact" & time == "After")]  ### interaction for BACI (DiD) estimation

## Unique identifier for each comparison
data[, model_no := interaction(dataset_id,comparison_id,measure_type_group)]

## Check whether each site has repeated measurements
setkeyv(data, c("dataset_id", "comparison_id", "site_id", "subsample", "treatment_group", "time"))

data[, group_time := factor(paste(treatment_group, time))]

data_summary <- data[, list(cb = sum(group_time == "Control Before"),
                            ca = sum(group_time == "Control After"),
                            ib = sum(group_time == "Impact Before"),
                            ia = sum(group_time == "Impact After")),
                     by = c("dataset_id", "comparison_id", "site_id")]

print(data_summary, 20)

################################################################################
##### Check whether each site has the same number of repeated measurements #####
################################################################################

anomalies  <- data_summary[, list(no_sites = length(unique(site_id)),
                                  var_no_group_time = var((cb > 0) + (ca > 0) + (ib > 0) + (ia > 0))),
                           by = c("dataset_id", "comparison_id")]
anomalies <- anomalies[var_no_group_time > 0]

print(anomalies, 100)

data <- data[dataset_id != 51] # Only dataset 51 has no repeated site measurements 


################################################################################
############################# Remove 0 comparisons #############################
################################################################################

data_nonzero <- data[, list(num_nonzero = sum(measure != 0, na.rm = TRUE),
                            total = length(measure)),
                     c("dataset_id", "comparison_id", "treatment_group", "time")]

data_nonzero

## data_keep <- data_nonzero[, list(keep = sum(num_nonzero > 2) >= 2),
##                           c("dataset_id", "comparison_id")]

data_keep <-
  data_nonzero[, list(keep =
                        sum(num_nonzero > 0) == 4 & ## No group is entirely zero
                        sum(num_nonzero / total > 0.1) >= 2 & ## at least two groups with 10\% nonzero
                        length(total) == 4), ## have all 4 groups
               c("dataset_id", "comparison_id")]

data <- merge(data, data_keep, by = c("dataset_id", "comparison_id"))
data <- data[keep == TRUE]
data$keep <- NULL

data <- data[measure_type_group != "population_change"] # Only one dataset (36) has this

################################################################################
############################### Check subsample ################################
################################################################################

## Most but not all datasets have repeated measurements within subsample. For the 3 datasets that don't, this should not be an issue as the subsamples are not supposed to be independent units within sites.

data_summary_subsample <-
  data[, list(within_site_contrast =
                (sum(treatment_group == "Control") > 0) + (sum(treatment_group == "Impact") > 0) - 1,
              rep_measure =
                (sum(time == "Before") > 0) + (sum(time == "After") > 0) - 1,
              num_measure = length(measure) / length(unique(comparison_id))),
       c("dataset_id", "site_id", "subsample")]

data_summary1 <-
  data_summary_subsample[, list(within_site_contrast = mean(within_site_contrast),
                                rep_measure = mean(rep_measure)),
                         ## num_measure = mean(num_measure)),
                         c("dataset_id")]

data_summary1[rep_measure < 1]


################################################################################
############################ Summary of the datasets ###########################
################################################################################

## Columns:
## - Dataset id
## - Number of comparisons
## - Number of sites
## - Number of subsamples
## - Randomised?
## - Measurement type
## - Within-site contrast?

data_summary1 <-
  data[, list(within_site_contrast =
                (sum(treatment_group == "Control") > 0) + (sum(treatment_group == "Impact") > 0) - 1),
       c("dataset_id", "site_id")]

data_summary1 <-
  data_summary1[, list(within_site_contrast = mean(within_site_contrast)),
                c("dataset_id")]

data_summary2 <-
  data[, list(num_comp = length(unique(comparison_id)),
              num_site = length(unique(site_id)),
              num_sub = length(unique(paste(site_id, subsample))),
              randomised = mean(as.numeric(randomised) - 1),
              measure_type = paste(unique(measure_type_group), collapse = ",")),
       c("dataset_id")]

data_summary <- merge(data_summary2, data_summary1, by = c("dataset_id"))

total <- data_summary[, list(dataset_id = "Total",
                             num_comp = sum(num_comp),
                             num_site = sum(num_site),
                             num_sub = sum(num_sub),
                             randomised = sum(randomised),
                             measure_type = "",
                             within_site_contrat = sum(within_site_contrast))]

knitr::kable(data_summary)
knitr::kable(total)


################################################################################
######################### Construct mixed effect model #########################
################################################################################

baci.fixed <- as.formula("measure ~ 1 + treatment_group + time + treatment_status")
baci.random <- as.formula("~ 1 | site_id/subsample")
baci.full <- as.formula("measure ~ 1 + treatment_group + time + treatment_status + (1 | site_id/subsample)")

ci.fixed <- as.formula("measure ~ 1 + treatment_status")
ci.random <- as.formula("~ 1 | site_id/subsample")
ci.full <- as.formula("measure ~ 1 + treatment_status + (1 | site_id/subsample)")

ba.fixed <- as.formula("measure ~ 1 + treatment_status")
ba.random <- as.formula("~ 1 | site_id/subsample")
ba.full <- as.formula("measure ~ 1 + treatment_status + (1 | site_id/subsample)")

lagged.fixed <- as.formula("measure ~ 1 + treatment_status + before_measure_average")
lagged.random <- as.formula("~ 1 | site_id/subsample")
lagged.full <- as.formula("measure ~ 1 + treatment_status + before_measure_average + (1 | site_id/subsample)")

get.family <- function(measure_type) {
  family <- switch(as.character(measure_type),
                   count = quasipoisson(link = "log"),
                   density = quasipoisson(link = "log"),
                   percentage = quasibinomial(link = "log"),
                   size = gaussian(link = "log"))
  family
}

unique(data$measure_type_group)

## Prepare dataset for covariance adjustment (lagged regression)
data_before <- data[time == "Before",
                    list(before_measure_average = mean(measure)),
                    c("dataset_id", "comparison_id", "site_id", "subsample")]
data_after <- data[time == "After"]

data_lagged <- merge(data_after, data_before,
                     by = c("dataset_id", "comparison_id", "site_id", "subsample"), all.x = TRUE)

################################################################################
################################## Fit model ###################################
################################################################################

glmmPQL_control1 <- nlme::lmeControl(maxIter = 100, msMaxIter = 100)
glmmPQL_control2 <- nlme::lmeControl(maxIter = 100, msMaxIter = 100, opt = "optim")

glmer_control1 <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
glmer_control2 <- glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 1e5))

all_comparisons <- unique(data$model_no)

fit.one.model <- function(formula, fixed, random, data2, family) {
  
  model <- NULL
  
  ## Gaussian log-link with zeros (fix initialization problems)
  if (data2$measure_type_group[1] == "size") {
    data2$measure <- pmax(data2$measure, min(data2$measure[data2$measure > 0]) / 100)
  }
  
  ## Average number of measurements per site
  avg_num_measure <- nrow(data2) / length(unique(data2$site_id))
  
  if (avg_num_measure < 2 || length(unique(data2$site_id)) < 10) { # Use glm (no random effect)
    try(model <- glm(fixed, data = data2, family = family, singular.ok = FALSE))
  } else if (!(family$family %in% c("quasibinomial", "quasipoisson"))) { # Use glmer
    try(model <- glmer(formula, data = data2, family = family,
                       control = glmer_control1))
    if (is.null(model) || isSingular(model)) {
      try(model <- glmer(formula, data = data2, family = family,
                         control = glmer_control2))
    }
    if (!is.null(model) && isSingular(model)) { ## If it is a singular model
      model <- NULL
    }
  } else {
    try(model <- glmmPQL(fixed, random, data = data2, family = family,
                         control = glmmPQL_control1))
    if (is.null(model)) {
      try(model <- glmmPQL(fixed, random, data = data2, family = family,
                           control = glmmPQL_control2))
    }
    
  }
  
  model
  
}

model.to.output <- function(model) {
  out <- tryCatch(coef(summary(model))["treatment_status", ], error = function(e) rep(NA, 4))
  if (length(out) > 4){
    out <- out[setdiff(names(out), "DF")]
  }
  out
}

fit.model <- function(id) {
  
  data1 <- data[model_no == id]
  
  family <- get.family(data1$measure_type_group[1])
  
  baci.model <- NULL
  ci.model <- NULL
  ba.model <- NULL
  
  try(baci.model <- fit.one.model(baci.full, baci.fixed, baci.random, data1, family))
  try(ci.model <- fit.one.model(ci.full, ci.fixed, ci.random, data1[time == "After"], family))
  try(ba.model <- fit.one.model(ba.full, ba.fixed, ba.random, data1[treatment_group == "Impact"], family))
  try(lagged.model <- fit.one.model(lagged.full, lagged.fixed, lagged.random, data_lagged[model_no == id], family))
  
  out1 <- model.to.output(baci.model)
  out2 <- model.to.output(ci.model)
  out3 <- model.to.output(ba.model)
  out4 <- model.to.output(lagged.model)
  
  output <- rbind(out1, out2, out3, out4)
  
  output <- data.frame(output)
  output$method <- c("baci", "ci", "ba", "lagged")
  output$dataset_id  <- data1$dataset_id[1]
  output$comparison_id  <- data1$comparison_id[1]
  output$model_no <- id
  
  output
}


############### Non-Windows ########################################
#library(parallel)
#output1 <- mclapply(1:500, function(i) {print(paste("Start:", i)); try(out <- fit.model(all_comparisons[i])); print(out); out}, mc.cores = 3)
#output2 <- mclapply(501:length(all_comparisons), function(i) {print(paste("Start:", i)); try(out <- fit.model(all_comparisons[i])); out}, mc.cores = 3)

#output <- c(output1, output2)

#output <- rbindlist(output)

#save(output, file = "model_output.rda")


############################## Windows ###################
library(doParallel)

cl.fun = function(i) {print(paste("Start:", i)); try(out <- fit.model(all_comparisons[i])); print(out); return(out)}

cl <- makeCluster(3)
clusterEvalQ(cl,c(library(data.table),library(lme4),library(MASS)))
clusterExport(cl,c('data_lagged','ba.fixed','ba.full','ba.random','baci.fixed','baci.full','baci.random','ci.fixed','ci.full','ci.random','lagged.fixed','lagged.full','lagged.random',
                   'glmer_control1','glmer_control2','glmmPQL_control1','glmmPQL_control2','get.family','all_comparisons','data','fit.model','fit.one.model','model.to.output'))

output1 <- parLapplyLB(cl,1:500,cl.fun)
output2 <- parLapplyLB(cl,501:length(all_comparisons),cl.fun)

stopCluster(cl)

output <- c(output1, output2)

output <- rbindlist(output)

save(output, file = "model_output.rda")


################################################################################
##################### Check if any result looks problematic ####################
################################################################################

## problematic <- unique(output[abs(t.value) > 10]$model_no)
## problematic <- unique(output[abs(t.value) < 0.001]$model_no)
problematic <- unique(output[is.na(t.value)]$model_no)


par(mfrow = c(4, 4))
count <- 0
for (id in problematic) {
  count <- count + 1
  data1 <- data[model_no == id]
  family <- get.family(data1$measure_type_group[1])
  boxplot(measure ~ treatment_group + time, data1, main = id)
  if (count %% 16 == 0) {
    readline(prompt="Press [enter] to continue")
  }
}

#######these errors were acceptable and small in number.
