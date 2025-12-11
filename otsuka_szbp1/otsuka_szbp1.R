# Workbench Sources:
# Using first regimen start: https://workbench.atroposhealth.biz/case/618529f9-621a-4c26-9457-e4d6603d4e98/
# Using last regimen start: https://workbench.atroposhealth.biz/case/48927b9e-8f21-4efb-a0e0-8d19284b75d9/
# Arcadia, using last regimen within 5 years of initial Dx: https://workbench.atroposhealth.biz/case/32b0d753-d5cd-4c06-a852-eb15c74525ee/
# Arcadia, using last regimen within 3 years of initial Dx: https://workbench.atroposhealth.biz/case/0422788f-2f5b-428b-bbde-1ca612adfe86/
# Arcadia, using last regimen within 2 years of initial Dx: https://workbench.atroposhealth.biz/case/0a32fa1d-4a44-45c8-9501-ded4c5cc3e17/
# Arcadia, using last regimen within 1 years of initial Dx: https://workbench.atroposhealth.biz/case/3c424764-20cb-4519-90ba-86fffdcbc2f4/ 
# Forian, using last regimen within 5 years of initial Dx: https://workbench.atroposhealth.biz/case/878bf89a-f5bc-40f7-8f35-b0d9c4db0d44/ 

rm(list=ls()); graphics.off(); gc(); options(scipen=99, digits=3)

meta <- list(ddr                = "~/Downloads/OtsukaSzBP1/"
            ,years              = 2015:2025
            ,strat              = c("intervention","src")[1]
            ,src                = c("all","Sz A Last2y")[2]
            ,lmt_fu             = c(NA,"12mo")[2]
            ,tgt_prim           = c(NA,"ip.visit","er.visit","suicide","regimen","side")[-1]
            ,sdn                = 1221                                           #Random number seed for reproducibility.
            ,train_pct          = 0.70                                           #Percent of available sample to use for training.
            ,focus_target       = c(outcome_ip.visit.or.regimen.change.12mo=2    #Focus targets in rank order for reporting and train/test split matching (NA will mean all targets)
                                   ,outcome_ip.visit.any.12mo=1
                                   ,outcome_regimen.change.12mo=1
                                   ,outcome_ip.visit.psych.diagnosis.12mo=1
                                   ,outcome_er.visit.psych.diagnosis.12mo=1
                                   )
            ,focus_intervention = c(Ari2MRTU=2)                                  #Focused cohort(s) in rank order for reporting and train/test split matching (NA will mean no primary)
            ,xcld_treat_pred    = c("followup","baseline_regimens.prior_cnt")[1:1] #Excluded from treatment prediction (pattern matching via grep).
            ,xcld_outcome_pred  = c("followup")                                  #Excluded from outcome prediction (pattern matching via grep)
            )

fls <- list.files(meta$ddr,".*csv")

dfA <- NULL
for(f in fls) {
   m <- tools::toTitleCase(gsub("_"," ",gsub("data_processed_|.csv|.gz","",f)))
   m <- gsub("Arcadia","A", gsub("Forian","F", gsub("Norstella","N", m)))
   tmp <- as.data.frame(data.table::fread(paste0(meta$ddr,f)))
   tmp$src <- m
   dfA <- rbind(dfA, subset(tmp, index_year %in% meta$years))
   rm(tmp,m)
}
rm(f,fls)

.fncClip <- function(data, row.names=TRUE, row.names.to.column=FALSE) {
   tmp <- data
   if(row.names.to.column) {
      tmp <- as.data.frame(tmp)
      tmp$rnm <- rownames(tmp)
      tmp <- tmp[,unique(c("rnm",names(tmp)))]
      names(tmp)[1] <- " "
      row.names <- FALSE
   }
   clip <- pipe("pbcopy", "w")                       
   write.table(tmp, file=clip, quote=FALSE, sep="\t", na="", row.names=row.names)                               
   close(clip)
   rm(clip)
}

# Remove unhelpful columns.
dfA$V1 <- NULL

# Fix names.
names(dfA) <- tolower(names(dfA))
names(dfA) <- gsub("_disc","_cnt",names(dfA))
names(dfA) <- gsub("comorb_","cci_",names(dfA))
names(dfA) <- gsub("baseline_baseline","baseline",names(dfA))
names(dfA) <- gsub("followup\\.|\\.disorder|_cont|_bin","",names(dfA))
names(dfA) <- gsub("blood\\.pressure","bp",names(dfA))
names(dfA) <- gsub("baseline_comorb","baseline",names(dfA))
names(dfA) <- gsub("inpatient","ip",names(dfA))
names(dfA) <- gsub("baseline_prior|baseline_scores","baseline",names(dfA))
names(dfA) <- gsub("number\\.of\\.regimens\\.previously","regimens\\.prior",names(dfA))
names(dfA) <- gsub("\\.followup","",names(dfA))
names(dfA) <- gsub("\\.3m","\\.03m",names(dfA))
names(dfA) <- gsub("\\.6m","\\.06m",names(dfA))

names(dfA)[grep("_3mo" ,names(dfA))] <- paste0(gsub("_3mo" ,"",names(dfA)[grep("_3mo" ,names(dfA))]),".03mo")
names(dfA)[grep("_6mo" ,names(dfA))] <- paste0(gsub("_6mo" ,"",names(dfA)[grep("_6mo" ,names(dfA))]),".06mo")
names(dfA)[grep("_12mo",names(dfA))] <- paste0(gsub("_12mo","",names(dfA)[grep("_12mo",names(dfA))]),".12mo")

names(dfA) <- gsub("outcome\\.","outcome_",names(dfA))

# Clean up intervention.
x <- ox <- unique(dfA$intervention)
x <- gsub("INTERVENTION_","",x)
x <- gsub("REF_","\\.",x)
x <- gsub("ARIPIPRAZOLE|ABILIFY","ARI",x)
x <- gsub("LONG.ACTING","LAI",x)
x <- gsub("ARI.ASIMTUFII","Ari2MRTU",x)
x <- gsub("ARI.MAINTENA","AOM",x)
x <- gsub("NON.ARI.LAI","LAI.Other",x)
x <- gsub("ARI.LAUROXIL","LAI.Ari.Laur",x)
x <- gsub("ARI.ORAL","Oral.Ari",x)
x <- gsub("CLOZAPINE","Clozapine",x)
x <- gsub("ATYPICAL.NON.ARI","Oral.Atyp.Other",x)
x <- gsub("TYPICAL","Typical",x)
names(x) <- ox
dfA$intervention <- x[dfA$intervention]
rm(x,ox)

data.frame(table(subset(dfA, src=="Sz A Last5y")$intervention))

# Fix ethnicity.
dfA$ethnicity[-grep("HISPANIC",dfA$ethnicity)] <- "Unknown"
dfA$ethnicity[grep("NOT",dfA$ethnicity)] <- "Not Hispanic or Latino"
dfA$ethnicity[-grep("Not|Unk",dfA$ethnicity)] <- "Hispanic or Latino"

# Fix zip data.
if(!is.character(dfA$zip)) dfA$zip <- sprintf(paste0("%0",max(nchar(dfA$zip), na.rm=TRUE),".0f"),dfA$zip)

# Get real index date.
dfA$index_date <- with(dfA, dob + index_date)

# Convert days to years.
for(n in names(dfA)[grep("days_",names(dfA))]) {
   dfA[[n]] <- dfA[[n]] / 365.25
   names(dfA)[which(names(dfA)==n)] <- gsub("days_","years_",n)
}
rm(n)

# Add follow-up < 1 yr.
dfA$years_followup_gt1 <- with(dfA, ifelse(years_followup >= 1,1,0))

# Add composite outcome any inpatient admission or regimen change (replicate Wu)
for(n in c("03mo","06mo","12mo")) {
   dfA[[paste0("outcome_ip.visit.or.regimen.change.",n)]] <- ifelse( dfA[[paste0("outcome_ip.visit.any.",n)]]==1
                                                                   | dfA[[paste0("outcome_regimen.change.",n)]]==1
                                                                   , 1, 0)
}
rm(n)

# Add train/validation flag.
# Randomly assign train/test groups by most closely matching full distribution
# across targets and interventions.
if(any(tolower(meta$src)=="all")) {
   tmp <- dfA
} else {
   tmp <- subset(dfA, tolower(src) %in% tolower(meta$src))
}

tmp <- as.data.frame(model.matrix(~.-1, data=tmp[,c(names(meta$focus_target),"intervention")]))
names(tmp) <- gsub("intervention","",names(tmp))

ful <- aggregate(. ~ 0, data=tmp, FUN=mean)
# Weight outcome rates based upon focus (can increase contribution to mean absolute difference)
ful[,names(meta$focus_target)] <- ful[,names(meta$focus_target)] * meta$focus_target
ful[,names(meta$focus_intervention)] <- ful[,names(meta$focus_intervention)] * meta$focus_intervention

rnd_mad <- NULL
for(i in 1:10^2) {
   set.seed(i)
   rid <- sample(nrow(tmp), size=nrow(tmp)*meta$train_pct)
   cmp <- aggregate(. ~ 0, data=tmp[rid,], FUN=mean)
   # Weight outcome rates based upon focus (can increase contribution to mean absolute difference)
   cmp[,names(meta$focus_target)] <- cmp[,names(meta$focus_target)] * meta$focus_target
   ful[,names(meta$focus_intervention)] <- ful[,names(meta$focus_intervention)] * meta$focus_intervention
   if(length(na.omit(meta$PrimaryCohort)) > 0) cmp <- cmp[cmp$intervention %in% as.character(na.omit(meta$PrimaryCohort)),]
   if(nrow(cmp) < length(unique(tmp$Src))) next
   rnd_mad <- c(rnd_mad, sum(abs(ful[,-1] - cmp[,-1])))
   rm(rid,cmp)
}
rm(i)

meta$train_sdn <- which(rnd_mad==min(rnd_mad))[1]
set.seed(meta$train_sdn)
dfA$set <- "Test"
dfA$set[sample(nrow(dfA), size=nrow(dfA)*meta$train_pct)] <- "Train"
rm(ful,tmp,rnd_mad)

# Function for moving items in vector to after a named one.
.fncMov <- function(x, itm, aft=NA) {
   sdx <- which(x %in% itm)
   dst <- which(x==aft)
   x[c(setdiff(1:dst,sdx)
       ,sdx
       ,setdiff((dst+1):length(x),sdx))]
}

vls <- list(id   =c("src","patient_id","index_date","set")
            ,tgt = names(dfA)[grep("outcome",names(dfA))]
            ,trt = "intervention"
            ,rem = c(names(dfA)[grep("outstart|log_|start_int|end_int|_date",names(dfA))]
                    ,"record_start","record_end","dob","zip","censor_year"
                    ,"index_era","race_original","gender.female","age_std"
                    ,"days_record","icd9","icd10","cpt","rx","death"
                    ,"index_start","years_record")
)
vls$tgt <- unique(c(vls$tgt[grep("ip.visit.or.regimen",vls$tgt)],vls$tgt))
vls$tgt <- unique(c(vls$tgt[grep("12mo",vls$tgt)],vls$tgt[grep("6mo",vls$tgt)],vls$tgt[grep("3mo",vls$tgt)]),vls$tgt)
vls$pot <- unique(c("age","age_group",setdiff(names(dfA), unlist(vls))))
if(all(!is.na(meta$lmt_fu))) vls$tgt <- vls$tgt[grep(paste(meta$lmt_fu,collapse="|"),vls$tgt)]
if(all(!is.na(meta$tgt_prim))) {
   vln <- NULL
   x <- meta$tgt_prim
   for(x in meta$tgt_prim) vln <- c(vln, vls$tgt[grep(x,vls$tgt)])
   rm(x)
   vls$tgt <- unique(c(vln,vls$tgt))
   rm(vln)
}

for(n in rev(sort(vls$pot[grep("charl|cci_",vls$pot)]))) {
   vls$pot <- .fncMov(vls$pot, itm=n, aft="index_year")
}
rm(n)

if(tolower(meta$strat)=="src") {
   vls$t1 <- c(vls$tgt,"intervention",vls$pot)
} else if(tolower(meta$strat)=="intervention") {
   vls$t1 <- c(vls$tgt,"src",vls$pot)
}

vls$t1 <- .fncMov(vls$t1, itm="years_followup_gt1", aft=tail(vls$t1[grep("outcome",vls$t1)],1))

data.table::data.table(dfA[,vls$t1])

bak <- dfA

if(!any(tolower(meta$src)=="all")) {
   dfA <- subset(dfA, tolower(src) %in% tolower(meta$src))
}

dfA$index_year <- paste0("y",dfA$index_year)

fct <- vls$t1[which(sapply(dfA[,vls$t1], function(x) class(x)[1] %in% c("character","factor") | all(x %in% c(0,1,NA))))]
fct <- unique(c(fct,"index_year"))

tb1 <- tableone::CreateTableOne(vars=vls$t1, factorVars=fct, strata=meta$strat, data=dfA
                               ,includeNA=TRUE, test=FALSE)
rm(fct)

tb1 <- print(tb1, missing=TRUE, smd=TRUE)
meta$tb1 <- tb1
rm(tb1)

.fncClip(meta$tb1)

#3 * "a"

###############################################################################
# Get data engineering and master data from TQL.                              #
###############################################################################

tmp <- readLines(paste0(meta$ddr,"query.tql"))
X <- tmp[intersect(grep("^var",tmp),grep("rx\\(|rx\\=",gsub(" ","",tolower(tmp))))]
X <- gsub("var ","",X)
X <- gsub('"',"'",X)

rxcui <- list()
for(x in X) {
   rxcui[[trimws(unlist(strsplit(x,"="))[1])]] <- as.character(na.omit(unique(sapply(regmatches(unlist(strsplit(x,",")), regexec("'\\s*(.*?)\\s*'", unlist(strsplit(x,",")))), function(y) y[2]))))
}
rm(x,X,tmp)
#rxcui

dfD <- NULL
for(n in names(rxcui)) {
   for(i in 1:length(rxcui[[n]])) {
      dfD <- rbind(dfD, data.frame(tql_var=n, rxcui=rxcui[[n]][i], brand_name=rxnorm::get_bn(rxcui[[n]][i]), rx_name=rxnorm::get_rx(rxcui[[n]][i])  ))
   }
   rm(i)
}
rm(n)
data.table::data.table(dfD)

meta$map_drug <- dfD
rm(dfD,rxcui)

.fncClip(meta$map_drug, row.names=FALSE)

###############################################################################
# Build xgboost model for treatment and outcome.                              #
###############################################################################

mdl <- list()

# Treatment selection prediction model.
y <- as.integer(as.factor(dfA[[vls$trt]]))-1                                     #Gather outcome (treatment), potential predictors.
X <- data.matrix(dfA[,vls$pot[-grep(paste(meta$xcld_treat_pred, collapse="|"),vls$pot)]])
trn <- which(dfA$set=="Train")
prm <- list(objective="multi:softprob"
           ,eta=0.1
           ,reg_alpha=1000, reg_lambda=2
           ,num_class=length(unique(y)))

set.seed(meta$sdn)
tin <- Sys.time()
xcv <- list()
xcv$best_iteration <- 100
xcv <- xgboost::xgb.cv(data=X[trn,], label=y[trn]
                      ,params=prm, nfold=5, nrounds=100, early_stopping_rounds=50
                      ,verbose=FALSE
                      )
xcv$best_iteration
Sys.time() - tin

set.seed(meta$sdn)
tin <- Sys.time()
xgb <- xgboost::xgboost(data=X[trn,], label=y[trn]
                        ,params=prm, nrounds=xcv$best_iteration, min_child_weight=2
                        ,verbose=FALSE
                        )
vmp <- xgboost::xgb.importance(xgb$feature_names, xgb)
xgboost::xgb.plot.importance(vmp[,1:2], rel_to_first=TRUE, xlab="Relative importance")
vmp
Sys.time() - tin

mdl$sz_treat <- list(xcv=xcv, xgb=xgb, vmp=vmp)

prd <- t(matrix(predict(xgb, newdata=X[,xgb$feature_names]), nrow=length(unique(y))))
colnames(prd) <- paste0(levels(as.factor(dfA$intervention)),"_pred_xgb")

tmp <- as.data.frame(model.matrix(~.-1, data=dfA[,"intervention",drop=FALSE]))
names(tmp) <- paste0(gsub("intervention","",names(tmp)),"_true")
tmp <- cbind(tmp,prd)
head(tmp)

mAUROC_trn <- suppressWarnings(multiROC::multi_roc(data=tmp[trn,]))
mAUPRC_trn <- suppressWarnings(multiROC::multi_pr(data=tmp[trn,]))
mAUROC_vld <- suppressWarnings(multiROC::multi_roc(data=tmp[-trn,]))
mAUPRC_vld <- suppressWarnings(multiROC::multi_pr(data=tmp[-trn,]))
rst <- as.data.frame(t(rbind(unlist(mAUROC_trn$AUC$xgb)
                            ,unlist(mAUROC_vld$AUC$xgb)
                            ,unlist(mAUPRC_trn$AUC$xgb)
                            ,unlist(mAUPRC_vld$AUC$xgb)
                            )))
names(rst) <- c("AUROC_Train","AUROC_Valid","AUPRC_Train","AUPRC_Valid")
rst$Target <- rownames(rst); rownames(rst) <- NULL
rst <- rst[,unique(c("Target",names(rst)))]
rst
.fncClip(rst, row.names=FALSE)

head(multiROC::test_data)

mtx <- with(dfA, table(intervention, baseline_regimens.prior_cnt))
mtx / rowSums(mtx)
rm(mtx)

vls$t2 <- unique(c(names(meta$focus_target),"baseline_regimens.prior_cnt",vmp$Feature))
tmp <- dfA[,c("intervention",vls$t2)]
tmp$baseline_regimens.prior_cnt <- paste0("c",tmp$baseline_regimens.prior_cnt)
fct <- vls$t2[which(sapply(tmp[,vls$t2], function(x) class(x)[1] %in% c("character","factor") | all(x %in% c(0,1,NA))))]
fct <- unique(c(fct,"index_year"))

tb1 <- tableone::CreateTableOne(vars=vls$t2, factorVars=fct, strata=meta$strat, data=tmp
                                ,includeNA=TRUE, test=FALSE)
rm(fct)

tb1 <- print(tb1, missing=TRUE, smd=TRUE)
meta$tb2 <- tb1
rm(tb1)

.fncClip(meta$tb2)

# Build xgboost model for outcome
y <- dfA[dfA$set=="Train",names(meta$focus_target)[1]]
X <- data.matrix(dfA[dfA$set=="Train",vls$pot[-grep("followup",vls$pot)]])
prm <- list(objective="binary:logistic"
            ,eta=0.1
            ,reg_alpha=1000, reg_lambda=2)

set.seed(meta$sdn)
tin <- Sys.time()
xcv <- list()
xcv$best_iteration <- 100
xcv <- xgboost::xgb.cv(data=X, label=y
                       ,params=prm
                       ,nfold=5
                       ,verbose=FALSE
                       ,nrounds=100, early_stopping_rounds=50)
xcv$best_iteration
Sys.time() - tin

set.seed(meta$sdn)
tin <- Sys.time()
xgb <- xgboost::xgboost(data=X, label=y
                        ,params=prm
                        ,verbose=FALSE
                        ,min_child_weight=2
                        ,nrounds=xcv$best_iteration)
vmp <- xgboost::xgb.importance(xgb$feature_names, xgb)
xgboost::xgb.plot.importance(vmp[,1:2], rel_to_first=TRUE, xlab="Relative importance")
vmp
Sys.time() - tin

