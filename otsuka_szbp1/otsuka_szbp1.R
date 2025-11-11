# Workbench Sources:
# First run across data assets: https://workbench.atroposhealth.biz/project/a0317f03-cb61-467f-a5ad-037a68ea36a8/edit/
# First run with using initial treatment start: https://workbench.atroposhealth.biz/case/479c2cd4-273e-4b2e-85f8-e730b98d4760/

rm(list=ls()); graphics.off(); gc(); options(scipen=99, digits=3)

meta <- list(ddr   = "~/Downloads/OtsukaSzBP1/"
            ,years = 2015:2025
            ,strat = c("intervention","src")[2]
            ,src   = c("all","Sz A Last")[1]
            )

fls <- list.files(meta$ddr,".*csv")

dfA <- NULL
for(f in fls) {
   m <- tools::toTitleCase(gsub("_"," ",gsub("data_processed_|.csv","",f)))
   m <- gsub("Arcadia","A", gsub("Forian","F", gsub("Norstella","N", m)))
   tmp <- as.data.frame(data.table::fread(paste0(meta$ddr,f)))
   tmp$src <- m
   dfA <- rbind(dfA, subset(tmp, index_year %in% meta$years))
   rm(tmp,m)
}
rm(f,fls)

# Remove unhelpful columns.
dfA$V1 <- NULL

# Fix names.
names(dfA) <- tolower(names(dfA))
names(dfA) <- gsub("comorb_","cci_",names(dfA))
names(dfA) <- gsub("baseline_baseline","baseline",names(dfA))
names(dfA) <- gsub("followup\\.|\\.disorder|_cont|_bin","",names(dfA))
names(dfA) <- gsub("blood\\.pressure","bp",names(dfA))
names(dfA) <- gsub("baseline_comorb","baseline",names(dfA))

# Clean up intervention.
x <- ox <- unique(dfA$intervention)
x <- gsub("INTERVENTION_","",x)
x <- tools::toTitleCase(tolower(gsub("\\.|\\_"," ",x)))
x <- gsub("Ref ","\\.",x)
names(x) <- ox
dfA$intervention <- x[dfA$intervention]
rm(x,ox)

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

# Add follow-up < 1 yr.
dfA$years_followup_gt1 <- with(dfA, ifelse(years_followup >= 1,1,0))

# Function for moving items in vector to after a named one.
.fncMov <- function(x, itm, aft=NA) {
   sdx <- which(x %in% itm)
   dst <- which(x==aft)
   x[c(setdiff(1:dst,sdx)
       ,sdx
       ,setdiff((dst+1):length(x),sdx))]
}

vls <- list(id =c("src","patient_id","index_date")
            ,tgt = names(dfA)[grep("outcome",names(dfA))]
            ,trt = "intervention"
            ,rem = c(names(dfA)[grep("outstart|log_|start_int|end_int|_date",names(dfA))]
                    ,"record_start","record_end","dob","zip","censor_year"
                    ,"index_era","race_original","gender.female","age_std"
                    ,"days_record","icd9","icd10","cpt","rx","death"
                    ,"index_start","years_record")
)
vls$pot <- unique(c("age","age_group",setdiff(names(dfA), unlist(vls))))

for(n in rev(sort(vls$pot[grep("charl|cci_",vls$pot)]))) {
   vls$pot <- .fncMov(vls$pot, itm=n, aft="index_year")
}

if(tolower(meta$strat)=="src") {
   vls$t1 <- c(vls$tgt,"intervention",vls$pot)
} else if(tolower(meta$strat)=="intervention") {
   vls$t1 <- c(vls$tgt,"src",vls$pot)
}

vls$t1 <- .fncMov(vls$t1, itm="years_followup_gt1", aft=tail(vls$t1[grep("outcome",vls$t1)],1))

data.table::data.table(dfA[,vls$t1])

if(any(tolower(meta$src)=="all")) {
   tmp <- dfA
} else {
   tmp <- subset(dfA, tolower(src) %in% tolower(meta$src))
}

fct <- vls$t1[which(sapply(tmp[,vls$t1], function(x) class(x)[1] %in% c("character","factor") | all(x %in% c(0,1,NA))))]
fct <- c(fct,"index_year")

tb1 <- tableone::CreateTableOne(vars=vls$t1, factorVars=fct, strata=meta$strat, data=tmp
                               ,includeNA=TRUE, test=FALSE)
rm(fct,tmp)

print(tb1, missing=FALSE, smd=TRUE)

