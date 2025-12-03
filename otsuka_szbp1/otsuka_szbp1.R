# Workbench Sources:
# Using first regimen start: https://workbench.atroposhealth.biz/case/618529f9-621a-4c26-9457-e4d6603d4e98/
# Using last regimen start: https://workbench.atroposhealth.biz/case/48927b9e-8f21-4efb-a0e0-8d19284b75d9/
# Using last regimen within 5 years of start: https://workbench.atroposhealth.biz/case/32b0d753-d5cd-4c06-a852-eb15c74525ee/
# Using last regimen within 3 years of start: https://workbench.atroposhealth.biz/case/0422788f-2f5b-428b-bbde-1ca612adfe86/
# Using last regimen within 2 years of start: https://workbench.atroposhealth.biz/case/0a32fa1d-4a44-45c8-9501-ded4c5cc3e17/

rm(list=ls()); graphics.off(); gc(); options(scipen=99, digits=3)

meta <- list(ddr   = "~/Downloads/OtsukaSzBP1/"
            ,years = 2015:2025
            ,strat = c("intervention","src")[1]
            ,src   = c("all","Sz A Last2y")[2]
            ,lmt_fu = c(NA,"12mo")[2]
            ,tgt_prim = c(NA,"ip.visit","er.visit","suicide","regimen","side")[-1]
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

tmp$index_year <- paste0("y",tmp$index_year)

fct <- vls$t1[which(sapply(tmp[,vls$t1], function(x) class(x)[1] %in% c("character","factor") | all(x %in% c(0,1,NA))))]
fct <- unique(c(fct,"index_year"))

tb1 <- tableone::CreateTableOne(vars=vls$t1, factorVars=fct, strata=meta$strat, data=tmp
                               ,includeNA=TRUE, test=FALSE)
rm(fct,tmp)

tb1 <- print(tb1, missing=TRUE, smd=TRUE)
clip <- pipe("pbcopy", "w")                       
write.table(tb1, file=clip, quote=FALSE, sep="\t", na="")                               
close(clip)
rm(clip)

3 * "a"

tmp <- readLines(paste0(meta$ddr,"query.tql"))
X <- tmp[intersect(grep("^var",tmp),grep("rx\\(|rx\\=",gsub(" ","",tolower(tmp))))]
X <- gsub("var ","",X)
X <- gsub('"',"'",X)

rxcui <- list()
for(x in X) {
   rxcui[[trimws(unlist(strsplit(x,"="))[1])]] <- as.character(na.omit(unique(sapply(regmatches(unlist(strsplit(x,",")), regexec("'\\s*(.*?)\\s*'", unlist(strsplit(x,",")))), function(y) y[2]))))
}
rm(x,X,tmp)
rxcui

dfD <- NULL
for(n in names(rxcui)) {
   for(i in 1:length(rxcui[[n]])) {
      dfD <- rbind(dfD, data.frame(tql_var=n, rxcui=rxcui[[n]][i], brand_name=rxnorm::get_bn(rxcui[[n]][i]), rx_name=rxnorm::get_rx(rxcui[[n]][i])  ))
   }
   rm(i)
}
rm(n)
data.table::data.table(dfD)
print(dfD, row.names=FALSE)

clip <- pipe("pbcopy", "w")                       
write.table(dfD, file=clip, quote=FALSE, sep="\t", na="", row.names=FALSE)                               
close(clip)
rm(clip)

