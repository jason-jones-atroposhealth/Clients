# Workbench Sources:
# Using first regimen start: https://workbench.atroposhealth.biz/case/c1cc2646-0be9-47ef-b381-aa32a5cc0f1e/
# Using last regimen start: https://workbench.atroposhealth.biz/case/4443d682-ae55-438e-b1e0-519455348804/

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


rxcui <- list()
rxcui$long_acting_injectables <- c("859870","859866","1719863","1719862","1719804","1719803","859872","1719848","1719847","859871","859868","1720027","1720026","859867","859873","859869","576130","859823","859824","859825","1359624","1360500","859826","859831","859833","861851","563755","977390","360376","25190","858046","1359483","858049","1360013","858052","858053","858054","858055","858056","858057","2591399","2591397","2591402","2591400","1650975","1650976","1650969","1650965","1650970","1650967","1650973","1650974","858048","858051","858073","858074","1650971","1650972","2591393","2591391","1650966","2591396","2591394","1650968","2570420","2570421","2570419","2570418","864546","858045")
rxcui$ariprazole_oral         <- c("1158261","378301","402131","602964","349545","368563","349490","404602","615172","352307","349553","352308","349547","352309","352310","485496","485495","1158260","643018","1295553","643019","643022","643020","1998456","1998457","1998462","643021","1998463","1998451","544412","544411","1998458","1998460","643023","1998453","1998454","1998459","1998461","1998455","1158262","402368","602963","350663","350654","576524","615171","576003","350670","576004","350664","576005","576006")
rxcui$ariprazole_longacting   <- c("1602169","1371189","1602605","1371192","1602171","1602607","1602163","1602604","672356","672354","1659786","672540","672538","1659787","2636638","2636636","2636641","2636639","1371194","2636645","2636646","2636637","2636640","1371191","1158259","1659782","1659784","1659812","1659814","1659816","1659818","1602162","1602603","1659811","1659813","1659815","1659817")
rxcui$ariprazole_lauroxil     <- c("1673272","1673276","1673271","1673280","1925266","1925264","1925263","1673274","1673278","2049345","2049343","2049342","1673265","1673268","1673267","1673275","1673266","1673279","1925265","1925262","1925261","1673269","1673277","2049344","2049341","2049340")

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


