# Generate visualization for Meta LLM benchmarks.

rm(list=ls()); graphics.off(); gc(); options(scipen=99, digits=3)

meta <- list(ddr = "~/Documents/GitHub/Clients/Meta/data/"
            ,seed = 46664
            ,R = 1000
            ,exclude = c("atropos","System Publication")
            ,include = c(NA,"Llama 4 Maverick","Llama 3.3 70B")[1]
            ,gen_pdf = c(TRUE,FALSE)[2]
            ,sim_meta = c(TRUE,FALSE)[2]
            ,top_llama_only = c(TRUE,FALSE)[2]
            )

# Function to get summary of benchmark using bootstrap.
.fncBnchDist <- function(data, llm="llm", score="correct", bench="x", R=meta$R, seed=meta$seed) {
   tmp <- data[,c(llm,score)]
   names(tmp) <- c("llm","score")
   rst <- NULL
   for(m in unique(tmp$llm)) {
      x <- rep(NA, R)
      set.seed(seed)
      for(r in 1:R) {
         x[r] <- mean(sample(subset(tmp, llm==m)$score, replace=TRUE), na.rm=TRUE)
      }
      rst <- rbind(rst, cbind(data.frame(Bench=bench, Model=m, n=length(na.omit(subset(tmp, llm==m)$score)))
                                        ,data.frame(t(quantile(x, probs=c(0.5,0.025,0.975))))))
      rm(x,r)
   }
   rm(m,tmp)
   names(rst) <- gsub("X50.","Center", gsub("X2.5.","L95", gsub("X97.5.","U95", names(rst))))
   return(rst)
}

dfL <- list()
dfS <- NULL

# MedHELM
fnm <- list.files(paste0(meta$ddr,"MedHELM"), pattern="stats.json", recursive=TRUE, full.names=TRUE)
fnm <- fnm[-grep("instance_stats",fnm)]

tmp <- NULL
for(f in fnm) {
   jsn <- as.data.frame(jsonlite::fromJSON(f))
   val <- max(jsn[intersect(grep("exact_match|accuracy",jsn$name$name),which(jsn$name$perturbation=="robustness")),"mean"], na.rm=TRUE)
   x <- unlist(strsplit(f,"/"))
   b <- sub("\\:.*","",x[length(x)-1])
   ci <- rep(NA,2)
   limits_not_ci <- 0
   if(b %in% c("medhallu","pubmed_qa")) {
      ci <- as.numeric(binom.test(x=round(val*1000), n=1000)$conf.int)
   } else {
      b <- paste(b,"(rescale, max=5)")
      val <- val / 5
      ci <- val + c(-0.025, 0.025)
      limits_not_ci <- 1
   }
   tmp <- rbind(tmp, data.frame(Bench=paste("MedHELM:",b)
                               ,Model=sub(".*\\_","",sub(".*\\:","",x[length(x)-1]))
                               ,n=1000
                               ,Center=val
                               ,L95=ci[1], U95=ci[2]
                               ,limits_not_ci=limits_not_ci
                               ))
   rm(jsn,val,x,b,ci)
}
dfL$med_helm <- tmp
rm(f,fnm,tmp)

dfS <- dplyr::bind_rows(dfS, as.data.frame(dfL$med_helm)[,c("Bench","Model","n","Center","L95","U95","limits_not_ci")])

# HealthBench
.fncBootCI <- function(x, R=10^3, seed=meta$seed) {
   set.seed(seed)
   x <- na.omit(x)
   rst <- rep(NA, R)
   for(r in 1:length(rst)) rst[r] <- mean(sample(x, replace=TRUE))
   return(list(mean=mean(x), lcl=as.numeric(quantile(rst, probs=0.025)), ucl=as.numeric(quantile(rst, probs=0.975)), n=length(x), R=R, seed=seed))
}

tmp <- data.table::fread(paste0(meta$ddr,"healthbench_granular_data.tsv.gz"))
tmp <- merge(tmp, tmp[, .(run_id = min(run_id)), by=model])
tmp$rubric_criterion_index <- NULL

tmH <- NULL
for(n in c(".","axis","theme")) {
   tmp$grp <- ".Overall"
   if(n != ".") tmp$grp <- paste(n,tmp[[n]],sep=":")
   tmT <- tmp[, .(score=sum(points * criterion_met) / sum(ifelse(points > 0L, points, 0L))), by=c("model","run_id","grp","conversation_id")]
   tmT$score[tmT$score %in% c(-Inf,NaN)] <- NA
   tmS <- tmT[, as.list(.fncBootCI(x=score)), by=c("model", "run_id", "grp")]
   tmH <- rbind(tmH, tmS)
   rm(tmT,tmS)
}
rm(n,tmp)
names(tmH) <- gsub("model","Model"
                          ,gsub("mean","Center"
                          ,gsub("lcl","L95"
                          ,gsub("ucl","U95"
                               ,names(tmH)))))
tmH$Bench <- with(tmH, paste("HealthBench:",tools::toTitleCase(gsub(":",": ", gsub("_"," ",grp)))))
tmH$Bench <- gsub(".overall",".Overall",tmH$Bench)
dfL$health_bench <- tmH
rm(tmH)
dfS <- dplyr::bind_rows(dfS, as.data.frame(dfL$health_bench)[,c("Bench","Model","n","Center","L95","U95")])

# HealthBench from summary data.
#dfL$health_bench <- NULL
#for(n in c("theme","axis")) {
#   tmp <- data.table::fread(paste0(meta$ddr,"healthbench_",n,"_data.csv"))
#   tmp$V1 <- NULL
#   if(n=="theme") {
#      tmp$theme[tmp$theme==""] <- ".Overall"
#   } else {
#      tmp <- subset(tmp, theme!="")
#   }
#   tmp$Bench <- with(tmp, paste0("HealthBench: ", ifelse(theme==".Overall","00", ifelse(n=="theme","01.","02.")), theme))
#   tmp$theme <- NULL
#   names(tmp) <- c("Model","Center","L95","U95","Bench")
#   tmp$n <- NA
#   dfL$health_bench <- rbind(dfL$health_bench, tmp)
#   rm(tmp)
#}
#rm(n)
#
#dfS <- dplyr::bind_rows(dfS, as.data.frame(dfL$health_bench)[,c("Bench","Model","n","Center","L95","U95")])

# Answered with Evidence.
dfL$awe <- data.table::fread(paste0(meta$ddr,"answered_with_evidence_summary.tsv"))
tmp <- as.data.frame(subset(dfL$awe, badge %in% c("Green","Red")))
names(tmp)[which(names(tmp)=="provider")] <- "Model"
names(tmp)[which(names(tmp)=="pct_of_provider")] <- "Center"
tmp$Center <- tmp$Center / 100
tmp$Center[tmp$badge=="Red"] <- 1-tmp$Center[tmp$badge=="Red"]
tmp$badge <- ifelse(tmp$badge=="Green","01.Green","02.Red (reverse scored)")
tmp$Bench <- paste("Answered with Evidence:",tmp$badge)
tmp[,c("L95","U95")] <- NA
for(i in 1:nrow(tmp)) {
   tst <- with(tmp[i,], binom.test(x=round(Center*total_count), n=total_count))
   tmp[i,c("L95","U95")] <- as.numeric(tst$conf.int)
   rm(tst)
}
rm(i)
tmp$n <- aggregate(total_count ~ provider, data=dfL$awe, FUN=sum)$total_count[1]
dfS <- dplyr::bind_rows(dfS, tmp[,c("Bench","Model","n","Center","L95","U95")])
rm(tmp)

# CPC.
dfL$cpc <- data.table::fread(paste0(meta$ddr,"cpc_diagnosis_judgements-gemini25flashlite.csv"))
dfL$cpc$candidate_id <- gsub("meta-llama/","",dfL$cpc$candidate_id)
dfL$cpc$correct <- with(dfL$cpc, ifelse(is.na(correct_diagnosis_index), 0, 1))
tmp <- data.table::data.table(aggregate(correct ~ candidate_id + case_id, data=dfL$cpc, FUN=mean))
names(tmp) <- gsub("candidate_id","llm",names(tmp))
dfL$cpc <- tmp
rm(tmp)

dfS <- dplyr::bind_rows(dfS, .fncBnchDist(data=as.data.frame(dfL$cpc), bench="CPC"))

# Rename models for consistency.
dfS$Model_Orig <- dfS$Model
dfS$Model[grep("llama3_1|llama-3.1-8b",dfS$Model)] <- "Llama 3.1 8b"
dfS$Model[grep("llama-3.3-70b|llama3_3",dfS$Model)] <- "Llama 3.3 70B"
dfS$Model[grep("llama_mav4|llama-4-maverick",dfS$Model)] <- "Llama 4 Maverick"
dfS$Model[grep("llama_scout4|llama-4-scout",dfS$Model)] <- "Llama 4 Scout"
dfS$Model[grep("o3",dfS$Model)] <- "o3"
dfS$Model[grep("gpt5",dfS$Model)] <- "gpt5"
data.frame(table(dfS$Model))
tmp <- unique(dfS[,c("Model_Orig","Model")])
tmp <- tmp[order(tmp$Model,tmp$Model_Orig),]
rownames(tmp) <- NULL
meta$model_map <- tmp
rm(tmp)
print(meta$model_map)

# Label best and worse within benchmarks.
dfS <- dfS[order(dfS$Bench, -dfS$Center),]
dfS$BestCenter <- NA
dfS[,c("Best","Worse","BestMeta","Meta")] <- 0
dfS$Meta[grep("llama",tolower(dfS$Model))] <- 1
for(n in unique(dfS$Bench)) {
   tmp <- dfS[dfS$Bench==n,][1,c("Center","L95")]
   dfS$BestCenter[dfS$Bench==n] <- tmp$Center
   dfS$Best[dfS$Bench==n & dfS$Center==tmp$Center] <- 1
   dfS$Worse[dfS$Bench==n & dfS$Center < tmp$L95] <- 1
   rm(tmp)
   dfS$BestMeta[dfS$Center & dfS$Meta==1 & dfS$Center==dfS[dfS$Bench==n & dfS$Meta==1,c("Center")][1]] <- 1
}
rm(n)

dfS$limits_not_ci[is.na(dfS$limits_not_ci)] <- 0

#print(dfS, row.names=FALSE)

# Plotting
tmp <- dfS[order(dfS$Bench, -dfS$Center),]
tmp <- subset(tmp, !(tolower(Model) %in% tolower(meta$exclude)))

all_models <- sort(unique(tmp$Model))

tmp$MetaFab <- NA
set.seed(meta$seed)
if(meta$sim_meta) {
   x <- tmp$Center[tmp$Model=="Llama 4 Maverick"]
   y <- with(tmp, tapply(Center, Bench, max))
   tmp$MetaFab[tmp$Model=="Llama 4 Maverick"] <- runif(length(x), min=x, max=y)
}

if(any(!is.na(meta$include))) tmp <- subset(tmp, tolower(Model) %in% tolower(meta$include))
one_model <- ifelse(length(unique(tmp$Model))==1,TRUE,FALSE)

if(meta$top_llama_only & !one_model) {
   tm2 <- aggregate(Center ~ Bench, data=subset(tmp, Model=="o3" | BestMeta==1), FUN=min)
   names(tm2)[2] <- "CenterMin"
   tmp <- merge(tmp, tm2, all.x=TRUE)
   tmp$CenterMin <- with(tmp, ifelse(Center>=CenterMin,1,0))
   tmp$Dum <- 1
   tm2 <- aggregate(cbind(CenterMin,Dum) ~ Bench, data=tmp, FUN=sum)
   tm2$Showing <- with(tm2, paste("(Showing",CenterMin,"of",Dum,"models)"))
   tm2$Showing <- with(tm2, ifelse(CenterMin==Dum, paste("(Showing all",Dum,"models)"), Showing))
   tmp <- merge(tmp, tm2[,c("Bench","Showing")], all.x=TRUE)
   tmp <- subset(tmp, CenterMin==1)
   rm(tm2)
   tmp$CenterMin <- NULL
} else {
   tmp$Showing <- NA
}

tm2 <- NULL
if(one_model) {
   tmp <- tmp[order(tmp$Center),]
   tmp$y <- 1:nrow(tmp)
   tmp <- dplyr::bind_rows(tmp, data.frame(Bench=tmp$Model[1], y=nrow(tmp)+1))
   tmp$BestMeta <- 0; tmp$Meta <- 0
} else {
   y <- 1
   for(i in 1:nrow(tmp)) {
      tmp$y[i] <- y
      y <- y+1
      if(i > 1 & i < nrow(tmp)) {
         if(tmp$Bench[i] != tmp$Bench[i+1]) {
            tm2 <- dplyr::bind_rows(tm2, data.frame(Bench=tmp$Bench[i+1], y=y, Showing=tmp$Showing[i+1]))
            y <- y+1
         }
      } else if(i==1) {
         tm2 <- dplyr::bind_rows(tm2, data.frame(Bench=tmp$Bench[i], y=0, Showing=tmp$Showing[i]))
      }
   }
   tmp <- dplyr::bind_rows(tmp,tm2)
   rm(y,i,tm2)
   tmp <- tmp[order(-tmp$y),]
   tmp$y <- rev(tmp$y)
}
head(tmp,20)

graphics.off()
if(meta$gen_pdf) {
   fnm <- paste0(meta$ddr,"BenchmarkBaseline",gsub(" ","_",ifelse(one_model,paste0("_",meta$include),ifelse(meta$top_llama_only,"_TopOnly",""))),".pdf")
   pdf(file=fnm, h=ifelse(one_model,4,12), w=6)
}

par(mar=c(2,ifelse(one_model,16,8),1,0.5), cex=0.8)
plot(x=c(0,1), y=range(tmp$y)+c(-1,1), type="n", ann=FALSE, axes=FALSE, yaxs="i")
if(!one_model) with(subset(tmp, BestMeta==1), rect(xleft=-10, xright=10, ybot=y-0.5, ytop=y+0.5, border=NA, col="khaki1"))
abline(v=axTicks(1), col="lightgray", lty=1)
with(subset(tmp, is.na(Center)), rect(xleft=-10, xright=10, ybot=y-0.5, ytop=y+0.5, border=NA, col="lightgray"))
with(subset(tmp, is.na(Center)), text(x=par("usr")[1], y=y, labels=Bench, pos=4, cex=0.8))
with(subset(tmp, is.na(Center)), text(x=par("usr")[2], y=y, labels=Showing, pos=2, cex=0.6, col=gray(0.5)))
for(i in 1:nrow(tmp)) {
   if(one_model) with(tmp[i,], lines(x=c(Center,BestCenter), y=c(y,y), lty=3, col="lightblue"))
   with(tmp[i,], lines(x=c(L95,U95), y=c(y,y), col=ifelse(limits_not_ci,"gray","black")))
   with(tmp[i,], lines(x=c(Center,MetaFab), y=c(y,y)))
}
rm(i)
if(one_model) with(tmp, points(x=BestCenter, y=y, pch=24, bg="blue", col="blue", cex=0.5))
with(tmp, points(x=Center, y=y, pch=20))
with(tmp, points(x=MetaFab, y=y, pch=23, bg="gold", cex=1.5))
with(subset(tmp, Best==1), points(x=Center, y=y, pch=24, bg="blue"))
with(subset(tmp, Worse==1), points(x=Center, y=y, pch=25, bg="red"))
if(one_model) {
   with(tmp, mtext(ifelse(!is.na(Center),Bench,NA), side=2, line=0.5, at=y, las=1, cex=0.6, font=ifelse(Best==1,2,ifelse(BestMeta==1,3,1))))
} else {
   with(tmp, mtext(Model, side=2, line=0.5, at=y, las=1, cex=0.6, font=ifelse(Best==1,2,ifelse(BestMeta==1,3,1))))
}

if(one_model) {
   legend("bottomright", box.col="white", bg="white", cex=0.7
         ,title="Compared to\nOthers In\nBenchmark"
         ,legend=c("Best","Worse","No Diff","Best at Bench")
         ,pch=c(24,25,20,24)
         ,pt.bg=c("blue","red","black","blue")
         ,col=c("black","black","black","blue")
         ,pt.cex=c(1,1,1,0.5)
         ,lty=c(1,1,1,3))
}
axis(1, cex.axis=0.8); box()

if(meta$gen_pdf) {
   graphics.off()
   vwr <- getOption("viewer")
   vwr(fnm)
   rm(vwr,fnm)
}

all_models
rm(all_models,tmp,one_model)