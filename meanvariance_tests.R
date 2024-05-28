library(pacman)
p_load(compositions, dplyr, phyloseq, DESeq2, mvabund, microViz)
moss.ps <- readRDS("~/Repos/borealMoss/data/R_out/mossMAGs.RDS") %>% 
  tax_filter(min_prevalence = 0.1)
source("myFunctions.R")

counts.raw <- moss.ps@otu_table %>% t
counts.clr <- counts.raw %>% compositions::clr(.)
counts.vst <- moss.ps %>% phyloseq_to_deseq2(~1) %>% 
  estimateSizeFactors(., geoMeans = apply(
    counts(.), 1, function(x) exp(sum(log(x[x>0]))/length(x)))) %>% 
  DESeq2::varianceStabilizingTransformation(blind=T) %>% # VST
  SummarizedExperiment::assay(.) %>% t %>% 
  { .[. < 0] <- 0; . }

test.fun <- function(counts) {
  data.frame(mean = counts %>% apply(2, mean),
             variance = counts %>% apply(2, var)) %>% 
  plot(variance ~ mean, data = .)
}
test.fun(counts.raw)
test.fun(counts.vst)
test.fun(counts.clr)


### mvabund
bact.mv <- mvabund(moss.ps@otu_table %>% t) 
group <- moss.ps@sam_data$Compartment %>% as.factor
moss <- moss.ps@sam_data$Host %>% as.factor

plot.mvabund(bact.mv, y= group, n.vars = 20)
moss.nb <- manyglm(bact.mv ~ moss*group, family = "negative binomial")
plot.manyglm(moss.nb)
meanvar.plot(bact.mv)
moss.anova2 <- anova.manyglm(moss.nb, #p.uni = "adjusted"
                           nBoot = 99)
