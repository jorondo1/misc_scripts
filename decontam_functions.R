
### Decontamination DECONTAM
# https://benjjneb.github.io/decontam/vignettes/decontam_intro.html

# 1. Find contaminants using the frequency method (based on DNA concentration)
decontaminate <- function(seqtab, samdata, var, p = 0.1) {
  require(decontam)
  
  idx <- which(samdata[,var] >0)
  conc <- as.matrix(samdata[idx, var])
  asv <- seqtab[idx,]
  
  out <- list()
  out$decontam <- isContaminant(seqtab = asv, method = 'frequency', conc = conc, threshold = p)
  how_many <- table(out$decontam$contaminant) # How many ASVs are contaminants ?
  asv_rank <- which(out$decontam$contaminant) # What is the abundance ranks of contaminants
  
  # Short report on decontam's findings; produces a plot
  out$p <- plot_frequency(asv, colnames(asv)[head(which(out$decontam$contaminant), n = 20)], conc=conc) +
    xlab("DNA Concentration")
  
  message(paste(how_many[2], 'ASVs identified as contaminants, out of', sum(how_many)))
  message(paste('Top abundance ranks of contaminants: ', paste(head(asv_rank),collapse = ', ')))
  # bigger numbers = lowest abundance ranks
  
  return(out)
}

# 2. Remove contaminants from ps object
prune_contam <- function(ps, decontam_table) {
  require(dplyr, phyloseq)
  
  # Save non-contaminant ASVs
  not_cont <- which(!decontam_table$contaminant) %>%
    decontam_table[.,] %>% rownames
  
  #Remove them
  ps_clean <- prune_taxa(not_cont, ps)
  
  # Proportion of counts lost
  message(paste(round(100*(1-sum(ps_clean@otu_table)/sum(ps@otu_table)),2),
                '% of reads were lost to decontamination.'))
  
  return(ps_clean)
}