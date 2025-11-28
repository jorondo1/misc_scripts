# PHYLOSEQ FUNCTIONS

### Functions 
source(url("https://raw.githubusercontent.com/jorondo1/misc_scripts/refs/heads/main/psflashmelt.R"))
source(url("https://raw.githubusercontent.com/jorondo1/misc_scripts/refs/heads/main/rarefy_even_depth2.R"))
source(url("https://raw.githubusercontent.com/jorondo1/misc_scripts/refs/heads/main/rarecurve_parallel.R"))

###############
# Functions ####
#################

# Keep samples with metadata
subset_samples <- function(seqtab, samples) {
  seqtab[rownames(seqtab) %in% samples, ] %>% # subset
    .[, colSums(.) > 0] # Remove ASVs with no hits
}

### %in% : est-ce que l'on trouve ce qu'il y a dans vecteur 1 dans vecteur 2 ?

# ASVs classified at the kingdom level and present in seqtab
subset_asvs <- function(taxonomy, seqtab, min_seq) {
  if (!is.data.frame(taxonomy)) {
    taxonomy <- as.data.frame(taxonomy)
  }
  
  asvs <- subset(taxonomy, Phylum != "Unclassified") %>% # subset needs the input to be a df
    rownames %>% 
    intersect(
      colnames(seqtab)[colSums(seqtab) >= min_seq] 
    ) # only keep asvs still present in seqtab
  taxonomy[asvs, ] %>% as.matrix()
}

# Remove samples with fewer than n sequences once taxa removed
remove_ultra_rare <- function(seqtab, taxonomy, n) { 
  result <- seqtab[, rownames(taxonomy), drop = FALSE]  # Ensure it stays a data frame
  result <- result[rowSums(result) > n, , drop = FALSE]  # Filter rows (samples). n = sum across ASVs in a sample
  result <- result[,colSums(result) > 1, drop = FALSE] # Remove singleton ASVs
  return(result)
}

# Visualise sequence count distribution across samples 
viz_seqdepth <- function(seqtab) {
  hist(rowSums(seqtab), breaks = 100, 
       xlab = "sample size", xaxt = "n", main = 'Distribution of sequence count per sample')
  axis(1, at = pretty(rowSums(seqtab), n = 40))  # adding ticks 
}

# Export ASVs as fasta
asv_to_fasta <- function(seqtab, path.out) {
  require(Biostrings)
  seqs <- colnames(seqtab)
  fasta <- DNAStringSet(seqs)
  names(fasta) <- paste0("ASV_", seq_along(seqs))
  writeXStringSet(fasta, path.out)
}

### DECONTAM

### Decontamination DECONTAM
# https://benjjneb.github.io/decontam/vignettes/decontam_intro.html

# 1. Find contaminants using the frequency method (based on DNA concentration)
decontaminate <- function(seqtab, samdata, var, p = 0.1) {
  require(decontam)
  
  idx <- which(samdata[,var] >0)
  conc <- samdata[idx, var]
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

# EXAMPLE USAGE
# # First, filter the taxonomy table:
# taxa_filtered <- subset_asvs(
#   taxonomy = tax_table, 
#   sextab = seqtab.nochim, 
#   min_seq = 50
# )  # minimum amount of sequences to keep an asv; this creates a new taxonomy table!
# 
# # visualise the histogram of sequencing depth
# viz_seqdepth(taxa_filtered) # use this to visually decide which low-depth samples to discard
# 
# # Filter the sequence table by (1) removing the taxa we filtered out using subset_asvs(), then (2) remove samples lower than what you decided with the histogram:
# seqtab_filtered <- remove_ultra_rare(
#   seqtab = seqtab.nochim, 
#   taxonomy = taxa_filtered, 
#   n = 9000 # Choose a number here by checking the histogram above!
# )
# 
# # Check what you've lost:
# dim(seqtab.nochim) # initial table
# dim(seqtab_filtered) # filtered table
# sum(seqtab_filtered)/sum(seqtab.nochim) # percentage of sequences left
# 
# # Create a phyloseq object:
# ps_16S <- phyloseq(
#   tax_table(tax_table),
#   otu_table(seqtab_filtered, taxa_are_rows = FALSE),
#   sample_data(your_metadata_file) # make sure sample names are consistent and in the row names 
# ) 