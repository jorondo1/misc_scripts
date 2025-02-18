# PHYLOSEQ FUNCTIONS

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
  
  asvs <- subset(taxonomy, Kingdom != "Unclassified") %>% # subset needs the input to be a df
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
  
  return(result)
}

# Export ASVs as fasta
asv_to_fasta <- function(seqtab, path.out) {
  require(Biostrings)
  seqs <- colnames(seqtab)
  fasta <- DNAStringSet(seqs)
  names(fasta) <- paste0("ASV_", seq_along(seqs))
  writeXStringSet(fasta, path.out)
}