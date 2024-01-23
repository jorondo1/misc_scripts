############################################################################
# This is a workaround if the DADA2 script busts your R memory when using ##
# addSpecies() or assignSpecies(). The function loops over subsets of data #
# using the output from assignTaxonomy as an input. There is also below a ##
# code for an alternative taxonomy assignment approach, referenced below. ##
############################################################################
### Author : Jonathan Rondeau-Leclaire #####################################
############################################################################

library(dada2)
# Assuming you have reached the assignTaxonomy step in the DADA2 workflow.
# Loading an output from bimera removal (see https://benjjneb.github.io/dada2/tutorial.html)
seqtab.nochim <- readRDS("~/Downloads/seqtab.nochim.rds")

# Function to addSpecies in chunks
addSpecies_custom <- function(tax.mx, chunkSize) {
  results <- list() # Initialize an empty list
  nRows <- nrow(tax.mx)
  for (i in seq(1, nRows, by = chunkSize)) {
    results[[length(results)+1]] <- 
      addSpecies(
        taxtab = tax.mx[i:min(i+chunkSize-1, nRows),], # subset 
        refFasta = "~/Downloads/silva_species_assignment_v138.1.fa") 
  }; do.call(rbind, results) # collapse in a single df
}

#############################################
### USED WITH THE DEFAULT DADA2 APPROACH #####
#############################################

taxa <- assignTaxonomy(readRDS("~/Downloads/seqtab.nochim.rds"),
                       "~/Downloads/silva_nr99_v138.1_train_set.fa.gz", 
                       multithread=TRUE)

# Collapse the list in a table:
taxa_Species <- addSpecies_custom(tax.mx = taxa, 1000)
saveRDS(taxa_Species, "~/Downloads/taxa_Species.rds")

########################
### IDTAXA APPROACH #####
########################
library(DECIPHER)

# Approach : https://microbiomejournal.biomedcentral.com/articles/10.1186/s40168-018-0521-5

# NOTE : To use parallel processing, openMP needs to be enabled on mac. If it's not,
# the IdTaxa() function will work but run on a single thread, which will take a while.
# On M1/M2 chips: https://github.com/Rdatatable/data.table/issues/5419#issuecomment-1906490594
# On intel: not tested yet.

# Following code mostly copied from DADA2 tutorial.
# Load the training set, available here http://www2.decipher.codes/Downloads.html
load("~/Downloads/SILVA_SSU_r138_2019.RData") 
ids <- IdTaxa(DNAStringSet(getSequences(seqtab.nochim)), # Create a DNAStringSet from the ASVs
              trainingSet, strand="top", processors=NULL) # use all processors
ranks <- c("domain", "phylum", "class", "order", "family", "genus", "species") # ranks of interest

# Convert the output object of class "Taxa" to a matrix analogous to the output from assignTaxonomy
taxDecipher <- t(sapply(ids, function(x) {
  taxa <- x$taxon[match(ranks, x$rank)]
  taxa[startsWith(taxa, "unclassified_")] <- NA
  taxa
}))
colnames(taxDecipher) <- ranks; rownames(taxDecipher) <- getSequences(seqtab.nochim)

# Apply our looping function
taxaDecipher_Species <- addSpecies_custom(taxDecipher, 1000)
