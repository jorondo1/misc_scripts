############################
### Parse Sourmash output ###
##############################

# handle cases of older sourmash versions where the variable was "name" 
# instead of "match_name" (introduced through the branchwater approach)
genome_col_name_version <- function(df) {
  if ("name" %in% names(df)) {
    return(str_replace_all(df$name, c(" .*" = "", ".fa.sig" = ""))) # remove _1 from MAG identifiers
  } else {
    return(str_replace_all(df$match_name, c(" .*" = "", ".fa.sig" = "")))
  }
}
parse_SM <- function(gather_files) {
  gather_data <- Sys.glob(gather_files) %>% 
    map_dfr(read_csv, col_types='ddddddddcccddddcccddcddcddddddd') %>%
    mutate(
      uniqueK = (unique_intersect_bp/scaled)*average_abund,
      genome = genome_col_name_version(pick(everything())), 
      run = str_replace(query_name, "_clean", ""), 
      .keep = "unused"
    )
    
    # Process output and return
    gather_data %>% 
      dplyr::select(run, uniqueK, genome) %>% 
      tidyr::pivot_wider(names_from = run,
                  values_from = uniqueK) %>% 
      replace(is.na(.), 0) %>% 
      arrange(genome) %>% 
      dplyr::mutate(across(where(is.numeric), \(x) round(x, digits=0)))
}

# To parse the gtdb taxonomy 
parse_GTDB_lineages <- function(file, colnames) {
  read_delim(file, show_col_types = FALSE,
             col_names = c('genome','rep','Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species')) %>% 
#    mutate(genome = str_remove(genome, "^[^_]*_")) %>% 
    mutate_all(~str_replace(., "^[A-Za-z]_+", ""))
}

parse_genbank_lineages <- function(file) {
  read_delim(file, show_col_types = FALSE, 
             col_names = c('ident','taxid','Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species','Strain')) %>% 
    dplyr::select(-taxid) %>% 
    mutate(genome = ident, .keep = 'unused')
  }

species_glom <- function(abundTable) {
  abundTable %<>% 
    group_by(Kingdom, Phylum, Class, Order, Family, Genus, Species) %>% #keep those
    dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE),  # Sum the sample abundance columns
              .groups = "drop")     
}

##############################
### Parse MetaPhlAn output ####
################################
default_colnames <- c('Taxonomy', 'Abundance')
read_filename <- function(filepath, column_names = default_colnames,
                          convert_to_counts = FALSE) {
  raw <- readLines(filepath)
  
  # MetaPhlan outputs relative abundances, we scale it using the total #reads
  # Optional, because other tools (Kraken, mOTUs) use the MPA-stye tables but
  # already output read counts.
  if (convert_to_counts) {
    processed <- grep("reads processed", raw, value = TRUE)
    scale_reads <- as.numeric(gsub("[^0-9]", "", processed))/100
  } else {
    scale_reads <- 1
    }
    #headers <- grep("#clade_name", raw, value = TRUE)
  raw %>% 
    grep("^[^#]", ., value = TRUE) %>% # discard header lines
    textConnection %>% # create connection to chr vector, enables file-reading fun for in-memory strings
    read.table(sep = '\t', header = FALSE, col.names=column_names, 
               quote = "", colClasses = 'character' # otherwise EOF error
               ) %>%
    mutate(sample = basename(filepath) %>% str_replace('_.*', ""),
           Abundance = round(as.numeric(Abundance)*scale_reads),0)
}

parse_MPA <- function(MPA_files, # path with wildcard to point to all files
                      column_names = default_colnames,
                      convert_to_counts = FALSE){ 
  Sys.glob(MPA_files) %>% 
    map(read_filename, column_names, convert_to_counts) %>% #compact %>% 
    list_rbind %>% tibble %>%  # Keep only lines with species, remove duplicates at strain level
    dplyr::filter(str_detect(Taxonomy, "s__") & 
                    !str_detect(Taxonomy,"t__")) %>% 
    dplyr::select(sample, Taxonomy, Abundance) %>% 
    mutate(Abundance = as.double(Abundance)) %>% 
    group_by(Taxonomy, sample) %>% 
    dplyr::summarise(Abundance = sum(Abundance), .groups='drop') %>% # sum strains into species if applicable
    tidyr::pivot_wider(names_from = sample, values_from = Abundance, values_fill = 0) %>%
    mutate(Kingdom = str_extract(Taxonomy, "k__[^|]+") %>% str_remove("k__"),
           Phylum = str_extract(Taxonomy, "p__[^|]+") %>% str_remove("p__"),
           Class = str_extract(Taxonomy, "c__[^|]+") %>% str_remove("c__"),
           Order = str_extract(Taxonomy, "o__[^|]+") %>% str_remove("o__"),
           Family = str_extract(Taxonomy, "f__[^|]+") %>% str_remove("f__"),
           Genus = str_extract(Taxonomy, "g__[^|]+") %>% str_remove("g__"),
           Species = str_extract(Taxonomy, "s__[^|]+") %>% str_remove("s__")) %>%
    dplyr::select(-Taxonomy) 
  }

###################################
######## Distance-based analyses ###
#####################################

# Variance-stabilizing transformation
vst_ps_to_mx <- function(ps) {
  phyloseq_to_deseq2(
    ps, ~ 1) %>% # DESeq2 object
    estimateSizeFactors(., geoMeans = apply(
      counts(.), 1, function(x) exp(sum(log(x[x>0]))/length(x)))) %>% 
    DESeq2::varianceStabilizingTransformation(blind=T) %>% # VST
    SummarizedExperiment::assay(.) %>% t %>% 
    { .[. < 0] <- 0; . } # replace negatives by zeros
}

# PCOA
# Return a list of 3 elements :
# 1. the phyloseq sample_data with 2 first PCo added
# 2. the eigenvalues
# 3. the distance/dissimilarity matrix
compute_pcoa <- function(ps, dist) {
  vst <- ifelse(dist == 'bray', TRUE, FALSE) 
  dist.mx <- ps %>%
    { 
      counts <- if (vst) vst_ps_to_mx(.) else otu_table(.)
      if (taxa_are_rows(ps) & !vst) t(counts) else counts
      } %>% 
    vegan::vegdist(method = dist)
  
  PCoA <- capscale(dist.mx~1, distance = dist)
  eig <- round(PCoA$CA$eig[1:3]/sum(PCoA$CA$eig),2)
  message(paste("First 3 PCo :",eig[1], ',', eig[2], ',', eig[3]))
  # create output list
  out <- data.frame(sample_data(ps))
  out$PCo1 <- scores(PCoA)$sites[,1]
  out$PCo2 <- scores(PCoA)$sites[,2]
  
  list(metadata = out, eig = PCoA$CA$eig, dist.mx = dist.mx)
}

