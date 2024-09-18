############################
### Parse Sourmash output ###
##############################
parse_SM <- function(gather_files) {
  Sys.glob(gather_files) %>% 
    map_dfr(read_csv, col_types="ddddddddcccddddcccd") %>%
    mutate(
      uniqueK = (unique_intersect_bp/scaled)*average_abund,
      genome = str_replace_all(name, c(" .*" = "", ".fa.sig" = "")), # remove _1 from MAG identifiers
      run = str_replace(query_name, "_clean", ""), 
      .keep="unused") %>% 
    dplyr::select(run, uniqueK, genome) %>% 
    pivot_wider(names_from = run,
                values_from = uniqueK) %>% 
    replace(is.na(.), 0) %>% 
    arrange(genome) %>% 
    mutate(across(where(is.numeric), \(x) round(x, digits=0)))
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
    summarise(across(where(is.numeric), sum, na.rm = TRUE),  # Sum the sample abundance columns
              .groups = "drop")     
}


##############################
### Parse MetaPhlAn output ####
################################
default_colnames <- c('Taxonomy', 'Abundance')
read_filename <- function(filepath, column_names = default_colnames) {
  readLines(filepath) %>% 
    grep("^[^#]", ., value = TRUE) %>% # discard header lines
    textConnection %>% # create connection to chr vector, enables file-reading fun for in-memory strings
    read.table(sep = '\t', header = FALSE, col.names=column_names, 
               quote = "" # otherwise EOF error
               ) %>%
    mutate(sample = basename(filepath) %>% str_replace('_.*', ""))
}

parse_MPA <- function(MPA_files, # path with wildcard to point to all files
                      column_names = default_colnames){ 
  Sys.glob(MPA_files) %>% 
    map(read_filename, column_names) %>% #compact %>% 
    list_rbind %>% # Keep only lines with species, remove duplicates at strain level
    dplyr::filter(str_detect(Taxonomy, "s__") & !str_detect(Taxonomy,"t__")) %>% 
    dplyr::select(sample, Taxonomy, Abundance) %>% 
    mutate(Abundance = as.double(Abundance)) %>% 
    group_by(Taxonomy, sample) %>% 
    summarise(Abundance = sum(Abundance), .groups='drop') %>% # sum strains into species if applicable
    pivot_wider(names_from = sample, values_from = Abundance, values_fill = 0) %>%
    mutate(Kingdom = str_extract(Taxonomy, "k__[^|]+") %>% str_remove("k__"),
           Phylum = str_extract(Taxonomy, "p__[^|]+") %>% str_remove("p__"),
           Class = str_extract(Taxonomy, "c__[^|]+") %>% str_remove("c__"),
           Order = str_extract(Taxonomy, "o__[^|]+") %>% str_remove("o__"),
           Family = str_extract(Taxonomy, "f__[^|]+") %>% str_remove("f__"),
           Genus = str_extract(Taxonomy, "g__[^|]+") %>% str_remove("g__"),
           Species = str_extract(Taxonomy, "s__[^|]+") %>% str_remove("s__")) %>%
    dplyr::select(-Taxonomy)
  }
