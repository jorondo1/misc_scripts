# From a phyloseq object, find the top N abundant species, aggregate
# the others into an "Others" category and produce a bar_plot showing
# the average relative abundance of samples per metadata.

library(pacman)
p_load(dplyr, magrittr, phyloseq, ggplot2)

# In this example we are using a moss microbiome dataset where the samples
# were taken on either of 4 host moss species. We'll do 3 things:
# 1. Summarise abundances of the top 10 families, creating and 'Others' group;
# 2. Plot these relative abundances on a bar chart, by Host
# 3. Extract the mean ±sd sample abundances of each family group by host

# We convert abundances to relative abundances, otherwise the aggregation
# of abundances will be biased by the different total of sequences per sample.

# Import the dataset :
moss.ps <- readRDS(url("https://github.com/jorondo1/borealMoss/raw/main/data/R_out/mossMAGs.RDS"))
topN <- 10 # <<< how many top taxa
taxRank <- 'Family' # <<< which taxonomic rank 

# Melt the phyloseq object, agglomerate taxonomy and standardize abundances
ps.melt <- moss.ps %>% # <<< !! your phyloseq object
  tax_glom(taxRank) %>% # aggregate at desired taxrank
  transform_sample_counts(function(x) x/sum(x)) %>%  # normalise counts to 1 by sample
  psmelt %>% # melt the object in a single (massive) table
  select(!!sym(taxRank), Abundance, Sample, # psmelt creates an Abundance variable
         Host, Compartment) # <<< !! sample data variables you want to use

# Create a taxon -> taxon group conversion table
taxonomie_agregee <- ps.melt %>% 
  group_by(!!sym(taxRank)) %>% # mean relative abundance by taxRank:
  summarise(sum_relab = mean(Abundance)) %>% 
  arrange(desc(sum_relab)) %>%          # most abundant first 
  mutate(aggTaxo = as.factor(case_when( # new aggregated taxonomy variable
    row_number() < topN ~ !!sym(taxRank),        # N-1 first keep their taxonomy
    row_number() >= topN ~ 'Others'))) %>% # 'Others' for every other taxa
  select(-sum_relab) # no need for this

# Reorder the vector so that 'Others' is first in plots
taxLvl <- taxonomie_agregee %$% aggTaxo %>%
  setdiff(., 'Others') %>% # remove Others
  c('Others',.) # put Others first 

# add the aggregated taxonomy variable to the raw data
ps.melt_agg <- left_join(ps.melt, taxonomie_agregee, 
                         by = taxRank) %>% 
  mutate(aggTaxo = factor(aggTaxo, levels = taxLvl)) %>%   # reorder factor levels
  group_by(aggTaxo, Host, Sample) %>% # sum all Others into one by sample
  summarise(Abundance = sum(Abundance)) 

# simple stacked barplot :
ps.melt_agg %>% 
  ggplot(aes(x = Host, y = Abundance, fill = aggTaxo)) +
  geom_bar(stat = 'identity', 
           position = 'fill') # 
  
# mean abundance by taxa, by some metadata:
ps.melt_agg %>%
  group_by(aggTaxo, Host) %>% 
  summarise(mean = mean(Abundance),
            sd = sd(Abundance))
