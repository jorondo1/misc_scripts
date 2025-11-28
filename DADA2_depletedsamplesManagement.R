library(tidyverse)


v1 <- c(A = 10,
        B= 20,
        C= 30
)

v2 <- c(A = 89,
        B= 1)

v3 <- c(A=12,
        C=5)

set <- list(
  rownames_to_column(data.frame(v1), 'id'),
  rownames_to_column(data.frame(v2), 'id'),
  rownames_to_column(data.frame(v3), 'id')
)

plyr::join_all(set, by = 'id', type='left')
