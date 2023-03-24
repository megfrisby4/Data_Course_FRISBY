library("seqinr")
sequence <- read.fasta(file = "./Data/sequence.fasta")

library(readr)
library(tidyverse)
Ebola <- read_csv("./Ebola/Data/western_africa_ebola.csv")

  df <- read.csv("https://query.data.world/s/ns5r3oijx5njx6u3l7zbvw5dvuinu3?dws=00000",
                 header=TRUE, stringsAsFactors=FALSE);
