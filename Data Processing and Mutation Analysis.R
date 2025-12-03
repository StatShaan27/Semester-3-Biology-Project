if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Biostrings")
install.packages("lubridate")
install.packages("seqinr")
install.packages("dplyr")
install.packages("data.table")

library(data.table)
library(dplyr)
library(lubridate)
library(seqinr)
library(Biostrings)
library(ggplot2)

meta <- fread("metadata.tsv", sep="\t")

india_meta <- meta %>% filter(grepl("^India", country))
india_meta <- india_meta %>% filter(date != "")
india_meta$date <- gsub("-XX", "-15", india_meta$date)
india_meta$date <- gsub("-00", "-01", india_meta$date)
india_meta$date <- suppressWarnings(ymd(india_meta$date))
india_meta <- india_meta %>% filter(!is.na(date))
india_meta <- india_meta %>% 
  filter(date >= "2020-01-01", date <= "2021-12-31")

aligned <- readDNAStringSet("aligned.fasta")

keep <- india_meta$strain
aligned_india <- aligned[names(aligned) %in% keep]

india_meta <- india_meta %>% mutate(month = floor_date(date, "month"))

set.seed(42)
sub_meta <- india_meta %>% 
  group_by(month) %>% 
  sample_n(size = min(120, n())) %>% 
  ungroup()

aligned_sub <- aligned_india[names(aligned_india) %in% sub_meta$strain]

msa <- read.fasta("aligned.fasta", seqtype="DNA", as.string=TRUE)
msa_india <- msa[names(msa) %in% names(aligned_sub)]

ref <- read.fasta("reference.fasta", seqtype="DNA", as.string=TRUE)[[1]]
ref <- unlist(strsplit(ref, ""))

call_muts <- function(seq_string, ref) {
  seq_chars <- unlist(strsplit(seq_string, ""))
  diffs <- which(seq_chars != ref & seq_chars != "N")
  if (length(diffs) == 0) return(NULL)
  data.frame(
    pos = diffs,
    ref = ref[diffs],
    alt = seq_chars[diffs]
  )
}

mut_list <- lapply(names(msa_india), function(id) {
  df <- call_muts(msa_india[[id]], ref)
  if (!is.null(df)) df$strain <- id
  df
})

mutation_table <- do.call(rbind, mut_list)

annot <- data.frame(
  gene = c("ORF1ab", "ORF1ab", "S", "ORF3a", "E", "M", "N"),
  start = c(266, 13468, 21563, 25393, 26245, 26523, 28274),
  end   = c(13467, 21555, 25384, 26220, 26522, 27191, 29533)
)

mutation_table$gene <- sapply(mutation_table$pos, function(p) {
  g <- annot$gene[which(p >= annot$start & p <= annot$end)]
  if (length(g) == 0) return(NA)
  g
})

mutation_table <- mutation_table %>%
  filter(pos >= 266, pos <= 29533) %>% 
  filter(alt != "-")

mutation_table <- mutation_table %>%
  left_join(sub_meta %>% select(strain, date, month), by="strain")

mut_freq <- mutation_table %>%
  group_by(month, pos, alt) %>%
  summarise(count = n(), .groups="drop")

seq_per_month <- sub_meta %>%
  group_by(month) %>%
  summarise(n_seq = n())

mut_freq <- mut_freq %>%
  left_join(seq_per_month, by="month") %>%
  mutate(freq = count / n_seq)

top_mut <- mutation_table %>%
  group_by(pos, alt) %>%
  summarise(count = n(), .groups="drop") %>%
  arrange(desc(count)) %>%
  slice_head(n = 20)

top_ids <- paste(top_mut$pos, top_mut$alt)

plot_data <- mutation_table %>%
  mutate(id = paste(pos, alt)) %>%
  filter(id %in% top_ids)

ggplot(plot_data, aes(x=month, fill=id)) +
  geom_bar(position="stack") +
  theme_minimal() +
  labs(
    title="Rise of Top Mutations in Indian SARS-CoV-2 Genomes (2020–2021)",
    x="Month",
    y="Mutation Count",
    fill="Mutation"
  )

top6 <- top_mut %>% slice_head(n=6)

mut_freq_top <- mut_freq %>%
  inner_join(top6 %>% select(pos, alt), by=c("pos","alt")) %>%
  mutate(id = paste(pos, alt))

ggplot(mut_freq_top, aes(x=month, y=freq, color=id, group=id)) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  theme_minimal() +
  labs(
    title="Frequency Trajectories of Key SARS-CoV-2 Mutations in India (2020–2021)",
    x="Month",
    y="Mutation Frequency",
    color="Mutation"
  )
