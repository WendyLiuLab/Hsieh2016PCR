load_cqs = function(gene) {
  readr::read_csv(paste0("raw-data/", gene, "/2016-09-04 ", gene, " -  Quantification Cq Results_0.csv")) %>%
    select(Well, Cq) %>%
    inner_join(annotations()) %>%
    mutate(Target=stringr::str_to_title(gene))
}

load_all_cqs = function() {
  genes = c("arg1", "il10", "nos2", "tnfa", "mrc1-tgfb1", "chi3l3-retnla", "kdm6b-ntcs")
  df = genes %>%
    lapply(load_cqs) %>%
    bind_rows

  split_plate = function(df, target) {
    targets = stringr::str_split_fixed(target, "-", 2) %>%
      sapply(stringr::str_to_title)
    within(df, {
      Target[Target == target & Timepoint == "4h"] = targets[1]
      Target[Target == target & Timepoint == "24h"] = targets[2]
      Timepoint[Target == targets[2]] = "4h"
    })
  }

  df %>%
    split_plate("Mrc1-Tgfb1") %>%
    split_plate("Chi3l3-Retnla") %>%
    split_plate("Kdm6b-Ntcs") %>%
    filter(Target != "Ntcs")
}