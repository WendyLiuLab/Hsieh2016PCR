load_cqs = function(gene) {
  readr::read_csv(paste0("raw-data/", gene, "/2016-09-04 ", gene, " -  Quantification Cq Results_0.csv")) %>%
    select(Well, Cq) %>%
    inner_join(annotations()) %>%
    mutate(Target=stringr::str_to_title(gene))
}

load_all_cqs = function() {
  genes = c("arg1", "il10", "nos2", "tnfa")
  genes %>%
    lapply(load_cqs) %>%
    bind_rows
}