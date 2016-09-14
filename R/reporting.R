load_cqs = function(gene) {
  readr::read_csv(paste0("raw-data/", gene, "/2016-09-04 ", gene, " -  Quantification Cq Results_0.csv")) %>%
    select(Well, Cq) %>%
    inner_join(annotations()) %>%
    mutate(Target=stringr::str_to_title(gene))
}

load_melt_curves = function(gene) {
  readr::read_csv(paste0("raw-data/", gene, "/2016-09-04 ", gene, " -  Melt Curve Derivative Results_SYBR.csv")) %>%
    select(-X1) %>%
    reshape2::melt(id.vars="Temperature", variable.name="Well", value.name="dF") %>%
    select(Well, Temperature, dF) %>%
    inner_join(annotations(zero_pad=FALSE)) %>%
    mutate(Target=stringr::str_to_title(gene))
}

split_plate = function(df, target) {
  targets = stringr::str_split_fixed(target, "-", 2) %>%
    sapply(stringr::str_to_title)
  within(df, {
    Target[Target == target & Timepoint == "4h"] = targets[1]
    Target[Target == target & Timepoint == "24h"] = targets[2]
    Timepoint[Target == targets[2]] = "4h"
  })
}

load_all_cqs = function() {
  genes = c("arg1", "il10", "nos2", "tnfa", "mrc1-tgfb1", "chi3l3-retnla", "kdm6b-ntcs")
  df = genes %>%
    lapply(load_cqs) %>%
    bind_rows

  df %>%
    split_plate("Mrc1-Tgfb1") %>%
    split_plate("Chi3l3-Retnla") %>%
    split_plate("Kdm6b-Ntcs") %>%
    filter(Target != "Ntcs")
}

load_all_melt_curves = function() {
  genes = c("arg1", "il10", "nos2", "tnfa", "mrc1-tgfb1", "chi3l3-retnla", "kdm6b-ntcs")
  df = genes %>%
    lapply(load_melt_curves) %>%
    bind_rows

  df %>%
    split_plate("Mrc1-Tgfb1") %>%
    split_plate("Chi3l3-Retnla") %>%
    split_plate("Kdm6b-Ntcs") %>%
    filter(Target != "Ntcs")
}

heatmap = function(plot_matrix, col=c(), ...) {
  if(length(col) == 0) {
    col = viridis::viridis(128)
  }
  gplots::heatmap.2(plot_matrix,
                    col=col,
                    srtCol=30,
                    trace="none",
                    denscol="black",
                    ...)
}