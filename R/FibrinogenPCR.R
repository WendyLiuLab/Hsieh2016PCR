annotations = function(zero_pad=TRUE) {
  rows = c("A", "B", "C", "D", "E", "F", "G", "H")
  cols = 1:12
  
  # column-indexed
  timepoint = c(rep.int("4h", 6), rep.int("24h", 6)) %>% factor(c("4h", "24h"))
  mouse = c(1, 1, 2, 2, 3, 3) %>% rep.int(2) %>% as.factor
  
  # row-indexed
  conditions = c(
    "Control",
    "F",
    "F+Fg",
    "Fg",
    "L",
    "L+I",
    "4+13",
    "L+4+13"
  )
  names(conditions) = rows
  
  if(zero_pad) cols = sprintf("%02d", cols)
  
  wells = expand.grid(Row=rows, Col=cols) %>%
    mutate(
      Well=paste0(Row, Col),
      Timepoint=timepoint[Col],
      Mouse=mouse[Col],
      Condition=conditions[Row]
    ) %>%
    mutate(
      Condition=factor(Condition,
        c("Control", "F", "F+Fg", "Fg",
          "L", "L+I", "4+13", "L+4+13"))
    )
}

average_cq = function(x) log2(length(x)/sum(2**-x))

gapdh = function() {
  readr::read_csv("raw-data/gapdh/2016-09-04 gapdh -  Quantification Cq Results_0.csv") %>%
    select(Well, Cq) %>%
    inner_join(annotations()) %>%
    group_by(Timepoint, Mouse, Condition) %>%
    summarize(Gapdh=average_cq(Cq))
}

fold_vs_gapdh = function(df) {
  df %>%
    inner_join(gapdh()) %>%
    mutate(FoldVsGapdh = 2**(Gapdh-Cq)) %>%
    select(-Gapdh)
}