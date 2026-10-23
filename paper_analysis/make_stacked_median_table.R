
minus <- "\u2212"
chem_lookup <- tribble(
  ~parameter,  ~base,            ~sub, ~sup,                 ~unit,
  "DOC",       "DOC",            "",   "",                   "mg L^-1",
  "DTN",       "TDN",            "",   "",                   "mg L^-1",
  "pH",        "pH",             "",   "",                   "unitless",
  "SC",        "SC",             "",   "",                   "\u00b5S cm^-1",
  "ANC",       "ANC",            "",   "",                   "\u00b5eq L^-1",
  "NO3",       "NO",             "3",  minus,                "mg L^-1",
  "K",         "K",              "",   "+",                  "mg L^-1",
  "NH4",       "NH",             "4",  "+",                  "mg L^-1",
  "SO4",       "SO",             "4",  paste0("2", minus),   "mg L^-1",
  "Ca",        "Ca",             "",   "2+",                 "mg L^-1",
  "Na",        "Na",             "",   "+",                  "mg L^-1",
  "Mg",        "Mg",             "",   "2+",                 "mg L^-1",
  "F",         "F",              "",   minus,                "mg L^-1",
  "Cl",        "Cl",             "",   minus,                "mg L^-1",
  "ChlA",      "Chlorophyll a",  "",   "",                   "\u00b5g L^-1",
  "TSS",       "TSS",            "",   "",                   "mg L^-1",
  "Turbidity", "Turbidity",      "",   "",                   "NTU",
  "PO4",       "PO",             "4",  paste0("3", minus),   "mg L^-1"
)

make_stacked_median_table <- function(med_list,
                                      params_as = c("rows", "cols"),
                                      est_source = c("model", "median"),
                                      digits    = 2,
                                      row_label = NULL,
                                      est_label = NULL,
                                      chem      = chem_lookup,
                                      base_size = 11, fe_size = 12, est_size = 10) {

  params_as <- match.arg(params_as)
  est_source <- match.arg(est_source)
  if (is.null(est_label))
    est_label <- if (est_source == "model") "Model estimate (95% CI)" else "Median (95% CI)"
  if (est_source == "model")
    med_list <- lapply(med_list, function(m) {
      m$median <- m$emm; m$ci_low <- m$emm_low; m$ci_high <- m$emm_high; m
    })
  rv <- if (params_as == "rows") "parameter" else "group"   # row variable
  cv <- if (params_as == "rows") "group" else "parameter"   # column variable
  if (is.null(row_label)) row_label <- if (params_as == "rows") "Parameter" else "Group"
  
  fmt <- function(x) formatC(x, format = "f", digits = digits)
  
  # ---- units / markers ----
  all_params <- unique(as.character(unlist(lapply(med_list, function(m) m$parameter))))
  tab <- table(chem$unit[match(all_params, chem$parameter)])
  default_unit <- if (length(tab)) names(which.max(tab)) else "mg L^-1"
  unit_of <- function(p) { u <- chem$unit[match(p, chem$parameter)]; if (is.na(u)) default_unit else u }
  others  <- setdiff(unique(vapply(all_params, unit_of, character(1))), default_unit)
  symbols <- c("*", "**", "***", "#", "\u2020", "\u2021")[seq_along(others)]
  marker_for <- function(u) if (u == default_unit) "" else symbols[match(u, others)]
  
  unit_chunks <- function(u) {
    if (grepl("^", u, fixed = TRUE)) {
      p <- strsplit(u, "^", fixed = TRUE)[[1]]
      list(as_chunk(p[1]), as_sup(gsub("-", minus, p[2], fixed = TRUE)))
    } else list(as_chunk(u))
  }
  
  label_par <- function(p) {
    i <- match(p, chem$parameter)
    base <- if (is.na(i)) p else chem$base[i]
    sub  <- if (is.na(i)) "" else chem$sub[i]
    sup  <- if (is.na(i)) "" else chem$sup[i]
    mk   <- marker_for(unit_of(p))
    ch <- list(as_chunk(base))
    if (nzchar(sub)) ch <- c(ch, list(as_sub(sub)))
    if (nzchar(sup)) ch <- c(ch, list(as_sup(sup)))
    if (nzchar(mk))  ch <- c(ch, list(as_sup(mk)))
    do.call(as_paragraph, ch)
  }
  
  # ---- table body ----
  cols_list <- lapply(med_list, function(m)
    if (is.factor(m[[cv]])) levels(droplevels(m[[cv]])) else unique(as.character(m[[cv]])))
  k    <- max(lengths(cols_list))
  keys <- paste0("c", seq_len(k))
  
  make_row <- function(first, vals = character(0)) {
    vals <- c(vals, rep("", k - length(vals)))
    as_tibble(as.list(c(grp = first, setNames(vals, keys))))
  }
  
  parts <- imap(med_list, function(m, nm) {
    vals     <- cols_list[[nm]]
    row_vals <- if (is.factor(m[[rv]])) levels(droplevels(m[[rv]])) else unique(as.character(m[[rv]]))
    
    wide <- m %>%
      mutate(parameter = as.character(parameter), group = as.character(group),
             est = ifelse(is.na(ci_low) | is.na(ci_high), fmt(median),
                          paste0(fmt(median), " (", fmt(ci_low), ", ", fmt(ci_high), ")"))) %>%
      pivot_wider(id_cols = all_of(rv), names_from = all_of(cv), values_from = est)
    
    wide <- wide[match(row_vals, wide[[rv]]), ]            # order rows by factor levels
    wide <- wide[, c(rv, vals)]                            # order columns
    names(wide) <- c("grp", keys[seq_along(vals)])         # creates "grp"
    for (key in setdiff(keys, names(wide))) wide[[key]] <- NA_character_
    wide <- wide[, c("grp", keys)] %>%
      mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
    
    cells <- if (params_as == "rows")
      tibble(rel = 1 + seq_len(nrow(wide)), j = "grp", param = wide$grp)
    else
      tibble(rel = 1, j = keys[seq_along(vals)], param = vals)
    
    list(rows = bind_rows(make_row(nm), make_row("", vals), wide), cells = cells)
  })
  
  tbl <- bind_rows(
    as_tibble(as.list(c(grp = row_label, setNames(c(est_label, rep("", k - 1)), keys)))),
    bind_rows(lapply(parts, `[[`, "rows"))
  )
  
  sizes     <- vapply(parts, function(p) nrow(p$rows), integer(1))
  starts    <- 1 + cumsum(c(0, head(sizes, -1))) + 1
  label_idx <- starts
  hdr_idx   <- starts + 1
  data_idx  <- setdiff(seq_len(nrow(tbl)), c(1, label_idx, hdr_idx))
  cells_all <- map2_dfr(parts, starts, function(p, s) mutate(p$cells, i = s + rel))
  
  # ---- footnote ----
  fn <- c(list(as_chunk("Units are ")), unit_chunks(default_unit), list(as_chunk(" except where noted.")))
  for (n in seq_along(others))
    fn <- c(fn, list(as_chunk("  "), as_sup(symbols[n]), as_chunk(" ")), unit_chunks(others[n]),
            list(as_chunk(if (n == length(others)) "." else ";")))
  
  # ---- flextable (compose first, then style) ----
  ft <- flextable(tbl) %>% delete_part(part = "header")
  for (r in seq_len(nrow(cells_all)))
    ft <- flextable::compose(ft, i = cells_all$i[r], j = cells_all$j[r],
                             value = label_par(cells_all$param[r]), part = "body")
  ft <- add_footer_lines(ft, values = do.call(as_paragraph, fn))
  
  ft <- ft %>%
    border_remove() %>%
    align(align = "center", part = "body") %>%
    align(j = "grp", align = "left", part = "body") %>%
    align(align = "left", part = "footer") %>%
    valign(valign = "center", part = "body") %>%
    fontsize(size = base_size, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(i = data_idx, j = keys, size = est_size, part = "body")   # median (95% CI)
  
  # Fixed effect labels: column headers if params are rows, row labels if params are columns
  ft <- if (params_as == "rows") {
    fontsize(ft, i = hdr_idx, j = keys, size = fe_size, part = "body")
  } else {
    fontsize(ft, i = data_idx, j = "grp", size = fe_size, part = "body")
  }
  
  ft %>%
    merge_h_range(i = 1, j1 = keys[1], j2 = keys[k], part = "body") %>%
    merge_h_range(i = label_idx, j1 = "grp", j2 = keys[k], part = "body") %>%
    bold(i = c(1, label_idx, hdr_idx), part = "body") %>%
    align(i = label_idx, align = "left", part = "body") %>%
    bg(i = label_idx, bg = "#EDEDED", part = "body") %>%
    hline(i = data_idx, border = officer::fp_border(color = "gray70", width = 0.5), part = "body") %>%
    hline(i = c(1, hdr_idx), border = officer::fp_border(color = "gray30", width = 1), part = "body") %>%
    hline_top(border = officer::fp_border(color = "gray30", width = 1), part = "body") %>%
    hline_bottom(border = officer::fp_border(color = "gray30", width = 1), part = "body") %>%
    set_table_properties(layout = "autofit", width = 1) %>%
    autofit(part = "body")
}

# Usage

