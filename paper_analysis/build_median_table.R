library(dplyr)
library(tidyr)
library(flextable)

build_median_table <- function(medians_df,
                              digits    = 2,
                              show_n    = TRUE,
                              row_label = "Parameter",
                              n_label   = "Samples per Group",
                              est_label = "Median (95% CI)") {
  
  fmt <- function(x) formatC(x, format = "f", digits = digits)
  
  df <- medians_df %>%
    mutate(
      est = ifelse(is.na(ci_low) | is.na(ci_high),
                   fmt(median),
                   paste0(fmt(median), " (", fmt(ci_low), ", ", fmt(ci_high), ")")),
      n = as.character(n)
    )
  
  groups <- unique(as.character(df$group))
  
  wide <- function(col, prefix) {
    df %>%
      select(parameter, group, all_of(col)) %>%
      pivot_wider(names_from = group, values_from = all_of(col),
                  names_prefix = prefix) %>%
      select(parameter, all_of(paste0(prefix, groups)))
  }
  
  est_wide <- wide("est", "est__")
  est_keys <- paste0("est__", groups)
  
  if (show_n) {
    tbl <- wide("n", "n__") %>%
      left_join(est_wide, by = "parameter") %>%
      mutate(sp1 = NA_character_, .after = last_col()) %>%
      relocate(sp1, .before = all_of(est_keys[1]))
    n_keys   <- paste0("n__", groups)
    spacers  <- "sp1"
    top_vals <- c(row_label, n_label, "", est_label)
    top_span <- c(1, length(groups), 1, length(groups))
  } else {
    tbl      <- est_wide
    n_keys   <- character(0)
    spacers  <- character(0)
    top_vals <- c(row_label, est_label)
    top_span <- c(1, length(groups))
  }
  
  labels <- c(list(parameter = ""),
              setNames(as.list(groups), n_keys),
              setNames(as.list(rep("", length(spacers))), spacers),
              setNames(as.list(groups), est_keys))
  
  ft <- flextable(tbl) %>%
    set_header_labels(values = labels) %>%
    add_header_row(values = top_vals, colwidths = top_span) %>%
    theme_vanilla() %>%
    align(align = "center", part = "all") %>%
    align(j = "parameter", align = "left", part = "body") %>%
    valign(valign = "center", part = "all") %>%
    fontsize(size = 11, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    bold(part = "header") %>%
    set_table_properties(layout = "autofit", width = 1) %>%
    autofit()
  
  if (length(spacers) > 0) ft <- width(ft, j = spacers, width = 0.15)
  
  ft
}

build_stacked_median_table <- function(med_list,
                                                                    digits    = 2,
                                                                    row_label = "Parameter",
                                                                    est_label = "Median (95% CI)") {
  
  fmt <- function(x) formatC(x, format = "f", digits = digits)
  
  groups_list <- lapply(med_list, function(m) unique(as.character(m$group)))
  k    <- max(lengths(groups_list))
  keys <- paste0("c", seq_len(k))
  
  make_row <- function(first, vals = character(0)) {
    vals <- c(vals, rep("", k - length(vals)))
    as_tibble(as.list(c(parameter = first, setNames(vals, keys))))
  }
  
  parts <- purrr::imap(med_list, function(m, nm) {
    groups <- groups_list[[nm]]
    
    wide <- m %>%
      mutate(group = factor(as.character(group), levels = groups),
             est = ifelse(is.na(ci_low) | is.na(ci_high), fmt(median),
                          paste0(fmt(median), " (", fmt(ci_low), ", ", fmt(ci_high), ")"))) %>%
      pivot_wider(id_cols = parameter, names_from = group, values_from = est,
                  names_expand = TRUE)
    wide <- wide[, c("parameter", groups)]
    names(wide) <- c("parameter", keys[seq_along(groups)])
    for (key in setdiff(keys, names(wide))) wide[[key]] <- NA_character_
    wide <- wide %>% mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
    
    list(rows = bind_rows(make_row(nm), make_row("", groups), wide[, c("parameter", keys)]),
         n_data = nrow(wide))
  })
  
  tbl <- bind_rows(
    as_tibble(as.list(c(parameter = row_label, setNames(c(est_label, rep("", k - 1)), keys)))),
    bind_rows(lapply(parts, `[[`, "rows"))
  )
  
  # Row indices by type
  sizes     <- vapply(parts, function(p) nrow(p$rows), integer(1))
  starts    <- 1 + cumsum(c(0, head(sizes, -1))) + 1   # +1 for title row
  label_idx <- starts
  hdr_idx   <- starts + 1
  data_idx  <- setdiff(seq_len(nrow(tbl)), c(1, label_idx, hdr_idx))
  
  ft <- flextable(tbl) %>%
    delete_part(part = "header") %>%
    border_remove() %>%
    align(align = "center", part = "body") %>%
    align(j = "parameter", align = "left", part = "body") %>%
    valign(valign = "center", part = "body") %>%
    fontsize(size = 11, part = "body") %>%
    font(fontname = "Times New Roman", part = "body") %>%
    merge_h_range(i = 1, j1 = keys[1], j2 = keys[k], part = "body") %>%
    merge_h_range(i = label_idx, j1 = "parameter", j2 = keys[k], part = "body") %>%
    bold(i = c(1, label_idx, hdr_idx), part = "body") %>%
    align(i = label_idx, align = "left", part = "body") %>%
    bg(i = label_idx, bg = "#EDEDED", part = "body") %>%
    hline(i = data_idx, border = officer::fp_border(color = "gray70", width = 0.5), part = "body") %>%
    hline(i = c(1, hdr_idx), border = officer::fp_border(color = "gray30", width = 1), part = "body") %>%
    hline_top(border = officer::fp_border(color = "gray30", width = 1), part = "body") %>%
    hline_bottom(border = officer::fp_border(color = "gray30", width = 1), part = "body") %>%
    set_table_properties(layout = "autofit", width = 1) %>%
    autofit()
  
  ft
}
build_stacked_median_table_t <- function(med_list,
                                        digits    = 2,
                                        row_label = "Group",
                                        est_label = "Median (95% CI)") {
  
  fmt <- function(x) formatC(x, format = "f", digits = digits)
  
  params_list <- lapply(med_list, function(m) unique(as.character(m$parameter)))
  k    <- max(lengths(params_list))
  keys <- paste0("c", seq_len(k))
  
  make_row <- function(first, vals = character(0)) {
    vals <- c(vals, rep("", k - length(vals)))
    as_tibble(as.list(c(grp = first, setNames(vals, keys))))
  }
  
  parts <- purrr::imap(med_list, function(m, nm) {
    params <- params_list[[nm]]
    
    wide <- m %>%
      mutate(group = as.character(group),
             est = ifelse(is.na(ci_low) | is.na(ci_high), fmt(median),
                          paste0(fmt(median), " (", fmt(ci_low), ", ", fmt(ci_high), ")"))) %>%
      pivot_wider(id_cols = group, names_from = parameter, values_from = est)
    wide <- wide[, c("group", params)]
    names(wide) <- c("grp", keys[seq_along(params)])
    for (key in setdiff(keys, names(wide))) wide[[key]] <- NA_character_
    wide <- wide %>% mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
    
    list(rows = bind_rows(make_row(nm), make_row("", params), wide[, c("grp", keys)]))
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
  
  flextable(tbl) %>%
    delete_part(part = "header") %>%
    border_remove() %>%
    align(align = "center", part = "body") %>%
    align(j = "grp", align = "left", part = "body") %>%
    valign(valign = "center", part = "body") %>%
    fontsize(size = 11, part = "body") %>%
    font(fontname = "Times New Roman", part = "body") %>%
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
    autofit()
}
