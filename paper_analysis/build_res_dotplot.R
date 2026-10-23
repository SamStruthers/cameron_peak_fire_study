#' Build a dot plot of a chemistry parameter across reservoirs, grouped by site and year
#'
#' Creates a dot plot (via `geom_dotplot`) of a single water chemistry parameter
#' for the seven primary study reservoirs, with each reservoir's years shown as
#' adjacent columns separated by small gaps. Optionally adds colored background
#' bands and bold site-name labels below the x-axis ("title" mode) for use as a
#' standalone/top-row plot; otherwise produces a stripped-down version with no
#' x-axis text, intended to stack under a titled plot (e.g. in a multi-panel figure).
#'
#' @param param Character. Name of the chemistry parameter to plot; must match
#'   both a column name in `tidy_chem` and a name in the `chem_expressions` list
#'   (used to generate the y-axis label). Default `"ChlA"`.
#' @param binwdth Numeric. Bin width passed to `geom_dotplot()`, in units of the
#'   parameter's y-axis scale. Controls how finely values are grouped into dots;
#'   smaller values create more, thinner bins. Default `0.3`.
#' @param sz Numeric. `dotsize` passed to `geom_dotplot()`, controlling the
#'   diameter of each dot relative to `binwdth`. Default `1`.
#' @param title_min Numeric or `NULL`. If provided, switches the plot into
#'   "title mode": adds colored background rectangles and bold site-name labels
#'   beneath the x-axis, and rotates x-axis tick labels 45°. `title_min` sets the
#'   lower y-bound of the background rectangles/labels (should be a negative
#'   number safely below the parameter's minimum plotted value, since it is
#'   plotted alongside data on the same y-axis). If `NULL` (default), the plot
#'   omits all x-axis text/ticks/title, for use as a non-labeled panel stacked
#'   with other plots.
#' @param b_size Numeric. Base font size passed to `theme_bw()`. Default `20`.
#'
#' @details
#' Reservoirs are hardcoded to a fixed order (Long Draw, Joe Wright, Hourglass,
#' Chambers, Comanche, Peterson, Barnes Meadow) via `site_code`s
#' `c("LNGR", "JOER", "HORR", "CBRR", "COMR", "PTRR", "BRNR")`. This order is
#' enforced through the `BS_order` column (must already exist in `tidy_chem`)
#' combined with `reorder(site_year, BS_order, max)` on the x-axis.
#'
#' Visual gaps between reservoirs are created by injecting 8 dummy rows
#' (`site_code = NA`) into the plotting data at fixed `BS_order` values
#' (0.5–7.5), which produce empty x-axis positions between each reservoir's
#' block of years. These dummy positions are hidden from the x-axis labels via
#' `scale_x_discrete(labels = ...)`, which strips any label containing `"Gap"`.
#'
#' In title mode, the x-axis break positions used for the background rectangles
#' and text labels (e.g. `xmin = 1.5, xmax = 5.5`, label at `x = 3.5`) are
#' **hardcoded to assume each reservoir has exactly 4 years of data** (4 bars +
#' gaps = 5-unit blocks) and that reservoirs appear in the fixed order above.
#' If the underlying data has a different number of years for any reservoir,
#' or a different site order, these positions will not line up correctly and
#' must be manually adjusted.
#'
#' @return A `ggplot` object.
build_res_plot <- function(param = "ChlA", binwdth = 0.3, sz = 1, title_min = NULL, b_size = 20) {
  # Order: Long Draw, Joe Wright, Hourglass, Chambers, Comanche, Peterson, Barnes Meadow
  sites <- c("LNGR", "JOER", "HORR", "CBRR", "COMR", "PTRR", "BRNR")
  
  y_label <- chem_expressions[[param]]  
  
  
  boxplot_res <- tidy_chem %>%
    filter(site_code %in% sites) %>%
    filter(!is.na(.data[[param]])) %>%
    mutate(site_year = paste(site_code, Year, sep = " "), 
           BS_order = as.numeric(BS_order))
  
  # Inject 6 gaps between the 7 reservoirs
  gaps <- data.frame(
    site_year = paste0("Gap", 1:8),
    site_code = NA, 
    BS_order = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5) 
  )
  
  plot_data <- bind_rows(boxplot_res, gaps)
  
  # Build Base Plot
  p <- ggplot(plot_data, aes(x = reorder(site_year, BS_order, max), y = .data[[param]], fill = site_code))
  
  # Title Annotations
  if (!is.null(title_min)) {
    p <-  p +
      # Site Grouping Backgrounds
      geom_rect(inherit.aes=FALSE, aes( ymin=title_min, ymax=-0.01),xmin = 1.5, xmax = 5.5, fill="#0072B2", alpha=0.01) + #LNGR
      geom_rect(inherit.aes=FALSE, aes( ymin=title_min, ymax=-0.01),xmin = 6.5, xmax = 10.5, fill="#56B4E9", alpha=0.05) + #JOER
      geom_rect(inherit.aes=FALSE, aes( ymin=title_min, ymax=-0.01),xmin = 11.5, xmax = 15.5, fill="#F0E442", alpha=0.05) + #HORR
      geom_rect(inherit.aes=FALSE, aes( ymin=title_min, ymax=-0.01),xmin = 16.5, xmax = 20.5, fill="#F9C74F", alpha=0.05) + #CBRR
      geom_rect(inherit.aes=FALSE, aes( ymin=title_min, ymax=-0.01),xmin = 21.5, xmax = 25.5, fill="#F8961E", alpha=0.05) + #COMR
      geom_rect(inherit.aes=FALSE, aes( ymin=title_min, ymax=-0.01),xmin = 26.5, xmax = 30.5, fill="#F3722C", alpha=0.05) + #PTRR
      geom_rect(inherit.aes=FALSE, aes( ymin=title_min, ymax=-0.01), xmin = 31, xmax = 36, fill="#d71a1d", alpha=0.008) + #BRNR
      
      # Labels positioned at the center of each 4-year block (allowing for gaps)
      annotate("text", x = 3.5,  y = title_min/2, label = "Long Draw", size = 7, fontface = "bold") +
      annotate("text", x = 8.5,  y = title_min/2, label = "Joe Wright", size = 7, fontface = "bold") +
      annotate("text", x = 13.5, y = title_min/2, label = "Hourglass", size = 7, fontface = "bold") +
      annotate("text", x = 18.5, y = title_min/2, label = "Chambers", size = 7, fontface = "bold") +
      annotate("text", x = 23.5, y = title_min/2, label = "Comanche", size = 7, fontface = "bold") +
      annotate("text", x = 28.5, y = title_min/2, label = "Peterson", size = 7, fontface = "bold") +
      annotate("text", x = 33.5, y = title_min/2, label = "Barnes Meadow", size = 7, fontface = "bold")+
      geom_dotplot(stackgroups = FALSE, binaxis = "y", method = "histodot",stackdir = "center", position = "dodge", binwidth = binwdth, dotsize = sz)+
      geom_abline(intercept = 0, slope = 0, color = "black") +
      scale_fill_manual(values = colorsBS_site_code) +
      theme_bw(base_size = b_size) +
      labs(y = y_label, x = NULL)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 18),
            legend.position = "none") +
      #Hide Gaps
      scale_x_discrete(labels = function(x) ifelse(grepl("Gap", x), "", gsub(".* ", "", x)))+
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
    
  } else {
    #Non title plots
    p <- p+
      geom_dotplot(stackgroups = FALSE, binaxis = "y", method = "histodot",stackdir = "center", position = "dodge", binwidth = binwdth, dotsize = sz)+
      geom_abline(intercept = 0, slope = 0, color = "black") +
      scale_fill_manual(values = colorsBS_site_code) +
      theme_bw(base_size = b_size) +
      labs(y = y_label, x = NULL)+
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  }
  
  return(p)
}