# ==============================================================================
# helpers_report.R  -  PRESENTATION helpers for reports/Chapter4_Results.qmd
# Only formatting / drawing. No statistical decision is made here: every number
# comes from the verified RDS objects (results_04 ... results_07).
# ==============================================================================
suppressPackageStartupMessages({ library(flextable); library(officer) })

TEXT_W_IN <- 6.5                      # usable page width (inches)

make_ft <- function(obj, font_size = 9) {
  dat <- obj$data
  has_type <- "row_type" %in% names(dat)
  cols <- setdiff(names(dat), "row_type")
  ft <- flextable(as.data.frame(dat[, cols, drop = FALSE]))
  ft <- set_header_labels(ft, values = setNames(as.list(obj$header), cols))
  ft <- border_remove(ft)
  ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- bold(ft, part = "header")
  ft <- align(ft, j = seq_along(cols)[-1], align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- valign(ft, valign = "center", part = "header")
  ft <- valign(ft, valign = "top", part = "body")
  ft <- padding(ft, padding.top = 1.5, padding.bottom = 1.5, padding.left = 3, padding.right = 3, part = "all")
  w <- obj$widths / sum(obj$widths) * TEXT_W_IN
  ft <- set_table_properties(ft, layout = "fixed", width = 1)
  for (j in seq_along(cols)) ft <- width(ft, j = j, width = w[j])

  line <- fp_border(color = "black", width = 1.25)
  thin <- fp_border(color = "black", width = 0.5)
  ft <- hline_top(ft, border = line, part = "header")
  ft <- hline_bottom(ft, border = thin, part = "header")
  ft <- hline_bottom(ft, border = line, part = "body")

  if (has_type) {
    sec <- which(dat$row_type == "section")
    var <- which(dat$row_type == "var")
    lev <- which(dat$row_type == "level")
    for (i in sec) {
      ft <- merge_at(ft, i = i, j = seq_along(cols))
      ft <- bold(ft, i = i, bold = TRUE, part = "body")
      ft <- bg(ft, i = i, bg = "#E7E6E6", part = "body")
      ft <- align(ft, i = i, align = "left", part = "body")
    }
    if (length(var)) ft <- bold(ft, i = var, j = 1, bold = TRUE, part = "body")
    if (length(lev) && obj$id %in% c("t41", "t42", "t44")) ft <- padding(ft, i = lev, j = 1, padding.left = 14, part = "body")
    if (obj$id == "t45") {
      ft <- bold(ft, j = 1, bold = TRUE, part = "body")
      ft <- align(ft, j = 2, align = "left", part = "all")
    }
    if (obj$id %in% c("t41", "t42")) ft <- align(ft, j = 2, align = "center", part = "all")
    # continuous summary rows in Table 4.1 sit on the 'var' line with a value
  } else {
    ft <- align(ft, j = 1:2, align = "left", part = "all")
    if (!is.null(obj$group_col)) {
      ft <- merge_v(ft, j = 1)
      ft <- bold(ft, j = 1, bold = TRUE, part = "body")
      ends <- which(!duplicated(dat[[obj$group_col]], fromLast = TRUE))
      ends <- ends[-length(ends)]
      if (length(ends)) ft <- hline(ft, i = ends, border = fp_border(color = "grey70", width = 0.5), part = "body")
    }
  }
  ft
}

# Table notes as Word paragraphs in the "Table Note" style (markdown-safe)
print_notes <- function(obj) {
  notes <- Filter(function(x) !is.null(x) && !is.na(x) && nzchar(x), obj$notes)
  notes <- gsub("\\*", "\\\\*", notes)
  notes[1] <- paste0("*Note.* ", notes[1])
  cat('::: {custom-style="Table Note"}\n', paste(notes, collapse = "\n\n"), '\n:::\n\n', sep = "")
}
print_caption <- function(txt) {
  cat('::: {custom-style="Table Caption"}\n', txt, '\n:::\n\n', sep = "")
}
print_fig_caption <- function(txt) {
  cat('::: {custom-style="Image Caption"}\n', txt, '\n:::\n\n', sep = "")
}

# ---- Figure 4.1 participant flow (ggplot2, observed counts) --------------------------------
plot_flow <- function(fd) {
  nodes <- fd$flow$nodes %>%
    mutate(xmin = x - width / 2, xmax = x + width / 2, ymin = y - height / 2, ymax = y + height / 2)
  ggplot() +
    geom_segment(data = fd$flow$edges, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = grid::arrow(length = grid::unit(0.12, "inches"), type = "closed"),
                 linewidth = 0.5, colour = "grey35") +
    geom_rect(data = nodes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
              colour = "grey25", linewidth = 0.5) +
    geom_text(data = nodes, aes(x = x, y = y, label = label), size = 2.8, lineheight = 0.95,
              family = "serif") +
    scale_fill_manual(values = c(main = "#EEF3F8", branch = "#FFF1D6", model = "#E8F3E8")) +
    coord_cartesian(xlim = c(-5.1, 8.8), ylim = c(0, 7.6), clip = "off") +
    labs(x = NULL, y = NULL) +
    theme_void(base_size = 11) +
    theme(legend.position = "none", plot.margin = margin(8, 8, 8, 8))
}

# ---- Figure 4.2 forest plot of adjusted PRs (log scale, reference line at 1) ------------------
plot_forest <- function(fd, panel_filter = NULL) {
  f <- fd$forest %>% filter(plotted)
  if (!is.null(panel_filter)) f <- f %>% filter(panel %in% panel_filter)
  f <- f %>%
    mutate(
      key = paste(panel, term, sep = "||"),
      sig_label = ifelse(sig, "Statistically significant (P < 0.05)", "Not statistically significant (P \u2265 0.05)")
    )
  f$key <- factor(f$key, levels = rev(unique(f$key)))
  f$panel_label <- factor(f$panel_label, levels = unique(f$panel_label))

  ggplot(f, aes(x = estimate, y = key, colour = sig_label, shape = sig_label)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey25", linewidth = 0.5) +
    geom_errorbar(aes(xmin = conf_low, xmax = conf_high), width = 0.28, linewidth = 0.55, orientation = "y") +
    geom_point(size = 2.3) +
    facet_wrap(~panel_label, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = c(
      "Statistically significant (P < 0.05)" = "#1B4F72",
      "Not statistically significant (P \u2265 0.05)" = "#566573"
    )) +
    scale_shape_manual(values = c(
      "Statistically significant (P < 0.05)" = 15,
      "Not statistically significant (P \u2265 0.05)" = 16
    )) +
    scale_y_discrete(labels = function(x) sub("^[AB]\\|\\|", "", x)) +
    scale_x_log10(
      breaks = c(0.25, 0.5, 1, 2, 4, 8, 16),
      labels = c("0.25", "0.5", "1", "2", "4", "8", "16")
    ) +
    coord_cartesian(xlim = c(0.22, 16)) +
    labs(x = "Adjusted prevalence ratio, aPR (log scale; dashed line at PR = 1)", y = NULL, colour = NULL, shape = NULL) +
    theme_bw(base_size = 9.5, base_family = "serif") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
      strip.background = element_rect(fill = "#E7E6E6", colour = "grey40"),
      strip.text = element_text(face = "bold", hjust = 0, size = 9.5),
      axis.text.y = element_text(size = 8.6, colour = "black"),
      axis.text.x = element_text(size = 8.8, colour = "black"),
      legend.position = "top",
      legend.justification = "right",
      legend.text = element_text(size = 8.5),
      legend.margin = margin(b = 2),
      plot.margin = margin(4, 10, 4, 4)
    )
}
