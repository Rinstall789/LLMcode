# R 4.5.2 | 在 RStudio 打开本文件并点击 Source。
# 23 篇研究的计数已写入脚本。PNG 和 PDF 保存到 getwd() 返回的目录。
library(grid)

output_dir <- getwd()
output_name <- file.path(output_dir, "Fig2_AMEGA_23_studies")

# 行对应 A1–A8，列对应 O1–O8。计数来自纳入的 23 篇研究。
counts <- matrix(c(
  2,  5,  1, 1, 1, 0, 4, 2,
  3,  7,  4, 4, 1, 0, 5, 4,
  5,  7,  3, 5, 1, 0, 4, 3,
  1,  2,  1, 1, 0, 0, 2, 0,
  3,  4,  2, 3, 0, 0, 0, 1,
  10, 12, 10, 8, 2, 0, 9, 5,
  4,  4,  3, 3, 1, 0, 1, 0,
  4,  4,  5, 3, 0, 0, 1, 2
), nrow = 8, byrow = TRUE,
dimnames = list(paste0("A", 1:8), paste0("O", 1:8)))

tasks <- c("Disease diagnosis", "Symptoms and signs", "Risk factors",
           "Differential diagnosis", "Next diagnostic tests", "Treatment strategies",
           "Complications and management", "Long-term management")
outcomes <- c("Guideline\nconcordance", "Clinical task\naccuracy",
              "Content quality /\nusability", "Safety / evidence\nreliability",
              "Clinical / care\nprocess outcomes", "Patient health\noutcomes",
              "Workflow\nefficiency", "User / patient\nacceptance")

figure_title <- "Fig. 2. Outcome coverage across subdimensions"
figure_font <- "Times New Roman"
ink <- "#253D52"
muted <- "#49667F"
cell_colors <- c("#EFF3F6", "#DFE7EE", "#BCCDDD", "#94ADC3", "#5D7D9A")
color_breaks <- c(0, 1, 2, 4, 7)

draw_heatmap <- function() {
  grid.newpage()
  left <- .247
  right <- .984
  bottom <- .142
  top <- .883
  cell_width <- (right - left) / ncol(counts)
  cell_height <- (top - bottom) / nrow(counts)
  x_centers <- left + (seq_along(outcomes) - .5) * cell_width
  y_centers <- top - (seq_along(tasks) - .5) * cell_height

  heading <- gpar(fontfamily = figure_font, fontsize = 8, fontface = "bold", col = muted)
  grid.text("Guideline-oriented model performance",
            x = left + 2 * cell_width, y = .942, gp = heading)
  grid.text("Clinical outcomes and implementation value",
            x = left + 6 * cell_width, y = .942, gp = heading)

  for (a in seq_along(tasks)) {
    grid.text(paste0(rownames(counts)[a], "  ", tasks[a]),
              x = left - .015, y = y_centers[a], just = "right",
              gp = gpar(fontfamily = figure_font, fontsize = 7.1, col = ink))
    for (o in seq_along(outcomes)) {
      value <- counts[a, o]
      fill <- cell_colors[findInterval(value, color_breaks)]
      label <- if (value == 0) "\u2013" else as.character(value)
      text_color <- if (value >= 7) "white" else if (value == 0) muted else ink
      grid.rect(x = x_centers[o], y = y_centers[a],
                width = cell_width, height = cell_height,
                gp = gpar(fill = fill, col = "white", lwd = .9))
      grid.text(label, x = x_centers[o], y = y_centers[a],
                gp = gpar(fontfamily = figure_font, fontsize = 9.4,
                          fontface = if (value > 0) "bold" else "plain", col = text_color))
    }
  }

  divider <- left + 4 * cell_width
  grid.lines(x = c(divider, divider), y = c(bottom, top), gp = gpar(col = "white", lwd = 2.5))
  for (o in seq_along(outcomes)) {
    grid.text(paste0(colnames(counts)[o], "\n", outcomes[o]),
              x = x_centers[o], y = bottom - .017, just = c("centre", "top"),
              gp = gpar(fontfamily = figure_font, fontsize = 6, lineheight = .98, col = ink))
  }

  grid.text(figure_title, x = .5, y = .030,
            gp = gpar(fontfamily = figure_font, fontsize = 10, col = "black"))
}

width_mm <- 183
height_mm <- 132
dpi <- 600
png_path <- paste0(output_name, ".png")
pdf_path <- paste0(output_name, ".pdf")

if (requireNamespace("ragg", quietly = TRUE)) {
  ragg::agg_png(png_path, width = width_mm, height = height_mm, units = "mm",
                res = dpi, background = "white")
} else {
  grDevices::png(png_path, width = width_mm, height = height_mm,
                 units = "mm", res = dpi, bg = "white", type = "cairo")
}
draw_heatmap()
invisible(grDevices::dev.off())

grDevices::cairo_pdf(pdf_path, width = width_mm / 25.4, height = height_mm / 25.4,
                     family = figure_font, bg = "white")
draw_heatmap()
invisible(grDevices::dev.off())

message("已保存 PNG: ", png_path, "\n已保存 PDF: ", pdf_path)
