# Все варианты готовятся в R: браузер только переключает готовые SVG.
mca_choices <- c("Общая карта", parties)
mca_panels <- list()
nearest_by_party <- list()

for (i in seq_along(mca_choices)) {
  selected <- mca_choices[i]
  point <- filter(party_points, category == selected)
  nearest <- NULL
  details <- NULL

  if (i == 1) {
    plot <- p_mca
    details <- htmltools::tags$p("Выберите партию: на карте выделятся четыре ближайших признака с n ≥ 5 и cos² ≥ 0,15.")
  } else if (nrow(point) == 0) {
    mca_panels[[i]] <- htmltools::tags$div(id = paste0("mca-panel-", i),
      class = "mca-party-panel", style = "display:none",
      htmltools::tags$p(paste0(selected, ": в выборке MCA нет сторонников. Положение партии и её окружение не рассчитываются.")))
    next
  } else {
    nearest <- feature_points |>
      filter(n >= 5, cos2 >= 0.15) |>
      mutate(distance = sqrt((x - point$x)^2 + (y - point$y)^2)) |>
      slice_min(distance, n = 4, with_ties = FALSE)
    nearest_by_party[[selected]] <- nearest
    plot <- draw_mca(selected, nearest)
    cards <- list()
    for (j in seq_len(nrow(nearest))) {
      feature <- nearest[j, ]
      supporters <- as.character(mca_data$party) == selected
      matching <- as.character(mca_data[[feature$variable]]) == feature$category
      count <- sum(supporters & matching)
      cards[[j]] <- htmltools::tags$li(
        htmltools::tags$strong(feature$label),
        htmltools::tags$p(paste0("Среди сторонников: ", count, " из ", sum(supporters),
          " (", scales::percent(count / sum(supporters), accuracy = 1), "). ",
          "Во всей выборке: ", scales::percent(mean(matching), accuracy = 1), ".")),
        htmltools::tags$small(paste0("cos² признака: ", round(feature$cos2, 2),
          " · расстояние на карте: ", round(feature$distance, 2))))
    }
    details <- htmltools::tags$div(class = "mca-neighbours",
      htmltools::tags$p(class = "mca-quality", paste0(selected, ": n = ", point$n,
        "; cos² партии = ", round(point$cos2, 2),
        ". Проекция партии слабая: соседство служит ориентиром, а не доказательством связи.")),
      htmltools::tags$h4("Ближайшие признаки на карте"),
      htmltools::tags$ol(class = "mca-neighbour-list", cards),
      htmltools::tags$p(class = "mca-quality",
        "Отбор — по расстоянию на двух осях среди достаточно частых и хорошо представленных категорий. Это не рейтинг силы влияния."))
  }
  widget <- girafe(ggobj = plot, width_svg = 13, height_svg = 11,
    options = list(opts_sizing(rescale = TRUE),
      opts_hover(css = "opacity:1;"),
      opts_tooltip(css = "background:white;color:#182c38;padding:12px;border:1px solid #dce1e4;border-radius:8px;"),
      opts_toolbar(position = "bottomright"),
      opts_zoom(min = 1, max = 3)))
  mca_panels[[i]] <- htmltools::tags$div(id = paste0("mca-panel-", i),
    class = "mca-party-panel", style = if (i == 1) "" else "display:none",
    widget, details)
}

mca_explorer <- htmltools::tagList(
  htmltools::tags$div(class = "mca-controls",
    htmltools::tags$label("Чьих сторонников рассматриваем?", id = "mca-party-label", `for` = "mca-party-choice"),
    htmltools::tags$select(id = "mca-party-choice", class = "form-select",
      lapply(seq_along(mca_choices), function(i) {
        htmltools::tags$option(value = i, mca_choices[i])
      }))),
  htmltools::tags$div(id = "mca-party-panels", mca_panels),
  htmltools::tags$script(htmltools::HTML("
    document.getElementById('mca-party-choice').addEventListener('change', function () {
      document.querySelectorAll('#mca-party-panels > .mca-party-panel').forEach(function(panel) {
        panel.style.display = panel.id === 'mca-panel-' + this.value ? '' : 'none';
      }, this);
      requestAnimationFrame(function () { window.dispatchEvent(new Event('resize')); });
    });
  ")))
