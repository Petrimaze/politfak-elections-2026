library(ggplot2)
library(patchwork)
library(ggiraph)

party_colors <- c("НАПАС" = "#D9363E", "Дозревай" = "#D85A91",
  "Зеленка" = "#25834A", "СТАЯ" = "#3068C5", "СВЯЗЬ" = "#D3A500",
  "Пока не определился / не определилась" = "#7B858D",
  "Не стал бы / не стала бы голосовать" = "#7B858D",
  "Предпочитаю не отвечать" = "#7B858D", "Нет ответа" = "#7B858D")

votes <- responses |>
  transmute(party = haven::as_factor(vote_first),
    gender = haven::as_factor(gender), course = haven::as_factor(course),
    track = haven::as_factor(track), eligible = as.numeric(eligible)) |>
  mutate(across(c(party, gender, course, track),
    ~if (anyNA(.x)) forcats::fct_na_value_to_level(.x, "Нет ответа") else .x))

# Порядок партий — только по голосам имеющих право голосовать.
party_order <- votes |>
  group_by(party, .drop = FALSE) |>
  summarise(n = sum(eligible == 1, na.rm = TRUE), .groups = "drop") |>
  arrange(!party %in% parties, desc(n), party) |>
  pull(party) |> as.character()
votes <- votes |> mutate(party = factor(party, levels = rev(party_order)))
visible_parties <- votes |> count(party) |> filter(n > 0) |> pull(party)

vote_summary <- votes |>
  mutate(overall = "Все участники") |>
  pivot_longer(c(overall, gender, course, track),
    names_to = "breakdown", values_to = "category") |>
  group_by(breakdown, category, party, .drop = FALSE) |>
  summarise(n = sum(eligible == 1, na.rm = TRUE),
    excluded = sum(eligible == 0, na.rm = TRUE), .groups = "drop") |>
  group_by(breakdown, category) |>
  mutate(total = sum(n),
    group = paste0(category, " · с правом голоса: ", total),
    share = if_else(total > 0, n / total, NA_real_),
    center = (share + 1.96^2 / (2 * total)) / (1 + 1.96^2 / total),
    margin = 1.96 * sqrt(share * (1 - share) / total +
      1.96^2 / (4 * total^2)) / (1 + 1.96^2 / total),
    lower = pmax(0, center - margin), upper = pmin(1, center + margin),
    label = scales::percent(share, accuracy = 1)) |>
  ungroup() |>
  filter(total > 0, party %in% visible_parties) |>
  mutate(id = paste(breakdown, category, party, sep = ":"),
    tooltip = paste0(as.character(party), "\n", category, "\n",
      n, " из ", total, " с правом голоса · ", label,
      "\n95% ДИ: ", scales::percent(lower, accuracy = 0.1), "–",
      scales::percent(upper, accuracy = 0.1)),
    excluded_tooltip = paste0(as.character(party), "\n", category,
      "\nБез права голоса: ", excluded))

party_labels <- c("Пока не определился / не определилась" = "Не определились",
  "Не стал бы / не стала бы голосовать" = "Не голосовали бы",
  "Предпочитаю не отвечать" = "Отказ от ответа")
chart_note <- paste0("Источник: опрос Мясницкого прачника · ",
  format(loaded_at, "%d.%m.%Y %H:%M", tz = "Europe/Moscow"),
  "\nТочка — доля среди имеющих право голоса; линия — 95% ДИ Уилсона.",
  "\nДобровольный опрос: ДИ не учитывают самоотбор. Не прогноз выборов.")

chart_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "#E5E7E8", linewidth = 0.25),
    axis.title = element_blank(), axis.text = element_text(colour = "#485963"),
    strip.text = element_text(face = "bold", hjust = 0, size = 10),
    plot.title = element_text(face = "bold", colour = "#182C38", size = 14),
    plot.subtitle = element_text(colour = "#59666E", size = 9),
    panel.spacing = grid::unit(1.4, "lines"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(14, 18, 14, 14))

# Одна функция сохраняет одинаковое оформление четырёх рисунков.
vote_chart <- function(data, title) {
  axis_max <- if (all(data$breakdown == "overall")) {
    min(1.17, ceiling((max(data$upper, na.rm = TRUE) + 0.08) * 10) / 10)
  } else 1.17
  main <- ggplot(data, aes(share, party, colour = party)) +
    geom_segment(aes(x = 0, xend = share, yend = party),
      linewidth = 5, alpha = 0.15, lineend = "round", na.rm = TRUE) +
    geom_segment(aes(x = lower, xend = upper, yend = party),
      linewidth = 0.7, na.rm = TRUE) +
    geom_point_interactive(aes(tooltip = tooltip, data_id = id), size = 3, na.rm = TRUE) +
    geom_text(aes(x = share + 0.012, label = if_else(n > 0, label, "")),
      position = position_nudge(y = 0.24), hjust = 0,
      size = 3.4, fontface = "bold", colour = "#182C38", na.rm = TRUE) +
    facet_wrap(~group, ncol = 1) +
    scale_colour_manual(values = party_colors, guide = "none") +
    scale_y_discrete(drop = FALSE, labels = function(x) dplyr::recode(x, !!!party_labels)) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1),
      limits = c(0, axis_max),
      breaks = if (axis_max < 1) seq(0, axis_max, 0.1) else seq(0, 1, 0.25)) +
    labs(title = "Имеют право голоса", subtitle = "% внутри группы · точка и 95% интервал") +
    chart_theme

  context <- ggplot(data, aes(excluded, party)) +
    geom_col_interactive(aes(tooltip = excluded_tooltip, data_id = id),
      width = 0.4, fill = "#C5C7C9") +
    geom_text(aes(label = if_else(excluded > 0, as.character(excluded), "")),
      hjust = -0.5, size = 3.2, colour = "#68747B") +
    facet_wrap(~group, ncol = 1) +
    scale_y_discrete(drop = FALSE) +
    scale_x_continuous(breaks = scales::breaks_width(1),
      expand = expansion(mult = c(0, 0.3))) +
    labs(title = "Без права голоса", subtitle = "Число ответов · отдельная шкала") +
    chart_theme +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      strip.text = element_text(colour = "transparent"))

  (main + context) + plot_layout(widths = c(3, 1.2)) +
    plot_annotation(title = title, caption = chart_note,
      theme = theme(plot.title = element_text(face = "bold", size = 22, colour = "#182C38"),
        plot.caption = element_text(hjust = 0, size = 8, colour = "#68747B"),
        plot.background = element_rect(fill = "white", colour = NA)))
}

# Один SVG-виджет для любого ggplot/patchwork; работает без сервера.
interactive_chart <- function(plot, height = 6) {
  girafe(ggobj = plot, width_svg = 12, height_svg = height,
    options = list(
      opts_sizing(rescale = TRUE),
      opts_hover(css = "opacity:1;stroke-width:2;"),
      opts_hover_inv(css = "opacity:0.25;"),
      opts_tooltip(css = paste0("background:white;color:#182c38;padding:12px;",
        "border:1px solid #dce1e4;border-radius:8px;",
        "box-shadow:0 4px 16px #00000018;font:14px sans-serif;"),
        delay_mouseover = 80, delay_mouseout = 120),
      opts_toolbar(saveaspng = TRUE)))
}
