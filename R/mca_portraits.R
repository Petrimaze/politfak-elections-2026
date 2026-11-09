# Карта MCA с портретами сторонников партий.
# В отличие от старого R/mca.R партия здесь — активная переменная:
# карта отвечает на вопрос «какие признаки встречаются вместе с выбором партии».
# Проверка устойчивости — та же карта с партией как дополнительной переменной.
library(FactoMineR)
library(ggrepel)

# Делим по медиане; при равенствах берём разрез, дающий более ровные группы.
median_split <- function(x, high, low) {
  m <- median(x, na.rm = TRUE)
  n <- sum(!is.na(x))
  at_or_below <- abs(sum(x <= m, na.rm = TRUE) - n / 2) <= abs(sum(x < m, na.rm = TRUE) - n / 2)
  below <- if (at_or_below) x <= m else x < m
  if_else(below, low, high)
}
course_short <- c("1 курс бакалавриата" = "1 курс", "2 курс бакалавриата" = "2 курс",
  "3 курс бакалавриата" = "3 курс", "4 курс бакалавриата" = "4 курс и старше",
  "Магистратура" = "4 курс и старше")
mca_colors <- c(party_colors, "Не определились" = "#7B858D")

# Подача: оформление, видео, регулярность, необычные форматы, юмор.
# Офлайн и помощь: практическая помощь, офлайн-мероприятия, личное знакомство.
# Так оценки группируются в данных; программа ни с чем не связана и идёт отдельно.
mca_source <- voters |>
  filter(!response_id %in% careless_ids) |>
  transmute(
    party = as.character(haven::as_factor(vote_first)),
    gender = unname(c("Мужской" = "мужчины", "Женский" = "женщины")[as.character(haven::as_factor(gender))]),
    course = unname(course_short[as.character(haven::as_factor(course))]),
    E = as.numeric(extraversion), A = as.numeric(agreeableness),
    C = as.numeric(conscientiousness), ES = as.numeric(emotional_stability),
    O = as.numeric(openness),
    show = rowMeans(cbind(as.numeric(factor_2_score), as.numeric(factor_3_score),
      as.numeric(factor_4_score), as.numeric(factor_5_score), as.numeric(factor_6_score)), na.rm = TRUE),
    contact = rowMeans(cbind(as.numeric(factor_7_score), as.numeric(factor_8_score),
      as.numeric(factor_9_score)), na.rm = TRUE),
    program = as.numeric(factor_1_score)) |>
  mutate(party = if_else(party == "Пока не определился / не определилась", "Не определились", party))
mca_dropped <- c(
  careless = sum(voters$response_id %in% careless_ids),
  incomplete = nrow(mca_source))
mca_source <- mca_source |>
  filter(party %in% c(parties, "Не определились"), !is.na(gender), !is.na(course),
    if_all(c(E, A, C, ES, O), ~!is.na(.x)), !is.nan(show), !is.nan(contact), !is.na(program)) |>
  add_count(party, name = "party_n") |> filter(party_n >= 5) |> select(-party_n)
mca_dropped["incomplete"] <- mca_dropped["incomplete"] - nrow(mca_source)

mca_data <- mca_source |>
  transmute(party, gender, course,
    extraversion = median_split(E, "общительные", "замкнутые"),
    agreeableness = median_split(A, "доброжелательные", "несговорчивые"),
    conscientiousness = median_split(C, "собранные", "несобранные"),
    stability = median_split(ES, "спокойные", "тревожные"),
    openness = median_split(O, "открытые новому", "закрытые новому"),
    show = median_split(show, "важна подача", "подача не важна"),
    contact = median_split(contact, "важны офлайн и помощь", "офлайн и помощь не важны"),
    program = if_else(program >= 4, "важна программа", "программа не важна")) |>
  mutate(across(everything(), factor))
mca_n <- nrow(mca_data)
variable_kind <- c(party = "Партия", gender = "Пол и курс", course = "Пол и курс",
  extraversion = "Черта характера", agreeableness = "Черта характера",
  conscientiousness = "Черта характера", stability = "Черта характера",
  openness = "Черта характера", show = "Что важно в кампании",
  contact = "Что важно в кампании", program = "Что важно в кампании")

# ncp с запасом: для поправки Гринакра нужны все собственные значения, а не первые пять.
n_categories <- sum(vapply(mca_data, nlevels, integer(1)))
mca_fit <- FactoMineR::MCA(as.data.frame(mca_data), ncp = n_categories, graph = FALSE)
mca_fit_sup <- FactoMineR::MCA(as.data.frame(mca_data), ncp = n_categories, quali.sup = 1, graph = FALSE)

# Доля инерции с поправкой Гринакра: сырые проценты MCA сильно занижены.
greenacre <- function(fit, q, j) {
  ev <- fit$eig[, 1]
  adjusted <- ifelse(ev > 1 / q, (q / (q - 1))^2 * (ev - 1 / q)^2, 0)
  total <- q / (q - 1) * (sum(ev^2) - (j - q) / q^2)
  100 * adjusted / total
}
mca_inertia <- round(greenacre(mca_fit, ncol(mca_data), n_categories))
mca_inertia_sup <- round(greenacre(mca_fit_sup, ncol(mca_data) - 1, n_categories - nlevels(mca_data$party)))

category_counts <- mca_data |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "category") |>
  count(variable, category)
point_table <- function(coord, v_test, contrib = NULL) {
  tibble(category = rownames(coord), x = coord[, 1], y = coord[, 2],
    v1 = v_test[, 1], v2 = v_test[, 2],
    c1 = if (is.null(contrib)) NA_real_ else contrib[, 1],
    c2 = if (is.null(contrib)) NA_real_ else contrib[, 2]) |>
    left_join(category_counts, by = "category") |>
    mutate(kind = unname(variable_kind[variable]))
}
mca_points <- point_table(mca_fit$var$coord, mca_fit$var$v.test, mca_fit$var$contrib)
party_sup <- point_table(mca_fit_sup$quali.sup$coord, mca_fit_sup$quali.sup$v.test)
features_sup <- point_table(mca_fit_sup$var$coord, mca_fit_sup$var$v.test)
# Знак осей в MCA произволен: выравниваем проверочную карту по основной.
for (axis in c("x", "y")) {
  joined <- inner_join(select(mca_points, category, a = all_of(axis)),
    select(features_sup, category, b = all_of(axis)), by = "category")
  if (sum(joined$a * joined$b) < 0) {
    party_sup[[axis]] <- -party_sup[[axis]]
    vcol <- if (axis == "x") "v1" else "v2"
    party_sup[[vcol]] <- -party_sup[[vcol]]
  }
}

# Портрет: доля признака у сторонников партии против всей выборки карты.
profile_long <- mca_data |> mutate(across(everything(), as.character)) |>
  pivot_longer(-party, names_to = "variable", values_to = "category")
profile_baseline <- profile_long |> count(variable, category, name = "all_n") |>
  group_by(variable) |> mutate(baseline = all_n / sum(all_n)) |> ungroup()
mca_profiles <- profile_long |>
  count(party, variable, category) |>
  complete(party, nesting(variable, category), fill = list(n = 0)) |>
  group_by(party, variable) |> mutate(total = sum(n), share = n / total) |> ungroup() |>
  left_join(profile_baseline, by = c("variable", "category")) |>
  mutate(diff = round(100 * share) - round(100 * baseline))
# До четырёх признаков с разницей от 15 п. п.; если таких меньше трёх — три самых заметных.
portrait_rows <- function(selected) {
  rows <- mca_profiles |> filter(party == selected) |> arrange(desc(diff))
  strong <- rows |> filter(diff >= 15) |> head(4)
  if (nrow(strong) < 3) head(rows, 3) else strong
}

party_points <- mca_points |> filter(variable == "party")
feature_points <- mca_points |> filter(variable != "party")
party_order_mca <- c(party_order[party_order %in% party_points$category],
  intersect("Не определились", party_points$category))
significance_note <- function(v) {
  if (v >= 2) "отличие от центра значимо (|v-тест| ≥ 2)"
  else if (v >= 1.6) "отличие на грани значимости"
  else "почти не отличается от центра карты"
}
pole <- function(points, axis, side) {
  points |> mutate(contrib = if (axis == "x") c1 else c2, coord = .data[[axis]]) |>
    filter(sign(coord) == side, contrib >= 5) |>
    arrange(desc(contrib)) |> head(3) |> pull(category)
}
x_title <- paste0("← ", paste(pole(feature_points, "x", -1), collapse = ", "),
  "     ось 1 · ", mca_inertia[1], "% инерции     ",
  paste(pole(feature_points, "x", 1), collapse = ", "), " →")
# Подпись оси Y повёрнута на 90°: стрелки ← и → после поворота смотрят вниз и вверх.
y_title <- paste0("← ", paste(pole(feature_points, "y", -1), collapse = ", "),
  "     ось 2 · ", mca_inertia[2], "% инерции     ",
  paste(pole(feature_points, "y", 1), collapse = ", "), " →")
map_span <- max(abs(c(mca_points$x, mca_points$y, party_sup$x, party_sup$y))) * 1.12
kind_shapes <- c("Черта характера" = 16, "Пол и курс" = 15, "Что важно в кампании" = 18)

draw_portrait_map <- function(selected = NULL, robustness = FALSE) {
  focused <- !is.null(selected)
  picked <- if (focused) portrait_rows(selected)$category else character(0)
  features <- feature_points |>
    mutate(picked = category %in% picked,
      alpha = if (focused) if_else(picked, 1, 0.22) else 0.75,
      colour = if_else(picked, "#182C38", "#8B989F"),
      tooltip = paste0(category, "\n", kind, " · n = ", n,
        "\nv-тест по осям: ", round(v1, 1), " / ", round(v2, 1)))
  parties_shown <- party_points |>
    mutate(size = 4 + sqrt(n) * 0.9,
      tooltip = paste0(category, "\nn = ", n, "\nv-тест по осям: ",
        round(v1, 1), " / ", round(v2, 1), "\n",
        vapply(pmax(abs(v1), abs(v2)), significance_note, character(1)))) |>
    mutate(alpha = if (focused) if_else(category == selected, 1, 0.45) else 1)
  labels <- bind_rows(
    if (focused) filter(features, picked) else filter(features, c1 + c2 >= 3),
    parties_shown |> mutate(colour = "#182C38")) |>
    mutate(face = if_else(variable == "party", "bold", "plain"),
      size = if_else(variable == "party", 4.2, 3.4))
  spokes <- if (focused) {
    origin <- filter(party_points, category == selected)
    filter(features, picked) |> mutate(x0 = origin$x, y0 = origin$y)
  } else NULL
  halo <- if (focused) filter(parties_shown, category == selected) else NULL
  sup <- if (robustness) left_join(party_sup, select(party_points, category, ax = x, ay = y), by = "category") |>
    mutate(tooltip = paste0(category, " · оси без партии\nv-тест по осям: ",
      round(v1, 1), " / ", round(v2, 1), "\n",
      vapply(pmax(abs(v1), abs(v2)), significance_note, character(1)))) else NULL

  ggplot() +
    geom_hline(yintercept = 0, colour = "#C5CDD2", linewidth = 0.5) +
    geom_vline(xintercept = 0, colour = "#C5CDD2", linewidth = 0.5) +
    {if (focused) geom_segment(data = spokes, aes(x = x0, y = y0, xend = x, yend = y),
      colour = mca_colors[[selected]], linewidth = 0.8, alpha = 0.6)} +
    {if (robustness) geom_segment(data = sup, aes(x = ax, y = ay, xend = x, yend = y, colour = category),
      linewidth = 0.8, alpha = 0.7, show.legend = FALSE)} +
    geom_point_interactive(data = features,
      aes(x, y, shape = kind, alpha = alpha, colour = colour, tooltip = tooltip, data_id = category),
      size = 3) +
    {if (focused) geom_point(data = halo, aes(x, y), size = halo$size * 2.2,
      colour = mca_colors[[selected]], alpha = 0.18)} +
    geom_point_interactive(data = parties_shown,
      aes(x, y, fill = category, size = size, alpha = alpha, tooltip = tooltip, data_id = category),
      shape = 21, colour = "white", stroke = 1.4) +
    {if (robustness) geom_point_interactive(data = sup,
      aes(x, y, colour = category, tooltip = tooltip, data_id = paste("sup", category)),
      shape = 21, fill = "white", size = 4.5, stroke = 1.6, show.legend = FALSE)} +
    geom_text_repel(data = labels,
      aes(x, y, label = category, colour = colour, size = size, fontface = face),
      seed = 26, box.padding = 0.45, point.padding = 0.6, force = 3,
      max.overlaps = Inf, min.segment.length = 0.4, segment.colour = "#BAC3C9") +
    scale_shape_manual(values = kind_shapes, name = NULL) +
    scale_alpha_identity() + scale_size_identity() +
    scale_colour_manual(values = c("#182C38" = "#182C38", "#8B989F" = "#8B989F", mca_colors), guide = "none") +
    scale_fill_manual(values = mca_colors, guide = "none") +
    coord_equal(clip = "off") +
    scale_x_continuous(limits = c(-map_span, map_span), expand = expansion(mult = 0.03)) +
    scale_y_continuous(limits = c(-map_span, map_span), expand = expansion(mult = 0.03)) +
    labs(title = if (robustness) "Проверка: оси построены без партии"
        else if (focused) paste0(selected, ": чем выделяются сторонники") else "Кто за кого голосует",
      subtitle = if (robustness) "Цветная точка — партия на основной карте; кольцо — та же партия, если строить оси без неё"
        else if (focused) "Лучи ведут к признакам, которые у сторонников встречаются заметно чаще, чем у всех"
        else paste0("MCA · ", mca_n, " анкет с правом голоса · близкие точки — признаки, которые часто встречаются вместе"),
      x = x_title, y = y_title, shape = NULL,
      caption = paste0("Поправка Гринакра для доли инерции. Партии с n ≥ 5; небрежные и неполные анкеты исключены.",
        "\nБлизость на двух осях — ориентир, а не доказательство связи; точные доли — в портрете под картой.")) +
    theme_minimal(base_size = 12) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#EEF0F2", linewidth = 0.3),
      axis.text = element_blank(),
      plot.title = element_text(size = 22, face = "bold", colour = "#182C38"),
      plot.subtitle = element_text(colour = "#59666E", lineheight = 1.3, margin = margin(b = 16)),
      plot.caption = element_text(hjust = 0, colour = "#68747B", size = 9, margin = margin(t = 16)),
      legend.position = "bottom", axis.title = element_text(colour = "#485963", size = 10),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.margin = margin(20, 24, 16, 20))
}

mca_widget <- function(plot) {
  girafe(ggobj = plot, width_svg = 12, height_svg = 11,
    options = list(opts_sizing(rescale = TRUE),
      opts_hover(css = "opacity:1;"),
      opts_tooltip(css = paste0("background:white;color:#182c38;padding:12px;",
        "border:1px solid #dce1e4;border-radius:8px;box-shadow:0 4px 16px #00000018;font:14px sans-serif;")),
      opts_toolbar(saveaspng = TRUE)))
}

portrait_card <- function(selected) {
  point <- filter(party_points, category == selected)
  rows <- portrait_rows(selected)
  cards <- lapply(seq_len(nrow(rows)), function(j) {
    r <- rows[j, ]
    htmltools::tags$li(
      htmltools::tags$strong(r$category),
      htmltools::tags$p(paste0(scales::percent(r$share, accuracy = 1), " сторонников против ",
        scales::percent(r$baseline, accuracy = 1), " во всей карте")),
      htmltools::tags$small(paste0(r$n, " из ", r$total, " · разница ",
        if_else(r$diff >= 0, "+", ""), r$diff, " п. п.")))
  })
  htmltools::tags$div(class = "mca-neighbours",
    htmltools::tags$p(class = "mca-quality", paste0(selected, ": n = ", point$n, "; ",
      significance_note(max(abs(point$v1), abs(point$v2))), ".")),
    htmltools::tags$h4("Портрет сторонников"),
    htmltools::tags$ol(class = "mca-neighbour-list", cards))
}

mca_choices <- c("Вся карта", party_order_mca)
mca_panels <- lapply(seq_along(mca_choices), function(i) {
  selected <- if (i == 1) NULL else mca_choices[i]
  details <- if (i == 1) htmltools::tags$p(class = "mca-quality",
    "Выберите партию: лучи покажут признаки её сторонников, а под картой появятся точные доли.")
    else portrait_card(selected)
  htmltools::tags$div(id = paste0("portrait-panel-", i), class = "mca-party-panel",
    style = if (i == 1) "" else "display:none",
    mca_widget(draw_portrait_map(selected)), details)
})
portrait_explorer <- htmltools::tagList(
  htmltools::tags$div(class = "mca-controls",
    htmltools::tags$label("Чьих сторонников рассматриваем?", `for` = "portrait-choice"),
    htmltools::tags$select(id = "portrait-choice", class = "form-select",
      lapply(seq_along(mca_choices), function(i) htmltools::tags$option(value = i, mca_choices[i])))),
  htmltools::tags$div(id = "portrait-panels", mca_panels),
  htmltools::tags$script(htmltools::HTML("
    document.getElementById('portrait-choice').addEventListener('change', function () {
      document.querySelectorAll('#portrait-panels > .mca-party-panel').forEach(function (panel) {
        panel.style.display = panel.id === 'portrait-panel-' + this.value ? '' : 'none';
      }, this);
      requestAnimationFrame(function () { window.dispatchEvent(new Event('resize')); });
    });
  ")))
p_mca_robust <- draw_portrait_map(robustness = TRUE)

robust_table <- function() {
  left_join(
    party_points |> transmute(Партия = category, n, a = pmax(abs(v1), abs(v2))),
    party_sup |> transmute(Партия = category, b = pmax(abs(v1), abs(v2))), by = "Партия") |>
    mutate(Партия = factor(Партия, levels = party_order_mca)) |> arrange(Партия) |>
    transmute(Партия, n,
      `Основная карта, max |v|` = formatC(a, format = "f", digits = 2, decimal.mark = ","),
      `Оси без партии, max |v|` = formatC(b, format = "f", digits = 2, decimal.mark = ",")) |>
    knitr::kable(align = c("l", "r", "r", "r"))
}
robust_significant <- party_sup |> filter(pmax(abs(v1), abs(v2)) >= 2) |> pull(category)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(bind_rows(mca_points |> mutate(role = "активная"), party_sup |> mutate(role = "дополнительная")),
  "data/processed/mca_portrait_categories.csv")
write_csv(mca_profiles, "data/processed/mca_portrait_profiles.csv")
