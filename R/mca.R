library(FactoMineR)
library(ggrepel)

# Активные признаки заданы заранее; партия не определяет оси.
# TIPI и пол проецируются дополнительно, чтобы не перегружать геометрию.
three_groups <- function(x, low, middle, high) {
  case_when(as.numeric(x) <= 2 ~ low, as.numeric(x) == 3 ~ middle,
    as.numeric(x) >= 4 ~ high, TRUE ~ NA_character_)
}
trait_group <- function(x, name) {
  if_else(as.numeric(x) > 4, paste0(name, " >4"), paste0(name, " ≤4"))
}
mca_data <- responses |>
  filter(as.numeric(eligible) == 1) |>
  transmute(
    course = as.character(haven::as_factor(course)),
    attention = case_when(as.numeric(campaign_attention) <= 3 ~ "Следят мало / умеренно",
      as.numeric(campaign_attention) == 4 ~ "Следят внимательно",
      as.numeric(campaign_attention) == 5 ~ "Следят очень внимательно"),
    belonging = three_groups(belonging_score, "Принадлежность: 1–2", "Принадлежность: 3", "Принадлежность: 4–5"),
    importance = three_groups(outcome_importance_score, "Важность исхода: 1–2", "Важность исхода: 3", "Важность исхода: 4–5"),
    ai = case_when(as.numeric(ai_attitude) == 1 ~ "ИИ: включая нейрослоп",
      as.numeric(ai_attitude) == 2 ~ "ИИ: с редактурой",
      as.numeric(ai_attitude) == 3 ~ "ИИ: только помощь",
      as.numeric(ai_attitude) == 4 ~ "ИИ: против",
      as.numeric(ai_attitude) == 5 ~ "ИИ: безразлично"),
    party = as.character(haven::as_factor(vote_first)),
    gender = as.character(haven::as_factor(gender)),
    extraversion = trait_group(extraversion, "Общительность"),
    agreeableness = trait_group(agreeableness, "Доброжелательность"),
    conscientiousness = trait_group(conscientiousness, "Добросовестность"),
    stability = trait_group(emotional_stability, "Эмоц. устойчивость"),
    openness = trait_group(openness, "Открытость опыту"))

active_names <- c("course", "attention", "belonging", "importance", "ai")
mca_excluded <- sum(!complete.cases(select(mca_data, all_of(active_names))))
mca_data <- mca_data |>
  filter(if_all(all_of(active_names), ~!is.na(.x))) |>
  mutate(across(everything(), ~factor(replace_na(.x, "Нет ответа"))))
# Константные переменные не несут информации для MCA.
mca_data <- mca_data |> select(where(~nlevels(.x) > 1))
active_names <- intersect(active_names, names(mca_data))
supplementary <- which(!names(mca_data) %in% active_names)
# Уникальные имена категорий не дают спутать одинаковые ответы разных вопросов.
mca_input <- mca_data |>
  mutate(across(everything(), ~factor(paste(cur_column(), .x, sep = "::"))))
mca_model <- FactoMineR::MCA(as.data.frame(mca_input), ncp = 5,
  quali.sup = supplementary, graph = FALSE)

category_counts <- mca_data |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "category") |>
  count(variable, category) |>
  mutate(key = paste(variable, category, sep = "::"))

active_points <- as_tibble(mca_model$var$coord[, 1:2], rownames = "key") |>
  setNames(c("key", "x", "y")) |>
  mutate(cos2 = rowSums(mca_model$var$cos2[, 1:2]),
    contribution = rowSums(mca_model$var$contrib[, 1:2]), role = "Активный признак")
supp_points <- as_tibble(mca_model$quali.sup$coord[, 1:2], rownames = "key") |>
  setNames(c("key", "x", "y")) |>
  mutate(cos2 = rowSums(mca_model$quali.sup$cos2[, 1:2]),
    contribution = NA_real_, role = "Дополнительный признак")
mca_points <- bind_rows(active_points, supp_points) |>
  left_join(category_counts, by = "key", relationship = "one-to-one") |>
  mutate(is_party = variable == "party",
    label = recode(category, !!!party_labels),
    label = sub(" курс бакалавриата", " курс", label),
    id = paste(variable, category),
    tooltip = paste0(label, "\n", role, "\nn = ", n,
      "\nКачество проекции (cos²): ", round(cos2, 2),
      if_else(n < 5, "\nМалая категория: трактовать осторожно", "")))

# Не подписываем всё подряд: редкие и плохо представленные категории
# остаются доступны по наведению. Партии с n=0 вообще не имеют позиции.
party_points <- mca_points |> filter(is_party)
feature_points <- mca_points |> filter(!is_party)
feature_labels <- feature_points |> filter(n >= 5, cos2 >= 0.15) |>
  slice_max(cos2, n = 14, with_ties = FALSE)
mca_inertia <- round(mca_model$eig[1:2, 2], 1)
map_labels <- bind_rows(
  feature_labels |> mutate(text_size = 3.1, text_face = "plain", text_color = "#485963"),
  party_points |> mutate(text_size = 4, text_face = "bold", text_color = "#182C38"))
map_span <- max(2.2, max(abs(c(mca_points$x, mca_points$y)), na.rm = TRUE) * 1.15)

draw_mca <- function(selected = NULL, nearest = NULL) {
  focused <- !is.null(selected)
  shown_labels <- if (focused) bind_rows(
    nearest |> mutate(text_size = 3.4, text_face = "plain", text_color = "#485963"),
    filter(map_labels, category == selected)) else map_labels
  shown_parties <- if (focused) filter(party_points, category == selected) else party_points
  ggplot() +
  annotate("text", x = -map_span, y = map_span,
    label = "БЕЗ ИИ\nОтказ от ИИ в контенте",
    colour = "#59666E", size = 3.3, hjust = 0, vjust = 1) +
  annotate("text", x = -map_span, y = -map_span,
    label = "ИИ С РЕДАКТУРОЙ\nДопускают отредактированный контент",
    colour = "#59666E", size = 3.3, hjust = 0, vjust = 0) +
  annotate("text", x = -map_span, y = 0.13,
    label = "ВЫБОРЫ ВАЖНЫ", colour = "#59666E", size = 3.1, hjust = 0) +
  annotate("text", x = map_span, y = 0.13,
    label = "ВЫБОРЫ МЕНЕЕ ВАЖНЫ", colour = "#59666E", size = 3.1, hjust = 1) +
  geom_hline(yintercept = 0, colour = "#C5CDD2", linewidth = 0.5) +
  geom_vline(xintercept = 0, colour = "#C5CDD2", linewidth = 0.5) +
  geom_point_interactive(data = feature_points,
    aes(x, y, shape = role, alpha = cos2, tooltip = tooltip, data_id = id),
    size = 2.8, colour = "#697780", show.legend = !focused) +
  scale_shape_manual(values = c("Активный признак" = 15, "Дополнительный признак" = 18)) +
  scale_alpha_continuous(range = c(0.18, 0.85), guide = "none") +
  {if (focused) geom_point(data = nearest, aes(x, y),
    size = 4, shape = 21, fill = "white", colour = party_colors[[selected]], stroke = 1.2)} +
  geom_point_interactive(data = shown_parties,
    aes(x, y, fill = category, tooltip = tooltip, data_id = id),
    shape = 21, size = 6, colour = "white", stroke = 1.2) +
  geom_label_repel_interactive(data = shown_labels,
    aes(x, y, label = label, tooltip = tooltip, data_id = id,
      size = text_size, fontface = text_face, colour = text_color),
    seed = 26, fill = "white", label.size = 0, box.padding = 0.5,
    point.padding = 0.7, force = 3, max.time = 3, max.iter = 30000,
    max.overlaps = Inf, min.segment.length = 0, segment.colour = "#BAC3C9") +
  scale_size_identity() + scale_colour_identity() +
  scale_fill_manual(values = party_colors, guide = "none") +
  coord_equal(clip = "off") +
  scale_x_continuous(limits = c(-map_span, map_span), expand = expansion(mult = 0.05)) +
  scale_y_continuous(limits = c(-map_span, map_span), expand = expansion(mult = 0.05)) +
  labs(title = "Кто поддерживает разные партии",
    subtitle = if (focused) paste0(selected, " · ближайшие хорошо представленные признаки",
      "\nОдинаковые координаты и масштаб во всех вариантах карты") else
      paste0("MCA · ", nrow(mca_data), " ответов с правом голоса · партии не определяют оси",
      "\nВыберите партию, чтобы рассмотреть её окружение"),
    x = paste0("Вовлечённость в выборы · ось 1 · ", mca_inertia[1], "%"),
    y = paste0("Контент и внимание к кампании · ось 2 · ", mca_inertia[2], "%"),
    shape = NULL,
    caption = "Вторая ось смешанная: сверху также «слежу очень внимательно», снизу — «слежу внимательно».\nБлизость на двух осях не доказывает связь; проверьте реальные доли в списке. Партии — дополнительные категории.") +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#EEF0F2", linewidth = 0.3),
    plot.title = element_text(size = 23, face = "bold", colour = "#182C38"),
    plot.subtitle = element_text(colour = "#59666E", lineheight = 1.3, margin = margin(b = 20)),
    plot.caption = element_text(hjust = 0, colour = "#68747B", size = 9, margin = margin(t = 20)),
    legend.position = "bottom", axis.title = element_text(colour = "#68747B"),
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.margin = margin(25, 30, 20, 25))
}
p_mca <- draw_mca()

# Прямое сравнение профилей: доля признака внутри партии и во всей MCA-выборке.
# Это описательные различия, без отбора по p-value.
profile_long <- mca_data |> mutate(across(everything(), as.character)) |>
  pivot_longer(-party, names_to = "variable", values_to = "category")
profile_baseline <- profile_long |> count(variable, category, name = "all_n") |>
  group_by(variable) |> mutate(baseline = all_n / sum(all_n)) |> ungroup()
party_profiles <- profile_long |> filter(party %in% parties) |>
  count(party, variable, category, name = "n") |>
  group_by(party, variable) |> mutate(total = sum(n), share = n / total) |> ungroup() |>
  left_join(profile_baseline, by = c("variable", "category")) |>
  mutate(difference = share - baseline) |>
  filter(n >= 3, total >= 5, all_n >= 5) |>
  group_by(party) |> slice_max(difference, n = 4, with_ties = FALSE) |> ungroup() |>
  filter(difference > 0)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(mca_points, "data/processed/mca_categories.csv")
write_csv(party_profiles, "data/processed/party_profiles.csv")
write_csv(as_tibble(mca_model$eig, rownames = "axis"), "data/processed/mca_inertia.csv")
saveRDS(mca_model, "data/processed/mca_model.rds")

profile_plot_data <- party_profiles |>
  mutate(label = sub(" курс бакалавриата", " курс", category),
    label = paste0(label, " [", party, "]"),
    label = forcats::fct_reorder(label, difference),
    panel = paste0(party, " · n = ", total),
    id = paste(party, variable, category),
    tooltip = paste0(party, ": ", category, "\n", n, " из ", total,
      " (", scales::percent(share, accuracy = 1), ")",
      "\nВсе в MCA: ", scales::percent(baseline, accuracy = 1),
      "\nРазница: +", round(100 * difference), " п.п."))
p_profiles <- ggplot(profile_plot_data, aes(y = label)) +
  geom_segment(aes(x = baseline, xend = share, yend = label, colour = party),
    linewidth = 1.2, alpha = 0.5) +
  geom_point(aes(x = baseline), shape = 21, fill = "white",
    colour = "#8B989F", size = 3) +
  geom_point_interactive(aes(x = share, colour = party, tooltip = tooltip, data_id = id), size = 3.8) +
  geom_text(aes(x = share + 0.025, label = scales::percent(share, accuracy = 1)),
    hjust = 0, size = 3.2, colour = "#182C38") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub(" \\[.*\\]$", "", x)) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1.15),
    breaks = seq(0, 1, 0.25)) +
  scale_colour_manual(values = party_colors, guide = "none") +
  labs(title = "Чем выделяются сторонники партий",
    subtitle = "Цветная точка — сторонники партии; пустая — все участники MCA",
    caption = "До четырёх наибольших положительных различий для партии с n ≥ 5; категория встречается у ≥ 3 сторонников и ≥ 5 участников.\nОписательные различия выбранных признаков, без проверки значимости; TIPI >4 — выше середины шкалы, не психологический диагноз.",
    x = NULL, y = NULL) +
  chart_theme +
  theme(axis.text.y = element_text(size = 10), plot.caption = element_text(hjust = 0, size = 8),
    plot.title.position = "plot", strip.text = element_text(face = "bold", hjust = 0))
