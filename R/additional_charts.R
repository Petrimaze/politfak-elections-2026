# Дополнительные графики: один и тот же круг имеющих право голоса.
voters <- responses |> filter(as.numeric(eligible) == 1)
voter_n <- nrow(voters)
choices <- voters |>
  transmute(first = as.character(haven::as_factor(vote_first)),
    second = as.character(haven::as_factor(vote_second))) |>
  mutate(invalid_second = first %in% parties & !is.na(second) & first == second)
invalid_second_n <- sum(choices$invalid_second)

choice_counts <- choices |>
  mutate(second = if_else(invalid_second, NA_character_, second)) |>
  pivot_longer(c(first, second), names_to = "priority", values_to = "party") |>
  filter(party %in% parties) |>
  count(party, priority) |>
  complete(party = parties, priority = c("first", "second"), fill = list(n = 0)) |>
  mutate(share = n / voter_n,
    party = factor(party, levels = rev(party_order[party_order %in% parties])),
    tooltip = paste0(party, "\n", if_else(priority == "first", "Первый", "Второй"),
      " выбор: ", n, " из ", voter_n, " (",
      scales::percent(n / voter_n, accuracy = 1), ")"))
choice_pairs <- choice_counts |> select(party, priority, share) |>
  pivot_wider(names_from = priority, values_from = share)
p_second <- ggplot(choice_counts, aes(share, party)) +
  geom_segment(data = choice_pairs, aes(x = first, xend = second, yend = party, colour = party),
    linewidth = 1, alpha = 0.35) +
  geom_point_interactive(aes(shape = priority, colour = party,
    tooltip = tooltip, data_id = as.character(party)), size = 4, stroke = 1.1, fill = "white") +
  scale_colour_manual(values = party_colors, guide = "none") +
  scale_shape_manual(values = c(first = 16, second = 21),
    breaks = c("first", "second"), labels = c("Первый выбор", "Второй выбор")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
    breaks = scales::breaks_width(0.05), expand = expansion(mult = c(0.05, 0.12))) +
  labs(title = "Первый выбор и запас вторых предпочтений",
    subtitle = paste0("Доля от всех ", voter_n, " ответивших с правом голоса · единый знаменатель"),
    shape = NULL, x = NULL, y = NULL,
    caption = paste0("Второй выбор не прибавляется к первому. Совпадений с первым исключено: ", invalid_second_n,
      ".\nНезаполненные ответы и отсутствие второй партии не считаются упоминаниями.")) +
  chart_theme + theme(legend.position = "top", plot.caption = element_text(hjust = 0, size = 9))

# Все ответы остаются в знаменателе строки, в том числе некорректные.
second_matrix <- choices |>
  filter(first %in% parties) |>
  mutate(second = case_when(invalid_second ~ "Совпадает с первым",
    is.na(second) ~ "Нет ответа",
    second == "Нет партии второго предпочтения" ~ "Нет второго выбора",
    second == "Пока не определился / не определилась" ~ "Не определились",
    second == "Предпочитаю не отвечать" ~ "Отказ от ответа",
    TRUE ~ second)) |>
  count(first, second) |>
  complete(first, second, fill = list(n = 0)) |>
  group_by(first) |> mutate(total = sum(n), share = n / total) |> ungroup() |>
  mutate(first = factor(first, levels = rev(party_order[party_order %in% parties])),
    second = factor(second, levels = c(party_order[party_order %in% parties],
      "Нет второго выбора", "Не определились", "Совпадает с первым", "Нет ответа", "Отказ от ответа")),
    row = paste0(first, " · n = ", total),
    row = factor(row, levels = unique(row[order(first)])),
    tooltip = paste0(first, " → ", second, "\n", n, " из ", total,
      " сторонников (", scales::percent(share, accuracy = 1), ")"))
p_second_matrix <- ggplot(second_matrix, aes(second, row, fill = share)) +
  geom_tile_interactive(aes(tooltip = tooltip, data_id = paste(first, second)),
    colour = "white", linewidth = 2) +
  geom_text(aes(label = if_else(n > 0, as.character(n), ""),
    colour = share > 0.45), size = 4, fontface = "bold") +
  scale_colour_manual(values = c("FALSE" = "#182C38", "TRUE" = "white"), guide = "none") +
  scale_fill_gradient(low = "#F1F4F5", high = "#087F8C", limits = c(0, 1),
    labels = scales::label_percent(), name = "Доля в строке") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, 13)) +
  labs(title = "К кому переходят вторые предпочтения",
    subtitle = "Строка — первый выбор; столбец — второй · числа в клетках — люди",
    x = NULL, y = NULL, caption = "Цвет показывает долю внутри строки. Пустая клетка — ноль ответов; подсказка содержит точный знаменатель.") +
  chart_theme + theme(panel.grid = element_blank(), legend.position = "bottom",
    axis.text.x = element_text(size = 9), plot.caption = element_text(hjust = 0, size = 9))

# Распределение ответов важнее одного среднего балла.
factor_distribution <- voters |>
  select(matches("^factor_[1-9]_score$")) |>
  mutate(across(everything(), as.numeric)) |>
  pivot_longer(everything(), names_to = "factor", values_to = "score") |>
  mutate(factor = factor_labels[as.integer(stringr::str_extract(factor, "[1-9]"))]) |>
  filter(!is.na(score)) |> count(factor, score) |>
  complete(factor, score = 1:5, fill = list(n = 0)) |>
  group_by(factor) |>
  mutate(total = sum(n), share = n / total,
    important = sum(n[score >= 4]) / total) |> ungroup() |>
  mutate(factor = forcats::fct_reorder(factor, important),
    score = factor(score, levels = 1:5),
    tooltip = paste0(factor, "\nОценка ", score, ": ", n, " из ", total,
      " (", scales::percent(share, accuracy = 1), ")"))
p_factors <- ggplot(factor_distribution, aes(factor, share, fill = score)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = paste(factor, score)),
    position = position_stack(reverse = TRUE), width = 0.6) +
  geom_text(data = distinct(factor_distribution, factor, important),
    aes(x = factor, y = 1.025, label = scales::percent(important, accuracy = 1)),
    inherit.aes = FALSE, hjust = 0, size = 3.5) +
  scale_fill_manual(values = c("#E4E8EB", "#BCC7CD", "#8699A3", "#61B0AA", "#087F82"),
    labels = c("1 · не важно", "2", "3", "4", "5 · очень важно"), name = NULL) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1.15),
    breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
  coord_flip() +
  labs(title = "Что важно при выборе партии",
    subtitle = "Распределение оценок 1–5 · справа — доля оценок 4–5",
    x = NULL, y = NULL,
    caption = "Только имеющие право голоса. Доли — среди ответивших на конкретный пункт; пропуски исключены из его знаменателя.") +
  chart_theme + theme(legend.position = "top", plot.caption = element_text(hjust = 0, size = 8))

# Профили TIPI без дискретизации шкал и без MCA.
trait_names <- c(extraversion = "Общительность", agreeableness = "Доброжелательность",
  conscientiousness = "Добросовестность", emotional_stability = "Эмоц. устойчивость",
  openness = "Открытость опыту")
traits_long <- voters |>
  transmute(party = as.character(haven::as_factor(vote_first)),
    across(all_of(names(trait_names)), as.numeric)) |>
  pivot_longer(all_of(names(trait_names)), names_to = "trait", values_to = "score")
trait_baseline <- traits_long |> group_by(trait) |>
  summarise(baseline = mean(score, na.rm = TRUE), .groups = "drop")
trait_profiles <- traits_long |> filter(party %in% parties, !is.na(score)) |>
  group_by(party, trait) |>
  summarise(n = n(), mean = mean(score), sd = sd(score), .groups = "drop") |>
  filter(n >= 5) |>
  mutate(margin = qt(0.975, df = n - 1) * sd / sqrt(n),
    lower = pmax(1, mean - margin), upper = pmin(7, mean + margin)) |>
  left_join(trait_baseline, by = "trait") |>
  mutate(trait = factor(trait, levels = rev(names(trait_names))),
    panel = paste0(party, " · n = ", n),
    tooltip = paste0(party, "\n", trait_names[as.character(trait)],
      ": ", round(mean, 2), " из 7\n95% ДИ среднего: ", round(lower, 2), "–", round(upper, 2),
      "\nn = ", n, " · Все с правом голоса: ", round(baseline, 2)))
p_traits <- ggplot(trait_profiles, aes(mean, trait)) +
  geom_point(aes(x = baseline), shape = 124, size = 8, colour = "#99A5AD") +
  geom_segment(aes(x = lower, xend = upper, yend = trait, colour = party), linewidth = 0.7) +
  geom_point_interactive(aes(colour = party, tooltip = tooltip, data_id = paste(party, trait)), size = 3.5) +
  scale_colour_manual(values = party_colors, guide = "none") +
  scale_y_discrete(labels = trait_names) +
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  facet_wrap(~panel, ncol = 1) +
  labs(title = "Личностные профили сторонников",
    subtitle = "TIPI · точка — среднее; линия — 95% интервал; серая черта — все с правом голоса",
    x = NULL, y = NULL,
    caption = "Только партии с ≥ 5 валидными ответами на черту. Интервалы среднего по t-распределению, не учитывают самоотбор.\nКороткий TIPI даёт приблизительные оценки; различия на малых группах не следует считать устойчивыми портретами.") +
  chart_theme + theme(plot.caption = element_text(hjust = 0, size = 8))
