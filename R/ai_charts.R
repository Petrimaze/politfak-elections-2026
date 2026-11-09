# Номинальные категории: не усредняем отношение к ИИ как числовую шкалу.
ai_labels <- c(
  "Приемлем любой ИИ-контент, включая нейрослоп" = "Допускают даже нейрослоп",
  "Приемлем качественно отредактированный ИИ-контент, но нейрослоп не одобряю" = "Допускают ИИ с редактурой",
  "Приемлем ИИ только для вспомогательных задач (например, корректуры)" = "Только вспомогательные задачи",
  "Не одобряю использование ИИ в контенте партий" = "Не одобряют ИИ в контенте",
  "Мне безразлично, использован ли ИИ" = "Безразлично",
  "Затрудняюсь ответить" = "Затрудняются ответить",
  "Нет ответа" = "Нет ответа")
ai_data <- voters |>
  transmute(party = as.character(haven::as_factor(vote_first)),
    answer = replace_na(as.character(haven::as_factor(ai_attitude)), "Нет ответа"))
ai_summary <- ai_data |> count(answer) |>
  mutate(share = n / voter_n,
    label = forcats::fct_reorder(recode(answer, !!!ai_labels), n),
    tooltip = paste0(answer, "\n", n, " из ", voter_n, " (",
      scales::percent(share, accuracy = 1), ")"))
p_ai <- ggplot(ai_summary, aes(share, label)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = answer),
    width = 0.5, fill = "#087F8C") +
  geom_text(aes(x = share + 0.012, label = scales::percent(share, accuracy = 1)),
    hjust = 0, size = 3.8, fontface = "bold", colour = "#182C38") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
    breaks = scales::breaks_width(0.1), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Где проходит граница приемлемого ИИ",
    subtitle = paste0("Все ", voter_n, " ответов с правом голоса · один вариант на человека"),
    x = NULL, y = NULL,
    caption = "Полная формулировка и число ответов — при наведении. Принятие отредактированного ИИ не означает принятие нейрослопа.") +
  chart_theme + theme(plot.caption = element_text(hjust = 0, size = 8))

ai_matrix <- ai_data |> filter(party %in% parties) |> count(party, answer) |>
  complete(party, answer = unique(ai_data$answer), fill = list(n = 0)) |>
  group_by(party) |> mutate(total = sum(n), share = n / total) |> ungroup() |>
  mutate(label = factor(recode(answer, !!!ai_labels),
    levels = recode(names(ai_labels), !!!ai_labels)),
    row = paste0(party, " · n = ", total),
    row = factor(row, levels = unique(row[order(match(party, rev(party_order)))])),
    tooltip = paste0(party, "\n", answer, "\n", n, " из ", total,
      " (", scales::percent(share, accuracy = 1), ")"))
p_ai_party <- ggplot(ai_matrix, aes(label, row, fill = share)) +
  geom_tile_interactive(aes(tooltip = tooltip, data_id = paste(party, answer)),
    colour = "white", linewidth = 2) +
  geom_text(aes(label = if_else(n > 0, scales::percent(share, accuracy = 1), ""),
    colour = share > 0.45), size = 3.8, fontface = "bold") +
  scale_colour_manual(values = c("FALSE" = "#182C38", "TRUE" = "white"), guide = "none") +
  scale_fill_gradient(low = "#F1F4F5", high = "#087F8C", limits = c(0, 1),
    labels = scales::label_percent(), name = "Доля в строке") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, 17)) +
  labs(title = "Отношение к ИИ среди сторонников партий",
    subtitle = "Проценты внутри партии · численность и полные ответы — при наведении",
    x = NULL, y = NULL,
    caption = "Только имеющие право голоса. Малые группы показаны описательно; различия не означают статистической значимости.") +
  chart_theme + theme(panel.grid = element_blank(), legend.position = "bottom",
    axis.text.x = element_text(size = 9), plot.caption = element_text(hjust = 0, size = 8))
