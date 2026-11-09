# Дополнительные разделы: как собирались ответы, каналы партий,
# нормы кампании, свободные ответы и качество анкет.
# Оформление и цвета — из vote_charts.R; круг для долей — имеющие право голоса.

wilson <- function(x, n, side = c("lower", "upper")) {
  side <- match.arg(side)
  p <- ifelse(n > 0, x / n, NA_real_)
  center <- (p + 1.96^2 / (2 * n)) / (1 + 1.96^2 / n)
  margin <- 1.96 * sqrt(p * (1 - p) / n + 1.96^2 / (4 * n^2)) / (1 + 1.96^2 / n)
  if (side == "lower") pmax(0, center - margin) else pmin(1, center + margin)
}

# --- Небрежные анкеты --------------------------------------------------------
# TIPI: один балл во всех десяти пунктах или согласие с противоположными
# утверждениями пары (средний разрыв пары от зеркального ответа ≥ 4 балла).
tipi_matrix <- responses |>
  select(all_of(paste0("tipi_", 1:10, "_score"))) |>
  mutate(across(everything(), as.numeric)) |> as.matrix()
tipi_complete <- rowSums(is.na(tipi_matrix)) == 0
tipi_straight <- tipi_complete & apply(tipi_matrix, 1, function(v) length(unique(v)) == 1)
tipi_gap <- rowMeans(abs(tipi_matrix[, 1:5] + tipi_matrix[, 6:10] - 8))
tipi_contradictory <- tipi_complete & !tipi_straight & tipi_gap >= 4
careless_ids <- responses$response_id[tipi_straight | tipi_contradictory]
is_voter <- as.numeric(responses$eligible) == 1 & !is.na(responses$eligible)

quality_rows <- tibble(
  check = c("Личностный тест: один балл во всех пунктах",
    "Личностный тест: согласие с противоположными утверждениями",
    "Второй выбор совпадает с первым",
    "Есть право голоса, но выбран ответ «не имею права голосовать»",
    "«Ни на один канал» вместе с названиями каналов",
    "Полностью совпадающие строки",
    "Личностный тест заполнен не полностью"),
  flag = list(tipi_straight, tipi_contradictory, quality$same_first_second,
    quality$eligibility_turnout_conflict %in% TRUE, quality$conflicting_subscriptions,
    quality$exact_duplicate, quality$incomplete_tipi)) |>
  mutate(all = vapply(flag, function(f) sum(f, na.rm = TRUE), integer(1)),
    voters = vapply(flag, function(f) sum(f & is_voter, na.rm = TRUE), integer(1))) |>
  select(-flag)
quality_table <- function() {
  quality_rows |>
    transmute(Проверка = check, `Все ответы` = all, `С правом голоса` = voters) |>
    knitr::kable(align = c("l", "r", "r"))
}

# Чувствительность: доли партий среди имеющих право голоса с небрежными анкетами и без них.
sensitivity <- voters |>
  transmute(party = as.character(haven::as_factor(vote_first)),
    careless = response_id %in% careless_ids) |>
  mutate(party = replace_na(party, "Нет ответа"))
sensitivity <- bind_rows(
  sensitivity |> mutate(sample = "Все с правом голоса"),
  sensitivity |> filter(!careless) |> mutate(sample = "Без небрежных анкет")) |>
  count(sample, party) |>
  complete(sample, party, fill = list(n = 0)) |>
  group_by(sample) |> mutate(total = sum(n), share = n / total) |> ungroup() |>
  filter(party %in% parties) |>
  group_by(party) |> filter(any(n > 0)) |> ungroup() |>
  mutate(party = factor(party, levels = rev(party_order)),
    sample = factor(sample, levels = c("Все с правом голоса", "Без небрежных анкет")),
    tooltip = paste0(party, "\n", sample, ": ", n, " из ", total,
      " (", scales::percent(share, accuracy = 1), ")"))
p_sensitivity <- ggplot(sensitivity, aes(share, party)) +
  geom_line(aes(group = party, colour = party), linewidth = 1, alpha = 0.35) +
  geom_point_interactive(aes(shape = sample, colour = party, tooltip = tooltip,
    data_id = paste(party, sample)), size = 4, stroke = 1.1, fill = "white") +
  scale_colour_manual(values = party_colors, guide = "none") +
  scale_shape_manual(values = c("Все с правом голоса" = 16, "Без небрежных анкет" = 21), name = NULL) +
  scale_y_discrete(drop = TRUE) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
    breaks = scales::breaks_width(0.05), expand = expansion(mult = c(0.05, 0.1))) +
  labs(title = "Меняют ли небрежные анкеты расклад",
    subtitle = paste0("Доля среди имеющих право голоса · без небрежных анкет: ",
      sum(is_voter) - sum(is_voter & responses$response_id %in% careless_ids), " из ", sum(is_voter)),
    x = NULL, y = NULL,
    caption = "Небрежные — один балл во всех пунктах TIPI или согласие с противоположными утверждениями.\nВ остальных разделах отчёта эти анкеты не исключаются; из карты MCA они исключены.") +
  chart_theme + theme(legend.position = "top", plot.caption = element_text(hjust = 0, size = 8))

# --- Как приходили ответы ----------------------------------------------------
# По оси X — номер анкеты по времени поступления: ночная пауза не растягивает график.
waves <- responses |>
  transmute(time = submitted_at, party = as.character(haven::as_factor(vote_first))) |>
  filter(!is.na(time)) |> arrange(time) |>
  mutate(order = row_number())
wave_lines <- tidyr::expand_grid(waves, line = parties) |>
  filter(line %in% waves$party) |>
  arrange(line, order) |>
  group_by(line) |> mutate(cum = cumsum(party == line)) |> ungroup() |>
  mutate(tooltip = paste0(line, ": ", cum, " к анкете № ", order, "\n",
    format(time, "%d.%m %H:%M", tz = "Europe/Moscow")))
wave_ends <- wave_lines |> filter(order == max(order))
wave_breaks <- waves |> filter(order %in% unique(c(1, seq(10, nrow(waves), 10), nrow(waves)))) |>
  mutate(label = paste0("№ ", order, "\n", format(time, "%H:%M", tz = "Europe/Moscow")))
if (nrow(wave_breaks) > 1 && diff(tail(wave_breaks$order, 2)) < 5) wave_breaks <- wave_breaks[-(nrow(wave_breaks) - 1), ]
pauses <- waves |>
  mutate(gap = as.numeric(difftime(time, lag(time), units = "hours"))) |>
  filter(gap > 2) |>
  transmute(x = order - 0.5, label = paste0("пауза ", round(gap), " ч"),
    vjust = 1.4 + 1.6 * (row_number() - 1))
p_waves <- ggplot(wave_lines, aes(order, cum, colour = line)) +
  geom_vline(data = pauses, aes(xintercept = x), linetype = "dashed", colour = "#BAC3C9") +
  geom_text(data = pauses, aes(x = x, y = Inf, label = label, vjust = vjust), inherit.aes = FALSE,
    hjust = 1.05, size = 3, colour = "#68747B") +
  geom_step(linewidth = 0.9) +
  geom_point_interactive(data = filter(wave_lines, party == line),
    aes(tooltip = tooltip, data_id = paste(line, order)), size = 1.7) +
  ggrepel::geom_text_repel(data = wave_ends, aes(label = paste0(line, "  ", cum)),
    hjust = 0, nudge_x = 1.5, direction = "y", segment.colour = NA,
    size = 3.6, fontface = "bold", show.legend = FALSE, seed = 26) +
  scale_colour_manual(values = party_colors, guide = "none") +
  scale_x_continuous(breaks = wave_breaks$order, labels = wave_breaks$label,
    expand = expansion(mult = c(0.01, 0.14))) +
  scale_y_continuous(breaks = scales::breaks_width(5), expand = expansion(mult = c(0.02, 0.08))) +
  labs(title = "Как приходили ответы",
    subtitle = paste0("Сколько раз партию назвали первым выбором к каждой анкете · все ",
      nrow(waves), " ответов, включая не имеющих права голоса"),
    x = NULL, y = NULL,
    caption = "Крутой участок одной линии — серия ответов её сторонников подряд: ссылка расходилась по партийным сетям.\nПоэтому выборка не случайная, а итог зависит от того, когда собран срез.") +
  chart_theme +
  theme(panel.grid.major.y = element_line(colour = "#E5E7E8", linewidth = 0.25),
    plot.caption = element_text(hjust = 0, size = 8))

# --- Каналы партий -----------------------------------------------------------
subscribed_to <- function(text, party) {
  vapply(strsplit(replace_na(text, ""), ", ", fixed = TRUE), function(v) party %in% v, logical(1))
}
channel_voters <- voters |>
  transmute(first = as.character(haven::as_factor(vote_first)),
    subs = as.character(haven::zap_labels(subscriptions)))
channel_summary <- bind_rows(lapply(parties, function(p) {
  sub <- subscribed_to(channel_voters$subs, p)
  vote <- channel_voters$first %in% p
  tibble(party = p,
    metric = c("Подписчики канала, которые голосуют за партию",
      "Избиратели партии, подписанные на её канал"),
    x = c(sum(sub & vote), sum(sub & vote)),
    n = c(sum(sub), sum(vote)))
})) |>
  filter(n > 0) |>
  mutate(share = x / n, lower = wilson(x, n, "lower"), upper = wilson(x, n, "upper"),
    party = factor(party, levels = rev(party_order)),
    metric = factor(metric, levels = c("Подписчики канала, которые голосуют за партию",
      "Избиратели партии, подписанные на её канал")),
    label = paste0(x, " из ", n),
    tooltip = paste0(party, "\n", metric, ": ", x, " из ", n, " (",
      scales::percent(share, accuracy = 1), ")\n95% ДИ: ",
      scales::percent(lower, accuracy = 1), "–", scales::percent(upper, accuracy = 1)))
all_five_n <- sum(vapply(strsplit(replace_na(channel_voters$subs, ""), ", ", fixed = TRUE),
  function(v) all(parties %in% v), logical(1)))
no_channel_n <- sum(grepl("^Ни на один", channel_voters$subs))
p_channels <- ggplot(channel_summary, aes(share, party, colour = party)) +
  geom_segment(aes(x = 0, xend = share, yend = party), linewidth = 5, alpha = 0.15, lineend = "round") +
  geom_segment(aes(x = lower, xend = upper, yend = party), linewidth = 0.7) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = paste(party, metric)), size = 3) +
  geom_text(aes(x = upper + 0.03, label = label), hjust = 0, size = 3.3, colour = "#485963") +
  facet_wrap(~metric, ncol = 2) +
  scale_colour_manual(values = party_colors, guide = "none") +
  scale_y_discrete(drop = TRUE) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1.3),
    breaks = seq(0, 1, 0.25)) +
  labs(title = "Подписка на канал ещё не голос",
    subtitle = paste0("Только имеющие право голоса (", voter_n, ") · точка и 95% интервал Уилсона"),
    x = NULL, y = NULL,
    caption = paste0("Подписаны на все пять каналов: ", all_five_n, " из ", voter_n,
      "; ни на один: ", no_channel_n, ". Многие читают всех, поэтому охват канала плохо предсказывает голос.")) +
  chart_theme + theme(plot.caption = element_text(hjust = 0, size = 8))

# --- Нормы кампании: критика соперников и интерес к выборам ------------------
likert_chart <- function(var, levels_order, fills, positive, title, subtitle, caption) {
  d <- voters |>
    transmute(party = as.character(haven::as_factor(vote_first)),
      answer = as.character(haven::as_factor(.data[[var]]))) |>
    filter(!is.na(answer))
  rows <- bind_rows(d |> mutate(row = "Все с правом голоса"),
    d |> filter(party %in% parties) |> mutate(row = party)) |>
    count(row, answer) |>
    complete(row, answer = levels_order, fill = list(n = 0)) |>
    group_by(row) |> mutate(total = sum(n), share = n / total,
      positive = sum(n[answer %in% positive]) / total) |> ungroup() |>
    filter(total >= 5) |>
    mutate(row_label = paste0(row, " · n = ", total),
      answer = factor(answer, levels = levels_order),
      tooltip = paste0(row, "\n", answer, ": ", n, " из ", total,
        " (", scales::percent(share, accuracy = 1), ")"))
  row_levels <- rows |> distinct(row, row_label) |>
    mutate(rank = if_else(row == "Все с правом голоса", 0L, match(row, party_order))) |>
    arrange(desc(rank)) |> pull(row_label)
  rows <- rows |> mutate(row_label = factor(row_label, levels = row_levels))
  ggplot(rows, aes(row_label, share, fill = answer)) +
    # group задан явно: иначе ggiraph упорядочит стопку по тексту подсказки, то есть по алфавиту.
    geom_col_interactive(aes(group = answer, tooltip = tooltip, data_id = paste(row, answer)),
      position = position_stack(reverse = TRUE), width = 0.6) +
    geom_text(data = distinct(rows, row_label, positive),
      aes(x = row_label, y = 1.025, label = scales::percent(positive, accuracy = 1)),
      inherit.aes = FALSE, hjust = 0, size = 3.5) +
    scale_fill_manual(values = setNames(fills, levels_order), name = NULL, drop = FALSE) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1.15),
      breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
    coord_flip() +
    guides(fill = guide_legend(nrow = 1, reverse = FALSE)) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, caption = caption) +
    chart_theme + theme(legend.position = "top", legend.text = element_text(size = 9),
      plot.caption = element_text(hjust = 0, size = 8))
}
likert_fills <- c("#B5525C", "#E3A6AC", "#E4E8EB", "#61B0AA", "#087F82", "#C5C7C9")
p_criticism <- likert_chart("criticism",
  c("Полностью не одобряю", "Скорее не одобряю", "Нейтрально", "Скорее одобряю",
    "Полностью одобряю", "Затрудняюсь ответить"),
  likert_fills, c("Скорее одобряю", "Полностью одобряю"),
  "Публичная критика других партий",
  "Распределение ответов · справа — доля одобряющих",
  "Только имеющие право голоса; партии с n ≥ 5. Пропуски исключены из знаменателя строки.")
p_interest <- likert_chart("election_interest",
  c("Совсем не интересными", "Скорее не интересными", "Умеренно интересными",
    "Скорее интересными", "Очень интересными", "Затрудняюсь ответить"),
  likert_fills, c("Скорее интересными", "Очень интересными"),
  "Насколько интересны выборы",
  "Распределение ответов · справа — доля считающих выборы скорее или очень интересными",
  "Только имеющие право голоса; партии с n ≥ 5. Пропуски исключены из знаменателя строки.")

# --- Свободные ответы --------------------------------------------------------
# Ответы с упоминанием конкретных людей не публикуются, даже если это шутка.
people_pattern <- "тимур|петри"
free_text <- responses |>
  transmute(text = trimws(as.character(haven::zap_labels(other_factors)))) |>
  mutate(text = gsub("\\s*\\n\\s*", " ", replace_na(text, "")),
    text = gsub("^[«\"]+|[»\".]+$", "", text),
    low = tolower(text),
    kind = case_when(
      low %in% c("", "ничего", "-", "—", "^_*") ~ "Ничего",
      startsWith(low, "предпочитаю не отвеч") ~ "Не стали отвечать",
      grepl(people_pattern, low) ~ "Упоминают конкретных людей (не публикуются)",
      low %in% c("похуй", "всё равно", "все равно") ~ "Всё равно",
      TRUE ~ "Содержательный ответ"))
free_counts <- free_text |> count(kind) |> arrange(desc(n))
free_answers <- free_text |> filter(kind == "Содержательный ответ") |> pull(text)
markdown_escape <- function(x) {
  x <- gsub("([*_`#\\[\\]<>])", "\\\\\\1", x, perl = TRUE)
  sub("^([0-9]+)\\.", "\\1\\\\.", x)  # «1. …» в начале не должно стать вложенным списком
}
