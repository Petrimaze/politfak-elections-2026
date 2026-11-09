# Только классические тесты из stats. Все альтернативы двусторонние.
# Пропуски исключаются отдельно для каждого вопроса, без импутации.
attention_pairs <- voters |>
  transmute(attention = as.numeric(campaign_attention),
    importance = as.numeric(outcome_importance_score)) |>
  filter(attention %in% 1:5, importance %in% 1:5)
attention_test <- cor.test(attention_pairs$attention, attention_pairs$importance,
  method = "spearman", exact = FALSE)
attention_counts <- attention_pairs |> count(attention, importance) |>
  mutate(tooltip = paste0("Внимание: ", attention, " из 5",
    "\nВажность исхода: ", importance, " из 5\nОтветов: ", n))
p_attention <- ggplot(attention_counts, aes(attention, importance)) +
  geom_point_interactive(aes(size = n, tooltip = tooltip,
    data_id = paste(attention, importance)), colour = "#087F8C", alpha = 0.8) +
  scale_size_area(max_size = 13, breaks = scales::breaks_pretty(3), name = "Ответов") +
  scale_x_continuous(breaks = 1:5, limits = c(0.6, 5.4),
    labels = c("Совсем\nне слежу", "Изредка", "Время\nот времени",
      "Внимательно", "Очень\nвнимательно")) +
  scale_y_continuous(breaks = 1:5, limits = c(0.6, 5.4)) +
  labs(title = "Внимание к кампании и важность исхода",
    subtitle = paste0(nrow(attention_pairs), " полных пар ответов с правом голоса · размер круга — число людей"),
    x = "Как внимательно следят за кампанией",
    y = "Важность исхода · 1–5",
    caption = "1 — совсем не важно; 5 — очень важно. Незаполненные и неопределённые ответы исключены.") +
  chart_theme +
  theme(axis.title = element_text(colour = "#59666E", size = 10),
    panel.grid.major.y = element_line(colour = "#E5E7E8", linewidth = 0.25),
    legend.position = "right", plot.caption = element_text(hjust = 0, size = 8))

factor_pairs <- voters |>
  transmute(humor = as.numeric(factor_6_score), program = as.numeric(factor_1_score)) |>
  drop_na()
factor_test <- wilcox.test(factor_pairs$humor, factor_pairs$program,
  paired = TRUE, alternative = "two.sided", exact = FALSE, correct = TRUE)
factor_difference <- median(factor_pairs$humor - factor_pairs$program)

ai_test_data <- voters |>
  transmute(party = as.character(haven::as_factor(vote_first)),
    attitude = as.numeric(ai_attitude)) |>
  filter(party %in% parties, attitude %in% 1:5)
ai_test_counts <- table(ai_test_data$party, ai_test_data$attitude)
# В разреженной таблице Фишер уместнее асимптотического хи-квадрат.
ai_test <- tryCatch(fisher.test(ai_test_counts, workspace = 2e7),
  error = function(e) {
    set.seed(2026)
    fisher.test(ai_test_counts, simulate.p.value = TRUE, B = 100000)
  })
ai_test_method <- if (grepl("simulated", ai_test$method)) "Фишер · Monte Carlo" else "Точный Фишер"
# V — описательная сила связи; это не дополнительная проверка гипотезы.
ai_expected <- outer(rowSums(ai_test_counts), colSums(ai_test_counts)) / sum(ai_test_counts)
ai_chisq <- sum((ai_test_counts - ai_expected)^2 / ai_expected)
ai_cramer_v <- sqrt(ai_chisq /
  (sum(ai_test_counts) * min(nrow(ai_test_counts) - 1, ncol(ai_test_counts) - 1)))

primary_tests <- tibble(
  family = "Основные вопросы",
  question = c("Внимание и важность исхода", "Юмор и программа", "Отношение к ИИ по партиям"),
  method = c("Спирмен", "Парный Уилкоксон", ai_test_method),
  n = c(nrow(attention_pairs), nrow(factor_pairs), nrow(ai_test_data)),
  effect = c(paste0("ρ = ", round(unname(attention_test$estimate), 2)),
    paste0("Медиана разностей (баллы): ", factor_difference),
    paste0("V Крамера = ", round(ai_cramer_v, 2))),
  p = c(attention_test$p.value, factor_test$p.value, ai_test$p.value))

# Один общий тест распределений по каждой черте, без перебора пар партий.
trait_test_rows <- list()
for (trait in names(trait_names)) {
  data <- traits_long |> filter(trait == .env$trait, party %in% parties, !is.na(score)) |>
    add_count(party, name = "group_n") |> filter(group_n >= 5)
  test <- kruskal.test(score ~ party, data = data)
  trait_test_rows[[trait]] <- tibble(family = "TIPI",
    question = unname(trait_names[trait]), method = "Краскел—Уоллис", n = nrow(data),
    effect = paste0("H = ", round(unname(test$statistic), 2),
      "; групп: ", n_distinct(data$party)), p = test$p.value)
}
trait_tests <- bind_rows(trait_test_rows)
trait_test_groups <- traits_long |> filter(party %in% parties, !is.na(score)) |>
  group_by(party, trait) |>
  summarise(n = n(), median = median(score), .groups = "drop") |>
  filter(n >= 5)

# Пары должны быть валидны целиком: некорректный второй ответ не равен «нет».
choice_test_data <- choices |> filter(!invalid_second, !is.na(first), !is.na(second))
choice_test_rows <- list()
for (party in parties) {
  first_only <- sum(choice_test_data$first == party & choice_test_data$second != party)
  second_only <- sum(choice_test_data$first != party & choice_test_data$second == party)
  discordant <- first_only + second_only
  # Точный Мак-Немар — обычный биномиальный тест несовпавших пар.
  p <- if (discordant > 0) binom.test(first_only, discordant, p = 0.5)$p.value else 1
  choice_test_rows[[party]] <- tibble(family = "Первый и второй выбор",
    question = party, method = "Точный Мак-Немар", n = nrow(choice_test_data),
    effect = paste0("Только первый: ", first_only, "; только второй: ", second_only),
    p = p)
}
choice_tests <- bind_rows(choice_test_rows)

# Одна общая поправка Холма на все проверки раздела.
test_results <- bind_rows(primary_tests, trait_tests, choice_tests) |>
  mutate(p_holm = p.adjust(p, method = "holm"),
    conclusion = if_else(p_holm < 0.05,
      "Свидетельства различия / связи", "Недостаточно свидетельств"))
format_p <- function(x) {
  ifelse(x < 0.001, "<0,001", formatC(x, format = "f", digits = 3, decimal.mark = ","))
}
test_table <- function(family_name, question_name = NULL) {
  test_results |> filter(family == family_name,
    if (is.null(question_name)) TRUE else question %in% question_name) |>
    transmute(Вопрос = question, Тест = method, n,
      Оценка = effect,
      p = format_p(p),
      `p · Холм` = format_p(p_holm)) |>
    knitr::kable(align = c("l", "l", "r", "l", "r", "r"))
}
write_csv(test_results, "data/processed/statistical_tests.csv")
write_csv(trait_test_groups, "data/processed/trait_test_groups.csv")
