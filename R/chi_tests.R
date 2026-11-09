# Хи-квадрат Пирсона для описательных разделов: независимы ли два признака.
# Только имеющие право голоса. Пустые строки и столбцы удаляются.
# Если хоть одна ожидаемая частота меньше 5, p считается методом Монте-Карло
# (100 000 таблиц, фиксированный seed): асимптотика на малых клетках ненадёжна.
chi_test <- function(x, y, question, family = "Хи-квадрат") {
  keep <- !is.na(x) & !is.na(y)
  tab <- table(x[keep], y[keep])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  n <- sum(tab)
  expected <- outer(rowSums(tab), colSums(tab)) / n
  sparse <- any(expected < 5)
  set.seed(2026)
  test <- suppressWarnings(if (sparse) chisq.test(tab, simulate.p.value = TRUE, B = 100000)
    else chisq.test(tab, correct = FALSE))
  statistic <- unname(test$statistic)
  df <- (nrow(tab) - 1) * (ncol(tab) - 1)
  v <- sqrt(statistic / (n * (min(dim(tab)) - 1)))
  tibble(family = family, question = question,
    method = if (sparse) "χ² Пирсона · Монте-Карло" else "χ² Пирсона",
    n = n,
    effect = paste0("χ² = ", formatC(statistic, format = "f", digits = 1, decimal.mark = ","),
      "; df = ", df, "; V Крамера = ", formatC(v, format = "f", digits = 2, decimal.mark = ",")),
    p = test$p.value)
}

collapse_small <- function(x, min_n = 5) {
  x[x %in% names(which(table(x) < min_n))] <- NA
  x
}
chi_voters <- voters |>
  transmute(
    party = as.character(haven::as_factor(vote_first)),
    party = if_else(party == "Пока не определился / не определилась", "Не определились", party),
    party = if_else(party %in% c(parties, "Не определились"), party, NA_character_),
    second = as.character(haven::as_factor(vote_second)),
    gender = as.character(haven::as_factor(gender)),
    gender = if_else(gender %in% c("Мужской", "Женский"), gender, NA_character_),
    course = recode(as.character(haven::as_factor(course)),
      "1 курс бакалавриата" = "1 курс", "2 курс бакалавриата" = "2 курс",
      "3 курс бакалавриата" = "3 курс", "4 курс бакалавриата" = "4 курс и старше",
      "Магистратура" = "4 курс и старше", .default = NA_character_),
    previous = as.character(haven::as_factor(vote_2025_2026)),
    criticism = case_when(
      as.numeric(criticism) %in% 1:2 ~ "не одобряют", as.numeric(criticism) == 3 ~ "нейтрально",
      as.numeric(criticism) %in% 4:5 ~ "одобряют"),
    interest = case_when(
      as.numeric(election_interest) %in% 1:2 ~ "не интересны", as.numeric(election_interest) == 3 ~ "умеренно",
      as.numeric(election_interest) %in% 4:5 ~ "интересны"),
    subs = as.character(haven::zap_labels(subscriptions))) |>
  mutate(party = collapse_small(party))

# Второй выбор: только те, кто назвал партию и первой, и (другую) второй.
second_pairs <- chi_voters |>
  filter(party %in% parties, second %in% parties, party != second)

subscription_tests <- bind_rows(lapply(parties, function(p) {
  subscribed <- subscribed_to(chi_voters$subs, p)
  votes <- !is.na(chi_voters$party) & chi_voters$party == p
  if (sum(votes) < 5) return(NULL)
  chi_test(if_else(subscribed, "подписан", "не подписан"),
    if_else(votes, "голосует", "не голосует"), p, family = "Подписка и голос")
}))

chi_tests <- bind_rows(
  chi_test(chi_voters$party, chi_voters$gender, "Выбор и пол"),
  chi_test(chi_voters$party, chi_voters$course, "Выбор и курс"),
  chi_test(chi_voters$party, chi_voters$previous, "Выбор и голос в 2025/26"),
  chi_test(second_pairs$party, second_pairs$second, "Первый и второй выбор"),
  chi_test(chi_voters$party, chi_voters$criticism, "Критика других партий"),
  chi_test(chi_voters$party, chi_voters$interest, "Интерес к выборам"),
  subscription_tests)

test_results <- bind_rows(test_results, chi_tests |>
  mutate(conclusion = if_else(p < 0.05, "Свидетельства различия / связи", "Недостаточно свидетельств")))
verdict <- function(question_name) {
  if (p_of(question_name) < 0.05) "Связь статистически значима (p < 0,05)."
  else "Свидетельств связи недостаточно (p ≥ 0,05)."
}
subscription_significant <- subscription_tests$question[subscription_tests$p < 0.05]
write_csv(test_results, "data/processed/statistical_tests.csv")
