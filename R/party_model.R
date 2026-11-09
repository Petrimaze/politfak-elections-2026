# Итоговая мультиномиальная логистическая модель первого выбора.
# Только имеющие право голоса, без небрежных анкет; партии с n ≥ 10 в выборке модели.
# Опорная категория — самая крупная партия. Эффекты — отношения относительных шансов (RRR):
# для непрерывных признаков — на одно стандартное отклонение, для бинарных — против опорной группы.
library(nnet)

model_predictors <- c(
  women = "Женщины (против мужчин)",
  junior = "1–2 курс (против 3–4 курса)",
  extraversion = "Общительность",
  stability = "Эмоциональная устойчивость",
  show = "Важность подачи",
  program = "Важность программы")
model_blocks <- c(women = "Кто", junior = "Кто", extraversion = "Характер",
  stability = "Характер", show = "Что важно", program = "Что важно")
z <- function(x) (x - mean(x)) / sd(x)

model_source <- voters |>
  filter(!response_id %in% careless_ids) |>
  transmute(party = as.character(haven::as_factor(vote_first)),
    gender = as.character(haven::as_factor(gender)),
    course = as.character(haven::as_factor(course)),
    extraversion = as.numeric(extraversion),
    stability = as.numeric(emotional_stability),
    show = rowMeans(cbind(as.numeric(factor_2_score), as.numeric(factor_3_score),
      as.numeric(factor_4_score), as.numeric(factor_5_score), as.numeric(factor_6_score)), na.rm = TRUE),
    program = as.numeric(factor_1_score)) |>
  mutate(women = case_when(gender == "Женский" ~ 1, gender == "Мужской" ~ 0),
    junior = case_when(course %in% c("1 курс бакалавриата", "2 курс бакалавриата") ~ 1,
      course %in% c("3 курс бакалавриата", "4 курс бакалавриата", "Магистратура") ~ 0)) |>
  filter(party %in% parties) |>
  filter(if_all(all_of(names(model_predictors)), ~!is.na(.x) & !is.nan(.x)))
model_counts <- model_source |> count(party, sort = TRUE)
model_parties <- model_counts |> filter(n >= 10) |> pull(party)
model_reference <- model_parties[1]
model_left_out <- model_counts |> filter(n < 10)
model_data <- model_source |>
  filter(party %in% model_parties) |>
  mutate(party = factor(party, levels = model_parties),
    across(c(extraversion, stability, show, program), z)) |>
  select(party, all_of(names(model_predictors)))
model_n <- nrow(model_data)

fit_multinom <- function(formula, data) {
  nnet::multinom(formula, data = data, Hess = TRUE, trace = FALSE, maxit = 1000)
}
model_formula <- reformulate(names(model_predictors), response = "party")
party_model <- fit_multinom(model_formula, model_data)
null_model <- fit_multinom(party ~ 1, model_data)
model_lr <- 2 * (as.numeric(logLik(party_model)) - as.numeric(logLik(null_model)))
model_df <- (length(model_parties) - 1) * length(model_predictors)
model_lr_p <- pchisq(model_lr, model_df, lower.tail = FALSE)
model_r2 <- 1 - as.numeric(logLik(party_model)) / as.numeric(logLik(null_model))

# Честная точность: каждого человека предсказываем моделью, обученной без него.
loo_hits <- vapply(seq_len(model_n), function(i) {
  fit <- fit_multinom(model_formula, model_data[-i, ])
  as.character(predict(fit, model_data[i, ])) == as.character(model_data$party[i])
}, logical(1))
model_loo_accuracy <- mean(loo_hits)
model_baseline <- max(table(model_data$party)) / model_n

# Вклад каждого признака: тест отношения правдоподобия без него, df = число партий − 1.
predictor_tests <- bind_rows(lapply(names(model_predictors), function(v) {
  reduced <- fit_multinom(reformulate(setdiff(names(model_predictors), v), response = "party"), model_data)
  lr <- 2 * (as.numeric(logLik(party_model)) - as.numeric(logLik(reduced)))
  tibble(predictor = v, lr = lr, df = length(model_parties) - 1,
    p = pchisq(lr, length(model_parties) - 1, lower.tail = FALSE))
}))
model_test_table <- function() {
  predictor_tests |>
    transmute(Признак = unname(model_predictors[predictor]),
      `LR χ²` = formatC(lr, format = "f", digits = 1, decimal.mark = ","), df,
      p = format_p(p)) |>
    knitr::kable(align = c("l", "r", "r", "r"))
}

# Все попарные сравнения: та же модель, переписанная с другой опорной партией.
pair_rows <- function(fit, reference, keep) {
  co <- coef(fit); se <- summary(fit)$standard.errors
  if (is.null(dim(co))) {
    co <- matrix(co, nrow = 1, dimnames = list(keep, names(co)))
    se <- matrix(se, nrow = 1, dimnames = dimnames(co))
  }
  as_tibble(co, rownames = "party") |>
    pivot_longer(-party, names_to = "predictor", values_to = "estimate") |>
    left_join(as_tibble(se, rownames = "party") |>
      pivot_longer(-party, names_to = "predictor", values_to = "se"), by = c("party", "predictor")) |>
    filter(party %in% keep, predictor != "(Intercept)") |>
    mutate(reference = reference)
}
pair_effects <- bind_rows(lapply(seq_len(length(model_parties) - 1), function(i) {
  reference <- model_parties[i]
  fit <- if (i == 1) party_model else
    fit_multinom(model_formula, mutate(model_data, party = relevel(party, reference)))
  pair_rows(fit, reference, model_parties[(i + 1):length(model_parties)])
}))
contrast_levels <- unlist(lapply(seq_len(length(model_parties) - 1), function(i)
  paste0(model_parties[(i + 1):length(model_parties)], ", а не ", model_parties[i])))
format_ratio <- function(x) {
  vapply(x, function(v) formatC(v, format = "f", digits = if (v < 10) 2 else 1, decimal.mark = ","), character(1))
}
model_effects <- pair_effects |>
  mutate(ratio = exp(estimate), lower = exp(estimate - 1.96 * se), upper = exp(estimate + 1.96 * se),
    p = 2 * pnorm(-abs(estimate / se)),
    clear = lower > 1 | upper < 1,
    contrast = factor(paste0(party, ", а не ", reference), levels = contrast_levels),
    block = factor(unname(model_blocks[predictor]), levels = c("Кто", "Характер", "Что важно")),
    label = factor(unname(model_predictors[predictor]), levels = rev(unname(model_predictors))),
    text = paste0("×", format_ratio(ratio), if_else(ratio > 10 | ratio < 0.1, " ›", "")),
    tooltip = paste0(model_predictors[predictor], "\n", contrast, "\nRRR = ", format_ratio(ratio),
      " · 95% ДИ ", format_ratio(lower), "–", format_ratio(upper), "\np = ", format_p(p),
      if_else(predictor %in% c("women", "junior"), "", "\nНа одно стандартное отклонение")))
# Очень широкие интервалы обрезаются краем шкалы; точные границы — в подсказке.
ratio_limits <- c(0.1, 10)
p_model <- ggplot(model_effects, aes(ratio, label, colour = party)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#F7F9FA") +
  geom_vline(xintercept = 1, colour = "#8B989F", linewidth = 0.5, linetype = "dashed") +
  geom_segment(aes(x = pmax(lower, ratio_limits[1]), xend = pmin(upper, ratio_limits[2]), yend = label),
    linewidth = 1.1, alpha = 0.55, lineend = "round") +
  geom_point_interactive(aes(x = pmin(pmax(ratio, ratio_limits[1]), ratio_limits[2]),
    fill = if_else(clear, as.character(party), "white"),
    tooltip = tooltip, data_id = paste(party, predictor)),
    shape = 21, size = 4.2, stroke = 1.5) +
  geom_text(aes(x = ratio_limits[2] * 1.8, label = text), hjust = 0, size = 3.6,
    fontface = "bold", show.legend = FALSE) +
  facet_grid(block ~ contrast, scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = party_colors, guide = "none") +
  scale_fill_manual(values = c(party_colors, white = "white"), guide = "none") +
  scale_x_log10(limits = c(ratio_limits[1], ratio_limits[2] * 6),
    breaks = c(0.1, 0.3, 1, 3, 10),
    labels = c("×0,1", "×0,3", "×1", "×3", "×10"), oob = scales::squish) +
  coord_cartesian(clip = "off") +
  labs(title = "Что предсказывает выбор партии",
    subtitle = paste0("Мультиномиальная логистическая регрессия · ", model_n,
      " анкет с правом голоса · все попарные сравнения: ", paste(model_parties, collapse = ", "),
      "\nСправа от ×1 — признак повышает шансы выбрать первую партию пары, а не вторую",
      "; закрашенная точка — 95% интервал не пересекает ×1"),
    x = NULL, y = NULL,
    caption = paste0("RRR — отношение относительных шансов при прочих равных; непрерывные признаки — на одно стандартное отклонение.",
      "\nИнтервалы и точки за пределами ×0,1–×10 прижаты к краю шкалы (значок ›); точные значения — в подсказке.",
      "\nМалая выборка: оценки неустойчивы, это разведочная модель, а не прогноз.")) +
  chart_theme +
  theme(strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, face = "bold", colour = "#59666E", hjust = 1),
    strip.text.x = element_text(face = "bold", size = 12, colour = "#182C38", hjust = 0),
    panel.grid.major.x = element_line(colour = "#E5E7E8", linewidth = 0.25),
    axis.text.y = element_text(size = 10.5, colour = "#182C38"),
    plot.caption = element_text(hjust = 0, size = 8),
    plot.margin = margin(14, 30, 14, 14))
