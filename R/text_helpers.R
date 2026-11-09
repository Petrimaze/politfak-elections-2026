# Слова для текста отчёта. Числа и выводы считаются из данных,
# поэтому текст не устаревает, когда в таблицу приходят новые ответы.
pct <- function(x) scales::percent(x, accuracy = 1)
dec <- function(x, digits = 1) formatC(x, format = "f", digits = digits, decimal.mark = ",")
plural <- function(n, forms) {
  n10 <- n %% 10; n100 <- n %% 100
  if (n10 == 1 && n100 != 11) forms[1] else if (n10 %in% 2:4 && !(n100 %in% 12:14)) forms[2] else forms[3]
}
people <- function(n) paste(n, plural(n, c("человек", "человека", "человек")))
votes_word <- function(n) paste(n, plural(n, c("голос", "голоса", "голосов")))
is_sig <- function(question_name) p_of(question_name) < 0.05
said <- function(question_name, yes, no) if (is_sig(question_name)) yes else no
# Падежи названий партий: склоняются только Зеленка и СТАЯ.
party_acc <- function(x) dplyr::recode(x, "Зеленка" = "Зеленку", "СТАЯ" = "СТАЮ")
party_gen <- function(x) dplyr::recode(x, "Зеленка" = "Зеленки", "СТАЯ" = "СТАИ", "СВЯЗЬ" = "СВЯЗИ")
and_list <- function(x) if (length(x) <= 1) paste(x) else paste0(paste(head(x, -1), collapse = ", "), " и ", tail(x, 1))

# Главный расклад среди тех, кто может голосовать.
overall_votes <- vote_summary |> filter(breakdown == "overall") |>
  mutate(party = as.character(party))
voters_total <- overall_votes$total[1]
party_rank <- overall_votes |> filter(party %in% parties, n > 0) |> arrange(desc(n))
undecided <- overall_votes |> filter(party == "Пока не определился / не определилась")
undecided_share <- if (nrow(undecided)) undecided$share else 0
leaders_overlap <- party_rank$lower[1] <= party_rank$upper[2]
lead_line <- paste0("Впереди **", party_rank$party[1], "** (", pct(party_rank$share[1]), "), за ним ",
  party_rank$party[2], " (", pct(party_rank$share[2]), ") и ",
  party_rank$party[3], " (", pct(party_rank$share[3]), "). ",
  if (leaders_overlap) paste0("Но разрыв между первыми двумя — ", votes_word(party_rank$n[1] - party_rank$n[2]),
    ", и это в пределах погрешности: кто на самом деле впереди, по этим данным сказать нельзя.")
  else "Отрыв лидера больше погрешности опроса.",
  " Ещё не определились ", pct(undecided_share), ".")

factor_share <- function(label) {
  factor_distribution |> filter(as.character(factor) == label) |> distinct(important) |> pull(important)
}
top_factor <- factor_distribution |> distinct(factor, important) |> arrange(desc(important)) |> slice(1)
program_share <- factor_share("Содержание программы партии")

# Второй выбор: самая частая пара «первая партия → вторая».
top_second <- second_pairs |> count(party, second, sort = TRUE) |> slice(1)
top_second_line <- paste0("Чаще всего запасной вариант сторонников ", party_gen(top_second$party),
  " — ", top_second$second, " (", people(top_second$n), ").")

# Отношение к ИИ: самый частый ответ.
ai_top <- ai_summary |> arrange(desc(n)) |> slice(1)

# Критика и интерес: доли среди всех, кто может голосовать.
share_of <- function(var, levels) {
  x <- as.numeric(voters[[var]]); x <- x[x %in% 1:5]
  mean(x %in% levels)
}
approve_share <- share_of("criticism", 4:5)
interest_share <- share_of("election_interest", 4:5)

# Характер: какие черты различаются и у кого они выше и ниже всего.
trait_lines <- vapply(names(trait_names), function(t) {
  name <- unname(trait_names[t])
  if (p_of(name) >= 0.05) return(NA_character_)
  d <- trait_profiles |> filter(trait == t) |> arrange(desc(mean))
  paste0(tolower(name), " (выше всего у сторонников ", party_gen(d$party[1]),
    ", ниже всего — у сторонников ", party_gen(d$party[nrow(d)]), ")")
}, character(1))
trait_lines <- trait_lines[!is.na(trait_lines)]

# Итоговая модель: заметные эффекты простыми словами.
effect_words <- list(
  women = c("больше женщин", "больше мужчин"),
  junior = c("больше младших курсов", "больше старших курсов"),
  extraversion = c("общительнее", "замкнутее"),
  stability = c("спокойнее", "тревожнее"),
  show = c("больше ценят подачу", "меньше ценят подачу"),
  program = c("больше ценят программу", "меньше ценят программу"))
# Одна строка на пару партий: «Зеленка против НАПАС: спокойнее, больше ценят программу».
model_findings <- model_effects |> filter(clear) |>
  arrange(contrast, desc(abs(log(ratio)))) |>
  mutate(word = vapply(seq_along(predictor),
    function(i) effect_words[[predictor[i]]][if (ratio[i] > 1) 1 else 2], character(1))) |>
  group_by(contrast, party, reference) |>
  summarise(words = and_list(word), .groups = "drop") |>
  mutate(line = paste0("**", party, " против ", reference, "**: ", words, "."))

factor_line <- if (top_factor$factor == "Содержание программы партии") {
  paste0("Главное при выборе — программа партии: она важна для ", pct(program_share), " ответивших.")
} else {
  paste0("Выбирают скорее за подачу, чем за содержание: пункт «", top_factor$factor, "» важен для ",
    pct(top_factor$important), " ответивших, программа партии — только для ", pct(program_share), ".")
}
