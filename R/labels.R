# Фиксированные коды, независимые от порядка ответов в выгрузке.
category_levels <- list(
  vote_first = c(parties, "Пока не определился / не определилась", "Не стал бы / не стала бы голосовать", "Предпочитаю не отвечать"),
  vote_second = c(parties, "Нет партии второго предпочтения", "Пока не определился / не определилась", "Предпочитаю не отвечать"),
  eligibility = c("Да", "Нет, так как состою в партии", "Нет, так как не являюсь студентом бакалавриата по политологии НИУ ВШЭ"),
  turnout_intent = c("Точно нет", "Скорее нет", "Скорее да", "Точно да", "Пока не знаю", "Не имею права голосовать на этих выборах"),
  campaign_attention = c("Совсем не слежу", "Слежу изредка", "Слежу время от времени", "Слежу внимательно", "Слежу очень внимательно", "Затрудняюсь ответить"),
  subscription_intent = c("Нет", "Да", "Пока не знаю"),
  criticism = c("Полностью не одобряю", "Скорее не одобряю", "Нейтрально", "Скорее одобряю", "Полностью одобряю", "Затрудняюсь ответить"),
  election_interest = c("Совсем не интересными", "Скорее не интересными", "Умеренно интересными", "Скорее интересными", "Очень интересными", "Затрудняюсь ответить"),
  vote_2025_2026 = c("Красная кнопка", "DON TREAD ON ME", "САМ РЕШУ", "Дух политфака", "Не голосовал(а)"),
  vote_2024_2025 = c("Не терпите", "НА ГРАНИ", "гоббс/локк", "исток", "Оркестр", "Не голосовал(а)"),
  course = c(paste(1:4, "курс бакалавриата"), "Магистратура", "Не учусь на политфаке", "Другое", "Предпочитаю не отвечать"),
  track = c("Аналитика", "Политическая теория", "Политическое управление", "Трек ещё не выбран", "Другое / ко мне не относится", "Предпочитаю не отвечать"),
  gender = c("Мужской", "Женский", "Предпочитаю не отвечать"),
  ai_attitude = c("Приемлем любой ИИ-контент, включая нейрослоп", "Приемлем качественно отредактированный ИИ-контент, но нейрослоп не одобряю", "Приемлем ИИ только для вспомогательных задач (например, корректуры)", "Не одобряю использование ИИ в контенте партий", "Мне безразлично, использован ли ИИ", "Затрудняюсь ответить")
)
responses_text <- responses
value_labels <- list()
for (v in names(responses)) {
  label <- dictionary$question[match(v, dictionary$variable)]
  if (inherits(responses[[v]], "POSIXt")) {
    attr(responses[[v]], "label") <- label
  } else if (v %in% names(category_levels)) {
    levels <- category_levels[[v]]
    unexpected <- setdiff(na.omit(unique(responses[[v]])), levels)
    if (length(unexpected)) stop("Неизвестные категории в ", v, ": ", paste(unexpected, collapse="; "))
    labels <- setNames(seq_along(levels), levels)
    responses[[v]] <- haven::labelled(match(responses[[v]], levels), labels=labels, label=label)
    value_labels[[v]] <- tibble(variable=v, code=seq_along(levels), value_label=levels)
  } else if (is.logical(responses[[v]])) {
    responses[[v]] <- haven::labelled(as.integer(responses[[v]]), c("Нет"=0L,"Да"=1L), label=label)
  } else if (is.numeric(responses[[v]])) {
    maximum <- if (grepl("^tipi_", v)) 7L else 5L
    labels <- if (grepl("_score$", v)) setNames(seq_len(maximum), as.character(seq_len(maximum))) else NULL
    responses[[v]] <- haven::labelled(responses[[v]], labels=labels, label=label)
  } else {
    # Текст, исходные шкальные ответы и множественный выбор не кодируем как одну категорию.
    responses[[v]] <- haven::labelled(responses[[v]], label=label)
  }
}
value_labels <- bind_rows(value_labels)
