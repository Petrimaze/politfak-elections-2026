# Запуск из корня проекта: source("R/prepare_data.R")
# Всегда читает Google Sheets заново; при ошибке останавливается.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

# ID таблицы не хранится в репозитории: по нему открываются сырые ответы.
# Положите его в файл sheet_id.txt (он в .gitignore) или в переменную POLITFAK_SHEET_ID.
sheet_id <- Sys.getenv("POLITFAK_SHEET_ID")
if (!nzchar(sheet_id) && file.exists("sheet_id.txt")) {
  sheet_id <- trimws(readLines("sheet_id.txt", warn = FALSE)[1])
}
if (!nzchar(sheet_id)) stop("Нет ID Google-таблицы: создайте sheet_id.txt или задайте POLITFAK_SHEET_ID.")
sheet_url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id,
                    "/export?format=csv")
# Первая вкладка таблицы ответов. Смена доступа на закрытый потребует OAuth.
raw_data <- tryCatch(
  read_csv(sheet_url, col_types = cols(.default = col_character()),
           name_repair = "minimal", na = c("", "NA"), progress = FALSE),
  error = function(e) stop("Не удалось обновить данные из Google Sheets: ",
                           conditionMessage(e), call. = FALSE)
)
loaded_at <- Sys.time()
if (anyDuplicated(names(raw_data))) stop("В таблице повторяются заголовки столбцов.")

# Сопоставляем по смысловым фрагментам, а не по номеру столбца.
patterns <- c(
  submitted_at = "^Отметка времени$",
  vote_first = "^Если бы выборы",
  vote_second = "^Какая партия для вас на втором",
  eligibility = "^Имеете ли вы право",
  turnout_intent = "^Планируете ли вы голосовать",
  campaign_attention = "^Насколько внимательно",
  subscriptions = "^На какие каналы партий",
  subscription_intent = "^Планируете ли вы подписаться",
  criticism = "^Как вы относитесь к публичной критике",
  election_interest = "^Насколько интересными",
  vote_2025_2026 = "^За какую партию.*2025/2026",
  vote_2024_2025 = "^За какую партию.*2024/2025",
  course = "^На каком курсе",
  track = "^На каком треке",
  gender = "^Ваш пол$",
  other_factors = "^Что ещё влияет",
  ai_attitude = "^Как вы относитесь к использованию партиями ИИ",
  belonging = "^Насколько вы чувствуете себя частью",
  outcome_importance = "^Насколько лично для вас важно"
)
factor_labels <- c("Содержание программы партии", "Визуальное оформление публикаций",
                   "Качество видеороликов", "Регулярность публикаций",
                   "Необычные форматы кампании", "Юмор", "Практическая помощь студентам",
                   "Офлайн-мероприятия", "Личное знакомство с участниками партии")
tipi_labels <- c("открытого, полного энтузиазма", "критичного, склонного спорить",
                 "надежного и дисциплинированного", "тревожного, меня легко расстроить",
                 "открытого для нового опыта, сложного", "замкнутого, тихого",
                 "сочувствующего, сердечного", "неорганизованного, беспечного",
                 "спокойного, эмоционально устойчивого", "обыкновенного, не творческого")
find_column <- function(pattern) {
  hits <- grep(pattern, names(raw_data))
  if (length(hits) != 1) stop("Ожидался ровно один столбец: ", pattern,
                            "; найдено: ", length(hits), call. = FALSE)
  hits
}
indices <- vapply(patterns, find_column, integer(1))
find_label <- function(label) {
  hits <- which(endsWith(trimws(names(raw_data)), paste0("[", label, "]")))
  if (length(hits) != 1) stop("Не найден однозначный столбец: ", label)
  hits
}
indices <- c(indices,
             setNames(vapply(factor_labels, find_label, integer(1)), paste0("factor_", 1:9)),
             setNames(vapply(tipi_labels, find_label, integer(1)), paste0("tipi_", 1:10)))
dictionary <- tibble(variable = names(indices), question = names(raw_data)[indices])
responses <- raw_data[, indices, drop = FALSE]
names(responses) <- names(indices)
responses <- responses |> mutate(across(everything(), ~na_if(trimws(.x), "")))
responses$response_id <- seq_len(nrow(responses)) # Номер строки текущей выгрузки, не ID человека.
responses$submitted_at <- as.POSIXct(responses$submitted_at,
                                    format = "%d.%m.%Y %H:%M:%S", tz = "Europe/Moscow")
responses$eligible <- ifelse(is.na(responses$eligibility), NA,
                             responses$eligibility == "Да")

# Оригинальные ответы сохраняются рядом с числовыми; неопределённость = NA.
scale_number <- function(x, maximum) {
  value <- suppressWarnings(as.integer(sub("^([0-9]+).*", "\\1", x)))
  value[!is.na(value) & !(value %in% seq_len(maximum))] <- NA_integer_
  value
}
for (v in c(paste0("factor_", 1:9), "belonging", "outcome_importance")) {
  responses[[paste0(v, "_score")]] <- scale_number(responses[[v]], 5)
}
for (v in paste0("tipi_", 1:10)) {
  responses[[paste0(v, "_score")]] <- scale_number(responses[[v]], 7)
}
# TIPI-RU: среднее двух пунктов, обратный пункт = 8 - ответ.
# При отсутствии любого из двух ответов показатель не рассчитывается.
# Сергеева, Кириллов, Джумагулова (2016), doi:10.17759/exppsy.2016090311.
responses <- responses |> mutate(
  extraversion = (tipi_1_score + 8 - tipi_6_score) / 2,
  agreeableness = (8 - tipi_2_score + tipi_7_score) / 2,
  conscientiousness = (tipi_3_score + 8 - tipi_8_score) / 2,
  emotional_stability = (8 - tipi_4_score + tipi_9_score) / 2,
  openness = (tipi_5_score + 8 - tipi_10_score) / 2
)
parties <- c("СВЯЗЬ", "СТАЯ", "Зеленка", "Дозревай", "НАПАС")
subscriptions_long <- responses |> select(response_id, subscriptions) |>
  separate_longer_delim(subscriptions, delim = ", ") |>
  rename(channel = subscriptions)
factors_long <- responses |> select(response_id, ends_with("_score")) |>
  pivot_longer(matches("^factor_[0-9]+_score$"), names_to = "factor", values_to = "score") |>
  select(response_id, factor, score) |>
  mutate(label = factor_labels[as.integer(sub("factor_([0-9]+)_score", "\\1", factor))])

# Только флаги: без удаления похожих ответов и без догадок о личности.
duplicate_exact <- duplicated(raw_data) | duplicated(raw_data, fromLast = TRUE)
quality <- responses |> transmute(response_id,
  invalid_time = is.na(submitted_at),
  exact_duplicate = duplicate_exact,
  same_first_second = vote_first %in% parties & vote_first == vote_second,
  conflicting_subscriptions = grepl("Ни на один", subscriptions) &
    grepl(",", subscriptions),
  eligibility_turnout_conflict = eligible & turnout_intent == "Не имею права голосовать на этих выборах",
  incomplete_tipi = rowSums(is.na(pick(all_of(paste0("tipi_", 1:10, "_score"))))) > 0
)
missingness <- tibble(variable = names(responses),
                      missing = vapply(responses, function(x) sum(is.na(x)), integer(1)))
derived_dictionary <- tibble(
  variable = setdiff(names(responses), dictionary$variable),
  question = ifelse(grepl("_score$", variable),
                    "Числовой ответ; неопределённость и пропуски = NA", "Производная переменная; см. R/prepare_data.R")
)
dictionary <- bind_rows(dictionary, derived_dictionary)
source("R/labels.R")
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(responses = responses, responses_text = responses_text,
             value_labels = value_labels, raw = raw_data, quality = quality,
             dictionary = dictionary, subscriptions = subscriptions_long,
             factors = factors_long, loaded_at = loaded_at, source = sheet_url),
        "data/processed/dataset.rds")
write_csv(responses_text, "data/processed/responses.csv", na = "")
write_csv(value_labels, "data/processed/value_labels.csv", na = "")
write_csv(dictionary, "data/processed/dictionary.csv", na = "")
write_csv(quality, "data/processed/quality.csv", na = "")
message("Загружено ответов: ", nrow(responses), "; время: ", loaded_at)
