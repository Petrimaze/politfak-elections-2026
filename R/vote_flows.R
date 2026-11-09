# Переходы только среди тех, кто сейчас имеет право голосовать.
flows <- responses |>
  filter(as.numeric(eligible) == 1) |>
  transmute(previous = as.character(haven::as_factor(vote_2025_2026)),
    current = as.character(haven::as_factor(vote_first))) |>
  mutate(across(everything(), ~replace_na(.x, "Нет ответа"))) |>
  count(previous, current, name = "n") |>
  mutate(current = factor(current, levels = party_order))

# Вертикальные позиции узлов; промежутки отделяют партии друг от друга.
left_nodes <- flows |> count(previous, wt = n, name = "n") |>
  arrange(previous == "Не голосовал(а)", desc(n)) |>
  mutate(top = cumsum(n + 1.5) - 1.5, bottom = top - n)
right_nodes <- flows |> count(current, wt = n, name = "n") |>
  arrange(current) |>
  mutate(top = cumsum(n + 1.5) - 1.5, bottom = top - n)

flows <- flows |>
  left_join(select(left_nodes, previous, left_bottom = bottom), by = "previous") |>
  left_join(select(right_nodes, current, right_bottom = bottom), by = "current") |>
  group_by(previous) |> arrange(current, .by_group = TRUE) |>
  mutate(left_low = left_bottom + cumsum(n) - n, left_high = left_low + n) |>
  ungroup() |>
  group_by(current) |> arrange(previous, .by_group = TRUE) |>
  mutate(right_low = right_bottom + cumsum(n) - n, right_high = right_low + n) |>
  ungroup() |>
  mutate(id = paste(previous, current, sep = " → "),
    tooltip = paste0(previous, " → ", current, "\n", n, " чел."))

# Плавные ленты: одинаковая толщина на обоих концах, высота пропорциональна n.
ribbons <- flows |>
  tidyr::crossing(edge = c("low", "high"), step = seq(0, 1, length.out = 60)) |>
  mutate(x = 0.015 + step * 0.97,
    bend = 3 * step^2 - 2 * step^3,
    y = if_else(edge == "low",
      left_low + (right_low - left_low) * bend,
      left_high + (right_high - left_high) * bend),
    path = if_else(edge == "low", step, 2 - step + 0.001)) |>
  arrange(id, path)

nodes <- bind_rows(
  left_nodes |> transmute(x = 0, name = previous, n, bottom, top,
    fill = "#ADB4BA", side = "left"),
  right_nodes |> transmute(x = 1, name = as.character(current), n, bottom, top,
    fill = unname(party_colors[as.character(current)]), side = "right")) |>
  mutate(id = paste(side, name), tooltip = paste0(name, "\n", n, " чел."),
    label = paste0(dplyr::recode(name, !!!party_labels), "  ", n))


# Второй прошлый год читается справа налево к тем же текущим партиям.
flows_24 <- responses |>
  filter(as.numeric(eligible) == 1) |>
  transmute(previous = as.character(haven::as_factor(vote_2024_2025)),
    current = as.character(haven::as_factor(vote_first))) |>
  mutate(across(everything(), ~replace_na(.x, "Нет ответа"))) |>
  count(previous, current, name = "n") |>
  mutate(current = factor(current, levels = party_order))

past_24 <- flows_24 |> count(previous, wt = n, name = "n") |>
  arrange(previous == "Не голосовал(а)", desc(n)) |>
  mutate(top = cumsum(n + 1.5) - 1.5, bottom = top - n)

flows_24 <- flows_24 |>
  left_join(select(past_24, previous, past_bottom = bottom), by = "previous") |>
  left_join(select(right_nodes, current, current_bottom = bottom), by = "current") |>
  group_by(previous) |> arrange(current, .by_group = TRUE) |>
  mutate(past_low = past_bottom + cumsum(n) - n, past_high = past_low + n) |>
  ungroup() |>
  group_by(current) |> arrange(previous, .by_group = TRUE) |>
  mutate(current_low = current_bottom + cumsum(n) - n, current_high = current_low + n) |>
  ungroup() |>
  mutate(id = paste("2024/25", previous, current, sep = ":"),
    tooltip = paste0("2024/25: ", previous, " → сейчас: ", current, "\n", n, " чел."))

ribbons_24 <- flows_24 |>
  crossing(edge = c("low", "high"), step = seq(0, 1, length.out = 60)) |>
  mutate(x = 1.985 - step * 0.97, bend = 3 * step^2 - 2 * step^3,
    y = if_else(edge == "low",
      past_low + (current_low - past_low) * bend,
      past_high + (current_high - past_high) * bend),
    path = if_else(edge == "low", step, 2 - step + 0.001)) |>
  arrange(id, path)

ribbons_both <- bind_rows(
  ribbons |> mutate(id = paste0("2025/26:", id),
    tooltip = paste0("2025/26: ", tooltip)),
  ribbons_24)
nodes_both <- bind_rows(
  nodes |> mutate(side = if_else(side == "right", "center", side)),
  past_24 |> transmute(x = 2, name = previous, n, bottom, top,
    fill = "#ADB4BA", side = "right",
    id = paste("2024/25", previous), tooltip = paste0("2024/25: ", previous, "\n", n, " чел."),
    label = paste0(previous, "  ", n))) |>
  mutate(label = dplyr::recode(name, !!!party_labels))

p_flows <- ggplot() +
  geom_polygon_interactive(data = ribbons_both,
    aes(x, y, group = id, fill = current, tooltip = tooltip, data_id = id),
    alpha = 0.42, colour = NA) +
  scale_fill_manual(values = party_colors, guide = "none") +
  geom_rect_interactive(data = nodes_both,
    aes(xmin = x - 0.015, xmax = x + 0.015, ymin = bottom, ymax = top,
      tooltip = tooltip, data_id = id), fill = nodes_both$fill, colour = NA) +
  geom_text(data = filter(nodes_both, side == "left"),
    aes(x = -0.035, y = (bottom + top) / 2, label = label),
    hjust = 1, size = 3.3, colour = "#485963") +
  geom_text(data = filter(nodes_both, side == "right"),
    aes(x = 2.035, y = (bottom + top) / 2, label = label),
    hjust = 0, size = 3.3, colour = "#485963") +
  geom_label_interactive(data = filter(nodes_both, side == "center"),
    aes(x = 1, y = (bottom + top) / 2, label = label, tooltip = tooltip, data_id = id),
    size = 3.3, fontface = "bold", fill = "white", colour = "#182C38", linewidth = 0) +
  scale_x_continuous(limits = c(-0.48, 2.48), expand = c(0, 0)) +
  scale_y_reverse(expand = expansion(mult = c(0.04, 0.04))) +
  labs(title = "Откуда приходят сторонники партий",
    subtitle = paste0("2025/26 → ТЕКУЩИЙ ВЫБОР ← 2024/25 · ",
      sum(flows$n), " ответов с правом голоса\nЦвет — текущая партия; ширина — число людей. Каждый человек учтён один раз с каждой стороны."),
    caption = "Это два сравнения с текущим выбором, а не последовательный маршрут через три года. Прошлое голосование — по воспоминаниям.") +
  theme_void(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(face = "bold", size = 22, colour = "#182C38", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, colour = "#59666E", margin = margin(b = 24)),
    plot.caption = element_text(size = 9, colour = "#68747B", hjust = 0, margin = margin(t = 18)),
    plot.margin = margin(20, 20, 20, 20))
