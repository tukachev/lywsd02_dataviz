library(tidyverse)
library(ggthemes)
library(shadowtext)
library(ggtext)
library(showtext)
library(patchwork)
library(emojifont)

# here::here()

# settings ---------------------------------------------------------------------
set_dpi <- 300
main_font <- "PT Sans"
file_path <- here::here("lywsd02_ext.log")
days <- 6

# load fonts -------------------------------------------------------------------
font_add_google(name = main_font, family = main_font)
showtext_opts(dpi = set_dpi)
showtext_auto()

# load data --------------------------------------------------------------------
data <- read_csv(
  file_path,
  col_types = cols(
    timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    timestamp_unix = col_double(),
    mac = col_character(),
    device_time = col_character(),
    device_time_unix = col_double(),
    temp = col_double(),
    hum = col_double(),
    battery = col_double(),
    emoticon = col_character(),
    comfort = col_character()
  ),
  na = "N/A"
)

data <- data %>%
  mutate(
    date = as.Date(timestamp),
    time_of_day = as.POSIXct(format(timestamp, "%H:%M"), format = "%H:%M", tz = "UTC"),
    is_last_day = if_else(date == max(date), "Последний день", "Предыдущие дни")
  )

mt_max <- max(data$temp) + 0.5
mt_min <- min(data$temp)

mh_max <- max(data$hum) + 1
mh_min <- min(data$hum) - 1

data <- data %>%
  filter(date >= max(date) - days)

previous_data <- data %>% filter(is_last_day == "Предыдущие дни")
last_data <- data %>% filter(is_last_day == "Последний день")

last_day_last_obs <- data %>%
  filter(date == max(date)) %>%
  slice_max(timestamp, n = 1)

# temp_color <- "#B22222"
# hum_color <- "#00008B"
previous_days_color <- "#8080804D" # c 30% прозрачностью

# colorblind safe
temp_color <- "#f33b15"
hum_color <- "#0f2080"

last_observation <- data %>%
  tail(1)

previous_observation <- data %>%
  slice(n() - 1)

temp_diff <- round(last_observation$temp - previous_observation$temp, 2)
hum_diff <- round(last_observation$hum - previous_observation$hum, 2)

mac_address <- last_day_last_obs$mac
battery_level <- last_day_last_obs$battery

subtitle_text <- glue::glue(
  "<span style='font-family:{main_font};'>Ежедневный мониторинг показателей термогигрометра Xiaomi каждые 15 минут<br>по Bluetooth Low Energy на Raspberry Pi Zero W</span> за текущие сутки и <span style='color:gray70;'>**предыдущие {days} дней**</span><br><br><span style='color:gray30;'>Температура, °C</span>"
)

caption_text <- glue::glue(
  "<span style='font-family:{main_font};'>Данные устройства: Xiaomi Temperature and Humidity Monitor Clock<br>
   LYWSD02 | MAC: {mac_address} | Уровень батареи: <span style='font-family:EmojiOne;'>{emoji('battery')}</span>{battery_level}%<br>
   Визуализация: Юрий Тукачёв, декабрь 2024 @weekly_charts</span>"
)

# Temperature plot -------------------------------------------------------------
temp <-
  ggplot(data,
         aes(
           x = time_of_day,
           y = temp,
           group = date,
           color = is_last_day
         )) +
  geom_step(
    data = previous_data,
    show.legend = FALSE,
    aes(linetype = is_last_day),
    linewidth = 0.35
  ) +
  geom_step(
    data = last_data,
    show.legend = FALSE,
    aes(linetype = is_last_day),
    linewidth = 0.6
  ) +
  geom_point(
    data = last_day_last_obs,
    aes(x = time_of_day, y = temp),
    color = temp_color,
    size = 1.5
  ) +
  scale_color_manual(values = c("Предыдущие дни" = previous_days_color, "Последний день" = temp_color)) +
  scale_linetype_manual(values = c("Предыдущие дни" = "solid", "Последний день" = "solid")) +
  scale_y_continuous(
    breaks = seq(floor(mt_min / 0.5) * 0.5, floor(mt_max / 0.5) * 0.5, 0.5),
    limits = c(floor(mt_min / 0.5) * 0.5 - 0.25, floor(mt_max / 0.5) * 0.5) + 0.2,
    labels = c(sprintf(
      "%.1f", seq(floor(mt_min / 0.5) * 0.5, floor(mt_max / 0.5) * 0.5 - 0.5, 0.5)
    ), glue::glue("{floor(mt_max / 0.5) * 0.5}"))
  ) +
  scale_x_datetime(date_labels = "%H:%M",
                   breaks = "3 hours",
                   expand = c(0.095, 0)) +
  geom_shadowtext(
    data = last_day_last_obs,
    aes(
      label = glue::glue(
        "{format(date, '%d.%m.%Y')} {format(time_of_day, '%H:%M')}\n{temp}°C"
      )
    ),
    vjust = ifelse(temp_diff >= 0.05, -0.35, 1.35),
    size = 4,
    color = temp_color,
    bg.color = "white",
    fontface = "bold",
    family = main_font,
    hjust = 0
  ) +
  labs(
    title = glue::glue(
      "**Изменение комнатной <span style='color:{temp_color};'>температуры</span> и <span style='color:{hum_color};'>влажности</span> в течение суток**"
    ),
    subtitle = subtitle_text,
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(family = main_font),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(10, 0, 10, 0),
    aspect.ratio = 0.25,
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(
      size = 13,
      hjust = 1,
      margin = margin(0, 5, 0, 0)
    ),
    title = element_markdown(size = 20),
    axis.ticks.length.y = unit(.3, "cm"),
    axis.ticks.length.x = unit(.2, "cm"),
    # axis.text.x = element_text(size = 13),
    plot.subtitle = element_markdown(
      size = 16,
      color = "black",
      lineheight = 1.2,
      margin = margin(5, 0, 5, 0)
    )
  )

# Humidity plot ----------------------------------------------------------------
hum <- ggplot(data, aes(
  x = time_of_day,
  y = hum,
  group = date,
  color = is_last_day
)) +
  geom_step(
    data = previous_data,
    show.legend = FALSE,
    aes(linetype = is_last_day),
    linewidth = 0.35
  ) +
  geom_step(
    data = last_data,
    show.legend = FALSE,
    aes(linetype = is_last_day),
    linewidth = 0.6
  ) +
  geom_point(
    data = last_day_last_obs,
    aes(x = time_of_day, y = hum),
    color = hum_color,
    size = 1.5
  ) +
  scale_color_manual(values = c("Предыдущие дни" = previous_days_color, "Последний день" = hum_color)) +
  scale_linetype_manual(values = c("Предыдущие дни" = "solid", "Последний день" = "solid")) +
  scale_y_continuous(
    breaks = seq(floor(mh_min / 5) * 5, floor(mh_max / 5) * 5, 5),
    limits = c(floor(mh_min / 5) * 5 - 1, floor(mh_max / 5) * 5 + 1),
    labels = c(as.character(seq(
      floor(mh_min / 5) * 5, floor(mh_max / 5) * 5 - 5, 5
    )), glue::glue("{floor(mh_max / 5) * 5}"))
  ) +
  scale_x_datetime(date_labels = "%H:%M",
                   breaks = "3 hours",
                   expand = c(0.095, 0)) +
  geom_shadowtext(
    data = last_day_last_obs,
    aes(
      label = glue::glue(
        "{format(date, '%d.%m.%Y')} {format(time_of_day, '%H:%M')}\n{hum}%"
      )
    ),
    vjust = ifelse(hum_diff >= 0.05, -0.35, 1.35),
    size = 4,
    color = hum_color,
    bg.color = "white",
    fontface = "bold",
    family = main_font,
    hjust = 0
  ) +
  labs(
    subtitle = glue::glue("<span style='color:gray30;'>Влажность, %</span>"),
    x = "Время суток",
    y = NULL,
    caption = caption_text
  ) +
  theme(
    text = element_text(family = main_font, size = 20),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(10, 0, 10, 0),
    aspect.ratio = 0.25,
    # = ar ?
    panel.background = element_blank(),
    axis.title.x = element_text(size = 16, colour = "gray30"),
    axis.text.y = element_text(
      size = 13,
      hjust = 1,
      margin = margin(0, 5, 0, 0)
    ),
    axis.ticks.length.y = unit(.3, "cm"),
    axis.ticks.length.x = unit(.2, "cm"),
    axis.ticks = element_line(colour = "gray30"),
    axis.text.x = element_text(size = 13),
    plot.subtitle = element_markdown(
      size = 16,
      color = "black",
      margin = margin(0, 0, 10, 0)
    ),
    plot.caption = element_markdown(
      hjust = 0,
      lineheight = 1.5,
      color = "gray60",
      size = 12,
      margin = margin(t = 20)
    ),
  )

temp / hum

# ggsave -----------------------------------------------------------------------
ggsave(
  "lywsd02_temp_hum.png",
  scale = 1,
  bg = "white",
  dpi = set_dpi,
  width = 10.5,
  height = 8.5
)

#PDF
ggsave(
  "lywsd02_temp_hum.pdf",
  scale = 1,
  bg = "white",
  dpi = 300,
  width = 10.5,
  height = 8.5
)
