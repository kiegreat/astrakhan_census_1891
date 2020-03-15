
library(sf)
library(ggmap)
library(tidyverse)
library(openxlsx)
library(extrafont)

french <- function(x) format(x, big.mark = " ", decimal.mark = ",")

map <- st_read('data/astrakhan_1896_shape/astrakhan_1896_precincts.shp')

map_centroids <- data.frame(
  precinct = c(1:6),
  lat = c(46.351081, 46.349246, 46.361827, 46.350823, 46.340575, 46.332967),
  lon = c(48.025131, 48.039734, 48.047835, 48.063930, 48.043670, 48.018465)
)

maps_theme <- theme(
  axis.ticks = element_blank(), 
  axis.text = element_blank(), 
  panel.grid = element_blank(),
  strip.background = element_blank(),
  title = element_text(family = 'Roboto Medium'), 
  plot.subtitle = element_text(family = 'Roboto Light'), 
  plot.caption = element_text(family = 'Roboto Light'), 
  legend.title = element_text(family = 'Roboto'),
  text = element_text(family = 'Roboto')
)

style_string <- '&style=element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road.arterial%7Celement:labels%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels%7Cvisibility:off&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Cvisibility:off&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e'

basemap <- get_googlemap(center = c(lon = 48.033574, lat = 46.347869), zoom = 12, inject = style_string, maptype = 'roadmap')

loadfonts(device = "win")

# 1. Map with nationalities ----

df <- read_csv2('data/1_marital_status.csv')

df.1 <- df %>% 
  mutate(nationality = ifelse(nationality %in% c('Русские', 'Татары', 'Армяне', 'Евреи', 'Немцы', 'Персы', 'Поляки'), nationality, 'Другие национальности')) %>% 
  group_by(precinct, nationality) %>% 
  summarise(
    n = sum(num)
  ) %>% 
  group_by(nationality) %>% 
  mutate(
    prc = n/sum(n), 
    prc_cat = case_when(
      prc <= 0.15 ~ 1,
      prc > 0.15 & prc <= 0.3 ~ 2,
      prc > 0.3 & prc <= 0.5 ~ 3,
      prc > 0.5 ~ 4
    ) %>% factor(levels = c(1:4), labels = c('до 15%', '15-30%', '30-50%', 'более 50%'))
  )

map.1 <- map %>% 
  left_join(df.1, by = 'precinct') %>% 
  ungroup() %>% 
  mutate(
    nationality = factor(nationality, levels = c('Русские', 'Татары', 'Армяне', 'Евреи', 'Немцы', 'Персы', 'Поляки', 'Другие национальности'))
  )

gg1 <- ggmap(basemap) +
  geom_sf(data = map.1, aes(fill = prc_cat), inherit.aes = FALSE, alpha = 0.65) +
  facet_wrap(~ nationality, nrow = 2, ncol = 4) +
  scale_fill_brewer(palette = 'Blues') +
  labs(
    x = '',
    y = '',
    title = 'Концентрация национальностей по полицейским участкам',
    subtitle = 'по данным однодневной переписи населения 1891 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R',
    fill = 'Доля от общей \nчисленности'
  ) +
  maps_theme +
  coord_sf(crs = st_crs(4326)) +
  scale_x_continuous(limits = c(47.97, 48.12), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46.29, 46.4), expand = c(0, 0))

gg1

ggsave(filename = 'gg1.png', plot = gg1, device = 'png', dpi = 400, width = 12, height = 7)

# Добавить бейзмап, изменить шрифт, подрехтовать лейблы

rm(df.1, map.1, gg1)

# 2. Nationality vs education ----

df <- read_csv2('data/5_education.csv')

glimpse(df)

df.2 <- df %>% 
  mutate(
    nationality = ifelse(nationality %in% c('Русские', 'Татары', 'Армяне', 'Евреи', 'Немцы', 'Персы', 'Поляки'), nationality, 'Другие \nнациональности'),
    nationality = factor(nationality, levels = c('Русские', 'Татары', 'Армяне', 'Евреи', 'Немцы', 'Персы', 'Поляки', 'Другие \nнациональности')),
    gender = ifelse(gender == 'Мужской', 'Мужчины', 'Женщины')
  ) %>% 
  group_by(gender, nationality, category) %>% 
  summarise(n = sum(num)) %>% 
  group_by(gender, nationality) %>% 
  mutate(prc = n/sum(n), n = sum(n)) %>% 
  filter(category == 'Грамотные')

gg2 <- ggplot(df.2, aes(x = nationality, y = prc, fill = gender)) +
  geom_col(position = 'dodge') +
  geom_text(data = df.2 %>% filter(gender == 'Мужчины'), aes(label = str_c(prc %>% round(2) * 100, '%')), nudge_x = 0.22, nudge_y = 0.03 ) +
  geom_text(data = df.2 %>% filter(gender == 'Женщины'), aes(label = str_c(prc %>% round(2) * 100, '%')), nudge_x = -0.22, nudge_y = 0.03 ) +
  scale_fill_manual(values = c('#F45B69', '#456990')) +
  scale_y_continuous(label = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = 'Грамотность населения по полу и национальности',
    x = '',
    y = 'Доля грамотных',
    fill = '',
    subtitle = 'по данным однодневной переписи населения 1891 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R'
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    legend.position = 'bottom',
    title = element_text(family = 'Roboto Medium'), 
    plot.subtitle = element_text(family = 'Roboto Light'), 
    plot.caption = element_text(family = 'Roboto Light'), 
    legend.title = element_text(family = 'Roboto'),
    text = element_text(family = 'Roboto'),
    axis.text.x = element_text(family = 'Roboto')
  )

gg2

ggsave(filename = 'gg2.png', plot = gg2, device = 'png', dpi = 400, width = 12, height = 7)

rm(df.2, gg2)

# Добавить общую долю граМотных, изменить цвета

# 3. Education level by precincts ----

df <- read_csv2('data/5_education.csv')

df.3 <- df %>% 
  group_by(precinct, category) %>% 
  summarise(n = sum(num)) %>% 
  group_by(precinct) %>% 
  mutate(
    prc = n/sum(n),
    prc_cat = case_when(
      prc > 0.25 & prc <= 0.3 ~ 1,
      prc > 0.3 & prc <= 0.35 ~ 2,
      prc > 0.35 & prc <= 0.4 ~ 3,
      prc > 0.4 ~ 4
    ) %>% factor(levels = c(1:4), labels = c('25-30%', '30-35%', '35-40%', '40-50%'))
  ) %>% 
  ungroup() %>% 
  filter(category == 'Грамотные')

map.3 <- map %>% left_join(df.3, by = 'precinct') %>% left_join(map_centroids, by = 'precinct')

gg3 <- ggmap(basemap) +
  geom_sf(data = map.3, aes(fill = prc_cat), inherit.aes = FALSE, alpha = 0.65) +
  geom_label(data = map.3, aes(x = lon, y = lat, label = scales::percent(prc, accuracy = 1)), label.r = unit(0, "lines"), label.size = 0, alpha = 0.75) +
  scale_fill_brewer(palette = 'OrRd') +
  labs(
    x = '',
    y = '',
    title = 'Грамотность населения по полицейским участкам',
    subtitle = 'по данным однодневной переписи населения 1891 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R',
    fill = 'Доля \nграмотных, %'
  ) +
  maps_theme +
  coord_sf(crs = st_crs(4326)) +
  scale_x_continuous(limits = c(47.97, 48.12), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46.29, 46.4), expand = c(0, 0))

gg3

ggsave(filename = 'gg3.png', plot = gg3, device = 'png', dpi = 400, width = 7, height = 7)

rm(df.3, map.3, gg3)

# 4. Physical disabilities ----

df <- read_csv2('data/3_physical_disabilities.csv')

df.4 <- df %>% 
  group_by(physical_disability) %>% 
  summarise(n = sum(num))

gg4 <- ggplot(df.4, aes(x = reorder(physical_disability, -n), y = n)) +
  geom_col(fill = '#1778b4') +
  geom_text(aes(label = n), nudge_y = 6) +
  labs(
    x = '',
    y = 'Численность, чел.',
    title = 'Физические недостатки населения',
    subtitle = 'по данным однодневной переписи населения 1891 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R'
  ) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    legend.position = 'bottom',
    title = element_text(family = 'Roboto Medium'), 
    plot.subtitle = element_text(family = 'Roboto Light'), 
    plot.caption = element_text(family = 'Roboto Light'), 
    legend.title = element_text(family = 'Roboto'),
    text = element_text(family = 'Roboto'),
    axis.text.x = element_text(family = 'Roboto')
  )

gg4

ggsave(filename = 'gg4.png', plot = gg4, device = 'png', dpi = 300, width = 11, height = 6)

rm(df.4, gg4)

# 5. Employment ----

df <- read_csv2('data/4_income_source.csv')

glimpse(df)

df.5 <- df %>% 
  mutate(income_source = ifelse(income_source == 'На средства родителей и родственников', 'На средства родственников', income_source)) %>% 
  group_by(precinct, income_source) %>% 
  summarise(n = sum(num)) %>% 
  group_by(income_source) %>% 
  mutate(
    prc = n / sum(n),
    prc_cat = case_when(
      prc <= 0.10 ~ 1,
      prc > 0.10 & prc <= 0.25 ~ 2,
      prc > 0.25 & prc <= 0.5 ~ 3,
      prc > 0.5 ~ 4
    ) %>% factor(levels = c(1:4), labels = c('до 10%', '10-25%', '25-50%', 'более 50%'))
  ) %>% 
  ungroup()

map.5 <- map %>% left_join(df.5, by = 'precinct')

gg5 <- ggmap(basemap) +
  geom_sf(data = map.5, aes(fill = prc_cat), inherit.aes = FALSE, alpha = 0.65) +
  scale_fill_brewer(palette = 'YlOrRd') +
  labs(
    x = '',
    y = '',
    title = 'Концентрация населения по источникам дохода',
    subtitle = 'по данным однодневной переписи населения 1891 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R',
    fill = 'Доля'
  ) +
  facet_wrap(~ income_source, nrow = 3, ncol = 5) +
  maps_theme +
  theme(strip.text.x = element_text(size = 8)) +
  coord_sf(crs = st_crs(4326)) +
  scale_x_continuous(limits = c(47.97, 48.12), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46.29, 46.4), expand = c(0, 0))

gg5

ggsave(filename = 'gg5.png', plot = gg5, device = 'png', dpi = 400, width = 10, height = 7)

rm(df.5, map.5, gg5)

# 6. Estates ----

df <- read_csv2('data/10_social_estates.csv')

df.6 <- df %>% 
  group_by(social_estate) %>% 
  mutate(
    prc = num / sum(num),
    prc_cat = case_when(
      prc <= 0.10 ~ 1,
      prc > 0.10 & prc <= 0.25 ~ 2,
      prc > 0.25 & prc <= 0.5 ~ 3,
      prc > 0.5 ~ 4
    ) %>% factor(levels = c(1:4), labels = c('до 10%', '10-25%', '25-50%', 'более 50%'))
  ) %>% 
  ungroup()

map.6 <- map %>% left_join(df.6, by = 'precinct')

gg6 <- ggmap(basemap) +
  geom_sf(data = map.6, aes(fill = prc_cat), inherit.aes = FALSE, alpha = 0.65) +
  scale_fill_brewer(palette = 'YlGn') +
  labs(
    x = '',
    y = '',
    title = 'Концентрация дворохозяйств по сословиям',
    subtitle = 'по данным однодневной переписи населения 1891 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R',
    fill = 'Доля'
  ) +
  facet_wrap(~ social_estate, nrow = 2, ncol = 4) +
  maps_theme +
  coord_sf(crs = st_crs(4326)) +
  scale_x_continuous(limits = c(47.97, 48.12), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46.29, 46.4), expand = c(0, 0))

gg6

ggsave(filename = 'gg6.png', plot = gg6, device = 'png', dpi = 400, width = 12, height = 7)

rm(df.6, map.6)

# 7. Avg height ----

df <- read_csv2('data/8_buildings_by_storeys.csv')

df.7 <- df %>% 
  mutate(
    storeys = case_when(
      storeys == 'Одноэтажные' ~ 1,
      storeys == 'Двухэтажные' ~ 2,
      storeys == 'Трехэтажные' ~ 3,
      storeys == 'Четырехэтажные' ~ 4
    ),
    total_storeys = storeys * num
  ) %>% 
  group_by(precinct) %>% 
  summarise(avg_storeys = sum(total_storeys) / sum(num))

map.7 <- map %>% left_join(df.7, by = 'precinct') %>% left_join(map_centroids, by = 'precinct')

gg7 <- ggmap(basemap) +
  geom_sf(data = map.7, aes(fill = avg_storeys), inherit.aes = FALSE, alpha = 0.65) +
  geom_label(data = map.7, aes(x = lon, y = lat, label = round(avg_storeys, 2)), label.r = unit(0, "lines"), label.size = 0, alpha = 0.75) +
  maps_theme +
  coord_sf(crs = st_crs(4326)) +
  scale_x_continuous(limits = c(47.97, 48.12), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46.29, 46.4), expand = c(0, 0)) +
  scale_fill_gradient(low = '#F48F76', high = '#833825') +
  labs(
    x = '',
    y = '',
    title = 'Высотность застройки по полицейским участкам',
    subtitle = 'по данным однодневной переписи населения 1891 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R',
    fill = 'Средняя этажность \nзданий'
  )

gg7

ggsave(filename = 'gg7.png', plot = gg7, device = 'png', dpi = 400, width = 7, height = 7)
rm(df.7, map.7, gg7)

# 8. Buildings by material ----

df <- read_csv2('data/6_buildings.csv')

df.8 <- df %>% 
  group_by(precinct, walls_material) %>% 
  summarise(n = sum(num)) %>% 
  group_by(precinct) %>% 
  mutate(pct = n / sum(n)) %>% 
  filter(walls_material == 'Камень')

map.8 <- map %>% left_join(df.8, by = 'precinct') %>% left_join(map_centroids, by = 'precinct')

gg8 <- ggmap(basemap) +
  geom_sf(data = map.8, aes(fill = pct), inherit.aes = FALSE, alpha = 0.65) +
  geom_label(data = map.8, aes(x = lon, y = lat, label = scales::percent(round(pct, 2))), label.r = unit(0, "lines"), label.size = 0, alpha = 0.75) +
  maps_theme +
  coord_sf(crs = st_crs(4326)) +
  scale_x_continuous(limits = c(47.97, 48.12), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46.29, 46.4), expand = c(0, 0)) +
  scale_fill_gradient(low = '#858B8E', high = '#202B31', labels = scales::percent) +
  labs(
    x = '',
    y = '',
    title = 'Доля строений с каменными стенами по полицейским участкам',
    subtitle = 'по данным однодневной переписи населения 1891 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R',
    fill = 'Доля'
  )

gg8

ggsave(filename = 'gg8.png', plot = gg8, device = 'png', dpi = 400, width = 7, height = 7)
rm(df.8, map.8, gg8)

# 9. Households share with insurance and average insurance amount ----

df <- read_csv2('data/9_households_by_insurance.csv')

df.9 <- df %>% 
  mutate(
    insurance_amount = str_replace_all(insurance_amount, pattern = ' ', replacement = '') %>% as.numeric(),
    avg_ins = insurance_amount / num
  ) %>% 
  group_by(precinct) %>% 
  mutate(pct = num / sum(num)) %>% 
  filter(got_insurance == 'Застрахованные имения')

map.9 <- map %>% left_join(df.9, by = 'precinct') %>% left_join(map_centroids, by = 'precinct')

gg9 <- ggmap(basemap) +
  geom_sf(data = map.9, aes(fill = pct), inherit.aes = FALSE, alpha = 0.75) +
  geom_label(data = map.9, aes(x = lon, y = lat, label = scales::percent(pct, accuracy = 1)), label.r = unit(0, "lines"), label.size = 0, alpha = 0.75) +
  maps_theme +
  coord_sf(crs = st_crs(4326)) +
  scale_x_continuous(limits = c(47.97, 48.12), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46.29, 46.4), expand = c(0, 0)) +
  scale_fill_gradient(low = '#9BC3D3', high = '#247BA0', labels = scales::percent) +
  labs(
    x = '',
    y = '',
    title = 'Доля застрахованных имений по полицейским участкам',
    subtitle = 'по данным однодневной переписи населения 1891 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R',
    fill = 'Доля'
  )

gg9

ggsave(filename = 'gg9.png', plot = gg9, device = 'png', dpi = 400, width = 7, height = 7)
rm(df.9, map.9, gg9)

# 10. Population density ----

map_centroids <- data.frame(
  precinct = c(1:6),
  lat = c(46.351081, 46.349246, 46.361827, 46.350823, 46.340575, 46.332967),
  lon = c(48.018131, 48.039734, 48.047835, 48.063930, 48.043670, 48.018465)
)

df <- read_csv2('data/1_marital_status.csv')

areas <- unclass(st_area(map))

df.10 <- df %>% 
  group_by(precinct) %>% 
  summarise(pop = sum(num)) %>% 
  ungroup() %>% 
  mutate(area = areas)

df.10 <- df.10 %>% mutate(area = area / 1000000, dens = pop / area)

map.10 <- map %>% left_join(df.10, by = 'precinct') %>% left_join(map_centroids, by = 'precinct')

gg10 <- ggmap(basemap) +
  geom_sf(data = map.10, aes(fill = dens), inherit.aes = FALSE, alpha = 0.75) +
  geom_label(data = map.10, aes(x = lon, y = lat, label = french(round(dens, 0))), label.r = unit(0, "lines"), label.size = 0, alpha = 0.75) +
  maps_theme +
  coord_sf(crs = st_crs(4326)) +
  scale_x_continuous(limits = c(47.97, 48.12), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46.29, 46.4), expand = c(0, 0)) +
  scale_fill_gradient(low = '#A8DFB4', high = '#1A936F', label = french) +
  labs(
    x = '',
    y = '',
    title = 'Плотность населения по полицейским участкам',
    subtitle = 'по данным однодневной переписи населения 1891 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R',
    fill = 'Плотность населения, \nчел./км²'
  )

gg10

ggsave(filename = 'gg10.png', plot = gg10, device = 'png', dpi = 400, width = 7, height = 7)
rm(df.10, map.10, gg10)















