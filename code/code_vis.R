librarian::shelf(tidyverse,
                 tidyverse/lubridate,
                 viridis, showtext, ggridges,
                 knitr, kableExtra, wrapr)

### Two VIS mission
### 1. Calendar Heatmap
### 2. Choropleth

### Prelude ----
readRDS(here::here('data', 'munged.rds')) -> tdf0
tdf0 %>% filter(year(date0) <= 2018) -> tdf0
font_add_google("Nanum Gothic", "nanumgothic")

## my gg-template
my_fill_theme <- function(ggv,
                          labelv = "LABEL",
                          titlev = "TITLE",
                          discretev = FALSE) {
  ggv +
    scale_fill_viridis(
      discrete = discretev,
      name = labelv,
      option = 'plasma',
      direction = -1
    ) +
      labs(x = NULL, y = NULL, title = titlev) +
      theme_bw() +
      theme(
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
}
my_color_theme <- function(ggv,
                           labelv = "LABEL",
                           titlev = "TITLE") {
  ggv +
    scale_color_viridis(name = labelv, discrete = TRUE) +
    labs(x = NULL, y = NULL, title = titlev) +
    theme_bw() +
    theme(
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9),
      #panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}
draw_calhmap <- function(dfv,
           wrapv = year0,
           labelv = "LABEL",
           titlev = "TITLE THIS!") {
    #
    enq_wrapv <- enquo(wrapv)

    dfv %>%
      ggplot() +
      aes(x = weekday,
          y = reorder(month0, desc(month0)),
          fill = n) +
      geom_tile(color = "white", size = 0.1) -> gg

    deparse(substitute(wrapv)) -> chr_wrapv
    is_wrap <- ifelse(chr_wrapv %in% names(dfv), "Y", "N")

    if (is_wrap == "N") {
      gg
    }
    else {
       gg + facet_wrap(vars(!!enq_wrapv))
    }
  }
my_pointline <- function(gg) {
  ggplot(gg) +
    aes(x = factor(year),
        y = values,
        color = vars) +
    geom_line(color = "gray",
              size = 1.5,
              alpha = 0.5) +
    geom_point(size = 2) +
    geom_line(aes(
      x = factor(year),
      y = values,
      group = vars
    ), size = 1) -> gg1
}

## showtext related
{
showtext_win <- function(ob) {
  showtext_auto()
  windows()
  ob
}
showtext_mac <- function(ob) {
  X11()
  ob
}
showtext_os <- function(ob){

  Sys.info()['sysname'] -> os
  showtext_auto()

  if (os == "Windows") {showtext_win(ob)} else {showtext_mac(ob)}

}
showtext_go <- function(ob, on_screen = T) {
  #
  if (on_screen == T) {
    showtext_os(ob)
  } else {
    ggsave(
      "SHOWTEXT.png",
      ob,
      width = 10,
      height = 4,
      dpi = 96
    )
  }
  #
}
}

# PLOT 1: 월별 요일별 산불발생 건수 ----
tdf0 %>%
  mutate(
    month0 = month(date0) %>% as_factor(),
    weekday = wday(date0, label=T)
  ) %>%
  count(month0, weekday) %>%  # count data with group month, weekday
  draw_calhmap(labelv="건 수", titlev="월/요일 별 산불 발생 (2003~2018)") %>%
  my_fill_theme("건 수", "월/요일 별 산불 (2003 ~ 2018)") %>%
  showtext_go(.)

# PLOT 2: 월별 요일별 산불발생 피해 면적 ----
tdf0 %>%
  filter(damage < 100) %>%
  mutate(
    month0 = month(date0) %>% as_factor(),
    weekday = wday(date0, label=T)
  ) %>%
  group_by(month0, weekday) %>%
  summarise(
    n = sum(damage)
  )  %>% # count data with group month, weekday
  draw_calhmap(damage) %>%
  my_fill_theme("피해면적(ha)", "월/요일 별 산불 (2003 ~ 2018)") %>%
  showtext_go()

# TABLE: 대형 산불 현황
tdf0 %>%
  filter(damage >= 100) %>%
  group_by(year(date0), month(date0)) %>%
  summarise(
    damage = sum(damage)
  ) %>% kable(col.names=c('연도', '월', '피해 면적(ha)')) %>% kable_styling()

## 연도별 피해 면적 및 발생 건수
( tdf0 %>%
  group_by(year(date0)) %>%
  summarise(
    n = n(),
    damage = sum(damage)
  ) %>%
  rename(year = `year(date0)`,
         `건수` = n ,
         `피해 면적` = damage
         ) %>%
  pivot_longer(-year, names_to="vars", values_to="values") %>%
  ggplot() +
  aes(x=as_factor(year), y=values) +
  geom_col() + facet_grid(rows = vars(vars), scales="free") ) %>%
  my_fill_theme("", "연도별 발생 건수와 피해 면적") %>%
  showtext_go()

tdf0 %>%
  group_by(year(date0)) %>%
  summarise(
    avg = mean(damage),
    med = median(damage)
  ) %>%
  rename(year = `year(date0)`,
         `평균` = avg,
         `중간값` = med
  ) %>%
  pivot_longer(-year, names_to="vars", values_to="values") -> gg0

tdf0 %>%
  filter(damage < 100) %>%
  group_by(year(date0)) %>%
  summarise(
    avg = mean(damage),
    med = median(damage)
  ) %>%
  rename(year = `year(date0)`,
         `평균` = avg,
         `중간값` = med
  ) %>%
  pivot_longer(-year, names_to="vars", values_to="values") -> gg1


gg0 %>% my_pointline() %>% my_color_theme('구분', '건 당 피해 규모 (전체)') %>% showtext_go()
  #ggsave("lineplot_1.png", ., width = 10, height = 4, dpi = 96)
gg1 %>% my_pointline() %>% my_color_theme('구분', '건 당 피해 규모 (100ha 미만)') %>% showtext_go()
  #ggsave("lineplot_2.png", ., width = 10, height = 4, dpi = 96)

#### Not proper
tdf0 %>%
  mutate(
    group = month(date0)
  ) %>%
  filter(damage < 5) %>%
  ggplot() +
  aes(x = damage, y=group %>% as_factor()) +
  geom_density_ridges() +
  scale_fill_viridis(name = "피해 면적(ha)", option = "C") +
  labs(title = '')

tdf0 %>%
  mutate(
    origin_cat = case_when(
      origin_cat == "입" ~ "입산자 실화",
      origin_cat == "쓰" ~ "쓰레기 소각",
      origin_cat == "논" ~ "농산폐기물 소각",
      origin_cat == "기" ~ "미상",
      TRUE ~ "기타"
    )
  ) -> tdf0a

tdf0a %>%
  group_by(year(date0), origin_cat) %>%
  summarise(
    n = n(),
    damage = sum(damage),
    avg = damage/n
  ) %>%
    rename(year = `year(date0)`)-> gg3

tdf0 %>% count(origin_cat)

tdf0a %>%
  group_by(origin_cat) %>%
  summarise(
    n = n(),
    damage0 = sum(damage),
    avg = mean(damage),
    med = median(damage)
  ) %>%
  arrange(desc(damage0)) %>% kable(col.names = c('발화 원인', '건 수', '피해 면적', '평균', '중간값')) %>%
  kable_styling()

### 연도별 산불 현황

(gg3 %>%
filter(between(damage, 0, 100)) %>%
  mutate(
    origin_cat = fct_infreq(origin_cat)
  ) %>%
  pivot_longer(
    -c(year, origin_cat),
    names_to='vars',
    values_to="values"
  ) %>%
  filter(vars == "damage") %>%
  ggplot() +
    aes(x=as_factor(year), y=values, fill=origin_cat) +
    geom_col() ) %>%
  my_fill_theme("발화 원인", "원인별 산불 현황 (2003 ~ 2018)", discretev=T) %>%
  showtext_go()

