librarian::shelf(tidyverse,
                 tidyverse/lubridate,
                 viridis, showtext, ggridges,
                 knitr, kableExtra,
                 tidyverse, tidyverse/lubridate,
                 ggmap, raster, rgdal, rmapshaper,
                 broom, gpclib, viridis,
                 sf, ggspatial, showtext)

librarian::shelf(thomasp85/patchwork, latex2exp,
                 rmdformats, rstudio/fontawesome)

font_add_google("Nanum Gothic", "nanumgothic")
### Functions

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
gen_sf <- function(dfv, yearv){
  #
  filter_year(yearv, tdf0) -> dff0
  dfv %>% full_join(dff0, by=c('addr_mat'='join_col')) -> dff1
  dff1 %>% filter(!is.na(SIG_CD)) %>%
    dplyr::select(one_of('SIG_CD', 'geometry')) -> geom
  dff1$geometry <- NULL

  dff1 %>%
    filter(
      ismat_1=="N", ismat_2=="N"
    ) %>% pluck('addr_mat1') -> listv

  listv %>% map_df(function(x){ integrate_SIG(x, dff1) }) -> tdf2a
  dff1 %>% filter(!SIG_CD %in% tdf2a$SIG_CD) -> tdf2b

  bind_rows(tdf2b, tdf2a) %>% arrange(SIG_CD) %>%
    dplyr::select(-one_of('addr_mat1', 'addr_mat2', 'ismat_1', 'ismat_2')) %>%
    replace_na(
      list(n = 0,
           damage = 0)
    ) %>%
    filter(!is.na(SIG_CD)) -> tdf3
  tdf3 %>% left_join(geom, by = 'SIG_CD') -> tdf4
  st_geometry(tdf4) <- tdf4$geometry
  tdf4 %>% st_transform(3857)
}
filter_year <- function(yearv, dfv){

  if (yearv==0) {
    dfv -> dff1
  } else {
    dfv %>% filter(year(date0) == yearv) -> dff1
  }

  dff1 %>%
    mutate(
      addr_mat1 = str_c(loc1, loc2),
      addr_mat2 = str_c(loc1, loc2, loc3)
    ) %>%
    group_by(addr_mat1) %>%
    summarise(
      addr_mat2 = first(addr_mat2),
      n = n(),
      damage = sum(damage)
    ) -> dff0

  dff0$addr_mat1[dff0$addr_mat1=="세종연서"] <- "세종세종"

  dff0 %>% mutate(
    ismat_1 = if_else(addr_mat1 %in% df_admap$addr_mat, "Y", "N"),
    ismat_2 = if_else(addr_mat2 %in% df_admap$addr_mat, "Y", "N"),
    join_col = case_when(
      ismat_1 == "Y" & ismat_2 == "N" ~ addr_mat1,
      ismat_1 == "N" & ismat_2 == "Y" ~ addr_mat2
    )
  )

}
gen_cpleth <- function(mapv, dfv, var, do_png = F, titlev="TITLE", labelv="LABEL", alphav=0.5){
  #
  enq_var <- enquo(var)

  ggmap(mapv) +
    coord_sf(crs = st_crs(3857)) +
    #ggplot(df_sf) +
    geom_sf(data=dfv, aes(fill = !!enq_var), color = 'darkgreen', alpha=alphav, inherit.aes = FALSE) +
    scale_fill_viridis(option="plasma", direction=-1) +
    theme_void() + labs(fill = labelv, title = titlev) -> gg

  if (do_png==F) {
    showtext_auto()
    print(gg) } else {
      ggsave(str_glue("{titlev}.png"), gg, width= 6)
    }
  #
}
integrate_SIG <- function(unitv, dfv){
  #
  dfv %>%
    filter(addr_mat1==unitv) %>%
    dplyr::select(one_of('n', 'damage')) -> vdf1

  dfv %>%
    filter(str_detect(addr_mat, unitv)) %>%
    mutate(
      n = vdf1$n,
      damage = vdf1$damage
    )
  #
}
### Load df

tdf0 <- readRDS(here::here('data', 'munged.rds')) %>%
  filter(year(date0) <= 2018)
map_bg <- readRDS(here::here('data', 'map_bg.rds'))
df_admap <- readRDS(here::here('data', 'df_admap.rds'))

### df generation

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

### Testing Modules
#gen_sf(df_admap, 2011) -> df_sf
#gen_cpleth(map_bg, df_sf, damage, F, "지역별 산불 피해 (2003~2018)", "피해 면적 (ha)")
