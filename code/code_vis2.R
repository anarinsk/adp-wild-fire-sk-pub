librarian::shelf(rprojroot, here, pillar,
                 tidyverse, tidyverse/lubridate,
                 ggmap, raster, rgdal, rmapshaper,
                 broom, gpclib, viridis,
                 sf, ggspatial, showtext)
#install.packages("gpclib", type = "source")
# https://kuduz.tistory.com/1042
# http://www.gisdeveloper.co.kr/?p=2332


### Prelude ----
font_add_google("Nanum Gothic", "nanumgothic")

cp_2_utf8 <- function(col){
  iconv(col, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)
}
change_enc <- function(var_name, df){
  #
  enq_var_name <- enquo(var_name)
  df %>%
    mutate(!!enq_var_name := cp_2_utf8(!!enq_var_name))
  #
}
gen_uppercode <- function(var1, var2, df1, df2, cut_from_right, only_geom=T){
  enq_1 <- enquo(var1)
  enq_2 <- enquo(var2)
  chr_1 <- deparse(substitute(var1))
  df1$geometry <- NULL
  df2 %>%
    mutate(
      !!enq_1 := str_extract(!!enq_2, paste0("^.{",cut_from_right,"}"))
    ) %>%
    left_join(
      df1, by=chr_1
    )
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
transform_CTP <- function(CTPv){
  #
  str_remove(CTPv, "도") %>%
    str_remove('광역시') %>%
    str_remove('특별자치시|특별자치|특별시') %>%
    str_replace('경상', '경') %>%
    str_replace('전라', '전') %>%
    str_replace('충청', '충')
  #
}
transform_SIG <- Vectorize( function(SIGv){
  #
  str_squish(SIGv) %>%
    str_remove('구$') %>%
    str_remove('(시|군|특별자치시)$') %>%
    str_remove('시 ') -> v1

  if(str_length(v1)>=3){
    str_sub(v1, 1, 3) -> p1
    str_remove(v1, p1) -> p2
    p1 %>% str_remove('(시)$') -> p1
    str_c(p1, p2)} else
    {
      v1
    }
} )

### Load map files
### prj 파일이 있어야 한다!
korea_1 <- st_read(here::here('data', 'shp', 'CTPRVN_201602', 'TL_SCCO_CTPRVN.shp'))
korea_2 <- st_read(here::here('data', 'shp', 'SIG_201602', 'TL_SCCO_SIG.shp'))
label_1 <- change_enc(CTP_KOR_NM, korea_1)
label_2 <- change_enc(SIG_KOR_NM, korea_2)

### Redering simplified map
## Transform label_1
gen_uppercode(CTPRVN_CD, SIG_CD, label_1, label_2, 2) %>%
  mutate(
    CTP_KOR_NM = transform_CTP(CTP_KOR_NM),
    SIG_KOR_NM = transform_SIG(SIG_KOR_NM),
    addr_mat = str_c(CTP_KOR_NM, SIG_KOR_NM)
  ) %>% ms_simplify(keep = 0.0015, keep_shapes = F) %>% st_transform(3857) -> df_admap

### Rendering Kor underlying map
my_key <- 'your-api-code'
register_google(key = my_key)
map <- get_map(location='south korea', zoom=7, maptype='roadmap', color='bw')
#ggmap(map)
ggmap_bbox <- function(map) {
  #
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
  #
}
map_bg <- ggmap_bbox(map)

#saveRDS(df_admap, here::here('data', "df_admap.rds"))
#saveRDS(map_bg, here::here('data', "map_bg.rds"))

df0 <- readRDS(here::here('data', 'munged.rds')) %>% filter(year(date0) <= 2018)

### Load data and assign address ----

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
filter_year(0, df0) -> tdf0

### Make df for choropleth

gen_sf <- function(dfv, yearv){
#
  filter_year(yearv, df0) -> dff0
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
gen_sf(df_admap, 0) -> df_sf

####

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
      windows()
      print(gg) } else {
      ggsave(str_glue("{titlev}.png"), gg, width= 6)
    }
#
}
gen_cpleth(map_bg, df_sf, damage, T, "지역별 산불 피해 (2003~2018)", "피해 면적 (ha)")


tmp_func <- function(yearv, dfv){

  gen_sf(dfv, yearv) -> dff0
  gen_cpleth(map_bg, dff0, damage, T, str_glue("산불 피해 ({yearv} 년도)"), "피해 면적 (ha)")

}

#tmp_func(2018, df_admap)

seq(2003, 2018) %>%
  walk(function(x){tmp_func(x, df_admap)})

