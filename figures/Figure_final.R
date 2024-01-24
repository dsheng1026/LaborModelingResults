# labor related metrics ----
# boxplot across regions, mean indicates global trend

library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsci)
library(sf)

fig.dir <- "Figure/" # subfolder path are defined with ggsave
input.dir <- "../"
iso_GCAM_regID <-  read.csv("iso_GCAM_regID.csv", skip = 6, header = T)
GCAM_region_names <-  read.csv("GCAM_region_names.csv", skip = 6, header = T)


# define plot theme ----
fontfamily = "Arial"
windowsFonts("Arial" = windowsFont("Arial"))

theme0 <- theme(
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x.top =  element_blank(),  axis.title.x.top = element_blank(),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10), #panel.spacing = unit(1, "lines"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

theme_leg <- theme(legend.justification = "center",
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank(),
                   legend.position = "bottom")

theme_add <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      strip.text.x = element_text(size = 12))

gather_time <- function(.data){
  .data %>%
    tidyr::gather(year, value, names(.)[grepl("[0-9]{4}", names(.))]) %>%
    mutate(year = as.integer(gsub("X", "", year))) %>%
    return()
}

YEAR_BASE <- 2015
YEAR_start <- 1975

# region mapping ----

cluster <- read.csv("ClusterRegion.csv") %>%
  select(region, REG, REG10_AR6, REG5_AR6 )

# 5 region ---
REG <- function(df){
  df %>%
    left_join(cluster, by = "region") %>%
    select(-region, -REG,-REG10_AR6) %>%
    group_by(across(c(-value))) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    rename(REG = REG5_AR6) ->
    df
  return(df)
}

N <- 5;
NCOL <-  3;
NWIDTH <- 12;

# 10 region ---
REG <- function(df){
  df %>%
    left_join(cluster, by = "region") %>%
    select(-region, -REG,-REG5_AR6) %>%
    group_by(across(c(-value))) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    rename(REG = REG10_AR6) ->
    df
  return(df)
}

N <- 10;
NCOL <-  5;
NWIDTH <- 18;

# 32 region ---
REG <- function(df){
  df %>%
    ungroup() %>%
    left_join(cluster, by = "region") %>%
    select(-region,-REG5_AR6, -REG10_AR6) %>%
    group_by(across(c(-value))) %>%
    summarise(value = sum(value, na.rm = T)) ->
    df
  return(df)
}

N <- 32;
NCOL <-  8;
NWIDTH <- 24;

# cust ---
REG <- function(df){
  df %>%
    ungroup() %>%
    left_join(cluster, by = "region") %>%
    select(-region, -REG,-REG5_AR6) %>%
    rename(REG = REG10_AR6) %>%
    mutate(REG = ifelse(REG %in% c("EUROPE", "LATIN_AM", "MIDDLE_EAST", "PAC_OECD", "REF_ECON", "REST_ASIA"), "ROW", REG)) %>%
    group_by(across(c(-value))) %>%
    summarise(value = sum(value, na.rm = T)) ->
    df
  return(df)
}

N <- "cust";
NCOL <-  3;
NWIDTH <- 12;

INDEX <- function(df){
  df %>%
    group_by(across(c(-value, -year))) %>%
    mutate(index = value / value[year == 2015]) %>%
    select(-value)->
    df
  return(df)
}

DIFF <-  function(df){
  df %>%
    ungroup() %>%
    group_by(REG, year) %>%
    mutate(index = 100*(value - value[scenario == "E3"])/abs(value[scenario == "E3"])) %>%
    select(-value) ->
    df
  return(df)
}

# ************ drivers ************ ----

# GDP ----
GDP <- read.csv("C:/Model/LaborModelingResults/GDP_gSSP2.csv") %>%
  select(-X, -scenario) %>%
  left_join(GCAM_region_names, by = "GCAM_region_ID") # million 1990$

GDP %>%
  select(-GCAM_region_ID) %>%
  REG() -> df_gdp

df_gdp %>%
  INDEX() ->
  index_gdp

index_gdp %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = REG)) +
  labs(x = "Year", y = "Relative change (2015 = 1)") +
  ggtitle("Poputlation") +
  theme_bw() + theme0 + theme_leg + theme_add -> p; p

# GDPpc ----
GDPpc <- read.csv("C:/Model/LaborModelingResults/GDPpc_gSSP2.csv") %>%
  select(-X, -scenario) %>%
  left_join(GCAM_region_names, by = "GCAM_region_ID") # million 1990$

GDPpc %>%
  select(-GCAM_region_ID) %>%
  REG() -> df_gdppc

df_gdppc %>%
  INDEX() ->
  index_gdppc

index_gdppc %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = REG)) +
  labs(x = "Year", y = "Relative change (2015 = 1)") +
  ggtitle("Poputlation") +
  theme_bw() + theme0 + theme_leg + theme_add -> p; p

# population ----

POP <- read.csv("POP_SSP2.csv")

POP %>%
  mutate(value = ifelse(year < 2015, hist, futr)) %>%
  select(-hist, -futr) %>%
  REG() ->
  df_pop

df_pop %>%
  INDEX() ->
  index_pop

index_pop %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = REG)) +
  labs(x = "Year", y = "Relative change (2015 = 1)") +
  ggtitle("Poputlation") +
  theme_bw() + theme0 + theme_leg + theme_add -> p; p

df_pop %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = REG)) +
  labs(x = "Year", y = "Population") +
  ggtitle("Poputlation") +
  theme_bw() + theme0 + theme_leg + theme_add -> p; p

# rural population ----

WB_rural <- read.csv("C:/Model/heatstress/API_SP.RUR.TOTL_DS2_en_csv_v2_5455093.csv", fileEncoding = 'UTF-8-BOM', skip = 4, header = T) %>%
  gather_time() %>%
  mutate(iso = tolower(Country.Code)) %>%
  left_join(iso_GCAM_regID, by = "iso") %>%
  left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
  select(year, region, GCAM_region_ID, value) %>%
  filter(year<= YEAR_BASE) %>%
  na.omit() %>%
  group_by(region, year) %>%
  summarize(hist = sum(value))

RuralPop <- read.csv("RuralPop.csv")
RuralPop %>%
  filter(year >= 2015) %>%
  rename(futr = Rural_pop) %>%
  full_join(WB_rural, by = c("region", "year")) ->
  RURALPOP

RURALPOP %>%
  mutate(value = ifelse(year < 2015, hist, futr)) %>%
  select(-X, -hist, -futr) %>%
  REG() ->
  df_rural

df_rural %>%
  INDEX() ->
  index_rural

index_rural %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = REG)) +
  labs(x = "Year", y = "Relative change (2015 = 1)") +
  ggtitle("Rural poputlation") +
  theme_bw() + theme0 + theme_leg + theme_add -> p; p

# labor force ----
LFS <- read.csv("LF_share.csv")

POP %>%
  mutate(POP = ifelse(year <= 2015, hist, futr)) %>%
  select(region, year, POP) %>%
  left_join(LFS, by = c("region", "year")) %>%
  filter(year >= YEAR_start) %>%
  group_by(region) %>%
  mutate(labor.force.share = na.approx(labor.force.share)) %>%
  mutate(LF = POP * labor.force.share / 10^3) -> # mpl
  LF

LF %>%
  select(region, year, value = LF) %>%
  ungroup() %>%
  REG() ->
  df_LF

df_LF %>%
  INDEX() ->
  index_LF

gcam_macro_TFP_open <-  read.csv("gcam_macro_TFP_open.csv", skip = 6, header = T) %>%
  filter(scenario == "gSSP2")

gcam_macro_TFP_open %>%
  filter(year >= 2015) %>%
  rename(value = productivity) %>%
  REG() %>%
  ungroup() %>%
  select(-scenario, -gcam.version) %>%
  mutate(value = ifelse(year == 2015, 1, value)) %>%
  INDEX() ->
  index_eta

index_eta %>%
  ggplot() +
  geom_line(aes(x = year, y = index), linewidth = 1.3) +
  facet_wrap(~ REG, ncol = 8)
# combined ----

# glb --

df_LF %>%
  group_by(year) %>% summarise(value = sum(value)) %>% mutate(var = "Labor force", var1 = "pop") %>%
  bind_rows(df_rural %>% group_by(year) %>% summarise(value = sum(value)) %>% mutate(var = "Rural population", var1 = "pop")) %>%
  bind_rows(df_pop %>%  group_by(year) %>% summarise(value = sum(value)) %>% mutate(var = "Population", var1 = "pop")) %>%
  bind_rows(df_gdp %>%  group_by(year) %>% summarise(value = sum(value)) %>% mutate(var = "GDP", var1 = "gdp")) %>%
  bind_rows(df_gdppc %>%  group_by(year) %>% summarise(value = sum(value)) %>% mutate(var = "GDP per capita", var1 = "gdp")) %>%
  mutate(REG = "WORLD") %>%
  INDEX() -> index_glb

index_LF %>% mutate(var = "Labor force", var1 = "pop") %>%
  bind_rows(index_pop %>% mutate(var = "Population", var1 = "pop")) %>%
  bind_rows(index_rural %>% mutate(var = "Rural population", var1 = "pop")) %>%
  bind_rows(index_gdp %>% mutate(var = "GDP", var1 = "gdp")) %>%
  bind_rows(index_gdppc %>% mutate(var = "GDP per capita", var1 = "gdp")) %>%
  filter(year >= YEAR_start) %>%
  select(names(index_glb)) %>%
  bind_rows(index_glb) %>%
  mutate(var1 = gsub("pop", "Population index", var1),
         var1 = gsub("gdp", "GDP index", var1)) %>%
  rename(driver = var) ->
  d.driver

d.driver %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = REG), linewidth = 1.3) +
  labs(x = "Year", y = "Relative change (2015 = 1)") +
  scale_color_npg() +
  ggtitle("") +
  facet_wrap(~ driver) +
  # geom_line(data = index_glb %>% filter(year >= YEAR_start),aes(x = year, y = index),color = "black", linetype ="dotted", linewidth = 1.3) +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "right") -> p; p

d.driver %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_rect(aes(xmin=1975, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.2, fill="light grey") +
  geom_line(aes(x = year, y = index, color = driver), linewidth = 1.3) +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  facet_grid(var1~ REG, scales = "free") +
  labs(x = "Year", y = "Relative change (2015 = 1)", color = "Driver") +
  scale_color_npg() +
  ggtitle("") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/drivers.png"), p,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/drivers.png"), p,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')

# ************ labor metrics ************ ----

SCENARIO <- c("E3", "E0")

SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = gsub("E0","Evolving", scenario),
           scenario = gsub("E3","Static", scenario)) %>%
    return()
}

# labor demand ----

USDA_Labor <- read.csv("USDA_Labor.csv")
USDA_Capital <- read.csv("USDA_Capital.csv")


query = "LaborDemandSec"
df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]

  filename = paste0(input.dir,sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)

  input %>%
    gather_time() %>%
    select(-Units, -X) %>%
    mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}
# collect total ag labor demand across scenarios
df_L <- do.call(rbind, df_list) %>%
  filter(!sector %in% c("Forest", "Pasture", "biomass"))

df_L %>%
  REG() %>%
  group_by(scenario, REG, year) %>%
  summarise(value = sum(value)) %>%
  filter(year >= 2020)-> # mpl
  df_LL

USDA_Labor %>%
  mutate(scenario = "E0") %>%
  bind_rows(USDA_Labor %>%
              mutate(scenario = "E3")) %>%
  mutate(value = labor_ppl/10^6) %>%
  select(scenario, region, year, value) %>%
  REG() %>%
  ungroup() %>%
  select(scenario, REG, year, value) %>%
  filter(year %in% c(seq(1975, 2015, 5))) %>%
  bind_rows(df_LL) ->
  df_LLL

  df_LLL %>%
    INDEX() ->
    index_L

  df_LLL %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value)) %>%
    INDEX() %>%
    mutate(REG = "Globe") %>%
    select(names(index_L)) ->
    index_L_glb

  YEAR_start <- 1975
index_box_L <- index_L %>% bind_rows(index_L_glb) %>%
  filter(year >= YEAR_start) %>%
  SCE_NM()

index_box_L %>%
  ggplot() +
  geom_line(data =index_box_L, aes(x = year, y = index, color = scenario), linewidth = 1.3, linetype = "dotdash") +
  geom_line(data =index_box_L %>% filter(year <= 2015), aes(x = year, y = index), color = "black", linewidth = 1.3) +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  # geom_line(data = index_box_L %>% filter(REG == "Globe"),aes(x = year, y = index),color = "black", linetype ="dotted", linewidth = 1.3) +
  labs(x = "Year", y = "Labor Demand (2015 = 1)") +
  scale_color_npg() +
  ggtitle("") +
  facet_wrap(~ REG, ncol = NCOL ) +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p


  ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/labor.png"), p,
         width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')


  # L/K ratio ----
  # capital demand --
  query = "CapitalDemandSec"
  df_list = list()
  for (i in 1:length(SCENARIO)){
    sce_name = SCENARIO[i]

    filename = paste0(input.dir,sce_name,'/',query,'.csv')
    filename
    input = read.csv(filename, skip = 1, header = T)

    input %>%
      gather_time() %>%
      select(-Units, -X) %>%
      mutate(scenario = sce_name) -> middle
    names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
    df_list[[i]] <- middle
  }
  # collect total ag labor demand across scenarios
  df_K <- do.call(rbind, df_list) %>%
    filter(!sector %in% c("Forest", "Pasture", "biomass"))

  df_K %>%
    REG() %>%
    group_by(scenario, REG, year) %>%
    summarise(value = sum(value)) %>%
    filter(year >= 2020)-> # billiob 1975 $
    df_KK

  USDA_Capital %>%
    mutate(scenario = "E0") %>%
    bind_rows(USDA_Capital %>%
                mutate(scenario = "E3")) %>%
    mutate(value = capital_1975USD / 10^9) %>%
    select(scenario, region, year, value) %>%
    REG() %>%
    ungroup() %>%
    select(scenario, REG, year, value) %>%
    filter(year %in% c(seq(1975, 2015, 5))) %>%
    bind_rows(df_KK) ->
    df_KKK

  df_KKK %>%
    INDEX() ->
    index_K


  df_LLL %>%
    rename(labor = value) %>%
    left_join(df_KKK %>% rename(capital = value),
              by = c("scenario", "REG", "year")) %>%
    mutate(value = labor/capital) %>%
    select(-labor, -capital) -> df_ratio

  # df_ratio %>%
  #   INDEX() ->
  #   index_ratio
  #
  # # glb --
  #
  # df_LLL %>%
  #   rename(labor = value) %>%
  #   left_join(df_KKK %>% rename(capital = value),
  #             by = c("scenario", "REG", "year")) %>%
  #   group_by(scenario, year) %>%
  #   summarise(labor = sum(labor),
  #             capital = sum(capital)) %>%
  #   mutate(value = labor/capital) %>%
  #   select(-labor, -capital) %>%
  #   INDEX() %>%
  #   mutate(REG = "Globe") %>%
  #   select(names(index_ratio)) ->
  #   index_ratio_glb
  # index_box_ratio <- index_ratio %>% bind_rows(index_ratio_glb) %>%
  #   SCE_NM() %>%
  #   filter(year >= YEAR_start) %>%
  #   SCE_NM()
  #
  # index_box_ratio %>%
  #   ggplot() +
  #   geom_line(data =index_box_ratio %>% filter(REG != "Globe"),aes(x = year, y = index, color = REG), linewidth = 1.3) +
  #   geom_line(data = index_box_ratio %>% filter(REG == "Globe"),aes(x = year, y = index),color = "black", linetype ="dotted", linewidth = 1.3) +
  #   labs(x = "Year", y = "Labor-Capital Ratio (2015 = 1)") +
  #   ggtitle("") +
  #   facet_wrap(~ scenario) +
  #   theme_bw() + theme0 + theme_leg + theme_add +
  #   theme(legend.position = c(1, .95), legend.justification = c("right", "top")) -> p; p

  df_LLL %>%
    rename(labor = value) %>%
    left_join(df_KKK %>% rename(capital = value),
              by = c("scenario", "REG", "year")) %>%
    group_by(scenario, year) %>%
    summarise(labor = sum(labor),
              capital = sum(capital)) %>%
    mutate(value = labor/capital) %>%
    select(-labor, -capital) %>%
    mutate(REG = "WORLD") %>%
    select(names(df_ratio)) ->
    df_ratio_glb

  df_box_ratio <- df_ratio %>% bind_rows(df_ratio_glb) %>%
    SCE_NM() %>%
    filter(year >= YEAR_start)

  df_box_ratio %>%
    ggplot() +
    geom_line(aes(x = year, y = value, color = scenario), linewidth = 1.3, linetype = "dotdash") +
    geom_line(data = df_box_ratio %>% filter(year <= 2015), aes(x = year, y = value), color = "black", linewidth = 1.3) +
    geom_vline(xintercept = 2015, linetype = "dotted") +
    # geom_line(data = df_box_ratio %>% filter(REG == "Globe"),aes(x = year, y = value),color = "black", linetype ="dotted", linewidth = 1.3) +
    labs(x = "Year", y = "Labor-Capital Ratio") +
    ggtitle("") +
    scale_color_npg() +
    facet_wrap(~ REG, ncol = NCOL , scales = "free_y") +
    theme_bw() + theme0 + theme_leg + theme_add +
    theme(legend.position = "bottom") -> p; p


  ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/ratio_level.png"), p,
         width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')



  # ag labor to labor force ratio ----
  LF %>%
    mutate(scenario = "E0") %>%
    bind_rows(LF %>%
                mutate(scenario = "E3")) %>%
    rename(value = LF) %>%
    ungroup() %>%
    REG() %>%
    group_by(scenario, REG, year) %>%
    summarise(LF = sum(value)) %>%
    left_join(df_LLL %>%  rename(L = value),
              by = c("scenario", "REG" ,"year")) %>%
    na.omit() -> df_share

  df_share %>%
    mutate(value = L/LF) %>%
    select(scenario, REG, year, value) %>%
    INDEX() ->
    index_share

  df_share %>%
    group_by(scenario, year) %>%
    summarise(LF = sum(LF),
              L = sum(L)) %>%
    mutate(value = L/LF) %>%
    select(scenario, year, value) %>%
    INDEX() %>%
    mutate(REG = "Globe") %>%
    select(names(index_share))->
    index_share_glb

  index_box_share <- index_share %>% bind_rows(index_share_glb) %>%
    SCE_NM() %>%
    filter(year >= YEAR_start)

  index_box_share %>%
    ggplot() +
    geom_line(data =index_box_share %>% filter(REG != "Globe"),aes(x = year, y = index, color = REG), linewidth = 1.3) +
    geom_line(data = index_box_share %>% filter(REG == "Globe"),aes(x = year, y = index),color = "black", linetype ="dotted", linewidth = 1.3) +
    labs(x = "Year", y = "Agricultural labor share of labor force (2015 = 1)") +
    scale_color_npg() +
    ggtitle("") +
    facet_wrap(~ scenario) +
    theme_bw() + theme0 + theme_leg + theme_add +
    theme(legend.position = c(0.5, .55), legend.justification = c("right", "bottom")) -> p; p

  index_box_share %>%
    ggplot() +
    geom_line(aes(x = year, y = index, color = scenario), linewidth = 1.3, linetype = "dotdash") +
    geom_line(data = index_box_share %>% filter(year <= 2015), aes(x = year, y = index), color = "black", linewidth = 1.3) +
    geom_vline(xintercept = 2015, linetype = "dotted") +
    labs(x = "Year", y = "Agricultural labor share of labor force (2015 = 1)") +
    scale_color_npg() +
    ggtitle("") +
    facet_wrap(~ REG, ncol = NCOL ) +
    theme_bw() + theme0 + theme_leg + theme_add +
    theme(legend.position = "bottom") -> p; p


ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/share.png"), p,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')


# ag labor to rural pop ----
df_rural %>%
  mutate(scenario = "E0") %>%
  bind_rows(df_rural %>%
              mutate(scenario = "E3")) %>%
  ungroup() %>%
  group_by(scenario, REG, year) %>%
  summarise(LF = sum(value)) %>%
  left_join(df_LLL %>%  rename(L = value),
            by = c("scenario", "REG" ,"year")) %>%
  na.omit() -> df_share

df_share %>%
  mutate(value = L/LF) %>%
  select(scenario, REG, year, value) %>%
  INDEX() ->
  index_share

df_share %>%
  group_by(scenario, year) %>%
  summarise(LF = sum(LF),
            L = sum(L)) %>%
  mutate(value = L/LF) %>%
  select(scenario, year, value) %>%
  INDEX() %>%
  mutate(REG = "Globe") %>%
  select(names(index_share))->
  index_share_glb

index_box_share_R <- index_share %>% bind_rows(index_share_glb) %>%
  SCE_NM() %>%
  filter(year >= YEAR_start)

index_box_share_R %>%
  ggplot() +
  geom_line(data =index_box_share %>% filter(REG != "Globe"),aes(x = year, y = index, color = REG), linewidth = 1.3) +
  geom_line(data = index_box_share %>% filter(REG == "Globe"),aes(x = year, y = index),color = "black", linetype ="dotted", linewidth = 1.3) +
  labs(x = "Year", y = "Agricultural labor share of rural pop (2015 = 1)") +
  scale_color_npg() +
  ggtitle("") +
  facet_wrap(~ scenario) +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = c(0.5, .55), legend.justification = c("right", "bottom")) -> p; p

index_box_share_R %>%
  filter(scenario == "Evolving") %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario), linewidth = 1.3) +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Agricultural labor share of labor force (2015 = 1)") +
  scale_color_npg() +
  ggtitle("") +
  facet_wrap(~ REG, ncol = NCOL , scales = "free_y") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p


ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/LA_RURAL_share.png"), p,
       width = 16, height = 10, dpi = 300, units = "in", device='png')

# effective labor ----
# GCAM report physical.L
# eff.L  = physical.L * productivity multiplier = physical.L / IO multiplier

gcam_macro_TFP_open <-  read.csv("gcam_macro_TFP_open.csv", skip = 6, header = T) %>%
  filter(scenario == "gSSP2")

df_L %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  left_join(gcam_macro_TFP_open %>% select(-scenario),
            by = c("region", "year")) %>%
  mutate(productivity = ifelse(year<=2015, 1, productivity),
         productivity = ifelse(scenario == "E3", 1, productivity),
         effective.labor = value * productivity) %>%
  select(scenario, region, year, value = effective.labor) %>%
  ungroup() %>%
  REG() ->
  df_eff

df_eff %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(REG = "WORLD") %>%
  group_by(REG, year) %>%
  mutate(index = value / value[scenario == "E3"]) %>%
  filter(scenario == "E0",
         year >= 2015) %>%
  select(scenario, REG, year, index) ->
  index_eff_glb

df_eff %>%
  group_by(REG, year) %>%
  mutate(index = value / value[scenario == "E3"]) %>%
  filter(scenario == "E0",
         year >= 2015) %>%
  select(scenario, REG, year, index) ->
  index_eff_reg

index_eff <- index_eff_glb %>% bind_rows(index_eff_reg)

index_eff %>%
  SCE_NM() %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = REG), linewidth = 1.3) +
  geom_hline(yintercept = 1) +
  labs(x = "Year", y = "Effective agricultural labor supply (Old reference = 1)") +
  scale_color_npg() +
  ggtitle("") +
  # facet_wrap(~ REG, ncol = NCOL ) +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p
ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/effe_labor_index.png"), p,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')


df_eff %>%
  group_by(REG, scenario) %>%
  INDEX() ->
  index_eff_reg

df_eff %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  mutate(REG = "WORLD") %>%
  group_by(REG, scenario) %>%
  INDEX() %>%
  select(names(index_eff_reg))->
  index_eff_glb

index_eff <- index_eff_glb %>% bind_rows(index_eff_reg)

index_eff %>%
  filter(year >= 2015) %>%
  SCE_NM() %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario), linewidth = 1.3, linetype = "dotdash") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  labs(x = "Year", y = "Effective agricultural labor supply (2015 = 1)") +
  scale_color_npg() +
  ggtitle("") +
  facet_wrap(~ REG, ncol = NCOL ) +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p
ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/effe_labor.png"), p,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')

# df_share %>%
#   group_by(scenario, year) %>%
#   summarise(LF = sum(LF),
#             L = sum(L)) %>%
#   mutate(value = L/LF) %>%
#   select(scenario, year, value) %>%
#   INDEX() %>%
#   mutate(REG = "Globe") %>%
#   select(names(index_share))->
#   index_share_glb
#
# index_box_share <- index_share %>% bind_rows(index_share_glb) %>%
#   SCE_NM() %>%
#   filter(year >= YEAR_start)

# index_eff %>% filter(year == 2050,
#                      index > 1) -> df_more
# MORE <-  unique(df_more$REG)

# index_eff %>%
#   mutate(group = ifelse(REG %in% MORE, "More", "Less")) %>%
#   ggplot() +
#   geom_line(aes(x = year, y = index, color = REG), linewidth = 1.3) +
#   facet_wrap(~ group) +
#   # geom_line(data = index_box_share %>% filter(REG == "Globe"),aes(x = year, y = index),color = "black", linetype ="dotted", linewidth = 1.3) +
#   labs(x = "Year", y = "Effective labor (Old reference = 1)") +
#   scale_color_npg() +
#   ggtitle("") +
#   theme_bw() + theme0 + theme_leg + theme_add +
#   theme(legend.position = "right") -> p; p

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"_effe_labor.png"), p,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')


# combined ----

index_box_L %>% mutate(var = "Labor demand", REG = gsub("Globe", "WORLD", REG)) %>%
  bind_rows(df_box_ratio %>% rename(index = value) %>%  mutate(var = "L/K ratio")) %>%
  bind_rows(index_box_share %>% mutate(var = "Ag labor share in labor force", REG = gsub("Globe", "WORLD", REG))) %>%
  bind_rows(index_eff %>% filter(year >= 2015) %>% mutate(var = "Effective labor supply")) %>%
  mutate(var = gsub("Labor demand", "Labor demand (2015 = 1)", var),
         var = gsub("Ag labor share in labor force", "Ag labor share (2015 = 1)", var),
         var = gsub("Effective labor supply", "Effective labor (2015 = 1)", var)) %>%
  mutate(REG = gsub("THE GLOBE", "WORLD", REG)) ->
  df_combined

df_combined %>%
  filter(year >= YEAR_start) %>%
  SCE_NM() %>%
  ggplot() +
  geom_rect(aes(xmin=1975, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.2, fill="light grey") +
  geom_line(aes(x = year, y = index, color = scenario), linewidth = 1.3, linetype = "dotdash") +
  geom_line(data = df_combined %>% filter(year <= 2015), aes(x = year, y = index), linewidth = 1.3, color = "black") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  labs(x = "", y = "", color = "Scenario") +
  scale_color_npg() +
  ggtitle("") +
  # facet_wrap( ~ var + REG)
  facet_wrap(~ factor(var, c("Effective labor (2015 = 1)", "Labor demand (2015 = 1)" , "Ag labor share (2015 = 1)",  "L/K ratio")) + REG, ncol = 6, scales = "free_y") +
  # facet_grid(REG ~ var, scales = "free") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/combined_labor_free_trans.png"), p,
       width = 16, height = 16, dpi = 300, units = "in", device='png')

df_combined %>%
  filter(year >= YEAR_start) %>%
  SCE_NM() %>%
  ggplot() +
  geom_rect(aes(xmin=1975, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.2, fill="light grey") +
  geom_line(aes(x = year, y = index, color = scenario), linewidth = 1.3, linetype = "dotdash") +
  geom_line(data = df_combined %>% filter(year <= 2015), aes(x = year, y = index), linewidth = 1.3, color = "black") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  labs(x = "", y = "", color = "Scenario") +
  scale_color_npg() +
  ggtitle("") +
  facet_grid(factor(var, c("Effective labor (2015 = 1)", "Labor demand (2015 = 1)" , "Ag labor share (2015 = 1)",  "L/K ratio")) ~ REG, scales = "free_y") +
  # facet_grid(REG ~ var, scales = "free") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/combined_labor_trans.png"), p,
       width = 24, height = 16, dpi = 300, units = "in", device='png')

df_combined %>%
  filter(year >= YEAR_start) %>%
  SCE_NM() %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario), linewidth = 1.3, linetype = "dotdash") +
  geom_line(data = df_combined %>% filter(year <= 2015, year >= YEAR_start), aes(x = year, y = index), linewidth = 1.3, color = "black") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  labs(x = "", y = "") +
  scale_color_npg() +
  ggtitle("") +
  facet_grid(var ~ REG) +
  # facet_grid(REG ~ var, scales = "free") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p

# ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/combined_labor_1990.png"), p,
#        width = NWIDTH, height = 16, dpi = 300, units = "in", device='png')

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/combined_labor_1975.png"), p,
       width = NWIDTH, height = 16, dpi = 300, units = "in", device='png')

df_combined %>%
  filter(year >= YEAR_start) %>%
  SCE_NM() %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario), linewidth = 1.3, linetype = "dotdash") +
  geom_line(data = df_combined %>% filter(year <= 2015, year >= YEAR_start), aes(x = year, y = index), linewidth = 1.3, color = "black") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  labs(x = "", y = "") +
  scale_color_npg() +
  ggtitle("") +
  facet_grid(REG ~ factor(var, c("Labor demand (2015 = 1)" , "Ag labor share (2015 = 1)", "Effective labor (2015 = 1)", "L/K ratio")),
             scales = "free_y") +
  # facet_grid(REG ~ var, scales = "free") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p

# ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/combined_labor_1990.png"), p,
#        width = NWIDTH, height = 16, dpi = 300, units = "in", device='png')

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/combined_labor_1975_trans.png"), p,
       width = NWIDTH, height = 16, dpi = 300, units = "in", device='png')

# ************ other metrics ************ ----

CLUSTER_SECTOR <- function(.data){
  .data %>%
    dplyr::mutate(group = sector,
                  group = ifelse(sector %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat"), "Livestock", group),
                  group = ifelse(sector %in% c("Corn", "OtherGrain", "Rice", "RootTuber", "Wheat"), "Staples", group),
                  group = ifelse(sector %in% c("FodderGrass", "FodderHerb"), "Fodder Crops", group),
                  group = ifelse(sector %in% c("Fruits", "Vegetables", "Legumes", "MiscCrop", "NutsSeeds",  "FiberCrop"), "Other Crops", group),
                  group = ifelse(sector %in% c("OilCrop","OilPalm", "Soybean"), "Oil Crops", group),
                  group = ifelse(sector == "biomass", "Biomass", group)) %>%
    return()
}
# labor demand across sectors ----

query = "LaborDemandSec"
df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]

  filename = paste0(input.dir,sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)

  input %>%
    gather_time() %>%
    select(-Units, -X) %>%
    mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}

L_sec <- do.call(rbind, df_list) %>%
  CLUSTER_SECTOR() %>%
  group_by(scenario, region, year, group) %>%
  summarise(value = sum(value))

L_sec %>%
  filter(year %in% c(2015, 2050, 2100)) %>%
  SCE_NM() %>%
  unite(var, scenario, year) %>%
  ggplot() +
  geom_bar(aes(x=var, y = value, fill = group), position="stack", stat="identity") +
  facet_wrap( ~ region, ncol = 8, scales = "free_y") +
  theme_bw() + theme0 + theme_leg + theme_add ->
  p; p

L_sec %>%
  group_by(region, year, group) %>%
  mutate(diff = value - value[scenario == "E3"]) %>%
  filter(year >= 2015,
         scenario == "E0") %>%
  SCE_NM() %>%
  ggplot() +
  geom_bar(aes(x=year, y = diff, fill = group), position="stack", stat="identity") +
  facet_wrap( ~ region, ncol = 8, scales = "free_y") +
  labs(y = "Relative change in labor demand: million people (static = 0)") +
  theme_bw() + theme0 + theme_leg + theme_add ->
  p; p

L_sec %>%
  ungroup() %>%
  left_join(cluster, by = "region") %>%
  select(-region, -REG,-REG5_AR6) %>%
  rename(REG = REG10_AR6) %>%
  mutate(REG = ifelse(REG %in% c("EUROPE", "LATIN_AM", "MIDDLE_EAST", "PAC_OECD", "REF_ECON", "REST_ASIA"), "ROW", REG)) %>%
  group_by(across(c(-value))) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  group_by(REG, group, year) %>%
  mutate(diff = value - value[scenario == "E3"]) %>%
  filter(year >= 2015, scenario == "E0") %>%
  SCE_NM() %>%
  ggplot() +
  geom_bar(aes(x=year, y = diff, fill = group), position="stack", stat="identity") +
  facet_wrap( ~ REG, ncol = 8, scales = "free_y") +
  labs(y = "Relative change in labor demand: million people (static = 0)") +
  theme_bw() + theme0 + theme_leg + theme_add ->
  p; p

YEAR <- 2015
L_sec %>%
  # filter(year %in% c(2015, 2050, 2100)) %>%
  filter(year == YEAR) %>%
  ungroup() %>%
  left_join(cluster, by = "region") %>%
  select(-region, -REG,-REG5_AR6) %>%
  rename(REG = REG10_AR6) %>%
  mutate(REG = ifelse(REG %in% c("EUROPE", "LATIN_AM", "MIDDLE_EAST", "PAC_OECD", "REF_ECON", "REST_ASIA"), "ROW", REG)) %>%
  group_by(across(c(-value))) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  filter(scenario == "E0") %>%
  filter(group != "Biomass") %>% # no biomass yet in 2015
  SCE_NM() %>%
  ggplot() +
  geom_bar(aes(x = as.factor(year), y = value, fill = group), position="stack", stat="identity") +
  facet_wrap( ~ REG, ncol = 8, scales = "free_y") +
  labs(x = "", y = paste0("Agricultural employment: million people in ", YEAR),
       fill = "") +
  scale_fill_npg() +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(axis.text.x=element_blank()) ->
  p; p

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/LA_2015_sec.png"), p,
       width = NWIDTH, height = 12, dpi = 300, units = "in", device='png')


L_sec %>%
  # filter(year %in% c(2015, 2050, 2100)) %>%
  filter(year == YEAR) %>%
  ungroup() %>%
  left_join(cluster, by = "region") %>%
  select(-region, -REG,-REG5_AR6) %>%
  rename(REG = REG10_AR6) %>%
  mutate(REG = ifelse(REG %in% c("EUROPE", "LATIN_AM", "MIDDLE_EAST", "PAC_OECD", "REF_ECON", "REST_ASIA"), "ROW", REG)) %>%
  group_by(across(c(-value))) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  filter(scenario == "E0") %>%
  filter(group != "Biomass") %>%
  group_by(scenario, REG, year) %>%
  mutate(total = sum(value),
         share = 100*value / total) ->
  check
# labor productivity growth vs rural ----
gcam_macro_TFP_open %>%
  left_join(cluster, by = "region") %>%
  filter(region != "Taiwan") %>%
  select(REG, year, productivity) %>%
  left_join(df_rural, by = c("REG", "year")) %>%
  filter(year >= 2015) %>%
  group_by(REG) %>%
  mutate(productivity = ifelse(year == 2015, 1, productivity),
         eta = productivity / productivity[year ==2015] - 1,
         rural = value / value[year == 2015] - 1,
         agg = eta + rural) %>%
  select(REG, year, eta, rural, agg) %>%
  gather(var, value, eta:agg) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = var), linewidth = 1.3) +
  facet_wrap(~ REG, ncol = NCOL ) +
  labs(x = "Year", y = "Relative change (2015 = 0)") +
  scale_color_npg() +
  ggtitle("") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "right") -> p; p

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/eta_rural_agg.png"), p,
       width = 16, height = 10, dpi = 300, units = "in", device='png')

# wage rate ----

query = "LaborPrice"

df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]

  filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)

  input %>%
    gather_time() %>%
    select(-Units, -X) %>%
    mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}

df_W <- do.call(rbind, df_list) %>%
  filter(year >= 2015) %>%
  mutate(region = gsub("Labor_Ag", "", market)) %>%
  select(scenario, region, year, value) %>%
  filter(region != "South America_Northern")

df_W %>%
  REG() %>%
  select(scenario, REG, year, value) %>%
  INDEX() ->
  index_W

index_W %>%
  SCE_NM() %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario), linewidth = 1.3) +
  geom_hline(yintercept = 1) +
  labs(x = "Year", y = "Wage rate (2015 = 1)") +
  scale_color_npg() +
  ggtitle("") +
  facet_wrap(~ REG, ncol = NCOL ) +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p
ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/wage.png"), p,
       width = NWIDTH, height = 10, dpi = 300, units = "in", device='png')

# effective wage rate ----
# GCAM reports physical wage
# eff.w  = w / productivity multiplier = w * IO multiplier


df_W %>%
  left_join(gcam_macro_TFP_open %>% select(-scenario),
            by = c("region", "year")) %>%
  mutate(productivity = ifelse(year<=2015, 1, productivity),
         productivity = ifelse(scenario == "E3", 1, productivity),
         effective.wage = value / productivity) %>%
  select(scenario, region, year, value = effective.wage) %>%
  ungroup() %>%
  REG() ->
  df_eff_w

df_eff_w %>%
  group_by(REG, scenario) %>%
  INDEX() ->
  index_eff_w_reg

df_W %>%
  left_join(gcam_macro_TFP_open %>% select(-scenario),
            by = c("region", "year")) %>%
  mutate(productivity = ifelse(year<=2015, 1, productivity),
         productivity = ifelse(scenario == "E3", 1, productivity),
         effective.wage = value / productivity) %>%
  select(scenario, region, year, effective.wage) %>%
  left_join(df_L %>%
              group_by(scenario, region, year) %>%
              summarise(value = sum(value)) %>%
              left_join(gcam_macro_TFP_open %>% select(-scenario),
                        by = c("region", "year")) %>%
              mutate(productivity = ifelse(year<=2015, 1, productivity),
                     productivity = ifelse(scenario == "E3", 1, productivity),
                     effective.labor = value * productivity) %>%
              select(scenario, region, year, effective.labor),
            by = c("scenario", "year", "region")) %>%
  mutate(exp = effective.wage * effective.labor) %>%
  group_by(scenario, year) %>%
  summarise(exp = sum(exp),
            effective.labor = sum(effective.labor)) %>%
  mutate(value = exp / effective.labor) %>%
  mutate(REG = "WORLD") -> check
  select(-exp, -effective.labor) %>%
  group_by(REG, scenario) %>%
  INDEX() %>%
  select(names(index_eff_w_reg))->
  index_eff_w_glb

index_w_eff <- index_eff_w_glb %>% bind_rows(index_eff_w_reg)

index_w_eff %>%
  filter(year >= 2015) %>%
  SCE_NM() %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario), linewidth = 1.3, linetype = "dotdash") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  labs(x = "Year", y = "Effective agricultural labor supply (2015 = 1)") +
  scale_color_npg() +
  ggtitle("") +
  facet_wrap(~ REG, ncol = NCOL ) +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p
ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/effe_labor.png"), p,
       width = NWIDTH, height = 10, dpi = 300, units = "in", device='png')


# food demand pp ----
query = "FoodDemandpp"

df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]

  filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)

  input %>%
    gather_time() %>%
    select(-Units, -X) %>%
    mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}

df_food <- do.call(rbind, df_list) %>%
  select(-gcam.consumer, -nodeInput) %>%
  REG()

index_food <- df_food %>% INDEX()

index_food %>%
  SCE_NM() %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario, linetype = input), linewidth = 1.3) +
  geom_hline(yintercept = 1) +
  labs(x = "Year", y = "Effective agricultural labor supply (2015 = 1)") +
  scale_color_npg() +
  ggtitle("") +
  facet_wrap(~ REG, ncol = NCOL , scales = "free_y") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.position = "bottom") -> p; p

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/food.png"), p,
       width = NWIDTH, height = 10, dpi = 300, units = "in", device='png')


# land use ----
query = "Land"

df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]

  filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)

  input %>%
    gather_time() %>%
    select(-Units, -X) %>%
    mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}

df_sce <- do.call(rbind, df_list) %>%
  landleaf() %>%
  filter(year >= 2015) %>%
  mutate(value = value / 10) # mil ha


df_land <- df_sce %>%
  rename(sector = Crop) %>%
  landname() %>%
  mutate(sector = gsub("C4", "", sector)) %>%
  group_by(scenario, region, year, sector) %>%
  summarise(value = sum(value, na.rm = T))

# land use stacked: glb ----

df_land %>%
  # filter(sector %in% c("Corn", "FiberCrop", "Fodder Crops", "Fruits", "Legumes", "MIscCrop",
  #                      "OilPlant", "OtherGrain", "Rice", "RootTuber", "Soybean", "SugarCrop",
  #                      "Vegetables", "Wheat", "biomass")) %>%
  mutate(sector = ifelse(sector == "OilPlant", "Oil Crops", sector)) %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2() ->
  df_LAND

df_LAND %>%
  landgroup() %>%
  cropgroup() %>%
  group_by(scenario, group, year) %>%
  summarise(value = sum(value)) %>%
  group_by(group, year) %>%
  mutate(diff = value - value[scenario == "E3"]) %>%
  filter(!group %in% c("NonArable", "Protected")) %>%
  filter(year >= 2020) ->
  df_LAND_glb

df_LAND_glb %>%
  ggplot() +
  geom_bar(aes(x=year, y = diff, fill = group), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "", y = "Million ha") +
  scale_fill_npg() +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) ->
  p; p

P2.LAND <- p + labs(title = "Global land use change"); P2.LAND

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/LAND.png"), P2.LAND,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')


# land use stacked: regions ----

# , "EU-15"
# REG_NM <- function(.data){
#   .data %>%
#     mutate(REG = ifelse(region %in% c("China", "USA", "Brazil", "Russia"),region, "ROW"),
#            REG = ifelse(grepl("Africa", region), "Africa", REG)) %>%
#     return()
# }

df_LAND %>%
  landgroup() %>%
  cropgroup() %>%
  ungroup() %>%
  REG() %>%
  group_by(scenario, REG, group, year) %>%
  summarise(value = sum(value)) %>%
  group_by(group, REG, year) %>%
  mutate(diff = value - value[scenario == "E3"]) %>%
  filter(!group %in% c("NonArable", "Protected")) %>%
  filter(year >= 2020) ->
  df_LAND_reg

df_LAND_glb %>%
  mutate(REG = "WORLD") %>%
  select(names(df_LAND_reg)) %>%
  bind_rows(df_LAND_reg) %>%
  ggplot() +
  geom_bar(aes(x=year, y = diff, fill = group), position="stack", stat="identity") +
  geom_hline(yintercept = 0) +
  facet_wrap(~ REG, ncol = NCOL ) +
  scale_fill_npg() +
  labs(x = "Year", y = "Million ha (static = 0)", title = "Land use change") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = c(0.85, 0.9)) ->
  p; p

P2.LAND.reg <- p; P2.LAND.reg

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/LAND.reg.png"), P2.LAND.reg,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')


# crop land ----

# glb --
df_LAND %>%
  landgroup() %>%
  landgroup() %>%
  filter(group %in% c("Staples","Other Crops","Fruits&Vege","Oil Crops",
                      "SugarCrop", "Biomass")) %>%
  ungroup() %>%
  group_by(scenario, group, year) %>%
  summarise(value = sum(value)) %>%
  group_by(group, year) %>%
  mutate(diff = value - value[scenario == "E3"]) %>%
  filter(!group %in% c("NonArable", "Protected")) ->
  df_cropland_glb

df_cropland_glb %>%
  ggplot() +
  geom_bar(aes(x=year, y = diff, fill = group), position="stack", stat="identity") +
  # facet_wrap(~ REG, ncol = NCOL , scales = 'free_y') +
  labs(x = "Year", y = "Million ha") +
  scale_fill_jco() +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) ->
  p; p

P2.CROPLAND.glb <- p; P2.CROPLAND.glb

# reg --
df_LAND %>%
  landgroup() %>%
  landgroup() %>%
  filter(group %in% c("Staples","Other Crops","Fruits&Vege","Oil Crops",
                      "SugarCrop", "Biomass")) %>%
  ungroup() %>%
  REG() %>%
  group_by(scenario, REG, group, year) %>%
  summarise(value = sum(value)) %>%
  group_by(group, REG, year) %>%
  mutate(diff = value - value[scenario == "E3"]) %>%
  filter(!group %in% c("NonArable", "Protected")) ->
  df_cropland_reg

df_cropland <- df_cropland_glb %>%  mutate(REG = "WORLD") %>%
  select(names(df_cropland_reg)) %>%
  bind_rows(df_cropland_reg)

df_cropland %>%
  filter(year >= 2020) %>%
  ggplot() +
  geom_bar(aes(x=year, y = diff, fill = group), position="stack", stat="identity") +
  # facet_wrap(~ REG, ncol = NCOL , scales = 'free_y') +
  facet_wrap(~ REG, ncol = NCOL ) +
  labs(x = "Year", y = "Million ha") +
  scale_fill_jco() +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = c(0.85, 0.85)) ->
  p; p

P2.CROPLAND.reg <- p; P2.CROPLAND.reg


ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/CropLand.reg.png"), P2.CROPLAND.reg,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')

# LAND ggarrange ----
figure <- ggarrange(P2.LAND.reg + ggtitle("(a) Land use") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
                    P2.CROPLAND.reg + ggtitle("(b) Cropland") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
                    labels = NULL,
                    ncol = 2, nrow = 1,
                    # common.legend = TRUE, legend = "bottom",
                    align = "hv",
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top")); figure

ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/ggarrange_LAND.reg.png"), figure,
       width = NWIDTH+8, height = NWIDTH, dpi = 300, units = "in", device='png')


# LUC emission  ----
query = "LUCemission"

df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]
  filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)
  input %>%
    gather_time() %>%
    dplyr::select(-Units, -X) %>%
    dplyr::mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}
df_emission <- do.call(rbind, df_list) # MtC/yr

# global --
# cumulative --
# linear intepolation between each steps
# 2015 + 2016 + 2017 + 2018 + 2019 + 2020 = 2015 + 2 * (2015 + 2020) + 2020
# cumulative in 2020 = cumulative in 2015 + 2 * (2015 + 2020) + 2020
df_emission %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  filter(year >= 2015) %>%
  group_by(scenario) %>%
  mutate(middle = 2* (value + lag(value)) ,
         middle = ifelse(is.na(middle), 0, middle),
         delta = value + middle,
         period = cumsum(delta)) %>%
  select(scenario, year, value = period ) %>%
  group_by(year) %>%
  mutate(index = value - value[scenario == "E3"]) %>%
  filter(scenario == "E0") %>%
  SCE_NM() ->
  Fig_emission_glb

Fig_emission_glb %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario), linewidth = 1.4) +
  labs(x = "Year", y = "MtC") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) ->
  p; p

# regional ---
df_emission %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  filter(year >= 2015) %>%
  ungroup() %>%
  REG() %>%
  group_by(scenario, REG) %>%
  mutate(middle = 2* (value + lag(value)) ,
         middle = ifelse(is.na(middle), 0, middle),
         delta = value + middle,
         period = cumsum(delta)) %>%
  select(scenario, REG, year, value = period ) %>%
  group_by(REG, year) %>%
  mutate(index = value - value[scenario == "E3"]) %>%
  filter(scenario == "E0") %>%
  SCE_NM() ->
  Fig_emission_reg

Fig_emission_glb %>%  mutate(REG = "WORLD") %>%
  select(names(Fig_emission_reg)) %>%
  bind_rows(Fig_emission_reg) ->
  Fig_emission


Fig_emission %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = REG), linewidth = 1.4) +
  geom_hline(yintercept = 0) +
  labs(x = "Year", y = "MtC", title = "Land use emission change") +
  scale_color_npg() +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "bottom") ->
  p; p


ggsave(filename = paste0(fig.dir, "Figure_Final/REG",N,"/EMISSION.reg.png"), p,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')


# ************ ag market outcome ************ ----

SELECT <- function(df){
  df %>%
    mutate(sector = tolower(sector)) %>%
    filter(sector %in% c("corn", "rice", "wheat", "roottuber", "othergrain")) ->
    df
  return(df)
}

M <-  "Stples"

# production ----
query = "CropProdSec"

df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]
  filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)
  input %>%
    gather_time() %>%
    dplyr::select(-Units, -X) %>%
    dplyr::mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}
do.call(rbind, df_list) %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2() %>%
  SELECT() -> Q



Q %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  REG() %>%
  DIFF() %>%
  filter(scenario == "E0",
         year == 2100) %>%
  mutate(var = "Production") ->
  df_Q

df_Q %>%
  ggplot() +
  geom_point(aes(x = REG, y = index, color = var), size = 3) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "(Old reference = 1)", title = "") +
  scale_color_npg() +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "bottom") ->
  p; p


# effective labor ----
df_eff %>%
  DIFF() %>%
  filter(scenario == "E0",
         year == 2100) %>%
  mutate(var = "Effective labor") ->
  df_EFF

# consumption ----

query = "CropDemand"

df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]
  filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)
  input %>%
    gather_time() %>%
    select(-Units, -X) %>%
    mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}
do.call(rbind, df_list) %>%
  select(-sector) %>%
  rename(sector = input) %>%
  SECTOR() %>%
  filter(sector %in% c("corn", "rice", "wehat", "soybean")) %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  REG() %>%
  DIFF() %>%
  filter(scenario == "E0",
         year == 2100) %>%
  mutate(var = "Consumption") ->
  df_D

  # import ----

  query = "Ag_import"

  df_list = list()
  for (i in 1:length(SCENARIO)){
    sce_name = SCENARIO[i]

    filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
    filename
    input = read.csv(filename, skip = 1, header = T)

    input %>%
      gather_time() %>%
      select(-Units, -X) %>%
      mutate(scenario = sce_name) -> middle
    names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
    df_list[[i]] <- middle
  }

 do.call(rbind, df_list) %>%
    filter(grepl('imported', subsector)) %>%
    select(scenario, region, input, year, value) %>%
    rename(sector = input) %>%
    mutate(type = "import",
           sector = gsub("traded ", "", sector)) %>%
    SECTOR() %>%
   filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>%
   group_by(scenario, region, year) %>%
   summarise(value = sum(value)) ->
   IM
 IM %>%
   REG() %>%
   DIFF() %>%
   filter(scenario == "E0",
          year == 2100) %>%
   mutate(var = "Import") ->
   df_IM

 # export ----
 query = "Ag_export"
 df_list = list()
 for (i in 1:length(SCENARIO)){
   sce_name = SCENARIO[i]
   filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
   filename
   input = read.csv(filename, skip = 1, header = T) %>%
     gather_time() %>%
     select(-Units, -X) %>%
     mutate(scenario = sce_name) -> middle
   names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
   df_list[[i]] <- middle
 }

do.call(rbind, df_list) %>% # data.frame that collects regional GDP across scenarios
   mutate(region = gsub(" traded.*","",subsector),
          input = tolower(sector),
          input = gsub("traded ", "", input),
          type = "export") %>%
   select(scenario, region, input, year, value, type) %>%
   rename(sector = input) %>%
   SECTOR() %>%
  filter(sector %in% c("corn", "rice", "wheat", "soybean")) %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) -> EX
EX %>%
  REG() %>%
  DIFF() %>%
  filter(scenario == "E0",
         year == 2100) %>%
  mutate(var = "Export") ->
  df_EX

# net export ----

IM %>% mutate(var = "IM") %>%
  bind_rows(EX %>%  mutate(var = "EX")) %>%
  spread(var, value) %>%
  mutate(NX = EX - IM) %>%
  select(scenario, region, year, value = NX) %>%
  REG() %>%
  DIFF() %>%
  filter(scenario == "E0",
         year == 2100) %>%
  mutate(var = "Net export") ->
  df_NX


# harvested area ----
query = "Land"

df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]

  filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T)

  input %>%
    gather_time() %>%
    select(-Units, -X) %>%
    mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}

df_sce <- do.call(rbind, df_list) %>%
  landleaf() %>%
  filter(year >= 2015) %>%
  mutate(value = value / 10) # mil ha

df_land <- df_sce %>%
  rename(sector = Crop) %>%
  landname() %>%
  mutate(sector = gsub("C4", "", sector)) %>%
  group_by(scenario, year, region, sector) %>%
  summarise(value = sum(value, na.rm = T))

df_land %>%
  filter(sector %in% c("Corn",  "Rice",  "Soybean",  "Wheat")) %>%
  group_by(scenario, year, region) %>%
  summarise(value = sum(value)) %>%
  REG() %>%
  DIFF() %>%
  filter(scenario == "E0",
         year == 2100) %>%
  mutate(var = "Harvested area") ->
  df_H

# crop price ----
# quantity-weighted crop price

query = "CropPrice"

df_list = list()
for (i in 1:length(SCENARIO)){
  sce_name = SCENARIO[i]
  filename = paste0(input.dir,'/',sce_name,'/',query,'.csv')
  filename
  input = read.csv(filename, skip = 1, header = T) %>%
    gather_time() %>%
    select(-Units, -X) %>%
    mutate(scenario = sce_name) -> middle
  names(middle) <- gsub("-", ".", names(middle)) # rename columns to replace dash to dot
  df_list[[i]] <- middle
}

# plot boxplot for the price impact
do.call(rbind, df_list) %>%
  filter(sector %in% c("Corn",  "Rice",  "Soybean",  "Wheat")) %>%
  select(scenario, region, sector, year, P = value) %>%
  left_join(Q %>%  select(scenario, region, sector, year, Q = value),
            by = c("scenario", "region", "sector", "year")) %>%
  mutate(exp = P * Q) %>%
  left_join(cluster, by = "region") %>%
  select(-region,-REG5_AR6, -REG10_AR6) %>%
  # left_join(cluster, by = "region") %>%
  # select(-region, -REG,-REG5_AR6) %>%
  # rename(REG = REG10_AR6) %>%
  # mutate(REG = ifelse(REG %in% c("EUROPE", "LATIN_AM", "MIDDLE_EAST", "PAC_OECD", "REF_ECON", "REST_ASIA"), "ROW", REG)) %>%
  group_by(scenario, REG, year) %>%
  summarise(exp = sum(exp, na.rm = T),
            Q = sum(Q, na.rm = T)) %>%
  mutate(value = exp / Q) %>%
  select(scenario, REG, year, value) %>%
  DIFF() %>%
  filter(scenario == "E0",
         year == 2100) %>%
  mutate(var = "Price") ->
  df_P

# combined ----

df_Q %>%
  bind_rows(df_EFF) %>%
  bind_rows(df_D) %>%
  # bind_rows(df_IM) %>%
  # bind_rows(df_EX) %>%
  bind_rows(df_NX) %>%
  bind_rows(df_H) %>%
  bind_rows(df_P) %>%
  mutate(REG = gsub("THE GLOBE", "WORLD", REG)) %>%
  ggplot() +
  # geom_point(aes(x = REG, y = index, color = var, shape = var),size = 3) +
  # geom_point(aes(x = REG, y = index, color = var), size = 4) +
  geom_text(aes(x = REG, y = index, color = var, label = var), size = 4) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "Relative change to static scenario (static = 0)", title = "") +
  scale_color_npg() +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "right") ->
  p; p
# ~ factor(var, c("Effective labor", "Production", "Consumption", "Price", "Net export" ,"Harvested area")))

df_Q %>%
  bind_rows(df_D) %>%
  bind_rows(df_IM) %>%
  bind_rows(df_EX) %>%
  bind_rows(df_NX) %>%
  bind_rows(df_H) %>%
  bind_rows(df_P) %>%
  left_join(df_EFF %>% select(scenario, REG, year, eff = index), by = c("scenario", "REG", "year")) %>%
  ggplot() +
  # geom_point(aes(x = REG, y = index, color = var, shape = var),size = 3) +
  # geom_point(aes(x = eff, y = index, color = var), size = 1) +
  geom_smooth(aes(x = eff, y = index, color = var), method = lm, se = F) +
  geom_hline(yintercept = 0,  linewidth = 0.5, color = "grey") +
  geom_vline(xintercept = 0,  linewidth = 0.5, color = "grey") +
  geom_abline(slope=1, color = "black", linetype = "dotdash", linewidth = 1.3) +
  labs(x = "% change in effective labor supplied at equilibrium (static = 0)",
       y = "% change in variables (static = 0)",
       title = "",
       color='') +
  scale_color_npg() +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = c(0.15, 0.85)) ->
  p; p

ggsave(filename = paste0(fig.dir, "Figure_Final/effective_lm.reg.png"), p,
       width = NWIDTH, height = NWIDTH, dpi = 300, units = "in", device='png')

df_Q %>%
  bind_rows(df_D) %>%
  bind_rows(df_IM) %>%
  bind_rows(df_EX) %>%
  bind_rows(df_NX) %>%
  bind_rows(df_H) %>%
  bind_rows(df_P) %>%
  left_join(df_EFF %>% select(scenario, REG, year, eff = index), by = c("scenario", "REG", "year")) %>%
  ggplot() +
  # geom_point(aes(x = REG, y = index, color = var, shape = var),size = 3) +
  geom_point(aes(x = eff, y = index, color = var),shape = 1, size =2) +
  geom_smooth(aes(x = eff, y = index, color = var), method = lm, se = F) +
  geom_hline(yintercept = 0,  linewidth = 0.5, color = "grey") +
  geom_vline(xintercept = 0,  linewidth = 0.5, color = "grey") +
  geom_abline(slope=1, color = "black", linetype = "dotdash", linewidth = 1.3) +
  labs(x = "% change in effective labor supplied at equilibrium (static = 0)",
       y = "% change in variables (static = 0)",
       title = "",
       color='') +
  xlim(-100, 100) +
  ylim(-100, 100) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = c(0.15, 0.85)) ->
  p; p

ggsave(filename = paste0(fig.dir, "Figure_Final/effective_lm_diag.reg.png"), p,
       width = 18, height = 18, dpi = 300, units = "in", device='png')

# weighted OLS ----

df_Q %>%
  bind_rows(df_D) %>%
  bind_rows(df_IM) %>%
  bind_rows(df_EX) %>%
  bind_rows(df_NX) %>%
  bind_rows(df_H) %>%
  bind_rows(df_P) %>%
  left_join(df_EFF %>% select(scenario, REG, year, eff = index), by = c("scenario", "REG", "year")) ->
  df_lm

W_OLS <- function(df_lm){
  VAR <- unique(df_lm$var)
  coeff_list <- list()
  for (i in VAR){
    df_lm %>%
      filter(var == i) -> df

    model <- lm(index ~ eff, data = df)
    plot(fitted(model), resid(model), xlab='Fitted Values', ylab='Residuals')

    bptest(model)

    wt <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2

    wls_model <- lm(index ~ eff, data = df, weights=wt)

    coeff <- wls_model$coefficients
    coeff <- as.data.frame(coeff)
    names(coeff) <- i

    coeff_list[[i]] <-  coeff
  }

  slope_all <- do.call(cbind, coeff_list)
  return(slope_all)
}

w_slope <- df_lm %>% W_OLS
w_slope$kb = rownames(w_slope)
w_slope %>%
  group_by(kb) %>%
  gather(var, value, Production:Price) %>%
  spread(kb, value) %>%
  rename(b = `(Intercept)`,
         k = eff)->
  kb

df_lm %>% left_join(kb, by = "var") %>%
  ggplot() +
  geom_point(aes(x = eff, y = index, color = var),shape = 1, size =2) +
  geom_abline(intercept = 2.26, slope = 0.243)


df_Q %>%
  bind_rows(df_D) %>%
  bind_rows(df_IM) %>%
  bind_rows(df_EX) %>%
  bind_rows(df_NX) %>%
  bind_rows(df_H) %>%
  bind_rows(df_P) %>%
  left_join(df_EFF %>% select(scenario, REG, year, eff = index), by = c("scenario", "REG", "year")) %>%
  ggplot() +
  geom_point(aes(x = eff, y = index, color = var),shape = 1, size =2) +
  # geom_smooth(aes(x = eff, y = index, color = var), method = lm, se = F) +
  geom_hline(yintercept = 0,  linewidth = 0.5, color = "grey") +
  geom_vline(xintercept = 0,  linewidth = 0.5, color = "grey") +
  geom_abline(mapping = aes(intercept = b, slope = k, col = var), data = kb, linewidth = 1.3) +
  geom_abline(slope=1, color = "black", linetype = "dotdash", linewidth = 1.3) +
  labs(x = "% change in effective labor supplied at equilibrium (static = 0)",
       y = "% change in variables (static = 0)",
       title = "Weighted OLS",
       color='') +
  xlim(-100, 100) +
  ylim(-100, 100) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() + theme0 + theme_leg + theme_add +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = c(0.15, 0.85)) -> p; p

ggsave(filename = paste0(fig.dir, "Figure_Final/effective_wlm_diag.reg.png"), p,
       width = 10, height = 10, dpi = 300, units = "in", device='png')
