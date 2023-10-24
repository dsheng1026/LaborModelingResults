library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)

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
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)

theme_leg <- theme(legend.position="right", legend.justification = "center",
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank())


gather_time <- function(.data){
  .data %>%
    tidyr::gather(year, value, names(.)[grepl("[0-9]{4}", names(.))]) %>%
    mutate(year = as.integer(gsub("X", "", year))) %>%
    return()
}

# ******** Figure 1.1: drivers ******** ----

YEAR_BASE <- 2015
YEAR_start <- 1975

POP <- read.csv("POP_SSP2.csv")

# ** REGIONAL ** ----

# population ----

POP %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = hist/10^3, color = "Historical"), linewidth = 1.4, linetype = "dotted") +
  geom_line(aes(x = year, y = futr/10^3, color = "Future"), linewidth = 1.4) +
  geom_vline(xintercept = YEAR_BASE) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  labs(x = "Year", y = "Million people",
       color = "Period") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 20)) +
  scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p

ggsave(filename = paste0(fig.dir, "Figure1/POP_reg.png"), p,
       width = 16, height = 10, dpi = 300, units = "in", device='png')

# rural population ----

RuralPop <- read.csv("RuralPop.csv")
RuralPop %>%
  filter(year >= 2015) %>%
  rename(futr = Rural_pop) %>%
  full_join(WB_rural, by = c("region", "year")) ->
  RURALPOP

RURALPOP %>%
  ggplot() +
  geom_line(aes(x = year, y = hist/10^6, color = "Historical"), linewidth = 1.4, linetype = "dotted") +
  geom_line(aes(x = year, y = futr/10^6, color = "Future"), linewidth = 1.4) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Million people",
       color = "Period",
       title="Rural population") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 20)) +
  scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p

ggsave(filename = paste0(fig.dir, "Figure1/RURALPOP_reg.png"), p,
       width = 16, height = 10, dpi = 300, units = "in", device='png')


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

# drivers combined ----
LF %>%
  mutate(source = ifelse(year <= 2015, "hist", "futr")) %>%
  spread(source, value = LF) %>%
  mutate(futr = ifelse(year == 2015, hist, futr),
         value = "Labor force") %>%
  filter(year <= 2100) %>%
  select(region, year, hist, futr, POP = value) %>%
  bind_rows(df_rural) %>%
  filter(year >= YEAR_start) -> df_comb

df_comb %>%
  ggplot() +
  geom_line(aes(x = year, y = hist, color = POP), linewidth = 1.4) +
  geom_line(aes(x = year, y = futr, color = POP), linewidth = 1.4, linetype = "dotted") +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Million people",
       color = "Population") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 20)) +
  # scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p

P.1.5 <- p;

ggsave(filename = paste0(fig.dir, "Figure1/POP_combined_reg.png"), P.1.5,
       width = 16, height = 10, dpi = 300, units = "in", device='png')




# ** GLOBAL ** ----

POP %>%
  group_by(year) %>%
  summarise(hist = sum(hist),
            futr = sum(futr)) ->
  POP_glb

POP_glb %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = hist/10^3, color = "Historical"), linewidth = 1.4, linetype = "dotted") +
  geom_line(aes(x = year, y = futr/10^3, color = "Future"), linewidth = 1.4) +
  geom_vline(xintercept = YEAR_BASE) +
  labs(x = "Year", y = "Million people",
       color = "Period") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 10)) +
  scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p

P.1.1 <- p + theme(legend.position = 'none'); P.1.1

ggsave(filename = paste0(fig.dir, "Figure1/POP.png"), P.1.1,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# rural population ----
RURALPOP %>%
  filter(year >= YEAR_start) %>%
  group_by(year) %>%
  summarise(futr = sum(futr)/10^6, # ppl to mpl
            hist = sum(hist)/10^6) ->
  RURALPOP_glb

RURALPOP_glb %>%
  ggplot() +
  geom_line(aes(x = year, y = hist, color = "Historical"), linewidth = 1.4, linetype = "dotted") +
  geom_line(aes(x = year, y = futr, color = "Future"), linewidth = 1.4) +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Million people",
       color = "Period",
       title="Rural population") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 10)) +
  scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p

P.1.2 <- p + theme(legend.position = 'none'); P.1.2

ggsave(filename = paste0(fig.dir, "Figure1/POP_rural.png"), P.1.2,
       width = 8, height = 10, dpi = 300, units = "in", device='png')

# drivers combined ----
LF %>%
  group_by(year) %>%
  summarise(value = sum(LF)) %>%
  mutate(source = ifelse(year <= 2015, "hist", "futr")) %>%
  spread(source, value) %>%
  mutate(futr = ifelse(year == 2015, hist, futr),
         value = "Labor force") %>%
  filter(year <= 2100) %>%
  bind_rows(df_rural %>%
              group_by(year, POP) %>%
              summarise(hist = sum(hist),
                        futr = sum(futr)) %>%
              rename(value = POP) %>%
              select(year, futr, hist, value)) %>%
  filter(year >= YEAR_start) -> df_comb

df_comb %>%
  ggplot() +
  geom_line(aes(x = year, y = hist, color = value), linewidth = 1.4) +
  geom_line(aes(x = year, y = futr, color = value), linewidth = 1.4, linetype = "dotted") +
  # geom_point(aes(x = year, y = value/10^6, color = source), size = 2) +
  # facet_wrap(~ region, ncol = 8, scales = "free_y") +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Million people",
       color = "Population") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 10)) +
  # scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p

P.1.5 <- p; P.1.5

ggsave(filename = paste0(fig.dir, "Figure1/POP_combined.png"), P.1.5,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# ******** Figure 1.2: labor metrics ******** ----

SCENARIO <- c("E3", "E0")

SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = gsub("E0","New reference", scenario),
           scenario = gsub("E3","Old reference", scenario)) %>%
    return()
}

# ** REGIONAL ** ----

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
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) -> # mpl
  df_LL

USDA_Labor %>%
  mutate(scenario = "E0") %>%
  bind_rows(USDA_Labor %>%
              mutate(scenario = "E3")) %>%
  group_by(scenario, region, year) %>%
  summarise(hist = sum(labor_ppl) / 10^6) %>%  # mpl
  full_join(df_LL %>% rename(futr = value) %>% filter(year >= 2015),
            by = c("scenario","region" ,"year")) %>%
  SCE_NM() ->
  df_LLL

df_LLL %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = futr, color = scenario), linewidth = 1.4, linetype = "dotdash") +
  geom_line(aes(x = year, y = hist, color = scenario), linewidth = 1.4) +
  geom_vline(xintercept = 2015) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  labs(x = "Year", y = "Million people") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 20)) +
  # scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p
P.1.7 <- p;

ggsave(filename = paste0(fig.dir, "Figure1/labor_demand_reg.png"), P.1.7,
       width = 16, height = 10, dpi = 300, units = "in", device='png')

# capital demand ----
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
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) -> # mpl
  df_KK

USDA_Capital %>%
  mutate(scenario = "E0") %>%
  bind_rows(USDA_Capital %>%
              mutate(scenario = "E3")) %>%
  group_by(scenario, region, year) %>%
  summarise(hist = sum(capital_1975USD) / 10^9) %>%  # bil 1975$
  full_join(df_KK %>% rename(futr = value) %>% filter(year >= 2015),
            by = c("scenario","region" ,"year")) %>%
  SCE_NM() ->
  df_KKK

df_KKK %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = futr, color = scenario), linewidth = 1.4, linetype = "dotdash") +
  geom_line(aes(x = year, y = hist, color = scenario), linewidth = 1.4) +
  geom_vline(xintercept = 2015) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  labs(x = "Year", y = "Billion 1975$") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 10)) +
  # scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p
P.1.8 <- p; P.1.8

ggsave(filename = paste0(fig.dir, "Figure1/capital_demand_reg.png"), P.1.8,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# L/K ratio ----
df_LLL %>%
  gather(time, labor, hist:futr) %>%
  left_join(df_KKK %>% gather(time, capital, hist:futr),
            by = c("scenario", "region", "year", "time")) %>%
  mutate(ratio = labor/capital) %>%
  select(-labor, -capital) %>%
  spread(time, ratio) -> df_ratio

df_ratio %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = futr, color = scenario), linewidth = 1.4, linetype = "dotdash") +
  geom_line(aes(x = year, y = hist, color = scenario), linewidth = 1.4) +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Million people per billion 1975$") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 20)) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  # scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p
P.1.6 <- p; P.1.6

ggsave(filename = paste0(fig.dir, "Figure1/LK_ratio_reg.png"), P.1.6,
       width = 16, height = 10, dpi = 300, units = "in", device='png')


# ag labor / labor force ----
LF %>%
  mutate(scenario = "E0") %>%
  bind_rows(LF %>%
              mutate(scenario = "E3")) %>%
  SCE_NM() %>%
  left_join(df_LLL %>%
              mutate(labor = ifelse(year <= 2015, hist, futr)) %>%
              select(scenario, region, year, labor),
            by = c("scenario", "region" ,"year")) %>%
  mutate(share = labor / LF) %>%
  mutate(period = ifelse(year < 2015, "hist", "futr")) %>%
  select(scenario, region, year, share, period) %>%
  spread(period, share) %>%
  mutate(hist = ifelse(year == 2015, futr, hist)) ->
  df_share

df_share %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = futr, color = scenario), linewidth = 1.4, linetype = "dotdash") +
  geom_line(aes(x = year, y = hist, color = scenario), linewidth = 1.4) +
  geom_vline(xintercept = 2015) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  labs(x = "Year", y = "Ag labor to labor force (%)") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 20)) +
  # scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p
P.1.9 <- p;

ggsave(filename = paste0(fig.dir, "Figure1/aglabor_share_reg.png"), P.1.9,
       width = 16, height = 10, dpi = 300, units = "in", device='png')

# effective labor ----

# read in calibrated tfp = labor productivity growth
gcam_macro_TFP_open <-  read.csv("gcam_macro_TFP_open.csv", skip = 6, header = T) %>%
  filter(scenario == "gSSP2")

df_LL %>%
  left_join(gcam_macro_TFP_open %>% select(-scenario),
            by = c("region", "year")) %>%
  mutate(productivity = ifelse(year<=2015, 1, productivity),
         effective.labor = value * productivity) ->
  df_eff

df_eff %>%
  SCE_NM() %>%
  filter(year >= 2015) %>%
  ggplot() +
  geom_line(aes(x = year, y = effective.labor, color = scenario), linewidth = 1.2) +
  facet_wrap(~ region, ncol = 8, scales = "free_y") +
  labs(x = "Year", y = "Effective labor") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 20)) +
  # scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p
P.1.0 <- p;

ggsave(filename = paste0(fig.dir, "Figure1/effective_labor_reg.png"), P.1.0,
       width = 16, height = 10, dpi = 300, units = "in", device='png')

# ** GLOBAL ** ----

# labor demand ----

df_LLL %>%
  filter(year >= YEAR_start) %>%
  group_by(scenario, year) %>%
  summarise(hist = sum(hist),
            futr = sum(futr)) %>%
  ggplot() +
  geom_line(aes(x = year, y = futr, color = scenario), linewidth = 1.4, linetype = "dotdash") +
  geom_line(aes(x = year, y = hist, color = scenario), linewidth = 1.4) +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Million people") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 10)) +
  # scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p
P.1.7 <- p;

ggsave(filename = paste0(fig.dir, "Figure1/labor_demand.png"), P.1.7,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# capital demand ----

df_KKK %>%
  filter(year >= YEAR_start) %>%
  group_by(scenario, year) %>%
  summarise(hist = sum(hist),
            futr = sum(futr)) %>%
  ggplot() +
  geom_line(aes(x = year, y = futr, color = scenario), linewidth = 1.4, linetype = "dotdash") +
  geom_line(aes(x = year, y = hist, color = scenario), linewidth = 1.4) +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Billion 1975$") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 10)) +
  # scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p
P.1.8 <- p; P.1.8

ggsave(filename = paste0(fig.dir, "Figure1/capital_demand.png"), P.1.8,
       width = 8, height = 10, dpi = 300, units = "in", device='png')

# L/K ratio ----
df_LLL %>%
  gather(time, labor, hist:futr) %>%
  left_join(df_KKK %>% gather(time, capital, hist:futr),
            by = c("scenario", "region", "year", "time")) %>%
  group_by(scenario, year, time ) %>%
  summarise(labor = sum(labor),
            capital= sum(capital)) %>%
  mutate(ratio = labor/capital) %>%
  select(-labor, -capital) %>%
  spread(time, ratio) -> df_ratio

df_ratio %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = futr, color = scenario), linewidth = 1.4, linetype = "dotdash") +
  geom_line(aes(x = year, y = hist, color = scenario), linewidth = 1.4) +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Million people per billion 1975$") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 10)) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p
P.1.6 <- p; P.1.6

ggsave(filename = paste0(fig.dir, "Figure1/LK_ratio.png"), P.1.6,
       width = 16, height = 10, dpi = 300, units = "in", device='png')

# ag labor / labor force ----
LF %>%
  mutate(scenario = "E0") %>%
  bind_rows(LF %>%
              mutate(scenario = "E3")) %>%
  SCE_NM() %>%
  left_join(df_LLL %>%
              mutate(labor = ifelse(year <= 2015, hist, futr)) %>%
              select(scenario, region, year, labor),
            by = c("scenario", "region" ,"year")) %>%
  group_by(scenario, year) %>%
  summarise(LF = sum(LF),
            labor = sum(labor)) %>%
  mutate(share = labor / LF) %>%
  mutate(period = ifelse(year < 2015, "hist", "futr")) %>%
  select(scenario,year, share, period) %>%
  spread(period, share) %>%
  mutate(hist = ifelse(year == 2015, futr, hist)) ->
  df_share

df_share %>%
  filter(year >= YEAR_start) %>%
  ggplot() +
  geom_line(aes(x = year, y = futr, color = scenario), linewidth = 1.4, linetype = "dotdash") +
  geom_line(aes(x = year, y = hist, color = scenario), linewidth = 1.4) +
  geom_vline(xintercept = 2015) +
  labs(x = "Year", y = "Ag labor to labor force (%)") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 20)) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p
P.1.9 <- p;

ggsave(filename = paste0(fig.dir, "Figure1/aglabor_share.png"), P.1.9,
       width = 16, height = 10, dpi = 300, units = "in", device='png')

# effective labor ----
df_eff %>%
  group_by(scenario, year) %>%
  summarise(effective.labor = sum(effective.labor)) %>%
  SCE_NM() %>%
  filter(year >= 2015) %>%
  ggplot() +
  geom_line(aes(x = year, y = effective.labor, color = scenario), linewidth = 1.2) +
  labs(x = "Year", y = "Effective labor") +
  scale_x_continuous(breaks = seq(YEAR_start, 2100, by = 5)) +
  # scale_color_discrete(labels = c("Future", "Historical")) +
  theme_bw() + theme0 + theme_leg +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p; p
P.1.0 <- p;

ggsave(filename = paste0(fig.dir, "Figure1/effective_labor.png"), P.1.0,
       width = 8, height = 10, dpi = 300, units = "in", device='png')
