library(ggsci)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyr)
# library(patchwork)
# pre-define process ----

fig.dir <- "Figure/" # subfolder path are defined with ggsave
input.dir <- "../"
iso_GCAM_regID <-  read.csv("C:/Model/heatstress/input/gcamdata/inst/extdata/common/iso_GCAM_regID.csv", skip = 6, header = T)
GCAM_region_names <-  read.csv("C:/Model/heatstress/input/gcamdata/inst/extdata/common/GCAM_region_names.csv", skip = 6, header = T)


# globally defined function: gather_years
gather_time <- function(.data){
  .data %>%
    tidyr::gather(year, value, names(.)[grepl("[0-9]{4}", names(.))]) %>%
    mutate(year = as.integer(gsub("X", "", year))) %>%
    return()
}

# globally define region name abbreviation function
short_name <- function(df){
  df %>%
    mutate(region = gsub("Central America and Caribbean", "Caribbean", region),
           region = gsub("European Free Trade Association", "EU Free Trade", region),
           region = gsub("South America_Southern", "South America_S" , region),
           region = gsub("South America_Northern", "South America_N" , region)) ->
    df
  return(df)
}

landleaf <- function(df){
  splits <- strsplit(df$LandLeaf, '_')
  splits <- do.call(rbind, splits)
  df[c("Crop","Basin","Ref_Irr", "hi_lo")] <- splits
  return(df)
}

landname <- function(df){
  df %>%
    mutate(sector = gsub("C4","", sector),
           sector = ifelse(grepl("biomass",sector), "biomass", sector),
           sector = ifelse(grepl("Fruits",sector), "Fruits", sector),
           sector = ifelse(grepl("MiscCrop",sector), "MiscCrop", sector),
           sector = ifelse(grepl("NutsSeeds",sector), "MiscCrop", sector),
           sector = ifelse(grepl("Fodder",sector), "Fodder Crops", sector),
           sector = ifelse(grepl("Oil",sector), "OilPlant", sector)) ->
    df
  return(df)
}

CLUSTER_CROP <- function(.data){
  .data %>%
    dplyr::filter(!sector %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")) %>%
    dplyr::mutate(group = sector,
                  group = ifelse(sector %in% c("Corn", "OtherGrain", "Rice", "RootTuber", "Wheat"), "Staples", group),
                  group = ifelse(sector %in% c("FodderGrass", "FodderHerb"), "Fodder Crops", group),
                  group = ifelse(sector %in% c("Fruits", "Vegetables"), "Fruits&Vege", group),
                  group = ifelse(sector %in% c("Legumes", "MiscCrop", "NutsSeeds",  "FiberCrop"), "Other Crops", group),
                  group = ifelse(sector %in% c("OilCrop","OilPalm", "Soybean"), "Oil Crops", group),
                  group = ifelse(sector == "biomass", "Biomass", group)) %>%
    return()
}

CLUSTER_CROP2 <- function(.data){
  .data %>%
    dplyr::mutate(group = ifelse(group == "Fruits&Vege", "Other Crops", group)) %>%
    return()
}

SECTOR <- function(.data){
  .data %>%
    dplyr::mutate(sector = gsub("regional ", "", sector),
                  sector = gsub("_", " ", sector),
                  sector = gsub(" ", "", sector)) %>%
    return()
}

# define plot theme ----
fontfamily = "Arial"
windowsFonts("Arial" = windowsFont("Arial"))

theme0 <- theme(
  #panel.grid.minor = element_line(size = 0.1, linetype = 2,colour = "grey75"),panel.grid.major = element_line(size = 0.1, linetype = 2,colour = "grey75"),
  #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  #axis.ticks = element_line(linetype = 1,size = 0.5),
  #axis.ticks.length = unit(-0.1, "cm"),
  #axis.text.y.right =  element_blank(),  axis.title.y.right = element_blank(),
  axis.text.x.top =  element_blank(),  axis.title.x.top = element_blank(),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)

theme_leg <- theme(legend.position="right", legend.justification = "center",
                   #legend.position=c(.1,0.7),
                   #legend.title = element_blank(),
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank())

SCENARIO <- c("E0")


# legend position
LP = "none"

# Figure 2 ----
# Fig. 2.4: production ----
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
df_crop <- do.call(rbind, df_list) %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2() %>%
  dplyr::filter(!group %in% c("Forest", "Pasture"))

df_sector <- df_crop %>%
  dplyr::select(sector) %>%
  unique() %>%
  dplyr::rename(SECTOR = sector) %>%
  dplyr::mutate(sector = tolower(SECTOR))

# simple sum for production
Figure <- df_crop %>%
  dplyr::group_by(scenario, year, group) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::filter(year >= 2015,
                group != "Biomass") %>%
  group_by(scenario, group) %>%
  dplyr::mutate(index = value / value[year == 2015],
                index = ifelse(group == "Biomass", value / value[year == 2025], index),
                index = ifelse(group == "Biomass" & year < 2025, NA, index))

MEAN <- Figure %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  mutate(index = value / value[year == 2015])


y_lim_u <-  max(Figure$index) + 0.2;  y_lim_u
y_lim_l <-  min(Figure$index) - 0.2;  y_lim_l

Figure %>%
  mutate() %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = group), linewidth = 1.4) +
  #  geom_ribbon(aes(x = year, ymin = upper, ymax = lower, group=group, fill=group), color= NA, alpha = 0.2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_line(data = MEAN,
            aes(x = year, y = index), linetype = "dotted", linewidth = 1.4) +
  labs(x = "Year", y = "Relative Change (2015 = 1)") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) +
  # scale_y_continuous(expand = c(0, 0)) ->
  scale_y_continuous(expand = c(0, 0), limits = c(y_lim_l, y_lim_u)) ->
  p; p


P2.4 <- p + labs(title = "Production"); P2.4


ggsave(filename = paste0(fig.dir, "Figure2/Figure2.4.png"), P2.4,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 2.6: Labor demand ----
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
df_L <- do.call(rbind, df_list)

df_LL <- df_L %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2() %>%
  # filter(!group %in% c("Forest", "Pasture"))
  filter(!group %in% c("Forest", "Pasture", "Biomass"))

# simple sum for labor
Figure <- df_LL %>%
  group_by(scenario, year, group) %>%
  summarise(value = sum(value)) %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, group) %>%
  mutate(index = value / value[year == 2015],
         index = ifelse(group == "Biomass", value / value[year == 2025], index),
         index = ifelse(group == "Biomass" & year < 2025, NA, index))

MEAN <- Figure %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  mutate(index = value / value[year == 2015])


y_lim_u <-  max(Figure$index) + 0.2;  y_lim_u
y_lim_l <-  min(Figure$index) - 0.2;  y_lim_l
Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = group), linewidth = 1.4) +
  #  geom_ribbon(aes(x = year, ymin = upper, ymax = lower, group=group, fill=group), color= NA, alpha = 0.2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_line(data = MEAN,
            aes(x = year, y = index), linetype = "dotted", linewidth = 1.4) +
  labs(x = "Year", y = "Relative Change (2015 = 1)") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) +
  # scale_y_continuous(expand = c(0, 0)) ->
  scale_y_continuous(expand = c(0, 0), limits = c(y_lim_l, y_lim_u)) ->
  p; p


P2.6 <- p + labs(title = "Labor demand"); P2.6


ggsave(filename = paste0(fig.dir, "Figure2/Figure2.6.png"), P2.6,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 2.1: wage rate ----
# labor demand-weighted wage rate

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
  mutate(region = gsub("Labor_Ag", "", market))

# absolute value of wage rate
ribbon <- df_W %>%
  group_by(scenario, year) %>%
  summarize(MAX = max(value),
            MIN = min(value),
            MEAN = mean(value))

# weighted average global wage rate
df_L %>%
  filter(year >= 2015) %>%
  group_by(scenario, region, year) %>%
  summarise(Labor = sum(value)) %>%
  left_join(df_W %>% select(-market) %>% rename(wage = value), by = c("scenario", "region", "year")) %>%
  mutate(EXP = Labor * wage) %>% # mpl * 1000 dollar
  group_by(scenario, year) %>%
  summarise(EXP = sum(EXP),
            Labor = sum(Labor)) %>%
  mutate(W = EXP / Labor) %>%
  group_by(scenario) %>%
  mutate(index = W / W[year == 2015])->
  df_W_index

df_W_index %>%
  ggplot() +
  geom_line(aes(x = year, y = W)) +
  geom_ribbon(data = ribbon, aes(x = year, ymin = MIN, ymax = MAX), color= NA, alpha = 0.2)

df_W %>%
  group_by(year) %>%
  summarize(q1 = quantile(value, probs = 0.25),
            q3 = quantile(value, probs = 0.75),
            q2 = quantile(value, probs = 0.5),
            iqr = q3 - q1,
            index01 = quantile(value, probs = 0.1),
            index09 = quantile(value, probs = 0.9)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot() +
  geom_boxplot(aes(x = year, ymin = index01, lower = q1, middle = q2, upper = q3, ymax = index09),
               alpha=0.5, lwd=0.5, fatten = 1, stat = "identity") +  # whisker to 10% and 90%
  geom_point(data = df_W_index %>% mutate(year = as.factor(year)), aes(x = year, y = W), size = 1.5) +
  labs(x = "Year", y = "Thousand dollars (1975$)") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 20)) ->
  p; p


P2.1 <- p + labs(title = "Wage rate"); P2.1

ggsave(filename = paste0(fig.dir, "Figure2/Figure2.1.png"), P2.1,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 2.2: crop price ----
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
df_P <- do.call(rbind, df_list) %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2() %>%
  filter(!group %in% c("Forest", "Pasture"))

Figure <- df_crop %>%
  filter(group != "Biomass") %>%
  rename(Q = value) %>%
  left_join(df_P %>% filter(group != "Biomass") %>% rename(P = value),
            by = c("scenario", "region", "sector", "group", "year")) %>%
  mutate(EXP = P * Q) %>%
  group_by(scenario, year, group) %>%
  summarise(EXP = sum(EXP, na.rm = T),
            Q = sum(Q, na.rm = T)) %>%
  mutate(wp = EXP / Q) %>%
  group_by(scenario, group) %>%
  mutate(index = wp / wp[year == 2015]) %>%
  filter(group!= "UnmanagedLand") %>%
  filter(year >= 2015)

MEAN <- Figure %>%
  group_by(year) %>%
  summarise(EXP = sum(EXP),
            Q = sum(Q)) %>%
  mutate(wp = EXP / Q,
         index = wp / wp[year == 2015])

y_lim_u <-  max(Figure$index) + 0.2;  y_lim_u
y_lim_l <-  min(Figure$index) - 0.2;  y_lim_l
Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = group), linewidth = 1.4) +
  #  geom_ribbon(aes(x = year, ymin = upper, ymax = lower, group=group, fill=group), color= NA, alpha = 0.2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_line(data = MEAN,
            aes(x = year, y = index), linetype = "dotted", linewidth = 1.4) +
  labs(x = "Year", y = "Relative Change (2015 = 1)") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) +
  # scale_y_continuous(expand = c(0, 0)) ->
  scale_y_continuous(expand = c(0, 0), limits = c(y_lim_l, y_lim_u)) ->
  p; p


P2.2 <- p + labs(title = "Crop price"); P2.2


ggsave(filename = paste0(fig.dir, "Figure2/Figure2.2.png"), P2.2,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig 2.3: consumption ----

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
  # middle %>%
  #   group_by(scenario, region, sector, year) %>%
  #   summarise(value = sum(value)) %>%
  #   ungroup()-> middle
  df_list[[i]] <- middle
}
D_crop <- do.call(rbind, df_list) %>%
  group_by(scenario, region, input, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(sector = input) %>%
  SECTOR() %>%
  mutate(sector = tolower(sector)) %>%
  left_join(df_sector, by = "sector") %>%
  select(-sector) %>%
  rename(sector = SECTOR) %>%
  na.omit() %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2()

# simple sum for consumption
Figure <- D_crop %>%
  group_by(scenario, year, group) %>%
  summarise(value = sum(value)) %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, group) %>%
  mutate(index = value / value[year == 2015],
         index = ifelse(group == "Biomass", value / value[year == 2025], index),
         index = ifelse(group == "Biomass" & year < 2025, NA, index))

MEAN <- Figure %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  mutate(index = value / value[year == 2015])


y_lim_u <-  max(Figure$index) + 0.2;  y_lim_u
y_lim_l <-  min(Figure$index) - 0.2;  y_lim_l
Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = group), linewidth = 1.4) +
  #  geom_ribbon(aes(x = year, ymin = upper, ymax = lower, group=group, fill=group), color= NA, alpha = 0.2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_line(data = MEAN,
            aes(x = year, y = index), linetype = "dotted", linewidth = 1.4) +
  labs(x = "Year", y = "Relative Change (2015 = 1)") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) +
  # scale_y_continuous(expand = c(0, 0)) ->
  scale_y_continuous(expand = c(0, 0), limits = c(y_lim_l, y_lim_u)) ->
  p; p

P2.3 <- p + labs(title = "Consumption"); P2.3

ggsave(filename = paste0(fig.dir, "Figure2/Figure2.3.png"), P2.3,
       width = 8, height = 10, dpi = 300, units = "in", device='png')

# Fig. 2.5: Labor yld: crop ----
# revealed; aggregated

Figure <- df_LL %>%
  filter(year >= 2015) %>%
  rename(L = value) %>%
  left_join(df_crop %>%  rename(Q = value),
            by = c("scenario", "region", "sector", "year", "group")) %>%
  group_by(scenario, group, year) %>%
  summarise(L = sum(L),
            Q = sum(Q)) %>%
  mutate(yld = Q / L) %>%
  group_by(scenario, group) %>%
  mutate(index = yld / yld[year == 2015]) %>%
  filter(group!="Biomass")

# Q-weighted average labor yld
MEAN <- Figure %>%
  group_by(year) %>%
  summarise(L = sum(L),
            Q = sum(Q)) %>%
  mutate(yld = Q / L,
         index = yld / yld[year == 2015])

y_lim_u <-  max(Figure$index) + 0.2;  y_lim_u
y_lim_l <-  min(Figure$index) - 0.2;  y_lim_l
Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = group), linewidth = 1.4) +
  #  geom_ribbon(aes(x = year, ymin = upper, ymax = lower, group=group, fill=group), color= NA, alpha = 0.2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_line(data = MEAN,
            aes(x = year, y = index), linetype = "dotted", linewidth = 1.4) +
  labs(x = "Year", y = "Relative Change (2015 = 1)") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) +
  # scale_y_continuous(expand = c(0, 0)) ->
  scale_y_continuous(expand = c(0, 0), limits = c(y_lim_l, y_lim_u)) ->
  p; p

P2.5 <- p + labs(title = "Labor productivity"); P2.5

ggsave(filename = paste0(fig.dir, "Figure2/Figure2.5.png"), P2.5,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


P2.5 <- ggarrange(p, b ,nrow=1, ncol = 2, widths = c(0.9,0.1), common.legend = F) %>%
  annotate_figure(top = text_grob("Labor yield", face = "bold", size = 16)); P2.5

ggsave(filename = paste0(fig.dir, "DocumentationPaper/Figure2/Figure2.5.png"), P2.5,
       width = 8, height = 10, dpi = 300, units = "in", device='png')

# Fig. 2.7: Harvested area: crop ----

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
  group_by(scenario, year, sector) %>%
  summarise(value = sum(value, na.rm = T))

df_land %>%
  filter(sector %in% c("Corn", "FiberCrop", "Fodder Crops", "Fruits", "Legumes", "MIscCrop",
                       "OilPlant", "OtherGrain", "Rice", "RootTuber", "Soybean", "SugarCrop",
                       "Vegetables", "Wheat", "biomass")) %>%
  mutate(sector = ifelse(sector == "OilPlant", "Oil Crops", sector)) %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2() %>%
  filter(group != "Biomass") ->
  df_LAND

# simple sum for land
Figure <- df_LAND %>%
  group_by(scenario, year, group) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, group) %>%
  mutate(index = value / value[year == 2015],
         index = ifelse(group == "Biomass", value / value[year == 2025], index),
         index = ifelse(group == "Biomass" & year < 2025, NA, index))

# boxplot for 2100 only
box <- Figure %>%
  filter(year ==2100) %>%
  group_by(scenario,  year) %>%
  # mutate(index01 = quantile(index, probs = 0.1),
  #        index09 = quantile(index, probs = 0.9))  %>% # keep 10% ~ 90% data
  # filter(index >= index01, index <= index09) %>%
  summarise(q1 = quantile(index, probs = 0.25),
            q3 = quantile(index, probs = 0.75),
            q2 = quantile(index, probs = 0.5),
            iqr = q3 - q1,
            wu = max(index),
            wl = min(index)) %>%
  mutate(year = as.factor(year))

MEAN <- Figure %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  mutate(index = value / value[year == 2015])


y_lim_u <-  max(Figure$index) + 0.2;  y_lim_u
y_lim_l <-  min(Figure$index) - 0.2;  y_lim_l
Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = group), linewidth = 1.4) +
  #  geom_ribbon(aes(x = year, ymin = upper, ymax = lower, group=group, fill=group), color= NA, alpha = 0.2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_line(data = MEAN,
            aes(x = year, y = index), linetype = "dotted", linewidth = 1.4) +
  labs(x = "Year", y = "Relative Change (2015 = 1)") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) +
  # scale_y_continuous(expand = c(0, 0)) ->
  scale_y_continuous(expand = c(0, 0), limits = c(y_lim_l, y_lim_u)) ->
  p; p

P2.7 <- p + labs(title = "Harvested area"); P2.7

ggsave(filename = paste0(fig.dir, "Figure2/Figure2.7.png"), P2.7,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 2.8: Land yld: crop ----
# revealed; aggregated

Figure <- df_crop %>%
  group_by(scenario, year, group) %>%
  summarise(Q = sum(value, na.rm = T)) %>%
  left_join(df_LAND %>%
              group_by(scenario, year, group) %>%
              summarise(Land = sum(value, na.rm = T)),
            by = c("scenario", "year", "group")) %>%
  filter(year >= 2015) %>%
  filter(group!= "Biomass") %>%
  mutate(yld = Q / Land) %>%
  group_by(scenario, group) %>%
  mutate(index = yld / yld[year == 2015])

# Q-weighted average labor yld
MEAN <- Figure %>%
  group_by(year) %>%
  summarise(Land = sum(Land),
            Q = sum(Q)) %>%
  mutate(yld = Q / Land,
         index = yld / yld[year == 2015])


y_lim_u <-  max(Figure$index) + 0.2;  y_lim_u
y_lim_l <-  min(Figure$index) - 0.2;  y_lim_l
Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = group), linewidth = 1.4) +
  #  geom_ribbon(aes(x = year, ymin = upper, ymax = lower, group=group, fill=group), color= NA, alpha = 0.2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_line(data = MEAN,
            aes(x = year, y = index), linetype = "dotted", linewidth = 1.4) +
  labs(x = "Year", y = "Relative Change (2015 = 1)") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) +
  # scale_y_continuous(expand = c(0, 0)) ->
  scale_y_continuous(expand = c(0, 0), limits = c(y_lim_l, y_lim_u)) ->
  p; p


P2.8 <- p + labs(title = "Crop yield"); P2.8


ggsave(filename = paste0(fig.dir, "Figure2/Figure2.8.png"), P2.8,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 2.9: import: crop ----
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

df_IM <- do.call(rbind, df_list) %>%
  filter(grepl('imported', subsector)) %>%
  select(scenario, region, input, year, value) %>%
  rename(sector = input) %>%
  mutate(type = "import",
         sector = gsub("traded ", "", sector)) %>%
  SECTOR() %>%
  left_join(df_sector, by = "sector") %>%
  select(-sector) %>%
  rename(sector = SECTOR) %>%
  na.omit() %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2()


# simple sum for consumption
Figure <- df_IM %>%
  group_by(scenario, year, group) %>%
  summarise(value = sum(value)) %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, group) %>%
  mutate(index = value / value[year == 2015],
         index = ifelse(group == "Biomass", value / value[year == 2025], index),
         index = ifelse(group == "Biomass" & year < 2025, NA, index))

MEAN <- Figure %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  mutate(index = value / value[year == 2015])


y_lim_u <-  max(Figure$index) + 0.2;  y_lim_u
y_lim_l <-  min(Figure$index) - 0.2;  y_lim_l
Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = group), linewidth = 1.4) +
  #  geom_ribbon(aes(x = year, ymin = upper, ymax = lower, group=group, fill=group), color= NA, alpha = 0.2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_line(data = MEAN,
            aes(x = year, y = index), linetype = "dotted", linewidth = 1.4) +
  labs(x = "Year", y = "Relative Change (2015 = 1)") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) +
  # scale_y_continuous(expand = c(0, 0)) ->
  scale_y_continuous(expand = c(0, 0), limits = c(y_lim_l, y_lim_u)) ->
  p; p

P2.9 <- p + labs(title = "Import"); P2.9

ggsave(filename = paste0(fig.dir, "Figure2/Figure2.9.png"), P2.9,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 2.10: export: crop ----
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

df_EX <- do.call(rbind, df_list) %>% # data.frame that collects regional GDP across scenarios
  mutate(region = gsub(" traded.*","",subsector),
         input = tolower(sector),
         input = gsub("traded ", "", input),
         type = "export") %>%
  select(scenario, region, input, year, value, type) %>%
  rename(sector = input) %>%
  SECTOR() %>%
  left_join(df_sector, by = "sector") %>%
  select(-sector) %>%
  rename(sector = SECTOR) %>%
  na.omit() %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2()


# simple sum for consumption
Figure <- df_EX %>%
  group_by(scenario, year, group) %>%
  summarise(value = sum(value)) %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, group) %>%
  mutate(index = value / value[year == 2015],
         index = ifelse(group == "Biomass", value / value[year == 2025], index),
         index = ifelse(group == "Biomass" & year < 2025, NA, index))

MEAN <- Figure %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  mutate(index = value / value[year == 2015])


y_lim_u <-  max(Figure$index) + 0.2;  y_lim_u
y_lim_l <-  min(Figure$index) - 0.2;  y_lim_l
Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = index, color = group), linewidth = 1.4) +
  #  geom_ribbon(aes(x = year, ymin = upper, ymax = lower, group=group, fill=group), color= NA, alpha = 0.2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_line(data = MEAN,
            aes(x = year, y = index), linetype = "dotted", linewidth = 1.4) +
  labs(x = "Year", y = "Relative Change (2015 = 1)") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=16, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = LP) +
  # scale_y_continuous(expand = c(0, 0)) ->
  scale_y_continuous(expand = c(0, 0), limits = c(y_lim_l, y_lim_u)) ->
  p; p

P2.10 <- p + labs(title = "Export"); P2.10

ggsave(filename = paste0(fig.dir, "Figure2/Figure2.10.png"), P2.10,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


