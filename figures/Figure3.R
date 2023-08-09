fig.dir <- "Figure/" # subfolder path are defined with ggsave
input.dir <- "../"
iso_GCAM_regID <-  read.csv("C:/Model/heatstress/input/gcamdata/inst/extdata/common/iso_GCAM_regID.csv", skip = 6, header = T)
GCAM_region_names <-  read.csv("C:/Model/heatstress/input/gcamdata/inst/extdata/common/GCAM_region_names.csv", skip = 6, header = T)

# define scenarios ----
SCENARIO <-  dir(path = input.dir, full.names = FALSE, recursive = FALSE, pattern = "E*_new"); SCENARIO

SCENARIO  <- c("E0", "E1", "E2", "E3")
# SCE_base ----
SCE_base <- "E0"

SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = gsub("E0","E0", scenario),
           scenario = gsub("E1","E1", scenario),
           scenario = gsub("E2","E2", scenario)) %>%
    return()
}

# Fig. 3: decompose ----

# Fig. 3.4: production ----
query = "CropProdSec"

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
df_crop <- do.call(rbind, df_list) %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2() %>%
  filter(!group %in% c("Forest", "Pasture"))

df_sector <- df_crop %>%
  select(sector) %>%
  unique() %>%
  rename(SECTOR = sector) %>%
  mutate(sector = tolower(SECTOR))

df_crop %>%
  SCE_NM() %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(scenario) %>%
  mutate(base = value[year == 2015]) %>%
  group_by(year) %>%
  mutate(base = mean(base),
         change = (value - value[scenario == SCE_base])) %>%
  select(year, scenario, value, base) %>%
  ungroup() %>%
  spread(scenario, value) %>%
  # mutate(agg = E3 - E0,
  #        LS = 100*(E3 - E2)/agg,
  #        tfp = 100*(E2 - E1)/agg,
  #        theta = 100*(E1 - E0)/agg,
  #        sum = LS + tfp + theta) %>% # percentage relative to E0 (E0 =)
  # mutate(agg = E3 - E0,
  #        LS = E3 - E2,
  #        tfp = E2 - E1,
  #        theta = E1 - E0,
  #        sum = LS + tfp + theta) %>% # absolute value relative to E0 (E0 =)
  # mutate(LS = 100 * (E3 - E2) / base,
#        tfp = 100 * (E2 - E1) / base,
#        theta = 100 * (E1 - E0) / base,
#        agg = 100 * (E3 - E0)/base) %>% # percentage relative to E0:2015 (E0 =)
mutate(LS = 100 * (E3 - E2) / E0,
       tfp = 100 * (E2 - E1) / E0,
       theta = 100 * (E1 - E0) / E0,
       agg = 100 * (E3 - E0)/E0) %>% # percentage relative to E0 (E0 =)
  select(year, LS, tfp, theta, agg) %>%
  gather(impact, value, LS:agg) %>%
  mutate(impact = gsub("LS", "Labor supply", impact),
         impact = gsub("tfp", "Productivity", impact),
         impact = gsub("theta", "Substitution", impact),
         impact = gsub("agg", "Total", impact),
         value = ifelse(year == 2015, 0, value)) ->
  df_Q_decomp

df_Q_decomp %>%
  filter(impact != "Total") %>%
  ggplot() +
  geom_bar(aes(x=year, y = value, fill = impact), position="stack", stat="identity") +
  geom_line(data = df_Q_decomp %>% filter(impact == "Total"), aes(x = year, y = value), linewidth = 1.4, linetype = "dashed", color = "black") +
  # labs(x = "Year", y = " Change in production (million ton)") +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
  # scale_fill_npg() +
  theme_bw() +
  theme0 + theme_leg +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=20,face="bold"),
        strip.text.x = element_text(size = 20)) +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p;p

P3.4 <- p + labs(title = "Crop production"); P3.4

ggsave(filename = paste0(fig.dir, "Figure3/Figure3.4.png"), P3.4,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 3.6: labor demand ----
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

df_LL %>%
  SCE_NM() %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(scenario) %>%
  mutate(base = value[year == 2015]) %>%
  group_by(year) %>%
  mutate(base = mean(base),
         change = (value - value[scenario == SCE_base])) %>%
  select(year, scenario, value, base) %>%
  ungroup() %>%
  spread(scenario, value) %>%
  mutate(LS = 100 * (E3 - E2) / E0,
         tfp = 100 * (E2 - E1) / E0,
         theta = 100 * (E1 - E0) / E0,
         agg = 100 * (E3 - E0)/E0) %>% # percentage relative to E0 (E0 =)
  select(year, LS, tfp, theta, agg) %>%
  gather(impact, value, LS:agg) %>%
  mutate(impact = gsub("LS", "Labor supply", impact),
         impact = gsub("tfp", "Productivity", impact),
         impact = gsub("theta", "Substitution", impact),
         impact = gsub("agg", "Total", impact),
         value = ifelse(year == 2015, 0, value)) ->
  df_L_decomp

df_L_decomp %>%
  filter(impact != "Total") %>%
  ggplot() +
  geom_bar(aes(x=year, y = value, fill = impact), position="stack", stat="identity") +
  geom_line(data = df_L_decomp %>% filter(impact == "Total"), aes(x = year, y = value), linewidth = 1.4, linetype = "dashed", color = "black") +
  # labs(x = "Year", y = " Change in production (million ton)") +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
  # scale_fill_npg() +
  theme_bw() +
  theme0 + theme_leg +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=20,face="bold"),
        strip.text.x = element_text(size = 20)) +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p;p

P3.6 <- p + labs(title = "Labor demand"); P3.6

ggsave(filename = paste0(fig.dir, "Figure3/Figure3.6.png"), P3.6,
       width = 8, height = 10, dpi = 300, units = "in", device='png')

# Fig. 3.1: wage rate ----
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
  mutate(W = EXP / Labor) ->
  df_W_index

df_W_index %>%
  SCE_NM() %>%
  group_by(year) %>%
  rename(value = W) %>%
  group_by(scenario) %>%
  mutate(base = value[year == 2015]) %>%
  group_by(year) %>%
  mutate(base = mean(base),
         change = (value - value[scenario == SCE_base])) %>%
  select(year, scenario, value, base) %>%
  ungroup() %>%
  spread(scenario, value) %>%
  mutate(LS = 100 * (E3 - E2) / E0,
         tfp = 100 * (E2 - E1) / E0,
         theta = 100 * (E1 - E0) / E0,
         agg = 100 * (E3 - E0) / E0) %>% # percentage relative to E0 (E0 =)
  select(year, LS, tfp, theta, agg) %>%
  gather(impact, value, LS:agg) %>%
  mutate(impact = gsub("LS", "Labor supply", impact),
         impact = gsub("tfp", "Productivity", impact),
         impact = gsub("theta", "Substitution", impact),
         impact = gsub("agg", "Total", impact)) ->
  df_W_decomp

df_W_decomp %>%
  filter(impact != "Total") %>%
  ggplot() +
  geom_bar(aes(x=year, y = value, fill = impact), position="stack", stat="identity") +
  geom_line(data = df_W_decomp %>% filter(impact == "Total"), aes(x = year, y = value), linewidth = 1.4, linetype = "dashed", color = "black") +
  # labs(x = "Year", y = " Change in production (million ton)") +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
  # scale_fill_npg() +
  theme_bw() +
  theme0 + theme_leg +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=20,face="bold"),
        strip.text.x = element_text(size = 20)) +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p;p

P3.1 <- p + labs(title = "Wage rate"); P3.1

ggsave(filename = paste0(fig.dir, "Figure3/Figure3.1.png"), P3.1,
       width = 8, height = 10, dpi = 300, units = "in", device='png')

# Fig. 3.2: crop price ----
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

df_P <- do.call(rbind, df_list) %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2()

df_crop %>%
  filter(!group %in% c("Forest", "Pasture")) %>%
  filter(group!= "UnmanagedLand") %>%
  filter(group != "Biomass") %>%
  rename(Q = value) %>%
  left_join(df_P %>% filter(group != "Biomass") %>% rename(P = value),
            by = c("scenario", "region", "sector", "group", "year")) %>%
  mutate(EXP = P * Q) %>%
  group_by(scenario, year) %>%
  summarise(EXP = sum(EXP, na.rm = T),
            Q = sum(Q, na.rm = T)) %>%
  mutate(wp = EXP / Q) %>%
  filter(year >= 2015) ->
  df_wp

df_wp %>%
  SCE_NM() %>%
  group_by(year) %>%
  rename(value = wp) %>%
  group_by(scenario) %>%
  mutate(base = value[year == 2015]) %>%
  group_by(year) %>%
  mutate(base = mean(base),
         change = (value - value[scenario == SCE_base])) %>%
  select(year, scenario, value, base) %>%
  ungroup() %>%
  spread(scenario, value) %>%
  mutate(LS = 100 * (E3 - E2) / E0,
         tfp = 100 * (E2 - E1) / E0,
         theta = 100 * (E1 - E0) / E0,
         agg = 100 * (E3 - E0) / E0) %>% # percentage relative to E0 (E0 =)
  select(year, LS, tfp, theta, agg) %>%
  gather(impact, value, LS:agg) %>%
  mutate(impact = gsub("LS", "Labor supply", impact),
         impact = gsub("tfp", "Productivity", impact),
         impact = gsub("theta", "Substitution", impact),
         impact = gsub("agg", "Total", impact)) ->
  df_P_decomp


df_P_decomp %>%
  filter(impact != "Total") %>%
  ggplot() +
  geom_bar(aes(x=year, y = value, fill = impact), position="stack", stat="identity") +
  geom_line(data = df_P_decomp %>% filter(impact == "Total"), aes(x = year, y = value), linewidth = 1.4, linetype = "dashed", color = "black") +
  # labs(x = "Year", y = " Change in production (million ton)") +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
  # scale_fill_npg() +
  theme_bw() +
  theme0 + theme_leg +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=20,face="bold"),
        strip.text.x = element_text(size = 20)) +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p;p

P3.2 <- p + labs(title = "Crop price"); P3.2

ggsave(filename = paste0(fig.dir, "Figure3/Figure3.2.png"), P3.2,
       width = 8, height = 10, dpi = 300, units = "in", device='png')

# Fig 3.3: consumption ----

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

D_crop %>%
  SCE_NM() %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(scenario) %>%
  mutate(base = value[year == 2015]) %>%
  group_by(year) %>%
  mutate(base = mean(base),
         change = (value - value[scenario == SCE_base])) %>%
  select(year, scenario, value, base) %>%
  ungroup() %>%
  spread(scenario, value) %>%
  mutate(LS = 100 * (E3 - E2) / E0,
         tfp = 100 * (E2 - E1) / E0,
         theta = 100 * (E1 - E0) / E0,
         agg = 100 * (E3 - E0) / E0) %>% # percentage relative to E0 (E0 =)
  select(year, LS, tfp, theta, agg) %>%
  gather(impact, value, LS:agg) %>%
  mutate(impact = gsub("LS", "Labor supply", impact),
         impact = gsub("tfp", "Productivity", impact),
         impact = gsub("theta", "Substitution", impact),
         impact = gsub("agg", "Total", impact)) ->
  df_D_decomp


df_D_decomp %>%
  filter(impact != "Total") %>%
  ggplot() +
  geom_bar(aes(x=year, y = value, fill = impact), position="stack", stat="identity") +
  geom_line(data = df_D_decomp %>% filter(impact == "Total"), aes(x = year, y = value), linewidth = 1.4, linetype = "dashed", color = "black") +
  # labs(x = "Year", y = " Change in production (million ton)") +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
  # scale_fill_npg() +
  theme_bw() +
  theme0 + theme_leg +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=20,face="bold"),
        strip.text.x = element_text(size = 20)) +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p;p

P3.3 <- p + labs(title = "Crop consumption"); P3.3

ggsave(filename = paste0(fig.dir, "Figure3/Figure3.3.png"), P3.3,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 3.7: Harvested area: crop ----

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
  mutate(value = value / 10)

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

df_LAND %>%
  SCE_NM() %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(scenario) %>%
  mutate(base = value[year == 2015]) %>%
  group_by(year) %>%
  mutate(base = mean(base),
         change = (value - value[scenario == SCE_base])) %>%
  select(year, scenario, value, base) %>%
  ungroup() %>%
  spread(scenario, value) %>%
  mutate(LS = 100 * (E3 - E2) / E0,
         tfp = 100 * (E2 - E1) / E0,
         theta = 100 * (E1 - E0) / E0,
         agg = 100 * (E3 - E0) / E0) %>% # percentage relative to E0 (E0 =)
  select(year, LS, tfp, theta, agg) %>%
  gather(impact, value, LS:agg) %>%
  mutate(impact = gsub("LS", "Labor supply", impact),
         impact = gsub("tfp", "Productivity", impact),
         impact = gsub("theta", "Substitution", impact),
         impact = gsub("agg", "Total", impact)) ->
  LAND_decomp

LAND_decomp %>%
  filter(impact != "Total") %>%
  ggplot() +
  geom_bar(aes(x=year, y = value, fill = impact), position="stack", stat="identity") +
  geom_line(data = df_D_decomp %>% filter(impact == "Total"), aes(x = year, y = value), linewidth = 1.4, linetype = "dashed", color = "black") +
  # labs(x = "Year", y = " Change in production (million ton)") +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
  # scale_fill_npg() +
  theme_bw() +
  theme0 + theme_leg +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=20,face="bold"),
        strip.text.x = element_text(size = 20)) +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p;p

P3.7 <- p + labs(title = "Harvested area"); P3.7

ggsave(filename = paste0(fig.dir, "Figure3/Figure3.7.png"), P3.7,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 3.9: import: crop ----

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
  CLUSTER_CROP2() %>%
  filter(!group %in% c("Forest", "Pasture", "Biomass"))

df_IM %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(scenario) %>%
  mutate(base = value[year == 2015]) %>%
  group_by(year) %>%
  mutate(base = mean(base),
         change = (value - value[scenario == SCE_base])) %>%
  select(year, scenario, value, base) %>%
  ungroup() %>%
  spread(scenario, value) %>%
  mutate(LS = 100 * (E3 - E2) / E0,
         tfp = 100 * (E2 - E1) / E0,
         theta = 100 * (E1 - E0) / E0,
         agg = 100 * (E3 - E0) / E0) %>% # percentage relative to E0 (E0 =)
  select(year, LS, tfp, theta, agg) %>%
  gather(impact, value, LS:agg) %>%
  mutate(impact = gsub("LS", "Labor supply", impact),
         impact = gsub("tfp", "Productivity", impact),
         impact = gsub("theta", "Substitution", impact),
         impact = gsub("agg", "Total", impact)) ->
  IM_decomp


IM_decomp %>%
  filter(impact != "Total") %>%
  ggplot() +
  geom_bar(aes(x=year, y = value, fill = impact), position="stack", stat="identity") +
  geom_line(data = IM_decomp %>% filter(impact == "Total"), aes(x = year, y = value), linewidth = 1.4, linetype = "dashed", color = "black") +
  # labs(x = "Year", y = " Change in production (million ton)") +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
  # scale_fill_npg() +
  theme_bw() +
  theme0 + theme_leg +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=20,face="bold"),
        strip.text.x = element_text(size = 20)) +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p;p

P3.9 <- p + labs(title = "Import"); P3.9

ggsave(filename = paste0(fig.dir, "Figure3/Figure3.9.png"), P3.9,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 3.10: export: crop ----

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
  CLUSTER_CROP2() %>%
  filter(!group %in% c("Forest", "Pasture", "Biomass"))


df_EX %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(scenario) %>%
  mutate(base = value[year == 2015]) %>%
  group_by(year) %>%
  mutate(base = mean(base),
         change = (value - value[scenario == SCE_base])) %>%
  select(year, scenario, value, base) %>%
  ungroup() %>%
  spread(scenario, value) %>%
  mutate(LS = 100 * (E3 - E2) / E0,
         tfp = 100 * (E2 - E1) / E0,
         theta = 100 * (E1 - E0) / E0,
         agg = 100 * (E3 - E0) / E0) %>% # percentage relative to E0 (E0 =)
  select(year, LS, tfp, theta, agg) %>%
  gather(impact, value, LS:agg) %>%
  mutate(impact = gsub("LS", "Labor supply", impact),
         impact = gsub("tfp", "Productivity", impact),
         impact = gsub("theta", "Substitution", impact),
         impact = gsub("agg", "Total", impact)) ->
  EX_decomp

EX_decomp %>%
  filter(impact != "Total") %>%
  ggplot() +
  geom_bar(aes(x=year, y = value, fill = impact), position="stack", stat="identity") +
  geom_line(data = IM_decomp %>% filter(impact == "Total"), aes(x = year, y = value), linewidth = 1.4, linetype = "dashed", color = "black") +
  # labs(x = "Year", y = " Change in production (million ton)") +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
  # scale_fill_npg() +
  theme_bw() +
  theme0 + theme_leg +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=20,face="bold"),
        strip.text.x = element_text(size = 20)) +
  theme(legend.key.size = unit(1.2, "cm"),
        legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
        plot.title = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p;p

P3.10 <- p + labs(title = "Export"); P3.10

ggsave(filename = paste0(fig.dir, "Figure3/Figure3.10.png"), P3.10,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# yield indicator cannot be directly decompose with this method
# # Fig. 3.5: revealed labor yield: crop
#
# df_LL %>%
#   filter(year >= 2015) %>%
#   rename(L = value) %>%
#   left_join(df_crop %>%  rename(Q = value),
#             by = c("scenario", "region", "sector", "year", "group")) %>%
#   filter(group != "Biomass") %>%
#   group_by(scenario, year) %>%
#   summarise(Q = sum(Q),
#             L = sum(L)) %>%
#   mutate(value = Q / L) %>%
#   group_by(scenario) %>%
#   mutate(base = value[year == 2015]) %>%
#   group_by(year) %>%
#   mutate(base = mean(base),
#          change = (value - value[scenario == SCE_base])) %>%
#   select(year, scenario, value, base) %>%
#   ungroup() %>%
#   spread(scenario, value) %>%
#   mutate(LS = 100 * (E3 - E2) / E0,
#          tfp = 100 * (E2 - E1) / E0,
#          theta = 100 * (E1 - E0) / E0,
#          agg = 100 * (E3 - E0) / E0) %>% # percentage relative to E0 (E0 =)
#   select(year, LS, tfp, theta, agg) %>%
#   gather(impact, value, LS:agg) %>%
#   mutate(impact = gsub("LS", "Labor supply", impact),
#          impact = gsub("tfp", "Productivity", impact),
#          impact = gsub("theta", "Substitution", impact),
#          impact = gsub("agg", "Total", impact)) ->
#   LaborYld_decomp
#
#
# LaborYld_decomp %>%
#   filter(impact != "Total") %>%
#   ggplot() +
#   geom_bar(aes(x=year, y = value, fill = impact), position="stack", stat="identity") +
#   geom_line(data = df_D_decomp %>% filter(impact == "Total"), aes(x = year, y = value), linewidth = 1.4, linetype = "dashed", color = "black") +
#   # labs(x = "Year", y = " Change in production (million ton)") +
#   labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
#   # scale_fill_npg() +
#   theme_bw() +
#   theme0 + theme_leg +
#   theme(legend.position="bottom",
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
#         axis.text.y = element_text(size=16),
#         axis.title = element_text(size=20,face="bold"),
#         strip.text.x = element_text(size = 20)) +
#   theme(legend.key.size = unit(1.2, "cm"),
#         legend.key.height=unit(1.2,"line"), legend.text = element_text(size = 14),
#         plot.title = element_text(size=20, face="bold"),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p;p
#
# P3.5 <- p + labs(title = "Harvested area"); P3.5
#
# ggsave(filename = paste0(fig.dir, "Figure3/Figure3.5.png"), P3.5,
#        width = 8, height = 10, dpi = 300, units = "in", device='png')
