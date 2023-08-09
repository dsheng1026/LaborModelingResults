# Fig. 6: sensitivity on labor productivitiy growth ----
SCENARIO  <- c("E0", "E4_eta_higher", "E4_eta_lower")

SCE_NM <- function(.data){
  .data %>%
    mutate(scenario = gsub("E0","Ref", scenario),
           scenario = gsub("E4_eta_higher","Higher", scenario),
           scenario = gsub("E4_eta_lower","Lower", scenario)) %>%
    return()
}

SECTOR <- function(.data){
  .data %>%
    mutate(sector = gsub("regional ", "", sector),
           sector = gsub("_", " ", sector),
           sector = gsub(" ", "", sector)) %>%
    return()
}
# SCE_base ----
SCE_base <- "Ref"

# Fig. 4.4: production ----
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
  # filter(!group %in% c("Forest", "Pasture")) %>%
  filter(!group %in% c("Forest", "Pasture", "Biomass")) %>%
  SCE_NM()

df_sector <- df_crop %>%
  select(sector) %>%
  unique() %>%
  rename(SECTOR = sector) %>%
  mutate(sector = tolower(SECTOR))

check <- df_crop %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(year) %>%
  mutate(change = 100 * (value / value[scenario == SCE_base] - 1)) ->
  Figure

prod2100 <- Figure %>%
  filter(year == 2100)

Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = change, color = scenario), linewidth = 1.4) +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
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
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p; p

P4.4 <- p + labs(title = "Crop production"); P4.4

ggsave(filename = paste0(fig.dir, "Figure6/Figure6.4.png"), P4.4,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 4.6: labor demand ----
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
  SCE_NM()

df_LL <- df_L %>%
  CLUSTER_CROP() %>%
  CLUSTER_CROP2() %>%
  # filter(!group %in% c("Forest", "Pasture"))
  filter(!group %in% c("Forest", "Pasture", "Biomass"))

df_LL %>%
  filter(year >= 2015,
         group != "Biomass") %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(year) %>%
  mutate(change = 100 * (value / value[scenario == SCE_base] - 1)) ->
  Figure

labor2100 <- Figure %>%
  filter(year == 2100)

Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = change, color = scenario), linewidth = 1.4) +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
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
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p; p

P4.6 <- p + labs(title = "Labor demand"); P4.6

ggsave(filename = paste0(fig.dir, "Figure6/Figure6.6.png"), P4.6,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 4.1: wage rate ----
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
  mutate(region = gsub("Labor_Ag", "", market)) %>%
  SCE_NM()

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
  mutate(value = EXP / Labor) ->
  df_W_index

df_W_index %>%
  group_by(year) %>%
  mutate(change = 100 * (value / value[scenario == SCE_base] - 1)) ->
  Figure

wage2100 <- Figure %>%
  filter(year == 2100)

Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = change, color = scenario), linewidth = 1.4) +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
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
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p; p

P4.1 <- p + labs(title = "Wage rate"); P4.1

ggsave(filename = paste0(fig.dir, "Figure6/Figure6.1.png"), P4.1,
       width = 8, height = 10, dpi = 300, units = "in", device='png')

# Fig. 4.2: crop price ----
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
  SCE_NM()

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
  mutate(value = EXP / Q) %>% # weighted price
  filter(year >= 2015) ->
  df_wp

df_wp %>%
  group_by(year) %>%
  mutate(change = 100 * (value / value[scenario == SCE_base] - 1)) ->
  Figure

price2100 <- Figure %>%
  filter(year == 2100)

Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = change, color = scenario), linewidth = 1.4) +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
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
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p; p

P4.2 <- p + labs(title = "Crop price"); P4.2

ggsave(filename = paste0(fig.dir, "Figure6/Figure6.2.png"), P4.2,
       width = 8, height = 10, dpi = 300, units = "in", device='png')

# Fig 4.3: consumption ----

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
  CLUSTER_CROP2() %>%
  SCE_NM()

D_crop %>%
  filter(year >= 2015) %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(year) %>%
  mutate(change = 100 * (value / value[scenario == SCE_base] - 1)) ->
  Figure

cons2100 <- Figure %>%
  filter(year == 2100)

Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = change, color = scenario), linewidth = 1.4) +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
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
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p; p

P4.3 <- p + labs(title = "Crop consumption"); P4.3

ggsave(filename = paste0(fig.dir, "Figure6/Figure6.3.png"), P4.3,
       width = 8, height = 10, dpi = 300, units = "in", device='png')

# Fig. 4.7: Harvested area: crop ----

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
  SCE_NM() %>%
  filter(group != "Biomass") ->
  df_LAND

df_LAND %>%
  filter(year >= 2015) %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(year) %>%
  mutate(change = 100 * (value / value[scenario == SCE_base] - 1)) ->
  Figure

land2100 <- Figure %>%
  filter(year == 2100)

Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = change, color = scenario), linewidth = 1.4) +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
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
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p; p

P4.7 <- p + labs(title = "Harvested area"); P4.7

ggsave(filename = paste0(fig.dir, "Figure6/Figure6.7.png"), P4.7,
       width = 8, height = 10, dpi = 300, units = "in", device='png')


# Fig. 4.9: import: crop ----

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
  filter(!group %in% c("Forest", "Pasture", "Biomass")) %>%
  SCE_NM()


df_IM %>%
  filter(year >= 2015) %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(year) %>%
  mutate(change = 100 * (value / value[scenario == SCE_base] - 1)) ->
  Figure

IM2100 <- Figure %>%
  filter(year == 2100)

Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = change, color = scenario), linewidth = 1.4) +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
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
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p; p

P4.9 <- p + labs(title = "Import"); P4.9

ggsave(filename = paste0(fig.dir, "Figure6/Figure6.9.png"), P4.9,
       width = 8, height = 10, dpi = 300, units = "in", device='png')




# Fig. 4.10: export: crop ----

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
  filter(!group %in% c("Forest", "Pasture", "Biomass")) %>%
  SCE_NM()


df_EX %>%
  filter(year >= 2015) %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value)) %>%
  group_by(year) %>%
  mutate(change = 100 * (value / value[scenario == SCE_base] - 1)) ->
  Figure

EX2100 <- Figure %>%
  filter(year == 2100)

Figure %>%
  ggplot() +
  geom_line(aes(x = year, y = change, color = scenario), linewidth = 1.4) +
  labs(x = "Year", y = " Relative change (New reference = 0) (%)") +
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
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p; p

P4.10 <- p + labs(title = "Export"); P4.10

ggsave(filename = paste0(fig.dir, "Figure6/Figure6.10.png"), P4.10,
       width = 8, height = 10, dpi = 300, units = "in", device='png')
