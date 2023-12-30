
# INFO ----

# TODO: 

# HEADER ----

# Preparation of macro European data - national level

## SOURCING ----
getwd()

source("./__library.R")
source("./__functions.R")

# CLEANING ----
## READING DATA ----
### from Eurostat
bop_exms6_q <- read_excel("../01_input_data/bop_exms6_q.xlsx", sheet = "Sheet 1", na = ":", skip = 9)
demo_find <- read_excel("../01_input_data/demo_find.xlsx", sheet = "Sheet 1", na = ":", skip = 6)
demo_mlexpec <- read_excel("../01_input_data/demo_mlexpec.xlsx", sheet = "Sheet 1", na = ":", skip = 9)
demo_pjan <- read_excel("../01_input_data/demo_pjan.xlsx", sheet = "Sheet 1", na = ":", skip = 9)
edat_lfse_03 <- read_excel("../01_input_data/edat_lfse_03.xlsx", sheet = "Sheet 1", na = ":", skip = 9)
gov_10dd_edpt1 <- read_excel("../01_input_data/gov_10dd_edpt1.xlsx", sheet = "Sheet 1", na = ":", skip = 8)
lfsa_urgan <- read_excel("../01_input_data/lfsa_urgan.xlsx", sheet = "Sheet 1", na = ":", skip = 8)
migr_imm1ctz <- read_excel("../01_input_data/migr_imm1ctz.xlsx", sheet = "Sheet 1", na = ":", skip = 10)
nama_10_cp_a21 <- read_excel("../01_input_data/nama_10_cp_a21.xlsx", sheet = "Sheet 1", na = ":", skip = 9)
nama_10_exi <- read_excel("../01_input_data/nama_10_exi.xlsx", sheet = "Sheet 1", na = ":", skip = 7)
nama_10_gdp <- read_excel("../01_input_data/nama_10_gdp.xlsx", sheet = "Sheet 1", na = ":", skip = 7)
nama_10_lp_ulc <- read_excel("../01_input_data/nama_10_lp_ulc.xlsx", sheet = "Sheet 1", na = ":", skip = 6)
nasa_10_ki <- read_excel("../01_input_data/nasa_10_ki.xlsx", sheet = "Sheet 1", na = ":", skip = 7)
prc_hicp_aind <- read_excel("../01_input_data/prc_hicp_aind.xlsx", sheet = "Sheet 1", na = ":", skip = 6)
rd_e_gerdtot <- read_excel("../01_input_data/rd_e_gerdtot.xlsx", sheet = "Sheet 1", na = ":", skip = 6)
road_if_roadsc <- read_excel("../01_input_data/road_if_roadsc.xlsx", sheet = "Sheet 1", na = ":", skip = 6)
spr_exp_pens <- read_excel("../01_input_data/spr_exp_pens.xlsx", sheet = "Sheet 1", na = ":", skip = 8)

### from World Bank Data
wdi <- read_excel("../01_input_data/wd_wdi.xlsx",col_types = c("text", "skip", "text", "skip", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric",
                                                               "numeric", "numeric", "numeric", "numeric",
                                                               "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric"), n_max = 621)
wgi <- read_excel("../01_input_data/wd_wgi.xlsx", col_types = c("text", "skip", "text", "skip", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", "numeric"), n_max = 594)
wd_health <- read_excel("../01_input_data/wd_hnps.xlsx", col_types = c("text", "skip", "text", "skip", "numeric", "numeric", "numeric", "numeric"), n_max = 621)

## OPENING DATA ----
exms <- bop_exms6_q %>%
  slice(2:622) %>%
  select(1:3) %>%
  rename("country" = 1,
         "year" = 2,
         "exms_gs" = 3)

frty <- demo_find %>%
  slice(2:595) %>%
  select(1:3) %>%
  rename("country" = 1,
         "year" = 2,
         "frty" = 3)

lexp <- demo_mlexpec %>%
  slice(1:621) %>%
  select(1:3) %>%
  rename("country" = 1,
         "year" = 2,
         "lexp" = 3)
  
pop <- demo_pjan %>%
  slice(1:621) %>%
  select(1:3) %>%
  rename("country" = 1,
         "year" = 2,
         "pop" = 3)

eduat <- edat_lfse_03 %>%
  slice(2:622) %>%
  select(1:3, seq(3,9, by = 2)) %>%
  rename("country" = 1,
         "year"= 2,
         "l02" = 3,
         "l38" = 4,
         "l34" = 5,
         "l58" = 6)

govb <- gov_10dd_edpt1 %>%
  slice(2:622) %>%
  rename("country" = 1,
         "year"= 2,
         "nip" = 3,
         "c_gross_debt" = 4)
  
unemp <- lfsa_urgan %>%
  slice(2:622) %>%
  select(1:3) %>%
  rename("country" = 1,
         "year" = 2,
         "unemp" = 3)

img <- migr_imm1ctz %>%
  slice(2:595) %>%
  select(1:3) %>%
  rename("country" = 1,
         "year" = 2,
         "img" = 3)

kprd <- nama_10_cp_a21 %>%
  slice(2:622) %>%
  select(1:3, 5) %>%
  rename("country" = 1,
         "year" = 2,
         "ass_emp_pp" = 3,
         "ass_w_h" = 4)

tb <- nama_10_exi %>%
  slice(2:622) %>%
  select(1:3, seq(3, 9, by = 2)) %>%
  rename("country" = 1,
         "year"= 2,
         "ex_eu" = 3,
         "ex_rw" = 4,
         "im_eu" = 5,
         "im_rw" = 6)

gdp <- nama_10_gdp %>%
  slice(2:622) %>%
  select(1:3, 5, 7, 13, 15, 17) %>%
  rename("country" = 1,
         "year"= 2,
         "gdp" = 3,
         "exp" = 4,
         "consump" = 5,
         "comp_emp" = 6,
         "tax" = 7,
         "subs" = 8)

lprd <- nama_10_lp_ulc %>%
  slice(3:623) %>%
  select(1:3) %>%
  rename("country" = 1,
         "year" = 2,
         "wage_pps" = 3)

#lots of missing obserations!
key_hh <- nasa_10_ki %>%
  slice(2:622) %>%
  select(1:3, seq(3, 9, by = 2)) %>%
  rename("country" = 1,
         "year"= 2,
         "hh_sr" = 3,
         "hh_ir" = 4,
         "hh_dir" = 5,
         "hh_far" = 6)

hicp <- prc_hicp_aind %>%
  slice(2:622) %>%
  select(1:3, 5) %>%
  rename("country" = 1,
         "year"= 2,
         "avg_hicp" = 3,
         "anrc_hicp" = 4)

gerd <- rd_e_gerdtot %>%
  slice(2:622) %>%
  select(1:3, 5) %>%
  rename("country" = 1,
         "year"= 2,
         "net_gerd" = 3,
         "share_gdp_gerd" = 4)

km_road <- road_if_roadsc %>%
  slice(2: 622) %>%
  select(1:3) %>%
  rename("country" = 1,
         "year"= 2,
         "km" = 3)

pens <- spr_exp_pens %>%
  slice(2:595) %>%
  select(1:3, 5) %>%
  rename("country" = 1,
         "year"= 2,
         "net_pens" = 3,
         "share_gdp_pens" = 4)

## REMOVE OLD DATA ----
rm(list = c("bop_exms6_q","demo_find","demo_mlexpec","demo_pjan","edat_lfse_03","gov_10dd_edpt1", "lfsa_urgan","migr_imm1ctz","nama_10_cp_a21","nama_10_exi","nama_10_gdp","nama_10_lp_ulc","nasa_10_ki","prc_hicp_aind","rd_e_gerdtot","road_if_roadsc","spr_exp_pens"))

## TRANSFORMING CHR TO NUM ----
data1 <- list(eduat, exms, frty, gdp, gerd, govb, hicp, img, key_hh, km_road, kprd, lexp, lprd, pens, pop, tb, unemp)
data1 <- lapply(data, convert_chr_num)

eduat <- data1[[1]]
exms <- data1[[2]]
frty <- data1[[3]]
gdp <- data1[[4]]
gerd <- data1[[5]]
govb <- data1[[6]]
hicp <- data1[[7]]
img <- data1[[8]]
key_hh <- data1[[9]]
km_road <- data1[[10]]
kprd <- data1[[11]]
lexp <- data1[[12]]
lprd <- data1[[13]]
pens <- data1[[14]]
pop <- data1[[15]]
tb <- data1[[16]]
unemp <- data1[[17]]

## REORDERING ----
wd_h <- wd_health %>%
  rename("year" = 1,
         "country" = 2,
         "h_exp" = 3,
         "n_h_bed" = 4,
         "rur_pop" = 5,
         "urb_pop" = 6) %>%
  arrange(country) %>%
  select(country, year, h_exp, n_h_bed, rur_pop, urb_pop)

wgi <- wgi %>%
  rename_with(~ c("year", "country", "corpt", "g_effect", "pol_stab", "qual", "rul_law", "voic_acc"), 1:8) %>%
  arrange(country) %>%
  select(country, year, corpt, g_effect, pol_stab, qual, rul_law, voic_acc)

wdi <- wdi %>%
  rename_with(~ c("year", "country", "pov_ratio", "elect", "army_pers", "DELETE1", "edu_exp_3", "edu_exp_2", "edu_exp_1", "DELETE2", "easy_bussy", "fdi_share", "gdp_gg", "gini", "edu_exp_tot", "infl", "army_exp", "DELETE3", "timexbussy", "share_trade"), 1:20) %>%
  arrange(country) %>%
  select(country, year, pov_ratio, elect, army_pers, -DELETE1, edu_exp_3, edu_exp_2, edu_exp_1, -DELETE2, easy_bussy, fdi_share, gdp_gg, gini, edu_exp_tot, infl, army_exp, -DELETE3, timexbussy, share_trade)

data2 <- list(wdi, wgi, wd_h)

## MERGING ----
macro_data_eurostat <- Reduce(function(x, y) merge(x, y, by = c("country", "year"), all = TRUE), data1)
macro_data_worldata <- Reduce(function(x, y) merge(x, y, by = c("country", "year"), all = TRUE), data2)

# SAVING NEW DATA ----
#write.csv(macro_data_clean, "../02_intermediary_data/macro_data.csv", row.names = FALSE)

