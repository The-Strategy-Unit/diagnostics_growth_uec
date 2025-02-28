# README
# [NCDR]
# Prepare data for models used in Phase II.

library("DBI")
library("here") 
library("dplyr")
library("purrr") 
library("furrr") 
library("readr") 
library("tidyr")
library("dbplyr")
library("tibble") 
library("forcats")
library("ggplot2") 
library("janitor")
library("stringr")
library("lubridate")


# 0. CONNECTIONS ----------------------------------------------------------

con_sandbox_su <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Sandbox_StrategyUnit",
  Trusted_Connection = "True"
)
con_sus_plus <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_SUSPlus_Live",
  Trusted_Connection = "True"
)

con_nhse_reference <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Reference",
  Trusted_Connection = "True"
)


# 1. LOAD DATA --------------------------------------------------------

tb_ref_invst_ec <- tbl(con_nhse_reference, in_schema("dbo", "tbl_Ref_DataDic_ECDS_Investigation"))

lkp_invst <- tb_ref_invst_ec |>
  select(InvestigationKey, InvestigationCode, InvestigationDescription) |>
  # # REMOVE NO INVESTIGATION FROM SEARCH LIST
  # (NOTE: ITS PRESENCE DOESN'T INDICATE NO INVESTIGATION):
  filter(InvestigationKey != 98) |>
  collect() |> 
  mutate(InvestigationCode = as.character(InvestigationCode))


vec_provider_selection <- c(
  # FROM DATA QUALITY ASSESSMENT:
  "RKB", "RCB", "RHM", "RCU", "RVJ",
  "RWW", "RK9", "RWH", "RLT", "RFF", 
  "RVW", "REM", "RBK", "RTF", "RX1", 
  "RYR", "RQW", "RWD", "RYJ", "RNN", 
  "RNZ", "RTG", "RHQ", "RXC", "RFS"
) |> 
  enframe(name = NULL, value = "procode") |>
  pull()

# SHARES FOUNDATIONAL QUERY WITH INDUSTRIAL ACTION WORK (FILTER FOR DESIRED YEAR):
tb_ecds_0 <- tbl(con_sandbox_su, in_schema("dbo", "1208_ind_action_model_extract_ecds_0"))


ecds_extract <- tb_ecds_0 |>
  filter(fyear == "2023/24") |>
  filter(procode %in% local(vec_provider_selection)) |>
  collect() 

ecds_extract <- ecds_extract |> 
  select(-matches("fyear|disdest|dur|inj|treat_|imd_|ethnic|_icb")) 


# 2. MISSING VALUES - DELETE RECORDS -----------------------------------------------

df_odds_na_rm <- ecds_extract |>
  filter(!is.na(acuity)) |>
  # CHIEF COMPLAINT MISSINGS HAVE BIGGEST IMPACT.
  filter(!is.na(chief_comp_grp)) |>
  # ALTERNATIVELY, COULD ASSUME NA IS NO INVESTIGATION? (AND IMPUTE)
  filter(!is.na(invst_all)) |>
  filter(!is.na(age)) 


# # BY DELETING ALL MISSING VALUES FROM KEY FIELDS,
# WE ARE LOSING 135,744 RECORDS (5.5%).
# nrow(ecds_extract) -nrow(df_odds_na_rm)
# 1 - df_odds_na_rm |> nrow() / ecds_extract |> nrow()


# 3. RE-ENGINEER VARS --------------------------------------------

## a. NEW CHIEF COMPLAINT --------------------------------------------
# AN IMPORTANT VAR AND FAR MORE INFORMATIVE THAN GROUPED COMPLAINT:

lkp_chief_comp_small <- df_odds_na_rm |>
  count(chief_comp_desc, chief_comp_grp, sort = T) |>
  mutate(p = round(n / sum(n), 4) * 100) |>
  # ungroup() |>
  # SMALL NUMBERS (LOW % OF CASES)
  filter(p < 0.1) |>
  # TOTAL % OF ALL CASES:
  group_by(chief_comp_grp, chief_comp_desc) |>
  summarise(n = sum(n), p = sum(p)) |> 
  ungroup() |> 
  arrange(n) |> 
  # head(20)
  # count(chief_comp_grp, wt = n, sort = T) |>
  # GROUP THOSE GROUPS WITH THE LOWEST OCCURENCES:
  mutate(chief_comp_grp = if_else(
    chief_comp_grp %in% c(
      "Not applicable to child terms",
      "Neurological"
    ),
    "Other",
    chief_comp_grp
  )) |>
  mutate(chief_comp_desc_small = str_c("Other_", chief_comp_grp)) |>
  # count(chief_comp_desc_small, wt = n, sort = T)
  select(-c(chief_comp_grp, n, p))

df_odds_fe1 <- df_odds_na_rm |> 
  # REPLACE CHIEF COMPLAINT WITH ENGINEERED VERSION:
  left_join(lkp_chief_comp_small, join_by(chief_comp_desc)) |>
  mutate(chief_comp_desc = if_else(
    !is.na(chief_comp_desc_small),
    chief_comp_desc_small,
    chief_comp_desc
  )) |>
  select(-chief_comp_desc_small) |>
  mutate(chief_comp_desc = as.factor(chief_comp_desc)) |>
  # SET REFERENCE LEVEL AS MOST FREQ COMPLAINT:
  mutate(chief_comp_desc = fct_relevel(chief_comp_desc, "Chest pain (finding)"))


## b. ATTENDANCE (REFERRAL) SOURCE ----------------------------------------------------

df_odds_fe2 <- df_odds_fe1 |> 
  mutate(refer_sorc = str_remove_all(att_source_desc, "[:punct:]")) |>
  mutate(refer_sorc = str_remove_all(refer_sorc, " procedure| finding| situation")) |> 
  mutate(refer_sorc = str_remove_all(refer_sorc, "Referred by |Referral by ")) |> 
  mutate(refer_sorc = str_remove_all(refer_sorc, " accident and emergency department")) |> 
  # distinct(attsorc) |> 
  # print(n=40)
  mutate(refer_sorc = case_when(
    refer_sorc == "Selfreferral to" ~ "self", 
    refer_sorc == "member of Primary Health Care Team" ~ "primary_care",
    refer_sorc == "urgent care service" ~ "urgent_care",
    T ~ "other"
  )) |> 
  mutate(refer_sorc = as.factor(refer_sorc)) |>
  # SET REFERENCE LEVEL AS SELF-REFERRAL:
  mutate(refer_sorc = fct_relevel(refer_sorc, "self")) |> 
  select(-starts_with("att")) 

## c. TIME-RELATED VARS ----------------------------------------------------

df_odds_fe3 <- df_odds_fe2 |>
  mutate(month = month(dttm_arr)) |>
  mutate(wkday = wday(dttm_arr, label = T, week_start = 1)) |>
  # REMOVE THE ORDERING ON WEEKDAY:
  mutate(wkday = as.character(wkday)) |>
  mutate(hour = hour(dttm_arr)) |>
  mutate(is_winter = if_else(month %in% c(12, 1:3), 1, 0)) |>
  mutate(is_wkend = if_else(wkday %in% c("Sat", "Sun"), 1, 0)) |>
  # NIGHT = 8 HRS FROM 22:00-05:59:
  mutate(is_night = if_else(hour %in% c(0:5, 22:23), 1, 0)) |> 
  mutate(across(starts_with("is_"), ~ as.factor(.))) |>
  mutate(day = day(dttm_arr)) |> 
  # select(-dttm_arr) |> 
  identity()


# 4. ADD OCCUPANCY VARIABLE --------------------------------------------------------

# lkp_occupancy <- readRDS("lkp_occupancy.RDS")

df_odds_occ <- df_odds_fe3 |> 
  left_join(
    lkp_occupancy, 
    join_by(procode, month, day, hour)
  )

# 5. SAMPLE 35% (OR > 1 MILLION RECORDS) ------------------------

# set.seed(1822)
# df_odds_sample <- df_odds_fe3 |>
#   slice_sample(prop = 0.35)
# 
# gc()

# # SHORTER MODEL RUN TIME FOR FASTER FEEDBACK
set.seed(1822)
df_odds_sample <- df_odds_occ |>
  slice_sample(prop = 0.3)

gc()

# 6. OUTCOME VARIABLES ------------------------------------------

# TODO URINE CULT SITUATION
## a. CREATE ----------------------------------------------------

# PULL EC TYPE CODES:
vec_invst_codes_ec <- lkp_invst |>
  mutate(InvestigationCode = as.character(InvestigationCode)) |>
  mutate(InvestigationKey = str_c("invst_", InvestigationKey)) |>
  select(InvestigationKey, InvestigationCode) |>
  deframe()

# NOTE: COUNTING INSTANCES HERE TO KEEP OPTIONS OPEN.
# BUT LATER ADJUSTED TO BINARY (TEST Y/N).
list_invst_counts <- imap(vec_invst_codes_ec, function(x, y) {
  df_odds_sample %>%
    transmute({{ y }} := str_count(invst_all, str_c("^", x, "| ", x, "|,", x)))
})

df_odds_invst <- list_invst_counts |>
  reduce(bind_cols) |>
  bind_cols(df_odds_sample, y = _)

gc()


# 7. FINAL DATA PREP ------------------------------------------------------

df_prep_binary <- df_odds_invst |>
  ### REMOVE VARS THAT WON'T BE USED IN BASIC MODEL:
  select(-matches("acuity_desc|^dttm|grp|invst_all|day|month|hour")) |>
  # colnames()
  # SWITCH OUTCOMES TO BINARY:
  mutate(across(starts_with("invst_"), ~ if_else(. > 0, 1, 0))) |> 
  mutate(occ_decile = fct_relevel(as.factor(occ_decile), "5")) |> 
  mutate(across(c(sex, arr_mode, acuity, procode), ~ as.factor(.))) |> 
  mutate(sex = fct_relevel(sex, "m")) |> 
  mutate(acuity = fct_relevel(acuity, "1")) |> 
  mutate(arr_mode = fct_relevel(arr_mode, "walk_in")) |>
  mutate(age = as.integer(age)) 

glimpse(df_prep_binary)

df_prep_binary |> count((age)) |> tail()
# ~~~~~~~~~ -------------------------------------------------------------------

# 8. FUNCTIONAL PROGRAMMING SETUP (FOR MULTIPLE MODELS) ---------------------
# FOUR MODELS RUN IN PARALLEL USING FURRR PACKAGE

# (SINGLE MODEL ALSO DEMO'ED)

# LOOK AT THE TOP 32 TESTS BY FREQUENCY 
# (THERE COULD BE UP TO 4 "OTHER" GROUPS FOR LOW FREQ TESTS BASED ON THE
# FOUR INVESTIGATION CLASSES WE MAY USE.)

# CREATE DATA FRAME FOR FUNCTIONAL PROGRAMMING:
# (TOP 10 TESTS BY APPEARANCE)
df1 <- df_prep_binary |>
  summarise(across(starts_with("invst_"), ~ sum(.))) |>
  pivot_longer(cols = everything()) |>
  arrange(-value) |>
  mutate(id = row_number()) |> 
  # X to Y (OF TOP Z) BY APPEARANCES:
  slice(c(2:4, 9)) |>
  # slice(9) |>
  mutate(InvestigationKey = as.numeric(str_extract(name, "[:digit:]{2}"))) |>
  left_join(lkp_invst, join_by(InvestigationKey)) |>
  select(name, value, InvestigationDescription, id) |> 
  # REMOVE SUPERFLUOUS TEXT:
  mutate(InvestigationDescription = str_remove_all(InvestigationDescription, "[:punct:]")) |>
  mutate(InvestigationDescription = str_remove_all(InvestigationDescription, " procedure")) |> 
  relocate(InvestigationDescription, .after = name)


# NOTE!!
# IF YOU NEED TO PREVIEW df2 (BELOW) BEST DONE IN CONSOLE WINDOW
# RATHER THAN WITH view()

# ADD REQUIRED DATA IN LIST COLUMN (AND MANIPULATE):
df2 <- df1 |>
  mutate(data = list(df_prep_binary)) |>
  mutate(data = map2(data, name, function(df, x) {
    df |>
      select(everything(), -matches("invst"), all_of(x)) |>
      mutate(across(all_of(x), ~ as.factor(.))) |>
      # TO AVOID COMPLICATION IN MODEL FORMULA:
      rename("invst" = x)
  }))

gc()
gc()

# PREVIEW:
df2


# -------------------------------------------------------------------------

df2$data[[1]] |> 
  count(arr_mode)

plan(multisession, workers = 4)

tictoc::tic()
df3 <- df2 |>
  mutate(model = future_map(data, function(df) {
    mgcv::gam(
      formula = invst ~
        # VAR OF INTEREST:
        occ_decile +
        # DEMOGRAPHICS:
        s(age, by = sex) + sex + # imd_dec +
        # CASE-MIX-RELATED:
        arr_mode + acuity + chief_comp_desc + refer_sorc +
        # TIME-RELATED:
        is_winter + is_wkend + is_night +
        # PROVIDER (RANDOM INTERCEPT):
        s(procode, bs = "re"),
      family = "binomial",
      method = "REML",
      data = df
    )
  })) 
tictoc::toc()
# < 1.5 hours
# 4925.32 /60

gc()

df
df3$model[[1]] |> summary()

df3$model[[3]] |> 
  broom::tidy(parametric = TRUE) |> 
  filter(str_detect(term, "decile")) |>
  mutate(odds = exp(estimate)) |>
  mutate(lci = exp(estimate - 1.96*std.error)) |> 
  mutate(uci = exp(estimate + 1.96*std.error)) |> 
  select(term, odds, lci, uci)

24*25*10

# 9. SAVE MODEL RESULTS -------------------------------------------------------------

df3 %>% 
  saveRDS(str_c("models_odds_top_", min(.$id), "to", max(.$id), ".rds"))

df_odds_results <- df3 |>
  mutate(results = map(model, function(df) {
    df |>
      broom::tidy(parametric = TRUE) |>
      filter(str_detect(term, "decile")) |>
      mutate(odds = exp(estimate)) |>
      mutate(lci = exp(estimate - 1.96*std.error)) |> 
      mutate(uci = exp(estimate + 1.96*std.error)) |> 
      select(term, odds, lci, uci)
    # pull(odds)
  })) |> 
  select(-c(data, model, value)) |>
  unnest(results) |>
  # # relocate(odds, .after = pdA) |> 
  # # relocate(std_error, .after = odds) |> 
  # # ADJUSTMENTS IF SMALLER SAMPLE SIZE FOR MODELS 13:28 (FACTOR OR 0.35/0.17):
  # # mutate(across(matches("^t"), ~ if_else(id %in% 13:28, . * (0.35 / 0.17), .))) %>%
  # saveRDS(str_c("from_ncdr_growth_attrb_v4_", min(.$id), "to", max(.$id), ".rds"))
  identity()

df_odds_results


# 10. PLOT ----------------------------------------------------------------

df_odds_results |>
  mutate(term = (str_remove_all(term, "[:alpha:]|_"))) |> 
  mutate(term = if_else(term == 10, term, str_c(0, term))) |> 
  # mutate(term = as.factor(term)) |> 
  # mutate(term = fct_relevel(term, ""))
  
  # filter(InvestigationDescription != "Serologic test") |>
  # mutate(invst_group = as_factor(invst_group)) |> 
  ggplot() +
  geom_hline(yintercept = 1, lty = "dashed", colour = "grey20", alpha = 0.5) +
  # geom_pointrange(aes(reorder(InvestigationDescription, odds), odds, ymin = lci, ymax = uci), col = "grey40", stroke = NA)+
  geom_pointrange(aes(term, odds, ymin = lci, ymax = uci), col = "grey40", stroke = NA)+
  theme_bw() +
  coord_flip() +
  scale_y_log10(
    limits = c(0.85, 1.15),
    breaks = c(0.9, 1, 1.1),
    labels = c(
      # "Half\nas likely in\n2023/24",
      # "Equally\nas likely in\n2023/24",
      # "2x\nas likely in\n2023/24",
      "90%\nas likely as\nav. occupancy",
      "Equally\nas likely as\nav. occupancy",
      "110%x\nas likely as\nav. occupancy"
    )
  )+
  theme(
    # axis.title.y = element_blank(),
    axis.title = element_text(size = 9),
    # plot.margin = margin(5, 20, 5, 5),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 9)
  ) +
  labs(
    x = "Occupany decile (10 = highest occupancy)",
    y = "\nCase-mix-adjusted odds ratio of test (occupancy decile vs 'average' occ) on log scale")+
  # facet_grid(invst_group ~ ., space = "free", scales = "free_y")
  facet_wrap(vars(InvestigationDescription))
