library(sf)
library(dplyr)
library(tidyr)
library(stringr)

get_percentages <- function(df, denom, vars) {
  df |> bind_cols(
    df |> 
      st_drop_geometry() |>
      select(c({{ denom }}, {{ vars }})) |>
      mutate(across({{ vars }}, ~ round(. / {{ denom }} * 100, 1))) |>
      select(-{{ denom }}) |>
      rename_with(~ paste0("pc_", .), {{ vars }})
  ) |>
    st_set_geometry("geometry") |>
    relocate(geometry, .after = last_col())
}

place <- "Whangarei"
alias <- "whangarei"

concord <- read.csv("/Users/david/Documents/teaching/intro-spatial-data-science/_labs/mini-project/datasets/sa1_concordance.csv") |>
  mutate(SA12018_V1_00 = as.character(SA12018_V1_00)) 

sa1 <- st_read("/Users/david/Documents/teaching/intro-spatial-data-science/_labs/mini-project/datasets/sa1.gpkg") |>
  left_join(concord, by = join_by(SA12018_V1_00 == SA12018_V1_00))

nz <- sa1 |>
  select() |> 
  st_union()

df <- sa1 |> 
  filter(UR2018_V1_00_NAME == place) |>
  select(SA12018_V1_00, SA22018_V1_00_NAME, starts_with("C18")) |>
  select(1:3, 10, 16:17, 132:135) |>
  mutate(across(4:10, ~ ifelse(. < 0, 0, .))) |>
  filter(C18_CURPop > 0) |>
  mutate(yadult  = C18_Age5year_20_24_years + C18_Age5year_25_29_years) |>
  select(-C18_Age5year_20_24_years, -C18_Age5year_25_29_years) |>
  rename(SA1     = SA12018_V1_00, 
         SA2     = SA22018_V1_00_NAME,
         pop     = C18_CURPop,
         female  = C18_Sex_Female,
         pakeha  = C18_Ethnicity_L1_European,
         maori   = C18_Ethnicity_L1_Maori,
         pacific = C18_Ethnicity_L1_Pacific_Ppls,
         asian   = C18_Ethnicity_L1_Asian) |>
  filter(pop > 0) |>
  get_percentages(pop, female:yadult)

df |> st_write(str_glue("{alias}-sa1-2018.gpkg"), delete_dsn = TRUE)

dissolve <- function(df, group_var, vars, fn = sum) {
  df |>
    group_by({{ group_var }}) |>
    summarise(across({{ vars }}, ~ fn(., na.rm = TRUE)))
}

df |>   
  dissolve(SA2, pop:yadult) |>
  get_percentages(pop, female:yadult) |> 
  st_write(str_glue("{alias}-sa2-2018.gpkg"), delete_dsn = TRUE)

bb <- (st_bbox(df) + c(-1, -1, 1, 1) * 250) |>
  st_as_sfc() |>
  data.frame() |>
  st_sf()

nz |> st_intersection(bb) |> 
  st_write(str_glue("{alias}-context.gpkg"), delete_dsn = TRUE)
