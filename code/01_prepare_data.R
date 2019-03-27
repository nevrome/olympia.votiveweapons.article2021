library(magrittr)

#### read raw database export ####
olympia_artefacts_0 <- readr::read_csv(
  "data/2019-03-27_Olympia_DB.csv",
  na = c("NA", "-1", ""),
  col_types = readr::cols(
    `Datierung_Metall::AnfDatJh` = readr::col_integer(),
    `Datierung_Metall::AnfPraezise` = readr::col_integer(),
    `Datierung_Metall::EndDatJh` = readr::col_integer(),
    `Datierung_Metall::EndPraezise` = readr::col_integer(),
    .default = readr::col_character()
  )
)

#### select rename and order variables ####
olympia_artefacts_1 <- olympia_artefacts_0 %>%
  dplyr::select(
    general_id = "Auto_Objektnummer",
    dating_typology_start_century = "Datierung_Metall::AnfDatJh",
    dating_typology_start_sign = "Datierung_Metall::AnfDatvn",
    dating_typology_start_century_section = "Datierung_Metall::AnfDatZeitraum",
    dating_typology_start_precise = "Datierung_Metall::AnfPraezise",
    dating_typology_end_century = "Datierung_Metall::EndDatJh",
    dating_typology_end_sign = "Datierung_Metall::EndDatvn",
    dating_typology_end_century_section = "Datierung_Metall::EndDatZeitraum",
    dating_typology_end_precise = "Datierung_Metall::EndPraezise",
    find_date = "Funddatum",
    find_area = "Herkunft_Lokal_Gebiet",
    typology_class_1 = "Klassifizierung_ObjektartAllgemein",
    typology_class_2 = "KurzbeschreibungMetall",
    typology_class_3 = "Klassifizierung_Form",
    typology_class_4 = "Material_Farbe",
    description = "Beschreibung"
  )

#### remove (rows and columns) artefacts and variables with no information ####
olympia_artefacts_2 <- olympia_artefacts_1 %>%
  janitor::remove_empty(which = c("rows", "cols")) 

#### simplify and standardize information ####
olympia_artefacts_3 <- olympia_artefacts_2

# small typology reordering
helmet_ident <- olympia_artefacts_3$typology_class_3 == "Helm" & !is.na(olympia_artefacts_3$typology_class_3)
olympia_artefacts_3$typology_class_3[helmet_ident] <- olympia_artefacts_3$typology_class_2[helmet_ident]
olympia_artefacts_3$typology_class_2[helmet_ident] <- "Helm"

# dating
source("code/99_dating_preparation_helper_functions.R")

olympia_artefacts_3 %<>% tibble::add_column(
  dating_typology_start = dating_decision(
    .$dating_typology_start_precise, 
    .$dating_typology_start_century, 
    .$dating_typology_start_century_section, 
    .$dating_typology_start_sign, 
    "start"
  ),
  dating_typology_end = ifelse(
    is.na(.$dating_typology_end_precise) & is.na(.$dating_typology_end_century),
    dating_decision(
      .$dating_typology_start_precise, 
      .$dating_typology_start_century, 
      .$dating_typology_start_century_section, 
      .$dating_typology_start_sign, 
      "end"
    ),
    dating_decision(
      .$dating_typology_end_precise, 
      .$dating_typology_end_century, 
      .$dating_typology_end_century_section, 
      .$dating_typology_end_sign, 
      "end"
    )
  ),
  .after = "general_id"
) %>% dplyr::select(
  -dating_typology_start_precise,
  -dating_typology_start_century,
  -dating_typology_start_century_section,
  -dating_typology_start_sign,
  -dating_typology_end_precise,
  -dating_typology_end_century,
  -dating_typology_end_century_section,
  -dating_typology_end_sign
)

# find_date
clean_find_date <- function(x) {
  dplyr::case_when(
    grepl("^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}$", x) ~ lubridate::dmy(x),
    grepl("^[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}$", x) ~ lubridate::ymd(x),
    TRUE ~ as.Date(NA)
  )
}
olympia_artefacts_3 %<>%
  dplyr::mutate(
    find_date = clean_find_date(find_date)
  )

# find_area
# remove ambiguous values 
olympia_artefacts_3$find_area[grep("und|oder|\\?|\\,", olympia_artefacts_3$find_area)] <- NA_character_
# simplification Southern Stadium area
olympia_artefacts_3$find_area[olympia_artefacts_3$find_area %in% c("Stadion-Südwest", "Stadion-Südwestecke")] <- "Stadion-West"

# cuisse orientation (for cuisses)
olympia_artefacts_3 %<>%
  dplyr::mutate(
    cuisse_orientation = dplyr::case_when(
      typology_class_2 == "Beinschiene" & grepl("Links", description) ~ "left",
      typology_class_2 == "Beinschiene" & grepl("Rechts", description) ~ "right",
      TRUE ~ NA_character_
    )
  )

# simplification overly complicated shield typology
olympia_artefacts_3[
  !is.na(olympia_artefacts_3$typology_class_3) & 
  olympia_artefacts_3$typology_class_3 == "Silhouettenbleche",
]$typology_class_4 <- NA_character_

olympia_artefacts_3[
  !is.na(olympia_artefacts_3$typology_class_2) &
  !is.na(olympia_artefacts_3$typology_class_4) &
  olympia_artefacts_3$typology_class_2 == "Schild oder Schildfragment",
]$typology_class_4 <- olympia_artefacts_3[
  !is.na(olympia_artefacts_3$typology_class_2) &
    !is.na(olympia_artefacts_3$typology_class_4) &
    olympia_artefacts_3$typology_class_2 == "Schild oder Schildfragment",
  ]$typology_class_4 %>% substr(1,5) %>% gsub("[a-z]|[αβγδεζηθικλμνξοπρςστυφχψω]", "", .)


#### finalize data types ####

# factors
olympia_artefacts_3 %<>%
  dplyr::mutate_at(
    .vars = dplyr::vars(
      tidyselect::one_of(
        "find_area",
        "cuisse_orientation"
      ),
      tidyselect::starts_with(
        "typology_class"
      )
    ),
    .funs = function(x) {
      as.factor(x)
    }
  )

#### create relevant subset for this paper and store them ####

# unfiltered
weapons_unfiltered <- olympia_artefacts_3 %>%
  dplyr::filter(
    typology_class_1 == "Waffe"
  )
save(weapons_unfiltered, file = "data/weapons_unfiltered.RData")

# filtered by time and area
weapons <- olympia_artefacts_3 %>%
  dplyr::filter(
    typology_class_1 == "Waffe" &
      !is.na(find_area) &
      !is.na(dating_typology_start) &
      !is.na(dating_typology_end) &
      dating_typology_start < -400 & 
      dating_typology_end > -1000
  )
save(weapons, file = "data/weapons.RData")

