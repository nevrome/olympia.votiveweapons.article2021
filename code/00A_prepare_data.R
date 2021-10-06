# The main dataset (here "data/2019-03-28_Olympia_DB.csv") can not be shared 
# so only the two derived datasets 'weapons' and 'weapons_unfiltered'
# are available in this repository with the files "data/weapons.tsv" and
# "data/weapons_unfiltered.tsv"

library(magrittr)

#### read raw database export ####
olympia_artefacts_0 <- readr::read_csv(
  "data/2019-03-28_Olympia_DB.csv",
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

# dating
source("code/00B_dating_preparation_helper_functions.R")

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
) %>%
  # fixed overlapping time windows issue
  dplyr::mutate(
    dating_typology_start = dating_typology_start + 1
  )

# find_area
# remove ambiguous values
olympia_artefacts_3$find_area[grep("und|oder|\\?|\\,", olympia_artefacts_3$find_area)] <- NA_character_
# simplification Southern Stadium area
olympia_artefacts_3$find_area[olympia_artefacts_3$find_area %in% 
  c("Stadion-Südwest", "Stadion-Südwestecke")] <- "Stadion-West"

# greave orientation
olympia_artefacts_3 %<>%
  dplyr::mutate(
    orientation = dplyr::case_when(
      typology_class_2 == "Beinschiene" & grepl("Links", description) ~ "left",
      typology_class_2 == "Beinschiene" & grepl("Rechts", description) ~ "right",
      typology_class_2 == "Glockenpanzer, Brustschale" ~ "front",
      typology_class_2 == "Glockenpanzer, Rückenschale" ~ "back",
      TRUE ~ NA_character_
    )
  ) %>% dplyr::select(-description)
olympia_artefacts_3$orientation %<>% forcats::fct_explicit_na()

# simplification of overly complicated shield typology
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

#### reduce selection to weapons ####
weapon_artefacts <- olympia_artefacts_3 %>%
  dplyr::filter(
    typology_class_1 == "Waffe"
  )

#### translation to english ####

# load translation tables
translation_typology_class_2 <- readr::read_csv(
  "data/translation_de_en_typology_class_2.csv",
  col_types = readr::cols(
    de = readr::col_character(),
    en = readr::col_character()
  )
)
translation_find_area <- readr::read_csv(
  "data/translation_de_en_find_area.csv",
  col_types = readr::cols(
    de = readr::col_character(),
    en = readr::col_character()
  )
)

# apply translation
weapon_artefacts %<>%
  dplyr::mutate(
    typology_class_2 = translation_typology_class_2$en[
      match(typology_class_2, translation_typology_class_2$de)
    ],
    find_area = translation_find_area$en[
      match(find_area, translation_find_area$de)
    ]
  )

#### fine-grained manual data cleaning ####

weapon_artefacts -> w
w$typology_class_2[w$typology_class_3 %in% c(
  "Kammhelm"
)] <- "Helmet"
w$typology_class_3[w$typology_class_3 %in% c(
  "Bronzene Sauroter (Lanzeschuhe)Bronzene Sauroter (Lanzeschuhe)",
  "Bronzene Sauroter (Lanzeschuhe)"
)] <- "Bronzene Saurotere (Lanzenschuhe)"
w$typology_class_3[w$typology_class_3 %in% c(
  "Befestigungen für HandhabenD2"
)] <- "Befestigungen für Handhaben"
w$typology_class_3[w$typology_class_3 %in% c(
  "Eiserne Machairen (einshneidige Hiebschwerter)"
)] <- "Eiserne Machairen (einschneidige Hiebschwerter)"
w$typology_class_4[w$typology_class_4 %in% c(
  "Unbekannt",
  "Einem einzigen Stück"
)] <- NA_character_
w$typology_class_4[w$typology_class_4 %in% c(
  "Unbekannt",
  "Einem einzigen Stück",
  "C Stufe  I (Unsicher)",
  "C Stufe I (Unsicher)"
)] <- NA_character_
w -> weapon_artefacts

#### finalize data types ####

weapon_artefacts %<>%
  dplyr::mutate_at(
    .vars = dplyr::vars(
      tidyselect::one_of(
        "find_area",
        "orientation"
      ),
      tidyselect::starts_with(
        "typology_class"
      )
    ),
    .funs = function(x) {
      as.factor(x)
    }
  ) %>%
  dplyr::mutate(
    dating_typology_start = as.integer(dating_typology_start),
    dating_typology_end = as.integer(dating_typology_end)
  )

#### create relevant subset for this paper and store them ####

# unfiltered
weapons_unfiltered <- weapon_artefacts %>%
  dplyr::filter(
    typology_class_1 == "Waffe" &
    !is.na(typology_class_2)
  )
readr::write_tsv(
  weapons_unfiltered,
  file = "data/weapons_unfiltered.tsv"
)
save(
  weapons_unfiltered, 
  file = "data/weapons_unfiltered.RData"
)

# filtered by time, type and area
weapons <- weapon_artefacts %>%
  dplyr::filter(
    # type
    typology_class_1 == "Waffe" &
    !is.na(typology_class_2) &
    # area
    !is.na(find_area) &
    # dating
    !is.na(dating_typology_start) &
    !is.na(dating_typology_end) &
    dating_typology_start < -400 & 
    dating_typology_end > -1000
  )
readr::write_tsv(
  weapons,
  file = "data/weapons.tsv"
)
save(
  weapons, 
  file = "data/weapons.RData"
)

