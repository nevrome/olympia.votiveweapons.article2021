#!/usr/bin/env stack
-- stack --resolver lts-18.0 script --package shake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
 
main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_build"} $ do
    want $ map (\x -> "plots" </> x) [
          "01_typology_class_2_distribution.png"
        , "02_general_weapons_distribution_map.png"
        , "03_find_area_time_series_map.png"
        , "04_typology_class_2_time_series.png"
        , "05_typology_fine_time_series.png"
        , "06_development_dynamic.png"
        , "07_panoply_completeness.png"
        ]    
    
    "plots/01_typology_class_2_distribution.png" %> \out -> do
        let script = "code" </> "01_typology_class_2_distribution_barplot.R"
            dataFiles = [
                  "data" </> "weapons_unfiltered.RData"
                ]
        need $ [script] ++ dataFiles
        cmd_ "Rscript" script

    "plots/02_general_weapons_distribution_map.png" %> \out -> do
        let script = "code" </> "02_general_weapons_distribution_map.R"
            dataFiles = [
                  "data" </> "weapons_unfiltered.RData"
                , "data" </> "olympia_area_polygons_25834/olympia_areas_25834.shp"
                , "data" </> "background_map_olympia_epsg25834.tif"
                , "data" </> "translation_de_en_find_area.csv"
                ]
        need $ [script] ++ dataFiles
        cmd_ "Rscript " script