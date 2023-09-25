# 2022
!python -m motuclient --motu https://nrt.cmems-du.eu/motu-web/Motu --service-id IBI_ANALYSISFORECAST_PHY_005_001-TDS --product-id cmems_mod_ibi_phy_anfc_0.027deg-3D_P1D-m --longitude-min -2 --longitude-max 1.2 --latitude-min 36.9 --latitude-max 41 --date-min "2022-06-01 00:00:00" --date-max "2022-09-30 23:59:59" --depth-min 0.49402499198913574 --depth-max 50 --variable thetao --out-dir data --out-name temp_2022_jun_sept.nc --user jatalah --pwd Estela2012

# 2021
!python -m motuclient --motu https://nrt.cmems-du.eu/motu-web/Motu --service-id IBI_ANALYSISFORECAST_PHY_005_001-TDS --product-id cmems_mod_ibi_phy_anfc_0.027deg-3D_P1D-m --longitude-min -2 --longitude-max 1.2 --latitude-min 36.9 --latitude-max 41 --date-min "2021-06-01 00:00:00" --date-max "2021-09-30 23:59:59" --depth-min 0.49402499198913574 --depth-max 50 --variable thetao --out-dir data --out-name temp_2021_jun_sept.nc --user jatalah --pwd Estela2012

# 2020 - 2022
!python -m motuclient --motu https://nrt.cmems-du.eu/motu-web/Motu --service-id MEDSEA_ANALYSISFORECAST_PHY_006_013-TDS --product-id cmems_mod_med_phy-tem_anfc_4.2km_P1D-m --longitude-min -1.65 --longitude-max 0.2 --latitude-min 37.2 --latitude-max 38.65 --date-min "2020-07-02 00:00:00" --date-max "2023-06-30 23:59:59" --depth-min 1.0182366371154785 --depth-max 51.379859924316406 --variable bottomT --variable thetao --out-dir data --out-name med_thetao_2020_2022.nc --user jatalah --pwd Estela2012


# long-term monthly data 1987-2023 0 - 10 m all MEDSEA_MULTIYEAR_PHY_006_004
!python -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id MEDSEA_MULTIYEAR_PHY_006_004-TDS --product-id med-cmcc-tem-rean-d --longitude-min -1.65 --longitude-max 0.2 --latitude-min 37.2 --latitude-max 38.65 --date-min "1987-07-01 00:00:00" --date-max "2020-07-01 23:59:59" --depth-min 1.0182366371154785 --depth-max 10 --variable thetao --out-dir data --out-name thetao_1987_2020_0_10m.nc --user jatalah --pwd Estela2012

# long-term monthly data 1987-2023 10 - 20 m all MEDSEA_MULTIYEAR_PHY_006_004
!python -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id MEDSEA_MULTIYEAR_PHY_006_004-TDS --product-id med-cmcc-tem-rean-d --longitude-min -1.65 --longitude-max 0.2 --latitude-min 37.2 --latitude-max 38.65 --date-min "1987-07-01 00:00:00" --date-max "2020-07-01 23:59:59" --depth-min 10 --depth-max 20 --variable thetao --out-dir data --out-name thetao_1987_2020_10_20m.nc --user jatalah --pwd Estela2012

# long-term monthly data 1987-2023 20 - 30 m all MEDSEA_MULTIYEAR_PHY_006_004
!python -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id MEDSEA_MULTIYEAR_PHY_006_004-TDS --product-id med-cmcc-tem-rean-d --longitude-min -1.65 --longitude-max 0.2 --latitude-min 37.2 --latitude-max 38.65 --date-min "1987-07-01 00:00:00" --date-max "2020-07-01 23:59:59" --depth-min 20 --depth-max 30 --variable thetao --out-dir data --out-name thetao_1987_2020_20_30m.nc --user jatalah --pwd Estela2012

# long-term monthly data 1987-2023 30 - 40 m all MEDSEA_MULTIYEAR_PHY_006_004
!python -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id MEDSEA_MULTIYEAR_PHY_006_004-TDS --product-id med-cmcc-tem-rean-d --longitude-min -1.65 --longitude-max 0.2 --latitude-min 37.2 --latitude-max 38.65 --date-min "1987-07-01 00:00:00" --date-max "2020-07-01 23:59:59" --depth-min 30 --depth-max 40 --variable thetao --out-dir data --out-name thetao_1987_2020_30_40m.nc --user jatalah --pwd Estela2012

# SST med all SST_MED_SST_L4_REP_OBSERVATIONS_010_021
!python -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id SST_MED_SST_L4_REP_OBSERVATIONS_010_021-TDS --product-id cmems_SST_MED_SST_L4_REP_OBSERVATIONS_010_021 --longitude-min -1.65 --longitude-max 0.2 --latitude-min 37.2 --latitude-max 38.65 --date-min "1981-08-25 00:00:00" --date-max "2022-12-17 23:59:59" --variable analysed_sst --out-dir data --out-name stt_med_all.nc --user jatalah --pwd Estela2012
