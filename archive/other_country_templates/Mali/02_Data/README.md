# Data folder for the risk incidence and attitudinal incidence notebooks

Each notebook in 01_Notebooks expects its data organized by country under this folder.

02_Data/
  raw/
    KEN/
      dhs/
        KE_2022_DHS_IR.dta
      mics/
        (MICS individual  files, if used for this country)
      boundaries/
        KEN_admin1.shp (and the accompanying shx, dbf, prj files)
  processed/
    KEN/
  

Add one folder per ISO3 country code under raw, matching the country_iso3 value set in Section 0 of each notebook. Raw individual recode files come from a registered DHS Program data request, most conveniently pulled with the rdhs R package, or from a direct MICS portal request, see 00_Methodology/02_Literature_and_Tools_Reviewed.md for both. Boundary shapefiles come from the DHS Spatial Data Repository or another authoritative admin boundary source, matching the admin_level set in Section 0.

Nothing in this folder is committed with real survey microdata by default, since DHS and MICS microdata carry redistribution restrictions, each country's files should be placed here locally after your own registered data request.
