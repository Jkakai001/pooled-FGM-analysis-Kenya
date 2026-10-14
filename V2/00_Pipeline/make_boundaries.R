suppressMessages({library(sf); library(dplyr)})
a <- commandArgs(TRUE); b <- st_read(a[1], quiet = TRUE) |> st_make_valid()
county_to_province <- c(
  "MOMBASA"="COAST","KWALE"="COAST","KILIFI"="COAST","TANA RIVER"="COAST","LAMU"="COAST","TAITA TAVETA"="COAST",
  "GARISSA"="NORTH EASTERN","WAJIR"="NORTH EASTERN","MANDERA"="NORTH EASTERN",
  "MARSABIT"="EASTERN","ISIOLO"="EASTERN","MERU"="EASTERN","THARAKA NITHI"="EASTERN","EMBU"="EASTERN","KITUI"="EASTERN","MACHAKOS"="EASTERN","MAKUENI"="EASTERN",
  "NYANDARUA"="CENTRAL","NYERI"="CENTRAL","KIRINYAGA"="CENTRAL","MURANG A"="CENTRAL","KIAMBU"="CENTRAL",
  "TURKANA"="RIFT VALLEY","WEST POKOT"="RIFT VALLEY","SAMBURU"="RIFT VALLEY","TRANS NZOIA"="RIFT VALLEY","UASIN GISHU"="RIFT VALLEY",
  "ELGEYO MARAKWET"="RIFT VALLEY","NANDI"="RIFT VALLEY","BARINGO"="RIFT VALLEY","LAIKIPIA"="RIFT VALLEY","NAKURU"="RIFT VALLEY",
  "NAROK"="RIFT VALLEY","KAJIADO"="RIFT VALLEY","KERICHO"="RIFT VALLEY","BOMET"="RIFT VALLEY",
  "KAKAMEGA"="WESTERN","VIHIGA"="WESTERN","BUNGOMA"="WESTERN","BUSIA"="WESTERN",
  "SIAYA"="NYANZA","KISUMU"="NYANZA","HOMA BAY"="NYANZA","MIGORI"="NYANZA","KISII"="NYANZA","NYAMIRA"="NYANZA",
  "NAIROBI"="NAIROBI")
b$COUNTY <- toupper(gsub("[^A-Za-z ]", " ", b$DHSREGEN)) |> gsub(pattern = " +", replacement = " ") |> trimws()
stopifnot(all(b$COUNTY %in% names(county_to_province)), length(unique(b$COUNTY)) == 47)
b$PROVINCE <- unname(county_to_province[b$COUNTY])
dir.create(a[2], recursive = TRUE, showWarnings = FALSE)
st_write(b |> select(COUNTY, PROVINCE), file.path(a[2], "KEN_counties.shp"), delete_dsn = TRUE, quiet = TRUE)
prov <- b |> group_by(PROVINCE) |> summarise(.groups = "drop") |> st_make_valid()
st_write(prov, file.path(a[2], "KEN_provinces.shp"), delete_dsn = TRUE, quiet = TRUE)
print(table(b$PROVINCE)); print(st_is_valid(prov))
