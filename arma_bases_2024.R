# Librerías -------
pacman::p_load(tidyverse, eph)
remove(list = ls())

# Carga de EPH ------
eph_individual <- get_microdata(year = 2007:2024,
                                type = "individual",
                                trimester = 1)


eph2004i <- foreign::read.dbf("C:/Users/josed/OneDrive/Bases/EPH/dbf//Ind_t104.dbf")
eph2005i <- foreign::read.dbf("C:/Users/josed/OneDrive/Bases/EPH/dbf//Ind_t105.dbf")
eph2006i <- foreign::read.dbf("C:/Users/josed/OneDrive/Bases/EPH/dbf//Ind_t106.dbf")

eph0406i <- eph2004i %>% 
  add_row(eph2005i) %>% 
  add_row(eph2006i) %>% 
  mutate(CH15_COD = as.integer(CH15_COD),
         CH16_COD = as.integer(CH16_COD))

eph_individual <- eph_individual %>% 
  add_row(eph0406i)

eph_hogar <- get_microdata(year = 2007:2024,
                           type = "hogar", 
                           trimester = 1)

eph2004h <- foreign::read.dbf("C:/Users/josed/OneDrive/Bases/EPH/dbf//Hog_t104.dbf")
eph2005h <- foreign::read.dbf("C:/Users/josed/OneDrive/Bases/EPH/dbf//Hog_t105.dbf")
eph2006h <- foreign::read.dbf("C:/Users/josed/OneDrive/Bases/EPH/dbf//Hog_t106.dbf")

eph0406h <- eph2004h %>% 
  add_row(eph2005h) %>% 
  add_row(eph2006h)

eph_hogar <- eph_hogar %>%
  add_row(eph0406h)

eph_hogar <- eph_hogar %>%
  select(!c("PONDERA", "PONDIH", "REGION", "MAS_500", "AGLOMERADO", "ITF",
            "DECIFR",  "IDECIFR", "RDECIFR", "GDECIFR", "PDECIFR", "ADECIFR",
            "IPCF", "DECCFR", "IDECCFR", "RDECCFR", "GDECCFR", "PDECCFR",
            "ADECCFR"))

eph_0424 <- eph_individual %>% 
  left_join(eph_hogar, by = c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR"))

remove(eph_hogar, eph_individual, eph2004h, eph2005h, eph2006h, eph2004i, eph2005i, eph2006i, eph0406h, eph0406i)


# Arreglos varios ------
## Correción de ponderador (sólo cuando trabajo con bases pegadas anteriores a 2016)----------

eph_0424$PONDIH_emp <- ifelse(is.na(eph_0424$PONDIH), eph_0424$PONDERA, eph_0424$PONDIH)
eph_0424$PONDII_emp <- ifelse(is.na(eph_0424$PONDII), eph_0424$PONDERA, eph_0424$PONDII)
eph_0424$PONDIIO_emp <- ifelse(is.na(eph_0424$PONDIIO), eph_0424$PONDERA, eph_0424$PONDIIO)

## Corrección variables ocupación -----
eph_0424$PP04D_COD[eph_0424$PP04D_COD==""] <- NA
eph_0424$PP11D_COD[eph_0424$PP11D_COD==""] <- NA

eph_0424$PP04D_COD <- str_pad(eph_0424$PP04D_COD, 5, pad = "0")
eph_0424$PP11D_COD <- str_pad(eph_0424$PP11D_COD, 5, pad = "0")


#Selección de variable-----------
# eph_0424 <- eph_0424 %>% 
#   select(CODUSU, NRO_HOGAR, COMPONENTE, ANO4, 
#          TRIMESTRE, REGION, PONDERA, PONDIH,PONDII, PONDIIO, CH03, CH04,
#          CH06, CH08, ESTADO, CAT_OCUP, CAT_INAC, 
#          PP02H, PP02I, PP3E_TOT, PP3F_TOT, INTENSI,
#          PP04A, PP04B_COD, PP04B1, PP04C, PP04C99, PP04D_COD,
#          PP07H, PP07I, PP10A, PP10E, PP11A, PP11B_COD,
#          PP11C, PP11C99, PP11D_COD, P21, TOT_P12, P47T, T_VI, II7, ITF, IPCF, PONDIH_emp, PONDII_emp,
#          PONDIIO_emp)

saveRDS(eph_0424, file = "bases/eph_0424.RDS")
