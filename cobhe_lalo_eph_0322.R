rm(list = ls())
eph <- readRDS("bases/eph_0424.RDS")

pacman::p_load(tidyverse)

# COBHE para EPH ------------------

##Separo dígitos del CNO-----
eph$cno <- ifelse(is.na(eph$PP04D_COD), eph$PP11D_COD, eph$PP04D_COD)

eph$cno12 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 1, 2), str_sub(eph$cno, 1, 1))
eph$cno3 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 3, 3), str_sub(eph$cno, 2, 2))
eph$cno4 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 4, 4), str_sub(eph$cno, 3, 3))
eph$cno5 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 5, 5), str_sub(eph$cno, 4, 4))

eph$cno12 <- as.numeric(eph$cno12)
eph$cno3 <- as.numeric(eph$cno3)
eph$cno4 <- as.numeric(eph$cno4)
eph$cno5 <- as.numeric(eph$cno5)

##Categoría ocupacional
eph <- eph %>% 
  mutate(categoria = case_when(CAT_OCUP == 1 & cno3 == 0 ~ 1,
                               CAT_OCUP == 2 | cno3 == 1 ~ 2,
                               (CAT_OCUP == 3 | CAT_OCUP == 4) | (cno3 == 2 | cno3 == 3) ~ 3,
                               (CAT_OCUP == 3 | CAT_OCUP == 4) & (cno3 == 0) ~ 3,
                               CAT_OCUP == 1 & cno3 > 1 ~ 1,
                               CAT_OCUP == 9 & cno3 == 0 ~ 1,
                               CAT_OCUP == 9 & cno3 > 1 ~ 3,
                               cno3 == 9 & CAT_OCUP > 2 ~ 3))

## Tamaño del establecimiento------
eph <- eph %>% 
  mutate(tamano = case_when((PP04C > 0 & PP04C <= 5) | (PP04C == 99 & PP04C99 == 1) ~ 1,
                            (PP04C > 5 & PP04C < 99) | (PP04C == 99 & (PP04C99 == 2 | PP04C99 == 3)) ~ 2,
                            ((PP04C == 0 & PP04C99 == 0) | (PP04C == 99 & PP04C99 == 9)) & PP04A == 1 ~ 2,
                            PP04A == 1 ~ 2,
                            TRUE ~ NA_real_)) 

eph <- eph %>% 
  mutate(tamano_desocup = case_when((PP11C > 0 & PP11C <= 5) | (PP11C == 99 & PP11C99 == 1) ~ 1,
                                    (PP11C > 5 & PP11C < 99) | (PP11C == 99 & (PP11C99 == 2 | PP11C99 == 3)) ~ 2,
                                    ((PP11C == 0 & PP11C99 == 0) | (PP11C == 99 & PP11C99 == 9)) & PP11A == 1 ~ 2,
                                    PP11A == 1 ~ 2,
                                    TRUE ~ NA_real_)) 

eph$tamano <- ifelse(is.na(eph$tamano), eph$tamano_desocup, eph$tamano)


## Clasificador --------------

eph <- eph %>% 
  mutate(cobhe = case_when(#Clase I: propietarios > 5 y directivos, gerentes, funcionarios de dirección
                          cno12 >= 0 & cno12 <= 4 ~ 1, 
                          cno12 == 6 | cno12 == 7 ~ 1,
                          
                          #Clase II: propietarios < 5 y directivos, gerentes, funcionarios de dirección  
                          cno12 == 5 ~ 2,
                          
                          #Clase III: cuenta propias profesionales/calificados
                          (cno12 == 32 | cno12 == 35 | cno12 == 51 | cno12 == 52 | cno12 == 53 |
                             cno12 == 54 | cno12 == 57 | cno12 == 58 | cno12 == 60 |
                             cno12 == 61 | cno12 == 62 | cno12 == 63 | cno12 == 64 |
                             cno12 == 65 | cno12 == 70 | cno12 == 71 | cno12 == 72 |
                             cno12 == 80 | cno12 == 82) & categoria == 2 & cno5 < 3 ~ 3,
                          
                          (cno12 == 10 | cno12 == 11 | cno12 == 20 | cno12 == 30 | cno12 == 31 | cno12 == 40 |
                             cno12 == 41 | cno12 == 42 | cno12 == 43 | cno12 == 44 | cno12 == 45 |
                             cno12 == 46 | cno12 == 47 | cno12 == 50 | cno12 == 81 | cno12 == 90 | 
                             cno12 == 91 | cno12 == 92) & categoria == 2 & cno5 <= 3 ~ 3,
                          
                          cno12 == 34 & categoria == 2 & cno5 <= 2 ~ 3,
                          cno12 == 34 & categoria == 2 & cno5 > 2 & cno4 > 1 ~ 3,
                          cno12 == 35 & categoria == 2 & cno5 > 2 & cno4 > 1 ~ 3,
                          
                          #Clase IV: trabajadores no manuales > 5
                          (cno12 >= 10 & cno12 <= 20) & categoria == 3 & tamano == 2 ~ 4,
                          cno12 == 30 & categoria == 3 & tamano == 2 ~ 4,
                          (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
                          cno12 == 35 & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
                          cno12 == 36 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
                          (cno12 >= 40 & cno12 <= 43) & categoria == 3 & tamano == 2 ~ 4,
                          cno12 == 44 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
                          (cno12 == 45 | cno12 == 46) & categoria == 3 & tamano == 2 ~ 4,
                          (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
                          (cno12 == 50 | cno12 == 52) & categoria == 3 & tamano == 2 ~ 4,
                          cno12 == 54 & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
                          cno12 == 58 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
                          cno12 == 80 & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
                          cno12 == 81 & categoria == 3 & tamano == 2 ~ 4,
                          cno12 == 91 & categoria == 3 & tamano == 2 ~ 4,
                          cno12 == 92 & categoria == 3 & tamano == 2 & cno5 == 1 ~ 4,
                          
                          #Clase V: trabajadores manuales > 5
                          (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
                          cno12 == 34 & categoria == 3 & tamano == 2 ~ 5,
                          cno12 == 35 & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
                          cno12 == 36 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          cno12 == 44 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          cno12 == 51 & categoria == 3 & tamano == 2 ~ 5,
                          cno12 == 53 & categoria == 3 & tamano == 2 ~ 5,
                          cno12 == 54 & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
                          (cno12 == 56 | cno12 == 57)  & categoria == 3 & tamano == 2 ~ 5,
                          cno12 == 58 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          (cno12 == 64 | cno12 == 65)  & categoria == 3 & tamano == 2 ~ 5,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          cno12 == 80 & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
                          cno12 == 80 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          (cno12 == 82 | cno12 == 90) & categoria == 3 & tamano == 2  ~ 5,
                          cno12 == 92 & categoria == 3 & tamano == 2 & cno5 > 1 ~ 5,
                          
                          #Clase VI: trabajadores no manuales < 5
                          (cno12 >= 10 & cno12 <= 20) & categoria == 3 & tamano == 1 ~ 6,
                          cno12 == 30 & categoria == 3 & tamano == 1 ~ 6,
                          (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
                          cno12 == 35 & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
                          cno12 == 36 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
                          (cno12 >= 40 & cno12 <= 43) & categoria == 3 & tamano == 1 ~ 6,
                          cno12 == 44 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
                          (cno12 == 45 | cno12 == 46) & categoria == 3 & tamano == 1 ~ 6,
                          (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
                          (cno12 == 50 | cno12 == 52) & categoria == 3 & tamano == 1 ~ 6,
                          cno12 == 54 & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
                          cno12 == 58 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
                          cno12 == 80 & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
                          cno12 == 81 & categoria == 3 & tamano == 1 ~ 6,
                          cno12 == 91 & categoria == 3 & tamano == 1 ~ 6,
                          cno12 == 92 & categoria == 3 & tamano == 1 & cno5 == 1 ~ 6,
                          
                          #Clase VII: trabajadores manuales < 5
                          (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
                          cno12 == 34 & categoria == 3 & tamano == 1 ~ 7,
                          cno12 == 35 & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
                          cno12 == 36 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          cno12 == 44 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          cno12 == 51 & categoria == 3 & tamano == 1 ~ 7,
                          cno12 == 53 & categoria == 3 & tamano == 1 ~ 7,
                          cno12 == 54 & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
                          (cno12 == 56 | cno12 == 57)  & categoria == 3 & tamano == 1 ~ 7,
                          cno12 == 58 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          (cno12 == 64 | cno12 == 65)  & categoria == 3 & tamano == 1 ~ 7,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          cno12 == 80 & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
                          cno12 == 80 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          (cno12 == 82 | cno12 == 90) & categoria == 3 & tamano == 1  ~ 7,
                          cno12 == 92 & categoria == 3 & tamano == 1 & cno5 > 1 ~ 7,
                          cno12 == 55 ~ 7,
                          is.na(tamano) & PP04B1 == 1 ~ 7,
                          
                          #Clase VIII: Cuenta propia semi-calificados y no calificados
                          (cno12 == 10 | cno12 == 32 | cno12 == 51 | cno12 == 52 | cno12 == 53 |
                             cno12 == 54 | cno12 == 57 | cno12 == 58 | cno12 == 60 |
                             cno12 == 61 | cno12 == 62 | cno12 == 63 | cno12 == 64 |
                             cno12 == 65 | cno12 == 70 | cno12 == 71 | cno12 == 72 |
                             cno12 == 80 | cno12 == 82) & categoria == 2 & (cno5 == 3 | cno5 == 4) ~ 8,
                          
                          (cno12 == 11 | cno12 == 20 | cno12 == 30 | cno12 == 31 | cno12 == 40 |
                             cno12 == 41 | cno12 == 42 | cno12 == 43 | cno12 == 44 | cno12 == 45 |
                             cno12 == 46 | cno12 == 47 | cno12 == 50 | cno12 == 81 | cno12 == 90 | 
                             cno12 == 91 | cno12 == 92) & categoria == 2 & (cno5 == 4) ~ 8,
                          
                          cno12 == 34 & categoria == 2 & cno5 > 2 & cno4 == 1 ~ 8,
                          cno12 == 35 & categoria == 2 & cno5 > 2 & cno4 == 1 ~ 8,
                          cno12 == 36 & categoria == 2 ~ 8,
                          cno12 == 56 & categoria == 2 ~ 8,
                          cno12 == 33 ~ 8,
                          categoria == 2 & cno5 == 4 ~ 8,
                          
                          #Clase IX: Inactivos
                          ESTADO == 3 | ESTADO == 4 ~ 9
                          ))

eph$cobhe_f <- factor(eph$cobhe, 
       labels = c('Propietarios y directivos >5',
                  'Propietarios y directivos <5',
                  'Cuenta propia profesionales / calificados',
                  'Trabajadores no manuales >5',
                  'Trabajadores manuales >5',
                  'Trabajadores no manuales <5',
                  'Trabajadores manuales <5',
                  'Cuenta propia no calificados',
                  'Inactivos'))

## Pruebas-----------
# perdidos <- eph %>% 
#   filter(is.na(cobhe) & !is.na(cno) & cno5 < 4 & !is.na(tamano)) %>% 
#   select(cno12, cno3, cno4, cno5, categoria, tamano, PP04D_COD, PP11D_COD, PP04C, PP04C99) %>% 
#   arrange(categoria, cno12, cno5) %>% 
#   view()

## Clase según dominancia de sexo ---------
eph_hog <- eph %>%
 filter(CH03 <= 2) %>% 
 group_by(CODUSU, NRO_HOGAR, ANO4) %>% 
 mutate(clase_dom = min(cobhe, na.rm = TRUE)) 

# seleccion <- eph_hog %>% 
#  select(CODUSU, NRO_HOGAR, CH03, ANO4, cobhe, clase_dom) %>% 
#  arrange(CODUSU, NRO_HOGAR, cobhe, clase_dom)

eph_hog <- eph_hog %>% 
 mutate(dom = case_when(cobhe == clase_dom ~ 1,
                        cobhe > clase_dom ~ 0)) %>% 
 group_by(CODUSU, NRO_HOGAR, ANO4) %>% 
 mutate(dom_sum = sum(dom, na.rm = TRUE))

# seleccion <- eph_hog %>% 
#   select(CODUSU, NRO_HOGAR, CH03, ANO4, cobhe, clase_dom, dom, dom_sum) %>% 
#   arrange(CODUSU, NRO_HOGAR, cobhe, clase_dom) %>% 
#   view()

eph_hog <- eph_hog %>% 
  mutate(dominancia = case_when(dom == 1 & dom_sum == 1 ~ 1,
                                dom == 0 & dom_sum == 1 ~ 0,
                                dom == 1 & dom_sum == 2 & CH03 == 1 ~ 1,
                                dom == 1 & dom_sum == 2 & CH03 >= 2 ~ 0))

eph_hog <- eph_hog %>% 
  select(ANO4, CODUSU, NRO_HOGAR, CH03, dominancia)

eph <- eph %>% 
  left_join(eph_hog, by = c("ANO4", "CODUSU", "NRO_HOGAR", "CH03"))

rm(eph_hog, seleccion, perdidos)

## Guardo la base--------
saveRDS(eph, file = "bases/eph_0424.RDS")
