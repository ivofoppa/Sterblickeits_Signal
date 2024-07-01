# Skript zum einlesen der Sterbefalldaten
# aggregieren der Sterbefälle nach Sterbedatum
# hinzufügen der Tagesmitteltemperatur


# Laden der Packages ------------------------------------------------------

pacman::p_load(tidyverse,data.table)

# Setzen des aktuellen Datums ---------------------------------------------

heute0 <- as.Date(as.POSIXlt(Sys.time(), tz = "Europe/Berlin")) ##aktuelles Datum

# Setzen der Working Directory --------------------------------------------

maindir <-dirname(dirname(rstudioapi::getSourceEditorContext()$path))

datenpfad <- file.path(maindir,"Daten")
skriptpfad <- file.path(maindir,"R-Skripte")

# Holen der aktuellsten Datei
source(file.path(skriptpfad,"Automatisches_Herunterladen.R"))

# dliste <- list.files(path = datenpfad,pattern = "Land_06_basis_sterb_stba_ab")


# Laden der Rohdaten ------------------------------------------------------

for (d in 0:100) {
  
  dateinme <- file.path(datenpfad,paste0("Land_06_basis_sterb_stba_ab2311_",heute0 - d,"_02-01-01.csv"))
  if (file.exists(dateinme)) {
    break
  }
}

datei <- fread(dateinme,quote = "\"",sep = "auto") %>% rename_all(tolower) %>%
  # mutate(across(eingang:sterbedatum,~as_date(.x,format = "%Y%m%d")))
  mutate(across(c(eingang,sterbedatum), ~ as.Date(as.character(.x),"%Y%m%d")))


# Nowcasting  ----------------------------------
feiertage <- read.table(file.path(datenpfad,"Feiertage.txt"),header = TRUE) %>%
  mutate(Datum = as.Date(Datum)) %>% as.vector()

# dliste <- list.files(path = datenpfad,pattern = "Land_06_basis_sterb_stba_ab")

## Daten in Hauptdatei eingelesen
maxsdatum <- max(datei$sterbedatum)

datei1 <- datei %>%
  filter(sterbedatum <= (maxsdatum-50),sterbedatum > (maxsdatum-200),
         !sterbedatum %in% c(feiertage$Datum,feiertage$Datum-1,feiertage$Datum-2,feiertage$Datum-3,feiertage$Datum-4,feiertage$Datum-5))

maxdatum <- max(datei1$eingang)
minsdatum <- min(datei1$sterbedatum)
heute1 <- maxdatum
heute2 <- max(datei$eingang)


nowcastdatei <- file.path(maindir,"Resultate",paste0("Nowcast_Daten_",heute0,".csv"))
codadatei <- file.path(maindir,"Resultate","Coda",paste0("coda_",heute0,".csv"))

if (!file.exists(nowcastdatei)) {
  source(file.path(skriptpfad,"Nowcast.R"))
  
  nowcastdaten <- nowcastdaten %>%
    rename(sterbedatum=datum) %>% mutate(sterbedatum = as.Date(sterbedatum))
} else {
  nowcastdaten <- read.csv2(nowcastdatei) %>% select(-X) %>%
    rename(sterbedatum=datum) %>% mutate(sterbedatum = as.Date(sterbedatum))
  
  codals1 <- read.csv2(codadatei) 
}
# Aggregieren nach Sterbedatum --------------------------------------------

datei_agg_Sterbedatum <- datei %>%
  group_by(sterbedatum) %>%
  summarise(Count = n())

# # Kombinieren der aggregierte Sterbedaten mit den Nowcast-Schätzungen
# datei_agg_Sterbedatum_plus <- datei_agg_Sterbedatum %>%
#   left_join(nowcastdaten,by="sterbedatum") %>%
#   mutate(sm=case_when(is.na(sm) ~ Count,
#                       TRUE ~ sm))


# Einlesen Temperaturdaten Flughafen Frankfurt ----------------------------

Flugh_Frankfurt_temp <- read.csv2(file.path(datenpfad, "Flugh_Frankfurt_temp_virt.csv")) %>%
  mutate(sterbedatum=as.Date(sterbedatum,"%d.%m.%Y"))
# View(Flugh_Frankfurt_temp)



# Merge Sterbefälle und Temperatur ----------------------------------------

datei_agg_temp_nowcast <- left_join(datei_agg_Sterbedatum, Flugh_Frankfurt_temp, by="sterbedatum")  %>%
  left_join(nowcastdaten,by="sterbedatum") %>%
  mutate(sm=case_when(is.na(sm) ~ Count,
                      TRUE ~ sm)) %>% 
  rename(Temp_24h = TMK)

#### Bis hier!


# Barplot with Counts -----------------------------------------------------

pacman::p_load(ggplot2,scales,tidyverse,zoo,install = FALSE)
# Filter the data for the specified date range

filtered_data <- datei_agg_temp_nowcast %>%
  rename_all(tolower) %>%
  filter(sterbedatum > maxsdatum-50 & sterbedatum <= maxsdatum) %>%
  mutate(tempkat = round(temp_24h,digits = 0),
         temp3 = rollmean(temp_24h, 3, fill = NA),
         temp3kat = ordered(round(temp3, digits=0)))


#########################################################################
# Tageswerte ------------------------------------------------------------
#########################################################################


# Tageswerte 2023 ---------------------------------------------------------


### Daten aus 2023 als csv einlesen

# Define gradient
gradient <- c("9" = "#1874CD", "10" = "#1874CD", "11" = "#009ACD", "12" = "#009ACD", 
              "13" = "#009ACD", "14" = "#87CEFA", "15" = "#87CEFA", "16" = "#87CEFA", 
              "17" = "#87CEFA", "18" = "#ADD8E6", "19" = "#ADD8E6", "20" = "#FFF68F", 
              "21" = "#ffc535", "22" = "#FF8C00", "23" = "#ff2200", "24" = "red",
              "25" = "#FF0000", "26" = "#EE0000", "27" = "#CD0000", "28" = "#8B0000", 
              "29" = "#8B008B")



ggplot(filtered_data %>% 
         filter(!is.na(tempkat)) %>% 
         mutate(tempkat = as.factor(tempkat)), 
       aes(x = sterbedatum, y = count)) +
  geom_rect(aes(xmin = sterbedatum - 0.5, xmax = sterbedatum + 0.5, ymin = 0, ymax = round(max(count, na.rm = TRUE), 10) + 100, fill = tempkat), alpha = 1) +
  scale_fill_manual(values = gradient) +  # NA values are already filtered out
  geom_bar(stat = "identity", fill = "black") +
  geom_pointrange(aes(y=sm,ymin=sll,ymax=sul),color="red",size=.5) + 
  geom_text(aes(label = round(count, digits = 0)), colour = "gray90", angle = 90, hjust = 1.5, size = 3) +
  scale_x_date(labels = date_format("%d.%B")) +
  xlab("sterbedatum") + 
  ylab("tägliche Sterbefälle\n") + 
  labs(fill = "Mittlere Tages-\ntemperatur in °C") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 16))

outfname <- file.path(maindir,"Resultate","Grafiken",paste0("Grafik_",heute2,".pdf"))
ggsave(filename = outfname, plot = last_plot(),width = 16,height = 9)#####################################################
#########################################################################
