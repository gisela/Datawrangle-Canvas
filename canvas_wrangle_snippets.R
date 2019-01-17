# library(dplyr)

# install.packages("readr")

# library(readr)

# Created by Gisela Bäcklander, 2019

# Här är en del kodsnuttar jag skrev för att sammanfoga inlämningar och närvaro från flera
# olika filer exporterade från Canvas. Man kan säkert lösa det mesta smidigt med VLOOKUP i 
# Excel också.


# MANUELLT: Ladda ner från Canvas:
#     1) Omdömen (godkända inlämningar/uppgifter)
#     2) Närvaro från Uppgifter -> Roll call -> Exportera rapport (kugghjulet)
#          från samtliga datum som hör till momentet
#     Eventuellt en fil med personnummer på studenterna

## Jag har laddat ner två filer från Canvas som innehåller Roll call (närvaro)-rapporter från
## två olika tillfällen och vill mergea dem

Hierarki_711_911 <- read_csv("Downloads/Hierarki_711_911.csv")
Hierarki_2811 <- read_csv("Downloads/Hierarki_2811.csv")

### Eftersom filerna är strukturellt identiska kan jag bara lägga till rader med rbind
Hierarki_alla <- rbind(Hierarki_711_911, Hierarki_2811)

#Hitta radnr på två studenter med fel frånvaro, som ska vara närvaro. Numret är ett Student ID.
grep(52447, Hierarki_alla$`Student ID`)
grep(47546, Hierarki_alla$`Student ID`)


# Kolla att namnet är rätt med hjälp av radnr. Namnet är här i kolumn 8.
Hierarki_alla[110,8]

## Ändra absent till present, i.e. skriv över innehållet i cellen
Hierarki_alla[110,10] = "present"
Hierarki_alla[1,10] = "present"

# Skriv en ny csv fil med hierarkinärvaron (en övning)
write_csv(Hierarki_alla, "Hierarki_närvaro_alla.csv", append = FALSE)

# Läs in närvaro INL 1 från Canvas-genererad Roll call fil.
Inl1_Narvaro_249 <- read_csv("Downloads/Inl1_Narvaro_249.csv")

# Läs in Godkänd inlämning INL 1: Detta läser eg. in hela betygsfilen från Canvas
# Byt filnamnet 
Export_Me1010 <- read_csv("Downloads/2019-01-16T1446_Omdömen-ME1010.csv")

# Kontrollera första raden
View(Export_Me1010[1,])

# Ta bort manuellt raden "points possible", eller kör denna rad, ta då bort # framför
# Export_Me1010 = Export_Me1010[-1,]

# Testa ID sök - genererar ett radnummer
grep(47546, Export_Me1010$ID)

# På radnr (4) I kolumn "Student", vad står i den rutan? (namnet)
Export_Me1010[4,"Student"]

# Här börjar jag lägga till varje uppgiftsinlämning och närvaro som nya kolumner. Alla 
# omdömen kan ju egentligen hämtas direkt från omdömesfilen men jag gjorde dem en och en för 
# att jag ville ha stenkoll initialt att det blev rätt. Längre ner, på "intervjuprojektet", 
# tar jag alla kolumner samtidigt.


# Med G_INL1 som bas, läs in och lägg till 1 kolumn med "närvaro inl1" matchat
# på student ID

## Hämta bara kolumnerna ID och godkänd-status
G_INL1 <- select(Export_Me1010, 2, 6)
N_INL1 <- select(Inl1_Narvaro_249, 7, 10)

##Byt namn på kolumner så Student ID heter lika, så de kan joinas 
names(G_INL1) <- c("Student ID", "INL1")

## Kontrollera namn
names(N_INL1)

# Här sker själva ihopslagningen med matchning på ID

table1.df <- full_join(Inl1_Narvaro_249, G_INL1, by="Student ID")

# ## Hämta bara kolumnerna ID och godkänd INL2-status
G_INL2 <- select(Export_Me1010, 2, 7)

##Byt namn på kolumner så Student ID heter lika 
names(G_INL2) <- c("Student ID", "INL2")

names(table1.df)

# Lägg till INL2 till INL1 (en till kolumn)
table2.df <- full_join(table1.df, G_INL2, by="Student ID")

# ## Hämta bara kolumnerna ID och godkänd INL3-status
G_INL3 <- select(Export_Me1010, 2, 10)

##Byt namn på kolumner så Student ID heter lika 
names(G_INL3) <- c("Student ID", "INL3")

names(table2.df)

# Lägg till INL3 till INL1 och 2
table3.df <- full_join(table2.df, G_INL3, by="Student ID")

# Förtydliga vilken närvaro som är vilken innan jag lägger till hierarki-betygen
# dvs byt namn på en kolumn
colnames(table3.df)[10] <- "INL1 närvaro"

G_HIE <- select(Hierarki_alla, 7, 10)
colnames(G_HIE)[2] <- "HIERARKI närvaro"

OVN_alla <- full_join(table3.df, G_HIE, by="Student ID")

# Skriv en csv fil med alla övningsresultat
write_csv(OVN_alla, "ÖVNING.csv", append = FALSE)

# Det som saknas nu är personnr. De hämtar jag från en excelfil jag har, klippt ID och PNR till
# en csv fils
twocols <- read_delim("Downloads/twocols.csv", 
                      +     ";", escape_double = FALSE, trim_ws = TRUE)

## Hämta bara de två kolumnerna med innehåll, fler kolumner hade följt meds
PNR <- select(twocols, 1, 2) 

OVN_o_PNR <- full_join(OVN_alla, PNR, by="Student ID")


# Skriv en csv fil med alla övningsresultat inkl pnr
write_csv(OVN_o_PNR, "ÖVNING_PNR.csv", append = FALSE)

# MANUELLT: Kontrollerar i Excel och dubbelkollar mot en annan lista, verkar rätt.

colnames(OVN_o_PNR)

OVN_o_PNR["INL1 närvaro"]
OVN_o_PNR["INL3"]

###############


###############

# Samla för Intervjuprojektet på samma sätt, få med pnr

ME1010_190117 <- read_csv("Downloads/2019-01-17T0957_Omdömen-ME1010.csv")

# Kontrollera första raden
View(ME1010_190117[1,])

# Ta bort manuellt raden "points possible", eller kör denna rad
ME1010_190117 = ME1010_190117[-1,]

# Testa ID sök Staffan - genererar ett radnummer
grep(47546, ME1010_190117$ID)

# På radnr (4) I kolumn "Student", vad står i den rutan? (namnet)
ME1010_190117[4,"Student"]


## Hämta bara kolumnerna ID, Intervjuunderlag inlämnad stäng, Intervjuunderlag faktiskt, 
## Peer review gedd, Rapport godkänd

Intervjuprojekt <- select(ME1010_190117, 1, 2, 8, 15, 9, 11)

# Byt namn på ID till STUDENT ID

colnames(Intervjuprojekt)[2] <- "Student ID"

# Måste ha närvaro också!
attendance_reports_attendance_fd8a1309_3731_4e64_9a2a_004582c57dab <- read_csv("Downloads/attendance_reports_attendance-fd8a1309-3731-4e64-9a2a-004582c57dab.csv")

N_IPR <- select(attendance_reports_attendance_fd8a1309_3731_4e64_9a2a_004582c57dab, 7, 9, 10)


## LÄgg till PNR

IP_o_PNR <- full_join(Intervjuprojekt, PNR, by="Student ID")
IPRoj <- full_join(IP_o_PNR, N_IPR, by="Student ID")

# Ta bort en som bara var personnr men ingen mer info i denna uppgift
IP_o_PNR = IP_o_PNR[-189,]

# Skriv en csv fil med alla Intervjuprojektresultat
write_csv(IPRoj, "Intervjuprojekt och personnr.csv", append = FALSE)
