############################
## Replication Material: Script for Definitions
## Paltra, Sältzer, Stecker
## Affective States - Cultural and Affective Polarization in a Multilevel-Multiparty System 
## 2025
## Political Behavior
############################

# Party names in LTW studies ---------------------------------------------------
ppgs <- list(
  afd = c("afd"),
  cdu = c("cdu"),
  csu = c("csu"),
  fdp = c("fdp", "f.d.p.", "fdp/dvp"),
  fwg = c("freie wähler", "fwg"),
  gru = c("grÜnen", "grÜne", "gal","buendnis 90-gruene", "b.90-gruene", "grã¼ne", "grã¼nen", "grÃ¼nen","gruene", "gruenen", "grüne", "grünen", "b90 gr", "b90", "die gruenen", "gruene"),
  pds = c("linke pds", "linke", "pds"),
  pro = c("schill-partei"),
  rep = c("republ.", "republikaner", "reps"),
  npd = c("npd"),
  spd = c("spd"),
  was = c("wasg")
)

ppgs_tb <- ppgs %>% map(., ~str_c(., collapse = "|")) %>% 
  as_tibble() %>% gather(key, value)

ppgs_regex <- str_c(ppgs_tb$value, collapse = "|")

party_dummy<-c("CDU/CSU",
               "SPD",
               "Grüne",
               "FDP",
               "PDS",
               "AFD")

# State names in LTW studies ---------------------------------------------------
land <- list(
  bb = c("brandenburg"),
  be = c("berlin"),
  bw = c("baden-württemberg","baden-wuerttemberg", "bw"),
  by = c("bayern", "bay."),
  hb = c("bremen"),
  hh = c("hamburg", "hh"),
  he = c("hessen"),
  mv = c("mecklenburg-vorpommern", "meck-vorp", "m.-v.", "mecklenburg-vorp."),
  ni = c("niedersachsen", "n-s", "niedersachen"),
  nw = c("nordrhein-westfalen", "nrw"),
  rp = c("rheinland-pfalz"),
  sh = c("schleswig-holstein", "sh", "schleswig-h."),
  sl = c("saarland", "saar","saarl"),
  sn = c("sachsen"),
  st = c("sachsen-anhalt", "sachs.-a", "sachs-anh.", "s.-a.", "sachs.-a."),
  th = c("thüringen", "thueringen")
)



land_tb <- land %>% map(., ~str_c(., collapse = "|")) %>% 
  as_tibble() %>% gather(key, value)

land_regex <- str_c(land_tb$value, collapse = "|")

# Names and codes for figures --------------------------------------------------
land_names <- c("bb" = "Brandenburg", "be" = "Berlin", "bw" = "Baden-Württemberg", "by" = "Bayern",
                "de" = "Deutschland", "hb" = "Bremen", "he" = "Hessen", "hh" = "Hamburg",
                "mv" = "Mecklenburg-Vorpommern", "ni" = "Niedersachsen", "nw" = "Nordrhein-Westfalen",
                "rp" = "Rheinland-Pfalz", "sh" = "Schleswig-Holstein", "sl" = "Saarland",
                "st" = "Sachsen-Anhalt", "th" = "Thüringen")

land_big <- c("Brandenburg", "Berlin", "Baden-Württemberg", "Bayern",
              "Bremen", "Hessen", "Hamburg",
              "Mecklenburg-Vorpommern",  "Niedersachsen","Nordrhein-Westfalen",
              "Rheinland-Pfalz", "Schleswig-Holstein", "Saarland", "Sachsen",
              "Sachsen-Anhalt",  "Thüringen")

land_title <- c("Brandenburg", "Berlin", "Baden-Württemberg", "Bavaria",
              "Bremen", "State of Hesse", "Hamburg",
              "Mecklenburg-Vorpommern",  "Lower Saxony","North Rhine-Westphalia",
              "Rhineland Palatinate", "Schleswig-Holstein", "Saarland", "Saxony",
              "Saxony-Anhalt",  "Thuringia")

land_small <- c("bb", "be", "bw", "by", "hb", "he", "hh",
                "mv", "ni", "nw", "rp", "sh", "sl", "sn",
                "st", "th")


parties <- c("cdu", "csu", "spd", "fdp", "gru", "pds",  "afd", "npd", "fwg")
ppgs_fct <- factor(parties, levels = parties)
party_names <- c("afd" = "AfD", "cdu" = "CDU", "csu" = "CSU", "fdp" = "FDP", "gru" = "Grüne", 
                 "koa" = "Koalitionsvertrag", "pds" = "Linke", "spd" = "SPD", "npd" = "NPD",
                 "fwg" = "Freie Wähler")
party_colors <- c("cdu" = "black", "csu" = "black", "spd" = "#E3000F", "fdp" = "#FFD600", 
                  "gru" = "#32CD32", "koa" = "grey", "pds" = "deeppink", "afd" = "blue", 
                  "npd" = "brown", "rep" = "brown", "fwg" = "lightblue", "pir" = "grey",
                  "b90" = "green", "dvu" = "brown", "afb" = "lightblue", "biw" = "lightblue", 
                  "sta" = "lightblue", "ssw" = "lightgreen")
