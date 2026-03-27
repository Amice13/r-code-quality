

# 1 packages -----------------------------------------------------------------------

library(tidyverse)
library(tm)
library(tidytext)
library(R.utils)
library(quanteda)
library(zoo)
library(lubridate)
library(tidytext)


# 2 read data -----------------------------------------------------------------------


# all strike releases
allstrikes2 <- read.csv("~/Desktop/20250328 Replication Files & Appendix/Data/data_all_text.csv")


# 3 dates -----------------------------------------------------------------------



reports <- allstrikes2
reports$date <- mdy(reports$date)
reports$date <- na.locf(reports$date,na.rm=F)

# 4 observations -----------------------------------------------------------------------


# just dashes

df <- reports %>%
  filter(str_detect(reports, "^-"))
df <- tibble::rowid_to_column(df, "doc_id")
df <- df %>%
  rename(text = reports)


# 5 phrases -----------------------------------------------------------------------



# find phrases in press releases and join the words together
my_replacements <- c("vbeid"="vbied","unman aerial vehicl"="uav","tactic unit" = "tacticalunit", "product facil" = "productionfacility", "storag facil"="storagefacility", 
                     "mortar posit"="mortarposition", "command control center"="commandandcontrolcenter","control command center"="commandandcontrolcenter",
                     "stage area"="stagingarea","fight posit"="fightingposition",
                     "vbi facil"="vbiedfacility","vbi factori"="vbiedfactory","machin gun"="machinegun",
                     "earth mover"="earthmover", "vehicl born ie"="vbied", "vehicleborn improvis explos devic"="vbied", "
                     vehicl born improvis devic"="vbied","vehicl born improvis explos devic"="vbied",
                     "artilleri piec"="artillerypiece","oil tanker truck"="oiltankertruck","oil tanker"="oiltanker","oil well head"="oilwellhead",
                     "well head"="wellhead","rocket-propelled grenadier"="rpg","pump jack"="pumpjack","oil refin"="oilrefin","weapon cach"="weaponscache","fire posit"="firingposition",
                     "rocket rail"="rocketrail","tunnel entranc"="tunnelentrance","armor vehicl"="armoredvehicle","armor personnel carrier"="armoredpersonnelcarrier",
                     "homemad explos"="homemadeexplosive", "frontend loader"="frontendloader","front end loader"="frontendloader",
                     "oil barrel"="oilbarrel","crude oil collect point"="crudeoilcollectionpoint",
                     "suppli rout"="supplyroute","suppli cach"="supplycache","mortar team"="mortarteam","mortar tube"="mortartube",
                     "storag area"="storagearea","assembl area"="assemblyarea","communic tower"="communicationstower",
                     "observ post"="observationpost","road block"="roadblock","bed locat"="beddownlocation","storag contain"="storagecontainer",
                     "fuel tank"="fueltank", "command control node" = "commandandcontrolnode", "supplymcach"="supplycache", "supplyrout"="supplyroute","vie"="vbied",
                     "vehiclesand"="vehicl and","vbiedfacilityit"="vbiedfacility","unitnorthwest"="unit northwest","twoisi"="two isi","twocommandandcontrolcenter"="two commandandcontrolcenter",
                     "tree"="three","oil transfer compressor"="oiltransfercompressor","high valu daesh leader"="highvaluedaeshleader","tractor trailor"="tractortrailer","sixmedium"="six medium",
                     "gas oil separ plant"="gasoilseparationplant","cave entrance"="caveentrance",
                     "financi node"="financialnode", "financi facil"="financialfacility", "financi headquart"="financialheadquarters",
                     "fuel suppli truck"="fuelsupplytruck", "vbi suppli sit"="vbiedsupplysite", "fuel suppli point"="fuelsupplypoint", "suppli road"="supplyroad", "suppli depot"="supplydepot", "suppli boat"="supplyboat",
                     "oil process equip item"="oilprocessequipmentitem","rocket propel grenad"="rpg","oil equip item"="oilequipmentitem","construct item"="constructionitem",
                     "communic infrastructur item"="communicationinfrastructureitem","ship contain"="shippingcontainer","hous born ie"="housebornied",
                     "safe hous"="safehouse","sniper team"="sniperteam","rocket fire team"="rocketfireteam","machinegun team"="machinegunteam",
                     "land featur"="landfeature","terrain featur"="terrainfeature","logist hub"="logisticshub","ie factori"="iedfactory","weapon factori"="weaponfactory",
                     "vbied factori"="vbiedfactory","homemadeexplosive factori"="iedfactory","improvis explos devic factori"="iedfactory",
                     "dump truck"="dumptruck","tanker truck"="tankertruck","petroleum oil lubric truck"="petroleumoilandlubricanttruck",
                     "semi truck"="semitruck","anti air system"="antiairsystem","anti aircraft artilleri system"="antiairsystem","anti air artilleri system"="antiairsystem",
                     "anti aircraft gun"="antiaircraftgun","water craft"="watercraft","water truck"="watertruck","water control system"="watercontrolsystem","water control network"="watercontrolnetwork",
                     "home made explos"="homemadeexplosive","fuel station"="fuelstation","recruit station"="recruitingstation","uav control station"="uavcontrolstation","communic station"="communicationstation",
                     "repeat tower power station"="repeattowerpowerstation","power station"="powerstation","weigh station"="weighstation","pump station"="pumpstation","technic vehicl"="technicalvehicle",
                     "observ tower"="observationtower","observ point"="observationpoint","bunker complex"="bunkercomplex","cave complex"="cavecomplex","check point"="checkpoint","tent shelter"="tentshelter",
                     "media cell"="mediacell","fourmedium"="four medium","sniper posit"="sniperposition", "figh posit"="fightingposition","mortarteam posit"="mortarteamposition",
                     "rocket posit"="rocketposition","machinegun posit"="machinegunposition","stage posit"="stagingposition","heavi weapon posit"="heavyweaponposition",
                     "mortar launch posit"="mortarlaunchposition","rpg posit"="rpgposition","artilleri posit"="artilleryposition","artilleri system posit"="artillerysystemposition",
                     "defens posit"="defensiveposition","grenad launcher posit"="grenadelauncherposition","artillerypiece posit"="artillerypieceposition",
                     "anti tank posit"="antitankposition","ie posit"="iedposition","improvis explos devic facil"="iedfacility","weapon manufactur facil"="weaponsmanufacturingfacility",
                     "homemadeexplosive manufactur facil"="homemadeexplosivemanufacturingfacility","vbi manufactur facil"="vbimanufacturingfacility","ie manufactur facil"="iedmanufacturingfacility","manufactur facil"="manufacturingfacility",
                     "petroleum oil lubric facil"="petroleumoillubricantfacility","weapon facil"="weaponfacility","ie facil"="iedfacility","vbied facil"="vbiedfacility","ua facil"="uafacility","logist facil"="logisticsfacility",
                     "command control facil"="commandandcontrolfacility","underground facil"="undergroundfacility","support facil"="supportfacility","oil petroleum lubric facil"="petroleumoillubricantfacility","stage facil"="stagingfacility",
                     "uav facil"="uavfacility","mainten facil"="maintenancefacility","explos facil"="explosivesfacility","communic facil"="communicationsfacility",
                     "financi exchang facil"="financialexchangefacility","uav launch facil"="uavlaunchfacility","oil facil"="oilfacility","fuel facil"="fuelfacility",
                     "unman aerial system facil"="uafacility","mechan facil"="mechanicalfacility","meet facil"="meetingfacility","mortar facil"="mortarfacility","media facil"="mediafacility",
                     "vehicl repair facil"="vehiclerepairfacility","vbi manufactur armor facil"="vbimanufacturingarmoringfacility","uav construct facil"="uavconstructionfacility",
                     "homemadeexplosive facil"="homemadeexplosivefacility","hme facil"="homemadeexplosivefacility","oil collect facil"="oilcollectionfacility","train facil"="trainingfacility","guard facil"="guardfacility",
                     "administr facil"="administrativefacility","recruit facil"="recruitmentfacility","ammunit facil"="ammunitionfacility","anti aircraft"="antiair","anti air"="antiair","anti tank"="antitank",
                     "mortar launch site"="mortarlaunchsite","ua launch site"="ualaunchsite","cash distribut site"="cashdistributsite","petroleum oil lubric site"="petroleumoillubricantsite",
                     "uav site"="uavsite","uav launch site"="uavlaunchsite","media site"="mediasite","communic site"="communicationsite","propaganda site"="propagandasite","artilleri site"="artillerysite",
                     "rocket storag site"="rocketstoragesite","weapon storag site"="weaponstoragesite","uav storag site"="uavstoragesite","fuel storag site"="fuelstoragesite",
                     "chemic storag site"="chemicalstoragesite","storag site"="storagesite","fuel site"="fuelsite","command control site"="commandandcontrolsite","rocket site"="rocketsite","ua site"="uasite",
                     "mortar site"="mortarsite","homemadeexplosive product site"="homemadeexplosiveproductionsite","camp site"="campsite","launch site"="launchsite","train site"="trainingsite",
                     "logist site"="logisticssite","indirect fire site"="indirectfiresite","assemblyarea site"="assemblyareasite","meet site"="meetingsite","stage site"="stagingsite",
                     "line communic"="lineofcommunication","communic line"="communicationline","trench line"="trenchline","logist center"="logisticscenter","media center"="mediacenter",
                     "train center"="trainingcenter","oper center"="operationscenter","financi storag center"="financialstoragecenter","financ storag center"="financialstoragecenter","cash distribut center"="cashdistributioncenter",
                     "financi center"="financialcenter","weapon storag center"="weaponstoragecenter","financ distribut center"="financialdistributioncenter","ammunit storag center"="ammunitionstoragecenter",
                     "financi distribut center"="financialdistributioncenter","financ center"="financialcenter","communic control center"="communicationscontrolcenter","headquart center"="headquarterscenter",
                     "oil collect point"="oilcollectionpoint","fuel point"="fuelpoint","cash collect point"="cashcollectionpoint","choke point"="chokepoint","cross point"="crosspoint","stage point"="stagingpoint",
                     "gas oil separ point"="gasoilseparationpoint","mortar fire point"="mortarfiringpoint","mortar suppli point"="mortarsupplypoint","suppli point"="supplypoint","resuppli point"="resupplypoint",
                     "mortar system suppli point"="mortarsystemsupplypoint","explos factori"="explosivefactory","chemic factori"="chemicalfactory","manufactur factori"="manufacturingfactory","ammunit factori"="ammunitionfactory",
                     "weapon product factori"="weaponproductionfactory","ua factori"="uafactory","mortar factori"="mortarfactory","munit product factori"="munitionproductionfactory","homemadeexplosive factori"="homemadeexplosivefactory",
                     "uav factori"="uavfactory","mortar booster factori"="mortarboosterfactory","armor factori"="armoringfactory","train area"="trainingarea","oper area"="operatingarea",
                     "resuppli area"="resupplyarea","cave entranc"="caveentrance","bunker entranc"="bunkerentrance","culvert entranc"="culvertentrance","inoper coalit equip"="inoperablecoalitionequipment","inoper equip"="inoperablecoalitionequipment",
                     "inoper piec partner nation equip"="inoperablecoalitionequipment","rocket team"="rocketteam","rpg team"="rpgteam","artilleri team"="artilleryteam","oil product equip"="oilproductionequipment",
                     "oil product machin"="oilproductionmachine","cave network"="cavenetwork","tunnel network"="tunnelnetwork","suppli truck"="supplytruck","suppli pile"="supplypile","suppli trailer"="supplytrailer",
                     "communic infrastructur"="communicationsinfrastructure","oil produc collect storag transport infrastructur"="oilproductioncollectionstoragetransportinfrastructure",
                     "oil produc process transport infrastructur"="oilproductionprocesstransportinfrastructure","logist complex"="logisticscomplex","weaponfactory complex"="weaponfactorycomplex",
                     "trench complex"="trenchcomplex","checkpoint complex"="checkpointcomplex","fight complex"="fightingcomplex","tunnel complex"="tunnelcomplex","cargo contain"="cargocontainer",
                     "oil field wellhead"="oilfieldwellhead","mine field section"="minefieldsection","oil field"="oilfield","river obstruct"="riverobstruction","fuel depot"="fueldepot",
                     "vehicl depot"="vehicldepot","river cross"="rivercross","river crosspoint"="rivercross","logist rout"="logisticsroute","mainten storag yard"="maintenanceandstorageyard",
                     "repeat station"="repeaterstation","petroleum station"="petroleumstation","fuel servic station"="fuelservicestation","cement mixer"="cementmixer",
                     "cement plant"="cementplant","oil process item"="oilprocessingitem","machineri piec"="machinerypiece","logist offic"="logisticsoffice","storag cylind"="storagecylinder",
                     "earth move"="earthmover","electron warfar garrison"="electronicwarfaregarrison","improvis ferri system"="improvisedferrysystem","ground unit"="groundunit",
                     "harden aircraft shelter"="hardenedaircraftshelter","oil hold tank"="oilholdingtank","jam system"="jammingsystem","research lab"="researchlab",
                     "mmedium machinegun"="mediummachinegun","artilleri mover"="artillerymover","prison camp"="prisoncamp","command control network"="commandcontrolnetwork",
                     "communic network"="communicationnetwork","associ network veteran al qaeda oper"="khorasan","trench network"="trenchnetwork","fuel truck"="fueltruck",
                     "pickup truck"="pickuptruck","oil truck"="oiltruck","surfac air missil truck"="surfaceairmissiletruck","tractor trailer truck"="tractortrailertruck",
                     "pipelin fitter truck"="pipelinefittertruck","pump truck"="pumptruck","armor truck"="armoredtruck","arm truck"="armedtruck","ammunit truck"="ammunitiontruck",
                     "oilrefineri equip item"="oilrefineryequipmentitem","logist node"="logisticsnode","commandand control node"="commandandcontrolnode","iedfacilityit node"="iedfacilitationnode",
                     "rpgi"="rpg","oil booster pump generat"="oilboosterpumpgenerator","maneuv element"="maneuverelement","resuppli posit"="resupplyposition",
                     "road cross"="roadcross","petroleum oil lubric storagefacility"="petroleumoillubricantstoragefacility","financi storagefacility"="financialstoragefacility",
                     "oil storag barrel"="oilstoragebarrel","oil storag tank"="oilstoragetank","fuel storag tank"="fuelstoragetank","fuel storagearea"="fuelstoragearea",
                     "fuel storagefacility"="fuelstoragefacility","fuel storag trailer"="fuelstoragetrailer","fuel storag locat"="fuelstoragelocation",
                     "petroleum storag tank"="petroleumstoragetank","weapon storagefacility"="weaponstoragefacility","weapon storagearea"="weaponstoragearea",
                     "weapon storag cach"="weaponstoragecache","weapon storag"="weaponstorage","vbied storagefacility"="vbiedstoragefacility","vbied storagearea"="vbiedstoragearea",
                     "ie storagefacility"="iedstoragefacility","ie storagearea"="iedstoragearea","ammunit storagefacility"="ammunitionstoragefacility","munit storag"="munititionstorag",
                     "vehicl storagefacility"="vehiclestoragefacility","vehicl storagearea"="vehiclestoragearea","vehicl storag"="vehiclestorage","artilleri storagefacility"="artillerystoragefacility",
                     "underground storagefacility"="undergroundstoragefacility","munit product storagefacility"="munitionproductionstoragefacility","vbi product storagefacility"="vbiedproductionstoragefacility",
                     "ie product storagefacility"="iedproductionstoragefacility","weapon manufactur storagefacility"="weaponmanufacturingstoragefacility",
                     "bunker storagefacility"="bunkererandstoragefacility","rocket storagefacility"="rocketstoragefacility","explos storagefacility"="explosivestoragefacility",
                     "vbi storagefacility"="vbiedstoragefacility","uav storagefacility"="uavstoragefacility","vbi storagearea"="vbiedstoragearea","chemic storagearea"="chemicalstoragearea",
                     "daesh facil"="daeshfacility","isi facil"="isisfacility","isil facil"="isilfacility","tactic vehicl"="tacticalvehicle","vehicl born improvis explos"="vbied",
                     "vehicl born improvis explosivedevic"="vbied","vehicl born i"="vbied","vehicl born improvis devic"="vbied","vehicl born explos devic"="vbied","isistruck"="isi truck",
                     "isisvehicl"="isi vehicl"
)


# 6 remove -----------------------------------------------------------------------



numbers <- c("near","two","one","three","four","five","six","seven","eight","nine","ten","eleven","third","1st","thirteen",
             "3rd","2nd","5th","10m","29th","four","one","six","twelv")

actionverbs <- c("destroy","strike","struck","airstrik","engag","suppress","damag","deni","hit","conduct","crater","disabl","degrad","neutral","stuck","airstik","breach","immobil","target","halt","help",
                 "illumin","disrupt","harass","anddestroy","mobil","suppressedthre","led","tran","took","place","destoy","facilit")

other <- c("separ","larg","access","isi","isilus","locat","wound","coalit","forc","result","inconclus","attack","use","advantag","produc","terrorist","fire","pilot","multipl","anoth","local","ministri",
           "time","approv","continu","may","support","april","compound","oper","safe","second","smaller","march","twall","abandon","also","approach","cjtfoir","report","sever","addit","along","base",
           "civilian","day","differ","fighterattack","friend","includ","juli","kurdish","mar","munit","territori","assess","assist","buri","captur","casualti","counter","current","deton","divis","ear",
           "effect","govern","incid","indirect","ineffect","infantri","initi","injur","investig","key","move","nation","nov","number","occur","offens","order","other","partner","potenti","prevent",
           "previous","request","seper","simultan","surround","undetermin","unreport","aug","sept","june","oct","jan","apr","dec","feb",
           "consist","militari","remot","stage","land","anisi","ain","bomb","ionstwo","23vehicl","aaa","alba‚Äôaj","and","andon","ba‚Äôaj","battl","centeron","eri","estim",
           "Äô","ionsand","isil‚Äìus","junction","loc","mission","nod","onecompound","onisi","raid","sha‚Äôfah","success","tow","twovehicl","way",
           "south","west","southeast","northeast","northwest","east","southwest","plus","portion","relat","repres","stray","syrian","today","vent","whose","wre",
           "signific","oir","cjtf","m","final","group","indic","us","rd","t","destroyedfour","associ","call","nd","sometim","th","veteran","alli","ant","arab","c",
           "creat","extern","least","malfunct","part","plot","qaeda","st","state","abil","airdrop","arabi","black","combat","deliv","design","due","earlier","enabl","enemi","failur","fall","forces","hand","heavili",
           "intend","interdict","involv","km","load","man","market","member","non","pro","water","main","earthen","serv","movement","denial")

location <- c("sinjar","ramadi","tal","afar","fallujah","bayji","kisik","zawr","dayr","mar'","manbij","qayyarah","hasakah","abdallah","sultan","baghdadi","kobani",
              "abu","kamal","huwayjah","ayn","isa","haditha","habbaniyah","kirkuk","qaim","rawah","hawl","makhmur","albu","hayat","asad","shadaddi","palmyra","rutbah","aleppo","bashir",
              "iraq","syria","tuz","abyad","washiyah","bab","iraqi","wale","anbar","provinc","taji","idlib","tanf","tikrit","baghdad","dayz","tamakh","zayr","dawr","samarra","erbil",
              "shaddadi","avenu","habbaniya","hawayjah","shadadi","washiya","awr","hami","huwayja","irbil","jibbin","mahkmur","abayad","bah","baqubah","barghooth","bukam","harri","hawijah",
              "huwijah","iblib","kamak","palymyra","qayayyarah","rut","tadmur","tallafar","vnear","walwe",
              "tabqah","hajin","khanukah","abdullah","hamrin","jalawla‚Äô","qara","rawa","tapa","taqbah",
              "wadi","ashai","basheer","alqaim","bek","khorasan","makmur","mayadin","sulayman","alasad","alhasakah","alrutbah",
              "jazeerah","rabiyah","shaddai","tabqa","taqba","zumar","abukam","alanbar","alba‚Äôaj","alhaskah","alkisik",
              "atshana","pes","pocket","qadisiyyah","qaida","qayyrarah","qurayat","rahwah","rga","sha‚Äôfah","shadaddai",
              "sharqat","sharra","talab","talafar","thar","tirkrit","tubal","wadiashai","zagatoon","zwar",
              "al","az","ar","raqqa","huwajah","hwayjah","peshmerga","sha‚Äôfah","soor","tel","mountain","north","lake","vicin","desert","ash","sha’fah","ninewah","nera",
              "khusham","jazeera","jalula","jalawla","haskah","haram","hajjaj","dulab","diyala","dhiban","dashisha","baji","baaj","aynzalah","atashanah","anah","aissa",
              "ba’aj","mosul","raqqah","border","hayi","q","area","tall"
)


unimportantterms <- c(numbers,location,actionverbs,other)


# 7 dash replace -----------------------------------------------------------------------



dashreplace <- c("-" = " ")

dfphrase <- df
dfphrase$text <- dfphrase$text %>%
  str_replace_all(dashreplace)


# 8 corpus -----------------------------------------------------------------------



# convert dataframe to corpus (with phrases of PR stated targets)
all_corpora <- VCorpus(DataframeSource(dfphrase))
allphrase_corpus <- tm_map(all_corpora, content_transformer(tolower))
allphrase_corpus <- tm_map(allphrase_corpus, content_transformer(removeWords), tm::stopwords('en'))
allphrase_corpus <- tm_map(allphrase_corpus, content_transformer(removePunctuation))
allphrase_corpus <- tm_map(allphrase_corpus, content_transformer(removeNumbers))
allphrase_corpus <- tm_map(allphrase_corpus, stemDocument)

# 9 categories -----------------------------------------------------------------------



business_finance <- c("cash","financ","distribut","financialnode","financialfacility","financi","financialheadquarters","financialstoragecenter",
                      "financialcenter","financialdistributioncenter","financialexchangefacility",
                      "cashdistributsite","cashcollectionpoint","cashdistributioncenter","financialstoragefacility")
construction_equipment <- c("tractortrailer","excav","frontendloader","bulldoz","crane","engin","dump","tractor","construct","dozer","earthmover",
                            "bull","grader","asphalt","endload","front","steamrol","backho","bobcat","forklift","frontend","loader","roadrol","roller","steam",
                            "end","constructionitem","dumptruck","cementmixer","cementplant","pickuptruck")
electrical_infrastructure <- c("panel","generat","power","solar")
media_telecom <- c("café","box","repeat","media","antenna","array","internet","communic","relay","tower","radio","communicationstower","repeattowerpowerstation",
                   "communicationstation","communicationsinfrastructur","communicationinfrastructureitem","mediafacility","mediasite","communicationsite","mediacenter",
                   "communicationscontrolcenter","communicationsinfrastructure","communicationnetwork","lineofcommunication","communicationsfacility",
                   "communicationline","billboard","propagandasite","repeaterstation","mediacell","propaganda")
personnel_equipment_facilities <- c("tactic","vbiedfactory","commandandcontrolnode","ie","tacticalunit","fightingposition","machinegun","system","mortarteam",
                                    "vbi","heavi","weaponscache","stagingarea","mortarposition","bunker","tunnel","rocket","commandandcontrolcenter","node",
                                    "weapon","headquart","vbied","rocketrail","artilleri","artillerypiece","tank","firingposition","fighter","light",
                                    "tunnelentrance","improvis","antiair","explos","sniper","rifl","recoilless","equip","grenad","trench","motorcycl",
                                    "antiaircraft","tube","armoredpersonnelcarrier","logist","propel","train","born","homemadeexplosive","positon","uav",
                                    "fight","rocketpropel","ammunit","camp","launcher","bomber","piec","homemad","vehicleborn","control","manufactur",
                                    "medium","productionfacil","secur","hbi","hme","aerial","chemic","launch","rpg","unman","cluster","decoy","tactial",
                                    "foreign","air","antitank","barrack","command","gun","machin","personnel","antaircraft","carrier","drone","encamp",
                                    "garrison","houseborn","meet","postion","shack","beddownlocation","grenadi","grenand","guid","hmmwv","isilvbi","macin","materi",
                                    "mine","minefield","missil","motor","outpost","rail","stockpil","stryker","suicid","surfac","surfaceair","underground",
                                    "valv","vbeid","devic","defens","armor","small","structur","belt","unit","ammo","crew","hazard","batteri","ingmposit",
                                    "ivedevic","leader","vbiedfacility","mortartube","observationpost","warehous","guard","platform","radar","regim","revet",
                                    "safehous","vehicle","vest","visual","warfar","mortar","assemblyarea","armoredvehicle","propelledgrenad","i",
                                    "cach","fightingpositionon","ua","checkpoint","roadblock","productionfacility","obstacl","post","humve","shelter","fightingposit",
                                    "emplac","firingpositionon","hq","uavcontrolstation","vbiedsupplysitee","onefightingposition","observationtower",
                                    "mortarpositionon","machinegundamag","isilvbiedfactory","ied","hmmwvs","highvaluedaeshleader","fightingmposit","fightingpositionionsand",
                                    "explosivedevic","commandandcontrolcenteron","commandand","recruitingstation","mortarsystem","fightingpositionionstwo",
                                    "antiaircraftgun","housebornied","machinegunteam","observationpoint","sniperteam","weaponfactory","iedfactory","antiairsystem","daeshfightingposition",
                                    "rocketfireteam","sentri","arm","technicalvehicle","bunkercomplex","bed","fortif","isistact","rocketfireteam","sentri",
                                    "arocket","beddown","controlnod","figh","fiveexplos","hostil","improv","recruit","sniperposition","mortarteamposition",
                                    "rocketposition","machinegunposition","stagingposition","heavyweaponposition","mortarlaunchposition","rpgposition","artilleryposition",
                                    "artillerysystemposition","defensiveposition","grenadelauncherposition","artillerypieceposition","antitankposition","iedposition",
                                    "iedfacility","weaponsmanufacturingfacility","homemadeexplosivemanufacturingfacility","vbimanufacturingfacility","iedmanufacturingfacility",
                                    "weaponfacility","iedfacility","vbiedfacility","uafacility","commandandcontrolfacility","supportfacility","uavfacility","maintenancefacility",
                                    "explosivesfacility","uavlaunchfacility","mortarfacility","vbimanufacturingarmoringfacility","uavconstructionfacility","homemadeexplosivefacility",
                                    "trainingfacility","guardfacility","recruitmentfacility","ammunitionfacility","antiair","mortarlaunchsite","ualaunchsite","uavsite","uavlaunchsite",
                                    "artillerysite","rocketstoragesite","weaponstoragesite","uavstoragesite","commandandcontrolsite","rocketsite","uasite","mortarsite",
                                    "homemadeexplosiveproductionsite","launchsite","trainingsite","indirectfiresite","assemblyareasite","stagingsite","trenchline","trainingcenter",
                                    "weaponstoragecenter","ammunitionstoragecenter","stagingpoint","mortarfiringpoint","mortarsupplypoint","mortarsystemsupplypoint","explosivefactory",
                                    "ammunitionfactory","weaponproductionfactory","uafactory","mortarfactory","munitionproductionfactory","homemadeexplosivefactory",
                                    "uavfactory","armoringfactory","trainingarea","operatingarea","rocketteam","rpgteam","artilleryteam","weaponfactorycomplex",
                                    "fightingcomplex","maneuverelement","observ","camera","canton","carri","convoy","downloc","electronicwarfaregarrison","entrench",
                                    "groundunit","intellig","jammingsystem","mediummachinegun","artillerymover","surfaceairmissiletruck","armedtruck","ammunitiontruck",
                                    "stagingfacility","iedfacilitationnode","sniperpositionon","mortarboosterfactory","minefieldsection","armoredtruck","operationscenter",
                                    "commandcontrolnetwork","tent","undergroundfacility","anti","bunkerentrance","meetingfacility","campsite","checkpointcomplex",
                                    "headquarterscenter","meetingsite","offic","resupplyarea","resupplyposition","resupplytrailer","site","berm",
                                    "logisticsnode","logisticshub","logisticsfacility","inoperablecoalitionequipment","logisticscenter","wall","obstruct","barrier",
                                    "logisticssite","weighstation","campsit","cavecomplex","cavenetwork","chokepoint","cover","garag","hangar","hardenedaircraftshelter",
                                    "logisticscomplex","logisticsoffice","riverobstruction","tentshelter","trenchcomplex","trenchnetwork","tunnelcomplex","tunnelnetwork",
                                    "posit","weaponstoragefacility","weaponstoragearea","weaponstoragecache","weaponstorage","vbiedstoragefacility","vbiedstoragearea",
                                    "iedstoragefacility","iedstoragearea","ammunitionstoragefacility","munititionstorag","vehiclestoragefacility","vehiclestoragearea",
                                    "vehiclestorage","artillerystoragefacility","undergroundstoragefacility","munitionproductionstoragefacility","vbiedproductionstoragefacility",
                                    "iedproductionstoragefacility","weaponmanufacturingstoragefacility","bunkererandstoragefacility","rocketstoragefacility","explosivestoragefacility",
                                    "supplycache","resuppli","supplydepot","resupplycache","supplypile","administrativefacility","maintenanceandstorageyard",
                                    "ammunititionstoragearea","avehiclestoragearea","uavstoragefacility","daeshfacility","isisfacility","isilfacility","tacticalvehicle",
                                    "resupplytruck","supplytruck","resupplyboat","supplyboat","vbiedmprovis","tacticalvehicleestoragefacility","vbiedivedevic")

oil_infrastructure <- c("pol","trunk","fueltanker","oil","oilwellhead","oiltanker","oiltankertruck","pumpjack","wellhead","tanker","crudeoilcollectionpoint",
                        "fuel","still","oilrefin","rig","trailer","gas","well","petroleum","workov","lubric","manifold","pump","oilrefineri","drill","inlet",
                        "oilbarrel","crude","collect","refineri","pipelin","barrel","refin","asset","compressor","derrick","drum","fitter","heater","mount",
                        "natur","oilpumpjack","oiltankertruckv","pool","servic","transfer","transport","vessel","work","head","process","distil","modular",
                        "pipe","ementstil","skid","stil","vertic","fueltank","refuelstation","oiltransfercompressor","oilstil","oilrefinementstil",
                        "fueltankertruck","fuelsupplytruck","fuelsupplypoint","pumpstation","fuelstation","petroleumoilandlubricanttruck","oilequipmentitem",
                        "oilprocessequipmentitem","tankertruck","gasoilseparationplant","arefineri","refuel","petroleumoillubricantfacility","oilfacility",
                        "fuelfacility","oilcollectionfacility","petroleumoillubricantsite","fuelstoragesite","fuelsite","oilcollectionpoint",
                        "fuelpoint","gasoilseparationpoint","oilproductionequipment","oilproductionmachine","oilproductioncollectionstoragetransportinfrastructure",
                        "oilproductionprocesstransportinfrastructure","oilfieldwellhead","oilfield","fueldepot","plant","petroleumstation","fuelservicestation",
                        "oilprocessingitem","oilholdingtank","oiltruck","fueltruck","pipelinefittertruck","oilrefineryequipmentitem","oilboosterpumpgenerator",
                        "pumptruck","refuelpoint","reservoir","oilstoragebarrel","oilstoragetank","fuelstoragetank","fuelstoragearea","fuelstoragefacility",
                        "fuelstoragetrailer","fuelstoragelocation","petroleumstoragetank","petroleumoillubricantstoragefacility","oilstoragetanker")
transportation_infrastructure <- c("highway","foot","canal","section","bridg","road","aircraft","pontoon","usedbridg","airfield","supplyroute","railway",
                                   "supplyroad","supplyroutee","resupplypoint","rivercross","rivercrosspoint","logisticsroute","dock","improvisedferrysystem",
                                   "crosspoint","roadcross")
water_infrastructure <- c("culvert","cross","dam","minor","watercontrolnetwork","watercontrolsystem","culvertentrance")
manufacturing_production_industrial_infrastructure <- c("mechanicalfacility","researchlab","manufacturingfacility","product",
                                                        "chemicalfactory","machineri","machinerypiece","manufacturingfactory",
                                                        "chemicalstoragesite","research","chemicalstoragearea","storagecontainer",
                                                        "connex","shed","storagefacility","yard","silo",
                                                        "shippingcontainer","storagearea","cargocontainer","contain","storagecylinder",
                                                        "storagesite","depot","suppli","supplypoint","supplytrailer","bundl","storag")

no_category <- c("held","daesh","isil","occupi","isis","iss","isl","network")

vehicles_not_military_oil <- c("watercraft","pickup","sedan","tech","van","suv","vehicl","semitruck","avehicl","onevehicl",
                               "semi","watertruck","truck","tractortrailertruck","vehicldepot",
                               "barg","boat")
residential_buildings_other_critical_infrastructure <- c("carport","build","hous","safehouse","buildingtwo","buildingv","heldbuild","facil","factori","prisoncamp","vehiclerepairfacility")
terrain <- c("terrain","landfeature","terrainfeature","cave","caveentrance")


# 10 tidy ----------------------------------------------------------------------


tidycorp <- tidy(allphrase_corpus,collapse="\n")

tidycorp$datetimestamp <- dfphrase$date[match(tidycorp$id, dfphrase$doc_id)]

data_stemmed_text <- tidycorp
write.csv(data_stemmed_text,"~/Desktop/20250328 Replication Files & Appendix/data_stemmed_text.csv")
# Creates an output with the stemmed text


# 11 phrases ----------------------------------------------------------------------



# Phrases

tidycorp$text <- tidycorp$text %>%
  str_replace_all(my_replacements)


# 12 unique ----------------------------------------------------------------------



alltokens_prephrase <- tidycorp %>%
  unnest_tokens(word,text) %>%
  distinct()


# 13 remove ----------------------------------------------------------------------



alltokens <- alltokens_prephrase

alltokens <- alltokens[!alltokens$word %in% unimportantterms, ]


# 14 categories ----------------------------------------------------------------------



alltokens$word[alltokens$word %in% residential_buildings_other_critical_infrastructure] <- "residential_buildings_other_critical_infrastructure"
alltokens$word[alltokens$word %in% vehicles_not_military_oil] <- "vehicles_not_military_oil"
alltokens$word[alltokens$word %in% no_category] <- "no_category"
alltokens$word[alltokens$word %in% terrain] <- "terrain"

alltokens$word[alltokens$word %in% business_finance] <- "business_finance"
alltokens$word[alltokens$word %in% construction_equipment] <- "construction_equipment"
alltokens$word[alltokens$word %in% electrical_infrastructure] <- "electrical_infrastructure"
alltokens$word[alltokens$word %in% media_telecom] <- "media_telecom"
alltokens$word[alltokens$word %in% personnel_equipment_facilities] <- "personnel_equipment_facilities"
alltokens$word[alltokens$word %in% oil_infrastructure] <- "oil_infrastructure"
alltokens$word[alltokens$word %in% transportation_infrastructure] <- "transportation_infrastructure"
alltokens$word[alltokens$word %in% water_infrastructure] <- "water_infrastructure"
alltokens$word[alltokens$word %in% manufacturing_production_industrial_infrastructure] <- "manufacturing_production_industrial_infrastructure"


# 15 unique ----------------------------------------------------------------------



alltokens <- alltokens %>%
  distinct()


# 16 count ----------------------------------------------------------------------



alltokens$id <- as.numeric(alltokens$id)

finalcount <- alltokens %>%
  count(word,sort=T)


# 17 year ----------------------------------------------------------------------



# By Year

df18 <- alltokens %>%
  filter(datetimestamp <= '2018-12-31' & datetimestamp >= '2018-01-01')
count18 <- df18 %>%
  count(word,sort=T) %>%
  rename(docs2018 = n)

df17 <- alltokens %>%
  filter(datetimestamp <= '2017-12-31' & datetimestamp >= '2017-01-01')
count17 <- df17 %>%
  count(word,sort=T) %>%
  rename(docs2017 = n)

df16 <- alltokens %>%
  filter(datetimestamp <= '2016-12-31' & datetimestamp >= '2016-01-01')
count16 <- df16 %>%
  count(word,sort=T) %>%
  rename(docs2016 = n)

df15 <- alltokens %>%
  filter(datetimestamp <= '2015-12-31' & datetimestamp >= '2015-01-01')
count15 <- df15 %>%
  count(word,sort=T) %>%
  rename(docs2015 = n)

df14 <- alltokens %>%
  filter(datetimestamp <= '2014-12-31' & datetimestamp >= '2014-01-01')
count14 <- df14 %>%
  count(word,sort=T) %>%
  rename(docs2014 = n)

counts <- merge(finalcount, count18[, c("word", "docs2018")], by="word", all.x=TRUE)
counts <- merge(counts, count17[, c("word", "docs2017")], by="word", all.x=TRUE)
counts <- merge(counts, count16[, c("word", "docs2016")], by="word", all.x=TRUE)
counts <- merge(counts, count15[, c("word", "docs2015")], by="word", all.x=TRUE)
counts <- merge(counts, count14[, c("word", "docs2014")], by="word", all.x=TRUE)
counts$percent2018 <- as.numeric(counts$docs2018) * 100 / nrow(filter(tidycorp,datetimestamp <= '2019-01-01' & datetimestamp >= '2018-01-01'))
counts$percent2017 <- as.numeric(counts$docs2017) * 100 / nrow(filter(tidycorp,datetimestamp <= '2018-01-01' & datetimestamp >= '2017-01-01'))
counts$percent2016 <- as.numeric(counts$docs2016) * 100 / nrow(filter(tidycorp,datetimestamp <= '2017-01-01' & datetimestamp >= '2016-01-01'))
counts$percent2015 <- as.numeric(counts$docs2015) * 100 / nrow(filter(tidycorp,datetimestamp <= '2016-01-01' & datetimestamp >= '2015-01-01'))
counts$percent2014 <- as.numeric(counts$docs2014) * 100 / nrow(filter(tidycorp,datetimestamp <= '2015-01-01' & datetimestamp >= '2014-01-01'))


# 18 quarter ----------------------------------------------------------------------



# Quarterly

#2018

df18q1 <- alltokens %>%
  filter(datetimestamp <= '2018-03-31' & datetimestamp >= '2018-01-01')
count18q1 <- df18q1 %>%
  count(word,sort=T) %>%
  rename(docs2018q1 = n)
df18q2 <- alltokens %>%
  filter(datetimestamp <= '2018-06-30' & datetimestamp >= '2018-04-01')
count18q2 <- df18q2 %>%
  count(word,sort=T) %>%
  rename(docs2018q2 = n)
df18q3 <- alltokens %>%
  filter(datetimestamp <= '2018-09-30' & datetimestamp >= '2018-07-01')
count18q3 <- df18q3 %>%
  count(word,sort=T) %>%
  rename(docs2018q3 = n)
df18q4 <- alltokens %>%
  filter(datetimestamp <= '2018-12-31' & datetimestamp >= '2018-10-01')
count18q4 <- df18q4 %>%
  count(word,sort=T) %>%
  rename(docs2018q4 = n)

#2017

df17q1 <- alltokens %>%
  filter(datetimestamp <= '2017-03-31' & datetimestamp >= '2017-01-01')
count17q1 <- df17q1 %>%
  count(word,sort=T) %>%
  rename(docs2017q1 = n)
df17q2 <- alltokens %>%
  filter(datetimestamp <= '2017-06-30' & datetimestamp >= '2017-04-01')
count17q2 <- df17q2 %>%
  count(word,sort=T) %>%
  rename(docs2017q2 = n)
df17q3 <- alltokens %>%
  filter(datetimestamp <= '2017-09-30' & datetimestamp >= '2017-07-01')
count17q3 <- df17q3 %>%
  count(word,sort=T) %>%
  rename(docs2017q3 = n)
df17q4 <- alltokens %>%
  filter(datetimestamp <= '2017-12-31' & datetimestamp >= '2017-10-01')
count17q4 <- df17q4 %>%
  count(word,sort=T) %>%
  rename(docs2017q4 = n)

#2016

df16q1 <- alltokens %>%
  filter(datetimestamp <= '2016-03-31' & datetimestamp >= '2016-01-01')
count16q1 <- df16q1 %>%
  count(word,sort=T) %>%
  rename(docs2016q1 = n)
df16q2 <- alltokens %>%
  filter(datetimestamp <= '2016-06-30' & datetimestamp >= '2016-04-01')
count16q2 <- df16q2 %>%
  count(word,sort=T) %>%
  rename(docs2016q2 = n)
df16q3 <- alltokens %>%
  filter(datetimestamp <= '2016-09-30' & datetimestamp >= '2016-07-01')
count16q3 <- df16q3 %>%
  count(word,sort=T) %>%
  rename(docs2016q3 = n)
df16q4 <- alltokens %>%
  filter(datetimestamp <= '2016-12-31' & datetimestamp >= '2016-10-01')
count16q4 <- df16q4 %>%
  count(word,sort=T) %>%
  rename(docs2016q4 = n)

#2015 

df15q1 <- alltokens %>%
  filter(datetimestamp <= '2015-03-31' & datetimestamp >= '2015-01-01')
count15q1 <- df15q1 %>%
  count(word,sort=T) %>%
  rename(docs2015q1 = n)
df15q2 <- alltokens %>%
  filter(datetimestamp <= '2015-06-30' & datetimestamp >= '2015-04-01')
count15q2 <- df15q2 %>%
  count(word,sort=T) %>%
  rename(docs2015q2 = n)
df15q3 <- alltokens %>%
  filter(datetimestamp <= '2015-09-30' & datetimestamp >= '2015-07-01')
count15q3 <- df15q3 %>%
  count(word,sort=T) %>%
  rename(docs2015q3 = n)
df15q4 <- alltokens %>%
  filter(datetimestamp <= '2015-12-31' & datetimestamp >= '2015-10-01')
count15q4 <- df15q4 %>%
  count(word,sort=T) %>%
  rename(docs2015q4 = n)

#2014

df14q4 <- alltokens %>%
  filter(datetimestamp <= '2014-12-31' & datetimestamp >= '2014-10-01')
count14q4 <- df14q4 %>%
  count(word,sort=T) %>%
  rename(docs2014q4 = n)

quarterlycounts <- merge(finalcount, count14q4[, c("word", "docs2014q4")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q1[, c("word", "docs2015q1")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q2[, c("word", "docs2015q2")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q3[, c("word", "docs2015q3")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count15q4[, c("word", "docs2015q4")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q1[, c("word", "docs2016q1")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q2[, c("word", "docs2016q2")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q3[, c("word", "docs2016q3")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count16q4[, c("word", "docs2016q4")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q1[, c("word", "docs2017q1")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q2[, c("word", "docs2017q2")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q3[, c("word", "docs2017q3")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count17q4[, c("word", "docs2017q4")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q1[, c("word", "docs2018q1")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q2[, c("word", "docs2018q2")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q3[, c("word", "docs2018q3")], by="word", all.x=TRUE)
quarterlycounts <- merge(quarterlycounts, count18q4[, c("word", "docs2018q4")], by="word", all.x=TRUE)


quarterlycounts$percent2014q4 <- as.numeric(quarterlycounts$docs2014q4) * 100 / nrow(filter(tidycorp,datetimestamp <= '2014-12-31' & datetimestamp >= '2014-10-01'))
quarterlycounts$percent2015q1 <- as.numeric(quarterlycounts$docs2015q1) * 100 / nrow(filter(tidycorp,datetimestamp <= '2015-3-31' & datetimestamp >= '2015-01-01'))
quarterlycounts$percent2015q2 <- as.numeric(quarterlycounts$docs2015q2) * 100 / nrow(filter(tidycorp,datetimestamp <= '2015-06-30' & datetimestamp >= '2015-04-01'))
quarterlycounts$percent2015q3 <- as.numeric(quarterlycounts$docs2015q3) * 100 / nrow(filter(tidycorp,datetimestamp <= '2015-09-30' & datetimestamp >= '2015-07-01'))
quarterlycounts$percent2015q4 <- as.numeric(quarterlycounts$docs2015q4) * 100 / nrow(filter(tidycorp,datetimestamp <= '2015-12-31' & datetimestamp >= '2015-10-01'))
quarterlycounts$percent2016q1 <- as.numeric(quarterlycounts$docs2016q1) * 100 / nrow(filter(tidycorp,datetimestamp <= '2016-3-31' & datetimestamp >= '2016-01-01'))
quarterlycounts$percent2016q2 <- as.numeric(quarterlycounts$docs2016q2) * 100 / nrow(filter(tidycorp,datetimestamp <= '2016-06-30' & datetimestamp >= '2016-04-01'))
quarterlycounts$percent2016q3 <- as.numeric(quarterlycounts$docs2016q3) * 100 / nrow(filter(tidycorp,datetimestamp <= '2016-09-30' & datetimestamp >= '2016-07-01'))
quarterlycounts$percent2016q4 <- as.numeric(quarterlycounts$docs2016q4) * 100 / nrow(filter(tidycorp,datetimestamp <= '2016-12-31' & datetimestamp >= '2016-10-01'))
quarterlycounts$percent2017q1 <- as.numeric(quarterlycounts$docs2017q1) * 100 / nrow(filter(tidycorp,datetimestamp <= '2017-3-31' & datetimestamp >= '2017-01-01'))
quarterlycounts$percent2017q2 <- as.numeric(quarterlycounts$docs2017q2) * 100 / nrow(filter(tidycorp,datetimestamp <= '2017-06-30' & datetimestamp >= '2017-04-01'))
quarterlycounts$percent2017q3 <- as.numeric(quarterlycounts$docs2017q3) * 100 / nrow(filter(tidycorp,datetimestamp <= '2017-09-30' & datetimestamp >= '2017-07-01'))
quarterlycounts$percent2017q4 <- as.numeric(quarterlycounts$docs2017q4) * 100 / nrow(filter(tidycorp,datetimestamp <= '2017-12-31' & datetimestamp >= '2017-10-01'))
quarterlycounts$percent2018q1 <- as.numeric(quarterlycounts$docs2018q1) * 100 / nrow(filter(tidycorp,datetimestamp <= '2018-3-31' & datetimestamp >= '2018-01-01'))
quarterlycounts$percent2018q2 <- as.numeric(quarterlycounts$docs2018q2) * 100 / nrow(filter(tidycorp,datetimestamp <= '2018-06-30' & datetimestamp >= '2018-04-01'))
quarterlycounts$percent2018q3 <- as.numeric(quarterlycounts$docs2018q3) * 100 / nrow(filter(tidycorp,datetimestamp <= '2018-09-30' & datetimestamp >= '2018-07-01'))
quarterlycounts$percent2018q4 <- as.numeric(quarterlycounts$docs2018q4) * 100 / nrow(filter(tidycorp,datetimestamp <= '2018-12-31' & datetimestamp >= '2018-10-01'))


# 19 category list ----------------------------------------------------------------------



termcategories <- c("business_finance","construction_equipment","electrical_infrastructure","media_telecom",
                    "personnel_equipment_facilities","oil_infrastructure","transportation_infrastructure",
                    "water_infrastructure","no_category","vehicles_not_military_oil",
                    "residential_buildings_other_critical_infrastructure","terrain","manufacturing_production_industrial_infrastructure")


# 20 count by quarter -----------------------------------------------------

categorycount <- quarterlycounts %>%
  subset(word %in% termcategories) %>%
  select(c("word",starts_with("docs"))) %>%
  t()  %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]

quarters <- data.frame(quarter = as.Date(c("2014-10-01","2015-01-01","2015-04-01","2015-07-01","2015-10-01","2016-01-01","2016-04-01","2016-07-01","2016-10-01","2017-01-01",
                                           "2017-04-01","2017-07-01","2017-10-01","2018-01-01","2018-04-01","2018-07-01","2018-10-01")))

categorycounts <- mutate_all(as.data.frame(categorycount), function(x) as.numeric(as.character(x)))
categorycounts[is.na(categorycounts)] <- 0
allcategorycount <- cbind(categorycounts,quarters)
longcategorycount <- allcategorycount %>%
  select(-no_category) %>%
  pivot_longer(any_of(termcategories),
               names_to="category",values_to="count")

# 21 percent by quarter ----------------------------------------------------------------------



categorypercent <- quarterlycounts %>%
  subset(word %in% termcategories) %>%
  select(c("word",starts_with("percent"))) %>%
  t()  %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]


categorypercents <- mutate_all(as.data.frame(categorypercent), function(x) as.numeric(as.character(x)))
categorypercents[is.na(categorypercents)] <- 0
allcategorypercent <- cbind(categorypercents,quarters)
longcategorypercent <- allcategorypercent %>%
  select(-no_category) %>%
  pivot_longer(any_of(termcategories),
               names_to="category",values_to="count")



# 22 graphs ----------------------------------------------------------------------


ggplot(data=longcategorycount) +
  geom_line(aes(x=quarter,y=count,color=category)) +
  scale_color_manual(values = c("personnel_equipment_facilities" = "brown1",
                                "oil_infrastructure"="black",
                                "construction_equipment"="bisque",
                                "business_finance"="aquamarine",
                                "electrical_infrastructure"="darkgoldenrod1",
                                "transportation_infrastructure"="darkred",
                                "water_infrastructure"="cyan",
                                "no_category"="darksalmon",
                                "terrain"="darkolivegreen",
                                "vehicles_not_military_oil"="darkslategray4",
                                "residential_buildings_other_critical_infrastructure"="burlywood4",
                                "manufacturing_production_industrial_infrastructure"="darkorchid1"
  )) +
  scale_x_date(date_breaks = "1 year") +
  xlab("Date") +
  ylab("Number of Press Releases Containing Category")

ggplot(data=longcategorypercent) +
  geom_line(aes(x=quarter,y=count,color=category)) +
  scale_color_manual(values = c("personnel_equipment_facilities" = "brown1",
                                "oil_infrastructure"="black",
                                "construction_equipment"="bisque",
                                "business_finance"="aquamarine",
                                "electrical_infrastructure"="darkgoldenrod1",
                                "transportation_infrastructure"="darkred",
                                "water_infrastructure"="cyan",
                                "no_category"="darksalmon",
                                "terrain"="darkolivegreen",
                                "vehicles_not_military_oil"="darkslategray4",
                                "residential_buildings_other_critical_infrastructure"="burlywood4",
                                "manufacturing_production_industrial_infrastructure"="darkorchid1"
  )) +
  scale_x_date(date_breaks = "1 year") +
  xlab("Date") +
  ylab("Percent of Press Releases Containing Category")


# 23 ISIL-"held" terms ----------------------------------------------------------------------

# "Held" terms

heldlist <- which(grepl("held", alltokens_prephrase$word, fixed=TRUE),alltokens_prephrase$word)
heldlist <- heldlist + 1
held <- alltokens_prephrase[heldlist,]

heldlist <- which(grepl("held", alltokens_prephrase$word, fixed=TRUE),alltokens_prephrase$word) + 1
held <- alltokens_prephrase[heldlist,]

alltokens[which(grepl("tactic", alltokens$word, fixed=TRUE),alltokens$word) + 1,]

# 24 Pearson correlation matrix  ----------------------------------------------------------------------

data <- allcategorycount

# Remove non-numeric columns 
numeric_data <- data[sapply(data, is.numeric)]

# Compute the Pearson correlation matrix
cor_matrix <- cor(numeric_data, method = "pearson", use = "pairwise.complete.obs")

# Save the correlation matrix to a CSV file
write.csv(cor_matrix, "~/Desktop/20250328 Replication Files & Appendix/correlation_matrix.csv", row.names = TRUE)

# Print confirmation
cat("Correlation matrix saved to 'correlation_matrix.csv'\n")

