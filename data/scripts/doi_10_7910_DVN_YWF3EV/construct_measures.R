# Reconstruct measures from original paper using WVS data


## read data
vs <- read.dta("data/raw/Integrated_values_surveys_1981-2008_final.dta",
               convert.factors = FALSE)

## remove some observations based on country
vs <- vs %>% 
  mutate(s002evs = ifelse(is.na(s002evs), -99, s002evs),
         s002 = ifelse(is.na(s002), -99, s002),
         x051 = ifelse(is.na(x051), -99, x051),
         g016 = ifelse(is.na(x051), -99, g016)) %>% # otherwise, the next line drops NAs, we don't want that yet
  filter(s002evs!=1) %>% 
  filter(s003!=909) %>% 
  filter(s003!=197) %>% 
  filter(s003!=915) %>% 
  filter(s003!=891) %>% 
  filter(s003!=646) %>% 
  filter(s003!=716)

vs$s003[vs$s003 == 688] <- 911

# set thresholds
mincountry <- 50
minques <- 70
minresp <- 500



## recode ethnicities
vs$x051[(vs$s003 == 807 & vs$x051 == 1390) |
          (vs$s003 == 368 & vs$x051 == 35)] <- 8000
vs$x051[(vs$s003 == 36 & vs$x051 == 420)] <- 101
vs$x051[(vs$s003 == 380 & vs$x051 == 1400)] <- 410
vs$x051[(vs$s003 == 704 & vs$x051 != 8000)] <- 1319
vs$x051[(vs$s003 == 826 & vs$x051 == 810) |
          (vs$s003 == 826 & vs$x051 == 130)] <- 90
vs$x051[(vs$s003 == 356 & vs$s002 == 3) |
          (vs$s003 == 36 & vs$s002 == 1) |
          (vs$s003 == 50 & vs$s002 == 4) |
          (vs$s003 == 862 & vs$s002 == 3) |
          (vs$s003 == 100 & vs$s002 == 5) |
          (vs$s003 == 642 & vs$s002 == 5) |
          (vs$s003 == 566 & vs$s002 == 2) |
          (vs$s003 == 76 & vs$s002 == 2) |
          (vs$s003 == 76 & vs$s002 == 3) |
          (vs$s003 == 156 & vs$s002 == 3) | 
          (vs$s003 == 156 & vs$s002 == 5) |
          (vs$s003 == 826 & vs$x051 == 8000) |
          (vs$s003 == 826 & vs$x051 == 8001) |
          (vs$s003 == 604 & vs$s002 == 3) |
          (vs$s003 == 604 & vs$s002 == 4) |
          (vs$s003 == 840 & vs$x051 == 8000) |
          (vs$s003 == 840 & vs$s002 == 5) |
          (vs$s003 == 124 & vs$x051 == 8000) |
          (vs$s003 == 484 & vs$s002 == 3) |
          (vs$s003 == 484 & vs$s002 == 4) |
          (vs$s003 == 710 & vs$s002 == 1) |
          (vs$s003 == 710 & vs$s002 == 2) |
          (vs$s003 == 710 & vs$s002 == 5) |
          (vs$s003 == 152 & vs$s002 == 2) |
          (vs$s003 == 152 & vs$s002 == 5) |
          (vs$s003 == 152 & vs$x051 == 8000)] <- NA

# recode languages 
vs$g016[(vs$s003 == 36 & vs$g016 == 144) |
          (vs$s003 == 124 & !(vs$g016 == 128 | vs$g016 == 144)) |
          (vs$s003 == 364 & !(vs$g016 == 358 | vs$g016 == 39)) |
          (vs$s003 == 566 & !(vs$g016 %in% c(128, 173, 190, 498))) |
          (vs$s003 == 752 & (vs$g016 %in% c(397, 466, 802, 806))) |
          (vs$s003 == 3 & (vs$g016 %in% c(279, 378))) |
          (vs$s003 == 704 & vs$g016 != 482) |
          (vs$s003 == 356 & (vs$g016 %in% c(297, 408))) |
          (vs$s003 == 840 & vs$g016 == 208)] <- 800
vs$g016[(vs$s003 == 608 & (vs$g016 %in% c(12, 11, 219, 804)))] <- 804
vs$g016[(vs$s003 == 705 & (vs$g016 %in% c(65, 90, 397)))] <- 805
vs$g016[vs$s003 == 364 & vs$g016 == 138] <- 358
vs$g016[vs$s003 == 246 & vs$g016 == 431] <- 141
vs$g016[vs$s003 == 36 & vs$g016 == 73] <- 101
vs$g016[(vs$s003 == 156 & vs$s002 == 3) |
          (vs$s003 == 246 & vs$g016 == 210) |
          (vs$s003 == 246 & vs$g016 == 482) |
          (vs$s003 == 268 & vs$g016 == 800) |
          (vs$s003 == 586 & vs$s002 == 3) |
          (vs$s003 == 630 & vs$g016 == 800) |
          (vs$s003 == 276 & vs$s002 == 3) |
          (vs$s003 == 152 & vs$g016 == 397) |
          (vs$s003 == 356 & vs$s002 == 3) |
          (vs$s003 == 498 & vs$s002 == 3) |
          (vs$s003 == 792 & (vs$s002 == 3 | vs$s002 == 4))] <- NA


# recode language as ethnicity where language is more appropriate 
# x051 is now ethnolinguistic identity
langcountries <- c(8, 12, 20, 51, 124, 
                   158, 196, 203, 233,
                   246, 268, 276, 348, 
                   356, 417, 440, 466,
                   504, 554, 566, 578,
                   586, 616, 643, 703,
                   705, 710, 724, 756, 
                   764, 792, 800, 804,
                   834, 854, 894)
vs <- vs %>% 
  mutate(newlang = ifelse(s003 %in% langcountries, 1, 0)) %>% 
  mutate(x051 = ifelse(newlang == 1, g016, x051))



# drop questions
adrop <- c("a001_co a002_co a003_co a004_co a005_co a006_co") %>% 
  strsplit(" ") %>% unlist()
edrop <- c("e179 e180 e181 e182 e192 e001_hk e002_hk e005_hk e006_hk e114_mx e115_mx e116_mx e117_iqa e117_iqb e117_mx e238_es e239_es e240_es e241_es e256") %>% 
  strsplit(" ") %>% unlist()
fdrop <- c("f025 f025_01 f027 f027_01") %>% 
  strsplit(" ") %>% unlist()
gdrop <- c("g001cs g002cs g003cs g003 g005 g007_02 g007_03 g007_04 g007_05 g007_06 g007_07 g007_08 g007_09 g007_10 g007_11 g007_12 g007_13 g007_14 g007_15 g007_16 g007_17 g007_18 g007_30 g007_31 g007_32 g007_37 g007_38 g007_39 g015 g016 g017 g018 g024 g025 g026 g027") %>% 
  strsplit(" ") %>% unlist()
azero <- c("a020 a021 a022 a023 a024 a028 a031 a033 a036 a037 a043 a050 a051 a052 a053 a054 a055 a056 a078 a095 a121 a122 a123 a124_44 a166 a167 a171 a172") %>% 
  strsplit(" ") %>% unlist()
czero <- c("c007 c030 c032 c049 c050 c051 c052 c053 c054 c055 c056 c057 c058") %>% 
  strsplit(" ") %>% unlist()
dzero <- c("d021 d044 d044a d045 d046 d047 d048 d049 d050 d051 d052 d053") %>% 
  strsplit(" ") %>% unlist()
ezero <- c("e011 e013 e024 e030 e031 e069_36 e069_37 e069_50 e178 e183 e187") %>% 
  strsplit(" ") %>% unlist()
fzero <- c("f002 f011 f012 f014 f015 f016 f017 f018 f019 f020 f021 f023 f039 f068 f069 f070 f071 f072 f073 f074 f075 f076 f077 f078 f079 f080 f081 f082 f083 f084 f085 f086 f087 f088 f089 f090 f091 f092 f093 f094 f095 f096 f156 f157 f158 f159") %>% 
  strsplit(" ") %>% unlist()
gzero <- c("g004") %>% 
  strsplit(" ") %>% unlist() 
gstars <- c("g007_2 g007_4 g007_5 g007_6") %>% strsplit(" ") %>% unlist()

dropvars <- c(adrop, edrop, fdrop, gdrop,
              azero, 
              czero, 
              dzero, 
              ezero, 
              fzero, 
              gzero
              )


vs <- vs %>% 
  dplyr::select(-dropvars) %>% 
  dplyr::select(-(starts_with(gstars)))

rm(adrop, edrop, fdrop, gdrop, azero,
   czero, dzero, ezero, fzero, gzero,
   gstars)


# turn NAs back and remove if missing ethnicity
vs <- vs %>% 
  mutate(s002evs = na_if(s002evs, -99),
                s002 = na_if(s002, -99),
                x051 = na_if(x051, -99),
                g016 = na_if(x051, -99)) %>% 
  filter(!is.na(x051))

# question lists
abinary <- c("a010 a011 a012 a013 a014 a015 a016 a017 a018 a019 a025 a027 a029 a030 a032 a034 a035 a038 a039 a040 a041 a042 a046 a047 a048 a049 a064 a065 a066 a067 a068 a069 a070 a071 a071b a071c a072 a073 a074 a075 a076 a077 a079 a080 a081 a082 a083 a084 a085 a086 a087 a088 a088b a088c a089 a090 a091 a092 a093 a094 a096 a097 a124_01 a124_02 a124_03 a124_04 a124_05 a124_06 a124_07 a124_08 a124_09 a124_10 a124_11 a124_12 a124_13 a124_14 a124_16 a124_17 a124_18 a124_19 a124_20 a124_21 a124_24 a124_25 a124_26 a124_27 a124_28 a124_29 a124_30 a124_31 a124_32 a124_34 a124_35 a124_36 a124_37 a124_38 a124_45 a124_46 a124_47 a124_48 a124_49 a124_50 a124_51 a124_52 a124_53 a165 a168 a124_42 a124_43 a124_54 a124_58 a124_59 a124_61") %>% 
  strsplit(" ") %>% unlist()
bbinary <- c("b011 b012 b013 b014 b015") %>% 
  strsplit(" ") %>% unlist()
cbinary <- c("c011 c012 c013 c014 c015 c016 c017 c018 c019 c020 c021 c022 c023 c024 c025 c027_1 c027_2 c027_3 c027_4 c028 c029 c042b1 c042b2 c042b3 c042b4 c042b5 c042b6 c042b7 c059")%>% 
  strsplit(" ") %>% unlist()
dbinary <- c("d003 d004 d005 d006 d007 d008 d009 d010 d011 d012 d013 d014 d015 d016 d018 d019 d022") %>% 
  strsplit(" ") %>% unlist()
ebinary <- c("e025b e026b e048 e049 e050 e051 e052 e053 e054 e055 e056 e062 e118 e119 e127 e128 e129c e132 e144 e145 e178_01 e193 e194 e195 e206 e209 e221b e222b e237 e248 e249 e250 e251 e252 e253 e254 e257") %>% 
  strsplit(" ") %>% unlist()
fbinary <- c("f024 f026 f029 f031 f032 f033 f035 f036 f037 f038 f040 f041 f042 f043 f044 f045 f046 f047 f048 f049 f050 f051 f052 f053 f054 f055 f056 f057 f058 f059 f060 f064 f065 f098 f177 f178 f179 f188") %>% 
  strsplit(" ") %>% unlist()



ascale <- c("a001 a002 a003 a004 a005 a006 a007 a008 a009 a170 a173 a057 a058 a059 a060 a061 a062 a063 a098 a099 a100 a101 a102 a103 a104 a105 a106 a107 a108 a109 a110 a111 a112 a113 a114 a115 a116 a117 a118 a119 a120 a050_01 a050_02 a050_03 a050_04 a168_01 a168a a174 a189 a190 a191 a192 a193 a194 a195 a196 a197 a198") %>% 
  strsplit(" ") %>% unlist()
bscale <- c("b001 b002 b003 b004 b005 b006 b007 b010 b018 b019 b020 b021 b022 b023 b024 b025 b026 b027 b028 b029") %>% 
  strsplit(" ") %>% unlist()
cscale <- c("c006 c008 c031 c033 c034 c035 c036 c037 c038 c039 c040 c041 c062 c063 c064") %>% 
  strsplit(" ") %>% unlist()
dscale <- c("d001 d002 d017 d020 d026 d026_01 d026_02 d026_03 d026_04 d026_05 d027 d028 d029 d030 d031 d032 d033 d034 d035 d036 d037 d038 d039 d040 d041 d042 d043 d043_01 d054 d055 d056 d057 d058 d059 d060 d061 d062 d063 d064 d064_01 d065 d066 d067 d068 d069 d070 d071 d073 d076 d077 d078 d079 d080") %>% 
  strsplit(" ") %>% unlist()
escale <- c("e007 e008 e009 e010 e014 e015 e016 e017 e018 e019 e020 e021 e023 e025 e026 e027 e028 e029 e033 e034 e035 e036 e037 e038 e039 e040 e041 e042 e043 e044 e045 e046 e047 e057 e058 e059 e060 e061 e063 e064 e065 e066 e067 e068 e069_01 e069_02 e069_03 e069_04 e069_05 e069_06 e069_07 e069_08 e069_09 e069_10 e069_11 e069_12 e069_13 e069_14 e069_15 e069_16 e069_17 e069_18 e069_19 e069_20 e069_21 e069_22 e069_23 e069_24 e069_25 e069_26 e069_27 e069_29 e069_30 e069_31 e069_33 e069_34 e069_35 e069_38 e069_39 e069_40 e069_41 e069_42 e069_43 e069_45 e069_46 e069_47 e069_48 e069_49 e069_52 e104 e105 e106 e107 e108 e109 e110 e111 e112 e113 e114 e115 e116 e117 e120 e121 e122 e123 e124 e125 e129 e129a e129b e130 e133 e134 e140 e141 e142 e146 e147 e148 e149 e150 e151 e152 e153 e154 e155 e156 e157 e158 e159 e160 e161 e162 e162_01 e163 e164 e165 e166 e167 e168 e169 e170 e171 e172 e173 e174 e175 e176 e177 e179_01 e181_01 e184 e185 e186 e188 e189 e196 e198 e203 e205 e207 e208 e212 e213 e214 e215 e216 e217 e218 e219 e220 e222 e224 e225 e226 e227 e228 e229 e230 e231 e232 e233 e234 e235 e236 e242 e243 e244 e245 e246 e247 e255") %>% 
  strsplit(" ") %>% unlist()
fscale <- c("f001 f003 f022_01 f028 f030 f062_01 f062_02 f062_03 f063 f066 f067 f097 f099 f100 f101 f102 f103 f104 f105 f108 f109 f110 f111 f112 f113 f114 f115 f116 f117 f118 f119 f120 f121 f122 f123 f124 f125 f126 f127 f128 f129 f130 f131 f132 f133 f134 f135 f136 f137 f138 f139 f140 f141 f142 f143 f144 f144_01 f144_02 f145 f146 f147 f148 f149 f150 f151 f152 f153 f154 f155 f164 f165 f166 f167 f168 f169 f170 f171 f172 f173 f174 f175 f176 f186 f187 f190 f198 f199") %>% 
  strsplit(" ") %>% unlist()
gscale <- c("g006 g007_01 g007_33 g007_34 g007_35 g007_36 g019 g020 g021 g022a g022b g022c g022d g022e g022f g022g g022h g022i g022j g022k g022l g022m g023 g028 g029 g030 g031 g032 g033 g034 g035 g036 g037 g038 g039 g040 g041 g042 g043 g044 g045 g046 g047 g048 g049 g050 g051") %>% 
  strsplit(" ") %>% unlist()



# multinomials
a3opt <- c("a026 a169 a026_01") %>% 
  strsplit(" ") %>% unlist()
a4opt <- c("a044 a045") %>% 
  strsplit(" ") %>% unlist()
b3opt <- c("b008 b009 b016 b017") %>% 
  strsplit(" ") %>% unlist()
c3opt <- c("c001 c002 c003 c004 c005 c061") %>% 
  strsplit(" ") %>% unlist()
c4opt <- c("c060") %>% 
  strsplit(" ") %>% unlist()
c5opt <- c("c009 c010") %>% 
  strsplit(" ") %>% unlist()
d3opt <- c("d023 d024 d025") %>% 
  strsplit(" ") %>% unlist()
e3opt <- c("e012 e022 e032 e131 e197 e204 e211") %>% 
  strsplit(" ") %>% unlist()
e4opt <- c("e001 e002 e003 e004 e005 e006 e143") %>% 
  strsplit(" ") %>% unlist()
e5opt <- c("e190 e191 e238 e239 e240 e241") %>% 
  strsplit(" ") %>% unlist()
e6opt <- c("e135 e136 e137 e138 e139") %>% 
  strsplit(" ") %>% unlist()
f3opt <- c("f004 f005 f006 f007 f008 f009 f010 f022") %>% 
  strsplit(" ") %>% unlist()
f4opt <- c("f034 f062") %>% 
  strsplit(" ") %>% unlist()
g5opt <- c("g001 g002") %>% 
  strsplit(" ") %>% unlist()

binary <- c(abinary, bbinary, cbinary, dbinary, ebinary, fbinary)

multinom <- c(a3opt, a4opt, b3opt, c3opt, c4opt, c5opt, d3opt, e3opt,
              e4opt, e5opt, e6opt, f3opt, f4opt, g5opt)

discrete <- c(binary, multinom)

scales <- c(ascale, bscale, cscale, dscale, escale, fscale, gscale)

allquestions <- c(discrete, scales)
finalqlist <- allquestions

rm(abinary, bbinary, cbinary, dbinary, ebinary, fbinary,
   a3opt, a4opt, b3opt, c3opt, c4opt, c5opt, d3opt, e3opt,
   e4opt, e5opt, e6opt, f3opt, f4opt, g5opt, ascale, bscale, 
   cscale, dscale, escale, fscale, gscale)

# TODO calculate distances 

  ## discrete

  ## scales 

# drop questions not meeting criteria
qcounts <- tibble(
  country = numeric(),
  question = character(),
  count = numeric()
)


for (q in allquestions) {
  
  # enquote item
  enquo(q)
  # subset data
  temp <- vs %>% 
    select(s003, !!q) %>%
    drop_na() %>% 
    count(s003) %>% 
    summarize(n = n()) %>% 
    as.numeric()
  
  # drop question if not asked in mincountry or more
  if (temp[[1]] < mincountry) {
    finalqlist <- finalqlist[!finalqlist %in% c(q)]
  }
  
  # calculate questions per country
  qcounts1 <- vs %>% 
    select(s003, !!q) %>% 
    drop_na() %>% 
    count(s003) %>% 
    mutate(count = ifelse(n > minresp, 1, 0),
           country = s003, 
           question = q) %>% 
    select(country, question, count)
  
  qcounts <- bind_rows(qcounts, qcounts1)
  
}

# remove countries with less than minresp to minques
cts <- qcounts %>% 
  group_by(country) %>% 
  summarize(numq = sum(count)) %>%
  filter(numq >= minques) %>% 
  select(country) %>% 
  unlist()
vs <- vs[vs$s003 %in% cts,]


rm(cts, qcounts, temp)

# group by country
vs <- vs %>% 
  group_by(s003)


# compute measures :

questionlist <- finalqlist
countrylist <- unique(vs$s003)

# create measures tibble
measures <- tibble(
  country = character(),
  question = character(),
  ethnic_elf = numeric(),
  ethnic_pol = numeric(),
  chisq = numeric(),
  fst = numeric(),
  cultelf = numeric(),
  cultpol = numeric()
)



# loop each country :
for ( y in countrylist ) {
  
  
  tempdata <- vs %>% 
    filter(s003 == y)
  
  # number of respondents total in country
  totpop <- nrow(tempdata)
  nethn <- length(unique(tempdata$x051))
  
  # calculate ethnic shares (ethnic respondents / all respondents)
  ethshares <- tempdata %>% 
    group_by(x051) %>% 
    summarize(n = n()) %>% 
    mutate(ethshare = n / totpop) %>% 
    mutate(ethsharesq = ethshare^2) %>% 
    mutate(p = ethsharesq * (1-ethshare))
  
  elfeth <- 1 - as.numeric(summarize(ethshares, ethsharesq = sum(ethsharesq)))
  poleth <- as.numeric(summarize(ethshares, p = sum(p)))
  
  # for each question (cultural items)
  for (q in questionlist) {
    # enquote item
    enquo(q)
    # subset data
    tempdata_oneq <- tempdata %>% 
      select(s003, x051, !!q)
    
    # number of respondents total to question (numbtot)
    numbtot <- tempdata_oneq %>%   
      drop_na(!!q) %>% 
      summarize(n = n()) %>% 
      select(n) %>% 
      as.numeric()
    
    # skip if no respondents
    if (is.na(numbtot) | numbtot == 0) {
      measures <- measures %>% 
        add_row(
          country = as.character(y), 
          question = q,
        )
      next
    }
    else {
      # all possible answers to the question
      maxnum <- tempdata_oneq %>% select(!!q) %>% drop_na() %>% unique() %>% pull()
      
      
      # get total number of ethnicities that answered question (numethn3)
      numethn3 <- tempdata_oneq %>% 
        drop_na(!!q) %>% select(x051) %>% unique() %>% pull()
      
      attach(tempdata_oneq)
      # cross tab counts of responses and ethnicities (in stata: tab `q' x051) (varble)
      resp_count <- table(x051, get(q))
      
      # number of ppl that responded to question (numbans2)
      numbans2 <- rowSums(resp_count)
      # share of responses to question in ethnicity (sum_eth/share) = numbans2/numbtot
      share <- numbans2/numbtot
      
      # ***compute share responding specific answer of total responses to question 
      # (freqans = varble[eth, ] / numbans2) / freqans_total = varble_all[resp] / numbtot
      freqans <- resp_count / numbans2
      freqans_total <- colSums(resp_count) / numbtot
      detach(tempdata_oneq)
      
      # compute probability of meeting somebody of other group with other preferences
      # (recursive) polover = sum over all responses[(freqans[resp]*share)^2 * (1-sum_eth - freqans_total + freqans*sum_eth)
      # polover <- rowSums( 
      #   (freqans * share)^2 * t(t(matrix(
      #     data=1, 
      #     nrow = length(numethn3), 
      #     ncol = length(maxnum))-share) - freqans_total) + (freqans*share) 
      # )
      # 
      # ***cultural elf = 1 - sum over all responses [ freqans^2 ] (cultelf/cultelf_all)
      cultelf <- 1- rowSums(freqans^2)
      cultelf_all <- 1 -sum(freqans_total^2)
      
      # ***cultural pol = sum over all responses[ (freqans^2) * (1-freqans)] (cultelf/cultpol_all)
      cultpol <- rowSums(  freqans^2 * (1-freqans) )
      cultpol_all <- sum( freqans_total^2 *(1-freqans_total))
      
      # chisq = sum over all responses [ ((freqans_total - freqans)*share)^2 / (freqans_total * share) ]
      # chisq[q, country] = sum over all ethnicities [chisq]
      chi_sq_temp <- sum(
        rowSums( 
          (t(freqans_total - t(freqans)) * share)^2 / (share %*% t(freqans_total)) 
        )
      )
      # fst[q, country] = ( 1 - (sum over all ethnicities [cultelf * share]  /  (cultelf_all)
      fst_temp <- 1 - sum(cultelf * share)/cultelf_all
      
      # TODO: distance scaled CF
      
      # TODO : fstdist[q, country] =  same form but use cultelf_dist instead
      
    } # end else block
    
    # add measures by question to dataset
    measures <- measures %>% 
      add_row(
        country = as.character(y), 
        question = q,
        chisq = chi_sq_temp,
        fst = fst_temp,
        cultelf = cultelf_all,
        cultpol = cultpol_all
      )
    
  } # end question loop
  
  # add elf to all rows with this country
  
  measures <- measures %>% 
    mutate(ethnic_elf = ifelse(country == as.character(y), elfeth, ethnic_elf),
           ethnic_pol = ifelse(country == as.character(y), poleth, ethnic_pol)
    )
  
} # end country loop


# add question characteristics to measures
# measures <- measures %>% 
#   mutate(discrete = ifelse(question  discrete, 1, 0),
#          binary = ifelse(question %in% binary, 1, 0),
#          scale = ifelse(question %in% scale, 1, 0))


# save measures dataset 
saveRDS(measures, file = "data/clean/wvs_measures_by_question.rds") 
