# load data
load("df2.RData")
load("vaa_responses.RData")


# remove second VAA attempts from vaa data

vaa_responses <- vaa_responses %>%
  arrange(ids, vaa_start) %>%  # Arrange by ids and vaa_start
  distinct(ids, .keep_all = TRUE)


# merge data
df <- left_join(df2, vaa_responses, by = "ids")

# save merged data
save(df, file = "df.RData")

