# FUNCTION TO RETRIEVE RELIABILITY AND SEPARATION STATS FOR 'tam.mml' MODELS
get_relstats <- function(
  model        # NAME OF TAM MODEL OBJECT
  ){
  data = IRT.data(model)

  # ------- INFIT & OUTFIT  ---------------- #
  # compute WLEs
  wles <- tam.wle(model)

  # extract item parameters
  b1 <- (- model$AXsi[ , -1 ])

  # assess fit
  inout <- sirt::pcm.fit(b = b1 ,
                  theta = wles$theta ,
                  dat = data)


  # PERSON INFIT & OUTFIT SDs
  person_insd  <- sd(inout$personfit$infit, na.rm = TRUE)
  person_outsd <- sd(inout$personfit$outfit, na.rm = TRUE)

  # ITEM INFIT & OUTFIT SDs
  item_inout <- msq.itemfitWLE(model)
  item_insd <- item_inout$fit_data_summary["Infit", "SD"]
  item_outsd <- item_inout$fit_data_summary["Outfit", "SD"]

  # ------- RELIABILITY   ---------------- #

  # PERSON
  person_rel <- WLErel(wles$theta, wles$error)

  # ITEM
  item_mse <- mean(model$xsi$se.xsi^2)
  item_rel <- 1 - (item_mse/var(model$xsi$xsi))

  # ------- SEPARATION   ---------------- #

  # PERSON
  person_sep <- person_rel/(1 - person_rel)

  # ITEM
  item_sep <- item_rel/(1 - item_rel)

  # ------- RETURN RESULTS   ---------------- #
  c(person_insd = person_insd,
    person_outsd = person_outsd,
    person_rel = person_rel,
    person_sep = person_sep,
    item_insd = item_insd,
    item_outsd = item_outsd,
    item_rel = item_rel,
    item_sep = item_sep)
}
