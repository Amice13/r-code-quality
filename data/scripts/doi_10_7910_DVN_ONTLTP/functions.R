
### [1]
Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
}
### [2]
aux1 <- function(x, vot, n){
        wid <- ncol(x)
        len <- nrow(x)
        x[,wid+1] <- x[,vot]
        x[1,wid+1] <- ifelse(x[1,vot] != 0, Mode(x[which(x$LEMA == x$LEMA[1]), vot]), 0)
        for(j in 2:n){
                x[,wid+j] <- x[,wid+j-1]
                x[(j-len*(j%/%len))+1,wid+j] <- ifelse(x[(j-len*(j%/%len))+1,vot] != 0, Mode(x[which(x$LEMA == x$LEMA[(j-len*(j%/%len))+1]), vot]), 0)
        }
        return(x[,(wid+1):(wid+n)])
}
### [3]
aux2 <- function(x,vots,n){
        out <- list()
        for(i in 1:length(vots)){out[[i]] <- aux1(x, vots[i], n)}
        return(out)
}


### [4]

wnuy <- function(data, legislature, run = numeric()){

        base2 <- data
        base2[is.na(base2)] <- 0L
        out <- list()
        for(i in 1:length(legislature)){
                legis  <- legislature[i]
                dataL  <- base2[base2$legislatura==legis, c(1:7,grep(paste("^a", legis, sep = ""), names(base2)))]
                dataL2 <- data.frame("NOMBRE"  = dataL[,2],
                                     "LEMA"    = dataL[,6],
                                     "SUBLEMA" = dataL[,7],
                                     "LISTA"   = "list", stringsAsFactors = FALSE)
                dataL2 <- cbind(dataL2, dataL[,8:ncol(dataL)])
                x    <- dataL2
                vots <- c(5:ncol(dataL2))
                n    <- run[i]*nrow(dataL2)
                prueba <- aux2(x, vots, n)
                out[[i]] <- cbind.data.frame(dataL2, as.data.frame(prueba))

        }
        out
}

### [5]

run_wn <- function(data, polar = character(), legislature = numeric()){

        base <- data
        polar_1 <- which(base$NOMBRE == polar)[1]
        legis_vector <- character()
        for(i in 1:nrow(base)){legis_vector[i] <- paste("Legislador", 99+i, sep = "")}
        names <- paste(legis_vector, as.character(base[,1]))
        base <- as.matrix(base)
        label_party <- matrix(base[, 2], nrow = length(base[, 2]), ncol = 1)
        colnames(label_party) <- "party"
        base <- base[, -c(1,2,3,4)]
        rc <- rollcall(base,
                       yea = 1,
                       nay = 4,
                       notInLegis = 0,
                       legis.names = names,
                       legis.data = label_party,
                       desc = "base Votes")
        result <- wnominate(rc, dim = 1, polarity = polar_1)
        coord <- result$legislator[,c("party", "coord1D")]
        coord$coord1D <- coord$coord1D * -1
        name  <- substring(rownames(coord), 15, 100)
        coord <- cbind(legislator = name, coord[,1:2])
        rownames(coord) <- NULL
        coord$legislature <- legislature
        coord$id_wn <- paste(coord$legislator, coord$party, coord$legislature)
        na.omit(coord)

}


header <- function(datos, separador = ": "){
        vec_name <- character()
        for(i in 1:length(datos)){
                if(datos[i] == 42) vec_name[i] <- paste("Legislature", "1985 - 1990", sep = separador)
                if(datos[i] == 43) vec_name[i] <- paste("Legislature", "1990 - 1995", sep = separador)
                if(datos[i] == 44) vec_name[i] <- paste("Legislature", "1995 - 2000", sep = separador)
                if(datos[i] == 45) vec_name[i] <- paste("Legislature", "2000 - 2005", sep = separador)
                if(datos[i] == 46) vec_name[i] <- paste("Legislature", "2005 - 2010", sep = separador)
        }
        vec_name
}



parse_names <- function(x){

        x <- chartr('\u00e1\u00e9\u00ed\u00f3\u00fa','aeiou', x)
        o <- x
        for(i in 1:length(x)){

                if(x[i] == "Alfie, Issac"){o[i] <- "Alfie, Isaac"}
                if(x[i] == "Alonso, Nelson R."){o[i] <- "Alonso, Nelson Rene"}
                if(x[i] == "Amaral"){o[i] <- "Amaral, Mario Alberto"}
                if(x[i] == "Amaro, Juan J."){o[i] <- "Amaro Corrado, Juan Justo"}
                if(x[i] == "Amaro, Juan Justo"){o[i] <- "Amaro Corrado, Juan Justo"}
                if(x[i] == "Andrade Ambrosoni, Jorge"){o[i] <- "Andrade Ambrosi, Jorge"}
                if(x[i] == "Andrade Rodríguez, Néstor"){o[i] <- "Andrade Rodriguez, Nestor Homero"}
                if(x[i] == "Araujo, German"){o[i] <- "Araujo, Jose German"}
                if(x[i] == "Arrarte, Roberto"){o[i] <- "Arrarte Fernandez, Roberto"}
                if(x[i] == "Asiain, Roberto"){o[i] <- "Asiain Oliver, Roberto"}
                if(x[i] == "Assi, Alfredo"){o[i] <- "Asti, Alfredo"}
                if(x[i] == "Baliero"){o[i] <- "Baliero Silva, Washington Fredy"}
                if(x[i] == "Baron Arena, Hector"){o[i] <- "Baron Arena, Hector Nestor"}
                if(x[i] == "Barrios, Artigas"){o[i] <- "Barrios, Artigas A."}
                if(x[i] == "Belvisi, Walter"){o[i] <- "Belvisi, Walter M."}
                if(x[i] == "Bentancur"){o[i] <- "Bentancur Ferreira, Juan Alvario"}
                if(x[i] == "Bergenstein"){o[i] <- "Bergstein, Nahum"}
                if(x[i] == "Berois, Ricardo"){o[i] <- "Berois Quinteros, Ricardo"}
                if(x[i] == "Berro"){o[i] <- "Berro Rodriguez, Bernardo Pedro"}
                if(x[i] == "Berro, Berarrdo"){o[i] <- "Berro Rodriguez, Bernardo Pedro"}
                if(x[i] == "Blasina, Jose L."){o[i] <- "Blasina, Jose Luis"}
                if(x[i] == "Bonilla Suarez, Edgard"){o[i] <- "Bonilla Suarez, Edgar Antonio"}
                if(x[i] == "Bosch, Federico"){o[i] <- "Bosch, Juan Federico"}
                if(x[i] == "Brause Berreta, Alberto"){o[i] <- "Brause Berreta, Alberto Juan"}
                if(x[i] == "Bruno"){o[i] <- "Bruno, Jorge"}
                if(x[i] == "Cadenas Boix"){o[i] <- "Cadenas Boix, Enrique"}
                if(x[i] == "Capeche, cayetano"){o[i] <- "Capeche, Cayetano"}
                if(x[i] == "Capeche, Eugenio"){o[i] <- "Capeche, Pio Eugenio"}
                if(x[i] == "Caputi, Tabare"){o[i] <- "Caputi, Tabare Angel"}
                if(x[i] == "Carambula, marcos"){o[i] <- "Carambula, Marcos"}
                if(x[i] == "Carbone, Alvaro"){o[i] <- "Carbone Rico, Enrique Alvaro"}
                if(x[i] == "Cardozo, Julio"){o[i] <- "Cardozo Ferreira, Julio"}
                if(x[i] == "Carrera Sapriza"){o[i] <- "Carrere Sapriza, Justino"}
                if(x[i] == "Cassina, Carlos"){o[i] <- "Cassina, Carlos Alberto"}
                if(x[i] == "Castro Riera"){o[i] <- "Castro Riera, Omar"}
                if(x[i] == "Castro, Omar"){o[i] <- "Castro Riera, Omar"}
                if(x[i] == "Castroman, Ricardo"){o[i] <- "Castroman Rodriguez, Ricardo"}
                if(x[i] == "Cigliuti, Carlos W."){o[i] <- "Cigliuti, Carlos Walter"}
                if(x[i] == "Clavijo, Hebert"){o[i] <- "Clavijo, Heber"}
                if(x[i] == "Conde Montes  de Oca, Jorge"){o[i] <- "Conde Montes de Oca, Jorge"}
                if(x[i] == "Conde Montes De Oca, Jorge"){o[i] <- "Conde Montes de Oca, Jorge"}
                if(x[i] == "Cortazzo, Victor"){o[i] <- "Cortazzo, Victor M."}
                if(x[i] == "Croce"){o[i] <- "Croce Urbina, Juan Pablo"}
                if(x[i] == "Curbelo"){o[i] <- "Curbelo Cravanzola, Luis Alberto"}
                if(x[i] == "Curbelo, Luis Alberto"){o[i] <- "Curbelo Cravanzola, Luis Alberto"}
                if(x[i] == "Cusano, Mauricio"){o[i] <- "Cussano, Mauricio"}
                if(x[i] == "Da rosa"){o[i] <- "Da Rosa Vazquez, Eber"}
                if(x[i] == "Da Rosa Viñoles, Eber"){o[i] <- "Da Rosa Vazquez, Eber"}
                if(x[i] == "Da Rosa, Eber"){o[i] <- "Da Rosa Vazquez, Eber"}
                if(x[i] == "Darricarrere"){o[i] <- "Damboriarena, Luis A."}
                if(x[i] == "de Pazos Parada, Mario Hermes"){o[i] <- "De Pazos Parada, Mario Hermes"}
                if(x[i] == "de Posadas, Ignacio"){o[i] <- "De Posadas, Ignacio"}
                if(x[i] == "Delgado, Daniel H."){o[i] <- "Delgado, Daniel Hugo"}
                if(x[i] == "Diaz Chavez, Jose E."){o[i] <- "Diaz Chaves, Jose Enrique"}
                if(x[i] == "Diaz, Jose"){o[i] <- "Diaz, Jose E."}
                if(x[i] == "Diaz Ruben"){o[i] <- "Diaz, Ruben"}
                if(x[i] == "Dosetti"){o[i] <- "Dossetti Rodriguez, Gustavo Jose"}
                if(x[i] == "Dossetti"){o[i] <- "Dossetti Rodriguez, Gustavo Jose"}
                if(x[i] == "Elso Goñi"){o[i] <- "Elso Goñi, Wilson"}
                if(x[i] == "Enciso, Carlos"){o[i] <- "Enciso Christiansen, Carlos"}
                if(x[i] == "Escardo"){o[i] <- "Escardo Monge, Alejandro Alberto"}
                if(x[i] == "Etcheverrry, Juan"){o[i] <- "Etcheverry, Juan"}
                if(x[i] == "Fa Robaina"){o[i] <- "Fa Robaina, Juan Carlos"}
                if(x[i] == "Favretti"){o[i] <- "Favretti Calvete, Aldo Ramon"}
                if(x[i] == "Fernandez Chaves, Alejo"){o[i] <- "Fernandez Chavez, Alejo"}
                if(x[i] == "Ferrari Sancez, Hugo"){o[i] <- "Ferrari Sanchez, Hugo"}
                if(x[i] == "Ferreira Chavez"){o[i] <- "Ferreira Chaves, Ruben"}
                if(x[i] == "Ferrizo Ferreira, Luis A."){o[i] <- "Ferrizo Ferreira, Luis Alberto"}
                if(x[i] == "Ferrizzo , Luis Alberto"){o[i] <- "Ferrizo Ferreira, Luis Alberto"}
                if(x[i] == "Francolino, Rubens W."){o[i] <- "Francolino, Rubens Walter"}
                if(x[i] == "Fresia, Carlos M."){o[i] <- "Fresia, Carlos Maria"}
                if(x[i] == "Gaione"){o[i] <- "Gaione Scaglia, Heriberto Ariel"}
                if(x[i] == "Galo"){o[i] <- "Galo, Jose Maria"}
                if(x[i] == "Gandini"){o[i] <- "Gandini, Jorge"}
                if(x[i] == "Garat, Carlos"){o[i] <- "Garat, Carlos M."}
                if(x[i] == "Gargano"){o[i] <- "Gargano, Reinaldo"}
                if(x[i] == "Gil, Orlando"){o[i] <- "Gil Solares, Orlando"}
                if(x[i] == "Gomez, Larriera"){o[i] <- "Gomez Larriera, Francisco"}
                if(x[i] == "Gonzalez Perla, Humberto"){o[i] <- "Gonzalez Perla, Humberto A."}
                if(x[i] == "Goñi Castelao, Hector H."){o[i] <- "Goñi Castelao, Hector Homero"}
                if(x[i] == "Guadalupe Chocho, Ramon"){o[i] <- "Guadalupe Chocho, Ramon Isaac"}
                if(x[i] == "Guerra, Antonio"){o[i] <- "Guerra Caraballo, Antonio"}
                if(x[i] == "Guerrero silva"){o[i] <- "Guerrero Silva, Arturo"}
                if(x[i] == "Guntin"){o[i] <- "Guntin, Jose Luis"}
                if(x[i] == "Hackenbruch, Tabare"){o[i] <- "Hackenbruch Legnani, Tabare"}
                if(x[i] == "Hakenbruch Legnani, Tabare"){o[i] <- "Hackenbruch Legnani, Tabare"}
                if(x[i] == "Heber Füllgraff, Arturo"){o[i] <- "Heber Fulgraff, Arturo"}
                if(x[i] == "Heber, Luis A."){o[i] <- "Heber, Luis Alberto"}
                if(x[i] == "Hernandez garcia"){o[i] <- "Hernandez Garcia"}
                if(x[i] == "Hierro Lopez"){o[i] <- "Hierro Lopez, Luis A."}
                if(x[i] == "Hualde"){o[i] <- "Hualde Silva, Jose"}
                if(x[i] == "Hunter"){o[i] <- "Hunter, Jorge"}
                if(x[i] == "Ibarra, Doreen"){o[i] <- "Ibarra, Doreen Javier"}
                if(x[i] == "Jaurena, Eduardo"){o[i] <- "Jaurena, Eduardo Humberto"}
                if(x[i] == "Lacalle, Luis Alberto"){o[i] <- "Lacalle Pou, Luis Alberto"}
                if(x[i] == "Laguarda"){o[i] <- "Laguarda, Manuel"}
                if(x[i] == "Lenzi, Oscar"){o[i] <- "Lenzi Lateulade, Oscar"}
                if(x[i] == "loblowitz"){o[i] <- "Loblowitz Ernst, Stefan"}
                if(x[i] == "Loblowitz"){o[i] <- "Loblowitz Ernst, Stefan"}
                if(x[i] == "Lombardo, Ricardo"){o[i] <- "Lombardo, Ricardo J."}
                if(x[i] == "Long"){o[i] <- "Long Garat, Ruperto Enzo"}
                if(x[i] == "Lopez Balestra, Oscar"){o[i] <- "Lopez Balestra, Oscar C."}
                if(x[i] == "Lopez martinez"){o[i] <- "Lopez Martinez, Nestor"}
                if(x[i] == "Maestre Diaz"){o[i] <- "Maestre Diaz, Washington Leonel" }
                if(x[i] == "Mahia, Jose"){o[i] <- "Mahia, Jose Carlos"}
                if(x[i] == "Mahia, Jose C."){o[i] <- "Mahia, Jose Carlos"}
                if(x[i] == "Manzi"){o[i] <- "Manzi Santos, Miguel Angel"}
                if(x[i] == "Matos Pugliese, Julio C."){o[i] <- "Mattos Pugliese, Julio C."}
                if(x[i] == "Mattos Pugliese, Julio"){o[i] <- "Mattos Pugliese, Julio C."}
                if(x[i] == "Mc Alister"){o[i] <- "Mc Alister Green, Ernesto Juan"}
                if(x[i] == "Mederos, Carminillo"){o[i] <- "Mederos Da Costa, Carminillo"}
                if(x[i] == "Melo Santamarina, Eden"){o[i] <- "Melo Santa Marina, Eden"}
                if(x[i] == "Millor, Pablo"){o[i] <- "Millor Coccaro, Pablo"}
                if(x[i] == "Muniz Durand"){o[i] <- "Muniz Durand, Horacio" }
                if(x[i] == "Muñoz"){o[i] <- "Muñoz Cosentino, Clemente"}
                if(x[i] == "Nan Sauleda"){o[i] <- "Nan Sauleda, Luis Mario"}
                if(x[i] == "Nion"){o[i] <- "Nion, Antonio"}
                if(x[i] == "Olazabal"){o[i] <- "Olazabal, Walter"}
                if(x[i] == "Ovalle"){o[i] <- "Ovalle, Jose Luis"}
                if(x[i] == "Oxacelhay, Juan"){o[i] <- "Oxacelhay, Juan Antonio"}
                if(x[i] == "Pacheco Klein, Jorge"){o[i] <- "Pacheco Klein, Jorge Manuel"}
                if(x[i] == "Paysse, Daniela"){o[i] <- "Paysee, Daniela"}
                if(x[i] == "Peña, Adriana"){o[i] <- "Peña Hernandez, Adriana"}
                if(x[i] == "Perdomo, Alberto"){o[i] <- "Perdomo Gamarra, Alberto"}
                if(x[i] == "Pereira, Ramon"){o[i] <- "Pereira Paben, Ramon"}
                if(x[i] == "Perez Alvarez"){o[i] <- "Perez Alvarez, Manuel"}
                if(x[i] == "Pica, Humberto"){o[i] <- "Pica Ferrari, Humberto" }
                if(x[i] == "Ponce de Leon, Martin"){o[i] <- "Ponce De Leon, Martin"}
                if(x[i] == "Porras, Elias Alberto"){o[i] <- "Porras Larralde, Elias Alberto" }
                if(x[i] == "Pou, Ma Julia"){o[i] <- "Pou, Maria Julia"}
                if(x[i] == "Pozzolo, Luis B."){o[i] <- "Pozzolo, Luis Bernardo"}
                if(x[i] == "Requiterena, Alfonso"){o[i] <- "Requiterena Vogt, Alfonso"}
                if(x[i] == "Riesgo, Walter"){o[i] <- "Riesgo Larraz, Walter"}
                if(x[i] == "Rocha Imaz, Ricardo"){o[i] <- "Rocha Imaz, Ricardo Juan"}
                if(x[i] == "Rodino, Eduardo"){o[i] <- "Rodino Varela, Eduardo"}
                if(x[i] == "Rodriguez Apelo"){o[i] <- "Rodriguez Apelo, Raul"}
                if(x[i] == "Rodriguez Camusso, A. Francisco"){o[i] <- "Rodriguez Camusso, Francisco"}
                if(x[i] == "Rondan, Juan C"){o[i] <- "Rondan, Juan Carlos"}
                if(x[i] == "Rossi,  Victor"){o[i] <- "Rossi, Victor"}
                if(x[i] == "Ruiz Morena"){o[i] <- "Ruiz Morena, Ruben Alberico"}
                if(x[i] == "Sanchez Cal, Dardo"){o[i] <- "Sanchez Cal, Dardo Angel"}
                if(x[i] == "Sande, Pedro"){o[i] <- "Sande, Adolfo Pedro"}
                if(x[i] == "Sanguinetti, Julio M."){o[i] <- "Sanguinetti, Julio Maria"}
                if(x[i] == "Sanguinetti, Jullio M."){o[i] <- "Sanguinetti, Julio M."}
                if(x[i] == "Saravia, Diana"){o[i] <- "Saravia Olmos, Diana"}
                if(x[i] == "Sedarri, Edison"){o[i] <- "Sedarri Luaces, Edison"}
                if(x[i] == "Senatore, Luis"){o[i] <- "Senatore, Luis A."}
                if(x[i] == "Siazaro Andreotti, Juan C,"){o[i] <- "Siazaro Andreotti, Juan C."}
                if(x[i] == "Silveira Zabala, Jorge"){o[i] <- "Silveira Zavala, Jorge Eladio"}
                if(x[i] == "Silveira Zavala"){o[i] <- "Silveira Zavala, Jorge Eladio"}
                if(x[i] == "Singer, Juan A." ){o[i] <- "Singer, Juan Adolfo" }
                if(x[i] == "Singlet"){o[i] <- "Singlet, Manuel Maria"}
                if(x[i] == "Singlet, Manuel"){o[i] <- "Singlet, Manuel Maria"}
                if(x[i] == "Soares de Lima"){o[i] <- "Soares De Lima, Carlos"}
                if(x[i] == "Soto Bermudez, Carlos N."){o[i] <- "Soto Bermudez, Carlos Norberto"}
                if(x[i] == "Stirling, Guilllermo"){o[i] <- "Stirling, Guillermo"}
                if(x[i] == "Storace, Nicolas"){o[i] <- "Storace Montes, Nicolas"}
                if(x[i] == "Suarez Lerena, Carlos"){o[i] <- "Suarez Lerena, Carlos Enrique"}
                if(x[i] == "Toledo, Hermes"){o[i] <- "Toledo Antunez, Hermes"}
                if(x[i] == "Tovagliari"){o[i] <- "Traversoni, Alfredo"}
                if(x[i] == "Varela, Carlos"){o[i] <- "Varela Nestier, Carlos"}
                if(x[i] == "Vega, Alvaro"){o[i] <- "Vega Llanes, Alvaro"}
                if(x[i] == "Vinci"){o[i] <- "Vinci Lopez, Leonardo Jose"}
                if(x[i] == "Zaffaroni Ortiz, Alfredo"){o[i] <- "Zaffaroni Ortiz, Alfredo Cesar"}
                if(x[i] == "Zanoniani"){o[i] <- "Zanoniani, Rodolfo"}
      }
return(o)
}





ws_uy <- function(data, legislature){

        base <- data
        legislatura <- legislature
        ## SE PREPARA BASE -----------------------------------------------------
        speech2 <-
                base %>%
                speech_recompiler(compiler_by = c("legislator", "party", "legislature", "chamber"))

        speech2$palabras  = speech_word_count(speech2$speech)
        speech2$ref_score = NA
        speech2$mediana   = median(speech2$palabras)
        speech2 <- speech2[which(speech2$legislature == legislatura), ]




        if(legislatura == 42){
                ## REF LEGISLATURE 42 ==========================================
                speech2[speech2$legislator == "Araujo, Jose German",           "ref_score" ] <- -1
                speech2[speech2$legislator == "Millor Coccaro, Pablo",         "ref_score" ] <- 1
                speech2[speech2$legislator == "Fau, Yamandu",                  "ref_score" ] <- 0
                speech2[speech2$legislator == "Heber, Luis Alberto",           "ref_score" ] <- 0.7
                speech2[speech2$legislator == "Lacalle Herrera, Luis Alberto", "ref_score" ] <- 0.8
                speech2[speech2$legislator == "Gargano, Reinaldo",             "ref_score" ] <- -0.8
        }
        if(legislatura == 43){
                ## REF LEGISLATURE 43 ==========================================
                speech2[speech2$legislator == "Millor Coccaro, Pablo",         "ref_score" ] <- 1
                speech2[speech2$legislator == "Gargano, Reinaldo",             "ref_score" ] <- -0.8
                speech2[speech2$legislator == "Garcia Pintos, Daniel",         "ref_score" ] <- 1
                speech2[speech2$legislator == "Heber, Luis Alberto",           "ref_score" ] <- 0.8
                speech2[speech2$legislator == "Chifflet, Guillermo",           "ref_score" ] <- -1
                speech2[speech2$legislator == "Fau, Yamandu, Pablo",           "ref_score" ] <- 0
        }
        if(legislatura == 44){
                ## REF LEGISLATURE 44 ==========================================
                speech2[speech2$legislator == "Millor Coccaro, Pablo",         "ref_score" ] <- 1
                speech2[speech2$legislator == "Mujica, Jose",                  "ref_score" ] <- -1
                speech2[speech2$legislator == "Fau, Yamandu, Pablo",           "ref_score" ] <- 0
                speech2[speech2$legislator == "Chifflet, Guillermo",           "ref_score" ] <- -1
                speech2[speech2$legislator == "Garcia Pintos, Daniel",         "ref_score" ] <- 1
                speech2[speech2$legislator == "Heber, Luis Alberto",           "ref_score" ] <- 0.8
        }
        if(legislatura == 45){
                ## REF LEGISLATURE 45 ==========================================
                speech2[speech2$legislator == "Millor Coccaro, Pablo",         "ref_score" ] <- 1
                speech2[speech2$legislator == "Garcia Pintos, Daniel",         "ref_score" ] <- 1
                speech2[speech2$legislator == "Mujica, Jose",                  "ref_score" ] <- -1
                speech2[speech2$legislator == "Fau, Yamandu, Pablo",           "ref_score" ] <- 0
                speech2[speech2$legislator == "Chifflet, Guillermo",           "ref_score" ] <- -1
                speech2[speech2$legislator == "Heber, Luis Alberto",           "ref_score" ] <- 0.8
        }
        if(legislatura == 46){
                ## REF LEGISLATURE 46 ==========================================
                speech2[speech2$legislator == "Heber, Luis Alberto",           "ref_score" ] <- 0.8
                speech2[speech2$legislator == "Couriel, Alberto",              "ref_score" ] <- -0.8
                speech2[speech2$legislator == "Rosadilla, Luis",               "ref_score" ] <- -0.8
                speech2[speech2$legislator == "Sanguinetti, Julio Maria",      "ref_score" ] <- 0.8
                speech2[speech2$legislator == "Penades, Gustavo",              "ref_score" ] <- 0.8
                speech2[speech2$legislator == "Topolansky, Lucia",             "ref_score" ] <- -1
        }

        ## CORPUS --------------------------------------------------------------
        corpus_speech <-  corpus(speech2$speech)
        docvars(corpus_speech, "ref_score")   <- speech2$ref_score
        docvars(corpus_speech, "Party")       <- speech2$party
        docvars(corpus_speech, "legislador")  <- speech2$legislator

        ## SCALING -------------------------------------------------------------
        dfm_42 <- dfm(corpus_speech,
                      stem = TRUE,
                      tolower = TRUE,
                      remove = c(stopwords("spanish"),"Uruguay","uru*","senador","ley","artículo","proyecto"),
                      remove_punct = TRUE,
                      remove_numbers = TRUE,
                      verbose = TRUE)

        ## MODEL ---------------------------------------------------------------
        modelo_42<- textmodel_wordscores(dfm_42, y = docvars(corpus_speech, "ref_score"), smooth = 1)
        pred_ws_42 <- predict(modelo_42, se.fit = TRUE, newdata = dfm_42)
        names(pred_ws_42$fit) <- speech2$legislator
        #textplot_scale1d(pred_ws_42, groups = speech2$party)

        out <- data.frame(legislador  = names(pred_ws_42$fit),
                          score       = unname(pred_ws_42$fit),
                          error       = pred_ws_42$se.fit,
                          legislatura = legislatura,
                          partido     = speech2$party,
                          metodo      = "ws",
                          stringsAsFactors = FALSE)
        out
}





wfish <- function(data, legislature){

     base <- data
     legislatura <- legislature

     ## SE PREPARA BASE -------------------------------------------------------------
     speech2 <-
          base %>%
          filter(chamber == "ASAMBLEA GENERAL") %>%
          speech_recompiler(compiler_by = c("legislator", "party", "legislature", "chamber"))


     speech2$palabras  = speech_word_count(speech2$speech)
     speech2$ref_score = NA
     speech2$mediana   = median(speech2$palabras)
     speech2 <- speech2[which(speech2$legislature == legislatura), ]

     ## SE PREPARA CORPUS -----------------------------------------------------------
     corpus_speech <-  corpus(speech2$speech)
     docvars(corpus_speech, "Party")       <- speech2$party
     docvars(corpus_speech, "legislador")  <- speech2$legislator

     ## SE INICIA ESCALAMIENTO ------------------------------------------------------
     dfm_tot <- dfm(corpus_speech,
            stem = TRUE,
            tolower = TRUE,
            remove = c(stopwords("spanish"),"Uruguay","uru*","senador","ley","artículo","proyecto"),
            remove_punct = TRUE,
            remove_numbers = TRUE,
            verbose = TRUE)


     fish <- textmodel_wordfish(dfm_tot, sparse=TRUE)
     fish$docs <- speech2$legislator
     datos_ll <- data.frame(legislador  = fish$docs,
                            score       = fish$theta,
                            error       = fish$se.theta,
                            partido     = speech2$party,
                            legislature = legislature)
     datos_ll
}



