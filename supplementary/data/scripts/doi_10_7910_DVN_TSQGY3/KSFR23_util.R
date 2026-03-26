###Q###Q###Q###Q###Q###Q
###Q###Q  UTIL  ###Q###Q
###Q###Q###Q###Q###Q###Q
# This script contains functions for creating custom contrast vectors for the emmeans package

## functions for creating contrast vectors ====
contrasterParty = function(object, treat, party, treat.ref = 'Cont'){
  stopifnot(class(object) == "emmGrid" & 
              length(treat) == length(party))
  out = list()
  obj.df = as.data.frame(object)
  for(i in 1:length(treat)){
    ref = numeric(nrow(obj.df))
    ref[which(obj.df$treat == treat.ref & 
                obj.df$PartySupport == party[i])] = 1
    ref[which(obj.df$treat == treat[i] & 
                obj.df$PartySupport == party[i])] = -1
    
    out[[i]] = -ref
  }
  names(out) = paste0(treat, party)
  
  # return difference
  out
}
contrasterParty2 = function(object, treat, party, treat.ref = 'Cont'){
  stopifnot(class(object) == "emmGrid" & 
              length(treat) == length(party))
  out = list()
  obj.df = as.data.frame(object)
  for(i in 1:length(treat)){
    ref = numeric(nrow(obj.df))
    n_party = str_count(party[i], '\\|') + 1
    ref[which(obj.df$treat == treat.ref & 
                grepl(party[i], obj.df$PartySupport))] = 1/n_party
    ref[which(obj.df$treat == treat[i] & 
                grepl(party[i], obj.df$PartySupport))] = -1/n_party
    
    out[[i]] = -ref # treatment - control
  }
  names(out) = paste0(treat, party)
  
  # return difference
  out
}

## contrasterLevel() ## 
# function to create custom contrasts
# takes the desired treatment, level and (optionally) party as input
# it then contrasts the difference between A and B (A-B) where 
# A is the marginal effect of the treatment compared to treat.ref (defaults to Cont) at level.ref (defaults to NatNat)
# B is the marginal effect of the treatment compared to treat.ref at level
# Thus in the context of the paper, the estimated contrast A - B is 
# zero when there is no difference between the treatments across levels,
# positive when relative punishment is weaker for level (i.e. B) than level.ref (i.e. A) (and negative vice versa)
contrasterLevel = function(object, treat, level, party, treat.ref = 'Cont', level.ref = 'NatNat'){
  stopifnot(class(object) == "emmGrid" & 
              length(treat) == length(level))
  out = list()
  obj.df = as.data.frame(object)
  if(missing(party)){
    for(i in 1:length(treat)){
      ref = com = numeric(nrow(obj.df))
      # NatNat Reference
      ref[which(obj.df$treat == treat.ref & obj.df$level == level.ref)] = 1
      ref[which(obj.df$treat == treat[i] & obj.df$level == level.ref)] = -1
      
      # Comparisons
      com[which(obj.df$treat == treat.ref & obj.df$level == level[i])] = 1
      com[which(obj.df$treat == treat[i] & obj.df$level == level[i])] = -1
      
      out[[i]] = ref - com
    }
    names(out) = paste0(treat, level)
  } else {
    for(i in 1:length(treat)){
      ref = com = numeric(nrow(obj.df))
      # NatNat Reference
      ref[which(obj.df$treat == treat.ref & 
                  obj.df$level == level.ref &
                  obj.df$PartySupport == party[i])] = 1
      ref[which(obj.df$treat == treat[i] & 
                  obj.df$level == level.ref &
                  obj.df$PartySupport == party[i])] = -1
      
      # Comparisons
      com[which(obj.df$treat == treat.ref & 
                  obj.df$level == level[i] &
                  obj.df$PartySupport == party[i])] = 1
      com[which(obj.df$treat == treat[i] & 
                  obj.df$level == level[i] &
                  obj.df$PartySupport == party[i])] = -1
      
      out[[i]] = ref - com
    }
    names(out) = paste0(treat, level, party)
  }
  
  # return difference
  out
}

contrasterInfo = function(object, treat, EUinfo_bin, party, EUinfo_bin.ref = 'High'){
  stopifnot(class(object) == "emmGrid" & 
              length(treat) == length(EUinfo_bin))
  out = list()
  obj.df = as.data.frame(object)
  if(missing(party)){
    for(i in 1:length(treat)){
      ref = com = numeric(nrow(obj.df))
      # NatNat Reference
      ref[which(obj.df$treat == treat[i] & obj.df$EUinfo_bin == EUinfo_bin.ref)] = 1
      ref[which(obj.df$treat == treat[i] & obj.df$EUinfo_bin == EUinfo_bin[i])] = -1
      
      out[[i]] = ref
    }
    names(out) = paste0(treat, EUinfo_bin)
  } else {
    for(i in 1:length(treat)){
      ref = com = numeric(nrow(obj.df))
      # NatNat Reference
      ref[which(obj.df$treat == treat[i] &
                  obj.df$EUinfo_bin == EUinfo_bin.ref &
                  obj.df$PartySupport == party[i])] = 1
      ref[which(obj.df$treat == treat[i] & 
                  obj.df$EUinfo_bin == EUinfo_bin[i] &
                  obj.df$PartySupport == party[i])] = -1
      
      out[[i]] = ref
    }
    names(out) = paste0(treat, EUinfo_bin, party)
  }
  
  # return contrast
  out
}

contrasterLevelInfo = function(object, treat, level, EUinfo_bin, treat.ref = 'Cont', level.ref = 'NatNat'){
  stopifnot(class(object) == "emmGrid" & 
              length(treat) == length(level))
  out = list()
  obj.df = as.data.frame(object)
  if(missing(EUinfo_bin)){
    for(i in 1:length(treat)){
      ref = com = numeric(nrow(obj.df))
      # NatNat Reference
      ref[which(obj.df$treat == treat.ref & obj.df$level == level.ref)] = 1
      ref[which(obj.df$treat == treat[i] & obj.df$level == level.ref)] = -1
      
      # Comparisons
      com[which(obj.df$treat == treat.ref & obj.df$level == level[i])] = 1
      com[which(obj.df$treat == treat[i] & obj.df$level == level[i])] = -1
      
      out[[i]] = ref - com
    }
    names(out) = paste0(treat, level)
  } else {
    for(i in 1:length(treat)){
      ref = com = numeric(nrow(obj.df))
      # NatNat Reference
      ref[which(obj.df$treat == treat.ref & 
                  obj.df$level == level.ref &
                  obj.df$EUinfo_bin == EUinfo_bin[i])] = 1
      ref[which(obj.df$treat == treat[i] & 
                  obj.df$level == level.ref &
                  obj.df$EUinfo_bin == EUinfo_bin[i])] = -1
      
      # Comparisons
      com[which(obj.df$treat == treat.ref & 
                  obj.df$level == level[i] &
                  obj.df$EUinfo_bin == EUinfo_bin[i])] = 1
      com[which(obj.df$treat == treat[i] & 
                  obj.df$level == level[i] &
                  obj.df$EUinfo_bin == EUinfo_bin[i])] = -1
      
      out[[i]] = ref - com
    }
    names(out) = paste0(treat, level, EUinfo_bin)
  }
  
  # return difference
  out
}
