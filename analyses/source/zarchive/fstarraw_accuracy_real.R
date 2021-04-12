### Started 3 March 2021 by Cat
## Source function to figure out most accurate method or site to determine fstar estimate using raw data

fstarrawfuncreal <- function(df){
  
  arb <- df[(df$site=="arb"),]
  arb$mingdd <- ave(arb$gdd, arb$species, arb$method, FUN=min)
  
  arb$mincheck <- ave(arb$gdd, arb$species, FUN=min)
  
  hf <- df[(df$site=="hf"),]
  hf$mingdd <- ave(hf$gdd, hf$species, hf$method, FUN=min)
  
  hf$mincheck <- ave(hf$gdd, hf$species, FUN=min)
  
  arb$mingdd_ws <- NA
  arb$mingdd_hobo <- NA
  hf$mingdd_ws <- NA
  hf$mingdd_hobo <- NA
  min_ws_arb <- vector()
  min_hobo_arb <- vector()
  min_ws_hf <- vector()
  min_hobo_hf <- vector()
  for(i in unique(arb$spp)){ #i="Tilia_americana"
    min_ws_arb <- arb$mingdd[arb$method=="ws" & arb$spp==i]
    arb$mingdd_ws[arb$spp==i ] <- unique(min_ws_arb)
    
    min_hobo_arb <- arb$mingdd[arb$method=="hobo" & arb$spp==i]
    arb$mingdd_hobo[arb$spp==i] <- unique(min_hobo_arb)
  }
  for(i in unique(hf$spp)){   
    min_ws_hf <- hf$mingdd[hf$method=="ws" & hf$spp==i]
    hf$mingdd_ws[hf$spp==i] <- unique(min_ws_hf)
    
    min_hobo_hf <- hf$mingdd[hf$method=="hobo" & hf$spp==i]
    hf$mingdd_hobo[hf$spp==i] <- unique(min_hobo_hf)
  }
  
  hobo_arb <- max(arb$mingdd_hobo) - min(arb$mingdd_hobo)
  ws_arb <- max(arb$mingdd_ws) - min(arb$mingdd_ws)
  
  hobo_hf <- max(hf$mingdd_hobo) - min(hf$mingdd_hobo)
  ws_hf <- max(hf$mingdd_ws) - min(hf$mingdd_ws)
  
  accuracylist <- list(hobo_arb=hobo_arb, ws_arb=ws_arb, 
                       hobo_hf=hobo_hf, ws_hf=ws_hf)
  best_sitemethod <- names(which.min(accuracylist))
  
  if(best_sitemethod=="hobo_arb"){
    
    for(i in unique(arb$spp)){ #i="Tilia_americana"
      hf$mingdd_hobo[i==hf$spp] <- unique(arb$mingdd_hobo[i==arb$spp])
    }
      arbhf <- rbind(arb, hf)
      arbhf$fstarspp_raw <- arbhf$mingdd_hobo
      df$fstarspp_raw<-NA
      for(i in unique(arbhf$spp)){ #i="Quercus_alba"
        df$fstarspp_raw[i==df$spp] <- unique(arbhf$fstarspp_raw[i==arbhf$spp])
      }
      df$accuracy_type <- "hobo_arb"
      
  } else if(best_sitemethod=="ws_arb"){
    
    for(i in unique(arb$spp)){
      hf$mingdd_ws[i==hf$spp] <- unique(arb$mingdd_ws[i==arb$spp])
    }
    arbhf <- rbind(arb, hf)
    arbhf$fstarspp_raw <- arbhf$mingdd_ws
    df$fstarspp_raw<-NA
    for(i in unique(arbhf$spp)){ #i="Quercus_alba"
      df$fstarspp_raw[i==df$spp] <- unique(arbhf$fstarspp_raw[i==arbhf$spp])
    }
    df$accuracy_type <- "ws_arb"
    
  } else if(best_sitemethod=="hobo_hf"){
    
    for(i in unique(hf$spp)){ #i="Acer_saccharum"
      arb$mingdd_hobo[i==arb$spp] <- unique(hf$mingdd_hobo[i==hf$spp])
    }
    arbhf <- rbind(arb, hf)
    arbhf$fstarspp_raw <- arbhf$mingdd_hobo
    df$fstarspp_raw<-NA
    for(i in unique(arbhf$spp)){ #i="Quercus_alba"
      df$fstarspp_raw[i==df$spp] <- unique(arbhf$fstarspp_raw[i==arbhf$spp])
    }
    df$accuracy_type <- "hobo_hf"
    
  } else if(best_sitemethod=="ws_hf"){
    
    for(i in unique(hf$spp)){ #i="Quercus_alba"
      arb$mingdd_ws[i==arb$spp] <- unique(hf$mingdd_ws[i==hf$spp])
    }
    arbhf <- rbind(arb, hf)
    arbhf$fstarspp_raw <- arbhf$mingdd_ws
    df$fstarspp_raw<-NA
    for(i in unique(arbhf$spp)){ #i="Acer_pensylvanicum"
      df$fstarspp_raw[i==df$spp] <- unique(arbhf$fstarspp_raw[i==arbhf$spp])
    }
    df$accuracy_type <- "ws_hf"
    
  }
  
  df$gdd_accuracy_raw <- df$gdd - df$fstarspp_raw
  
  return(df)
}