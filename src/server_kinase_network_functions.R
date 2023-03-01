
foKinaseNetworkSubset <- function(ST, NetworkData, indices, Wkin2site, Wkin2onsite){
  ST = ST[!is.na(indices), ]
  indices = indices[!is.na(indices)]
  Wk2s = Wkin2site[, indices]
  Wk2os = Wkin2onsite[, indices]
  validKins = (rowSums(Wk2s) + rowSums(Wk2os)) > 0
  validSites = (colSums(Wk2s) + colSums(Wk2os)) > 0
  KT = NetworkData$Kinase[validKins, ]
  ST = ST[validSites, ]
  Wk2s = Wk2s[validKins, validSites]
  Wk2os = Wk2os[validKins, validSites]
  return(list(KT = KT, ST = ST, Wk2s = Wk2s, Wk2os = Wk2os))
}

foKinaseNetworkDraw <- function(K, KT, Wk2s, Wk2os, minzscore, topk, keepsinglekinases, items_txt, footer_txt, show_significant_only){
  thereAreNoItemsError = paste("There are no", items_txt, "that can pass the specified threshold with a known kinase.")
  valids = (abs(K$ZScore) >= minzscore)
  if(show_significant_only == T){
    valids = valids & K$isSignificant
  }
  validate(
    need(nnzero(valids) > 1, thereAreNoItemsError)
  )
  
  
  Ks <- K[valids,]
  Wk2s = Wk2s[, valids]
  Wk2os = Wk2os[, valids]
  
  zstar = 3
  si <- order(abs(Ks$Phos) - zstar*Ks$StdErr, decreasing = TRUE)
  valids <- si[1:min(topk, length(si))]
  Ks <- Ks[valids, ]
  Wk2s <- Wk2s[, valids]
  Wk2os <- Wk2os[, valids]
  
  #validKins = (rowSums(Wk2s) + rowSums(Wk2os)) > !keepsinglekinases
  validKins = (rowSums(Wk2s) > !keepsinglekinases) | (rowSums(Wk2os) > 0)
  
  validate(
    need(nnzero(validKins) > 0, "There are no kinases to show for the specified options.")
  )
  
  KT = KT[validKins, ]
  
  if(nnzero(validKins)  == 1){
    Wk2s = t(as.matrix(Wk2s[validKins, ]))
    Wk2os = t(as.matrix(Wk2os[validKins, ]))
  } else {
    Wk2s =(Wk2s[validKins, ])
    Wk2os = (Wk2os[validKins, ])
  }
  
  validSites = (colSums(Wk2s) + colSums(Wk2os)) > 0
  Ks = Ks[validSites, ]
  Wk2s = Wk2s[, validSites]
  Wk2os = Wk2os[, validSites]
  
  validate(
    need(nnzero(validSites) > 0, thereAreNoItemsError)
  )
  
  nItem = nrow(Ks)
  nKinase = nrow(KT)
  
  c_limit = 4
  coloringVar = Ks$ZScore
  #Ks$ColoringVar = pmax( -1*c_limit, pmin(Ks$ColoringVar, c_limit))
  
  aconst = 1.4;
  fq <- function(x) 1 - 2/(1+2^(x/aconst));
  qScaled = as.matrix(fq(coloringVar))
  n = nrow(qScaled)
  fc <- function(a,b,c) t(as.matrix(c(a,b,c)))
  mA = (as.matrix(pmax(qScaled, 0)) %*% fc(+0.04, -0.96, -0.96))
  mB = (as.matrix(abs(pmin(qScaled, 0))) %*% fc(-0.96, -0.96, +0.04))
  clx <- matrix(0.96, nrow = n, ncol = 3) + mA + mB
  
  color_hex = apply(clx, 1, function(x) rgb(x[1], x[2], x[3]))
  
  # color_hex = sapply(colors, function(x) rgb(x[1], x[2], x[3], maxColorValue=255))
  
  
  siteIds = nKinase + (1:nItem)
  kinaseIds = 1:nKinase
  phosConf = paste0(round(Ks$Phos, digits = 2), 
                    " [", round(Ks$Phos - 1.96*Ks$StdErr * sign(Ks$Phos), digits = 2), 
                    ", ", round(Ks$Phos + 1.96*Ks$StdErr * sign(Ks$Phos), digits = 2), "]", sep = "")
  
  item_txt = paste(toupper(substr(items_txt, 1, 1)), substr(items_txt, 2, nchar(items_txt) - 1), sep = "")
  
  line1 = c(rep("", nKinase), paste0("<br> Z-Score: ", round(Ks$ZScore, digits = 3), sep = ""))
  line2 = c(rep("", nKinase), paste0("<br> log2-FC: ", phosConf, sep = ""))
  line3 = c(rep("", nKinase), paste0("<br> FDR: ", round(Ks$FDR, digits = 3), sep = ""))
  
  
  names = c(KT$KinaseName, Ks$ID)
  nodes = data.frame(id = c(kinaseIds, siteIds), label = names,
                     group = c(rep("Kinase", nKinase), rep(item_txt, nItem)),
                     color = c(rep("orange", nKinase), color_hex),
                     title = paste0("<b>", names, "</b>", line1, line2, line3))
  
  a <- which(Wk2s, arr.ind = T)
  nRowA = nrow(a)
  if(length(a) > 0){
    a[, 2] =  a[, 2] + nKinase
  } else {
    nRowA = 0
  }
  b <- which(t(Wk2os), arr.ind = T)
  nRowB = nrow(b)
  if(length(b) > 0){
    b[, 1] =  b[, 1] + nKinase
  } else {
    nRowB =0
  }
  C = rbind(a, b)
  
  edges = data.frame(from = C[, 1], to = C[, 2], 
                     width = c(rep(6, nRowA), rep(10, nRowB)),
                     color = c(rep("black", nRowA), rep("#EE9900", nRowB)),
                     arrows = c(rep("to", nRowA), rep("", nRowB))
  )
  
  ledges = data.frame(width = c(20, 30), 
                      color = c("black", "EE9900"),
                      label = c("Phosphorylates", "Is on the kinase"), 
                      arrows = c("", ""),
                      shadow = c(F, F))
  
  # visNetwork(nodes, edges, width = "100%",
  #            main = paste("Kinases connected to the identified", items_txt),
  #            footer = list(text = footer_txt, style = "font-size:13px;")) %>%
  visNetwork(nodes, edges, width = "100%",
             main = paste("Kinases connected to the identified", items_txt)) %>%
    visEdges(shadow = FALSE,
             arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
             color = list(color = "black", highlight = "red")) %>%
    visGroups(groupname = "Kinase", color = "orange", shape = "rectangle", 
              shadow = list(enabled = TRUE), font = list(size = 30)) %>% 
    visGroups(groupname = item_txt, color = "darkred", shape = "square", font = list(size = 26)) %>%
    visLegend(width = 0.13, position = "right") %>%
    visEdges(smooth = FALSE) %>%
    visInteraction(hideEdgesOnDrag = TRUE) %>%
    visInteraction(navigationButtons = TRUE) %>%
    visLayout(randomSeed = 100) %>%
    visEvents(doubleClick = c("function(properties) {",
                              " label = this.body.data.nodes.get(properties.nodes[0]).label", 
                              " group = this.body.data.nodes.get(properties.nodes[0]).group", 
                              " txt = label.concat('_', group)", 
                              " Shiny.setInputValue('site_kinase_network_doubleclick', txt, {priority: 'event'});",
                              "}")) %>%
    # visExport(type = "jpeg", name = "export-network", 
    #           float = "left", label = "Save network", 
    #           background = "purple", style= "") %>% 
    # visEvents(doubleClick = "function(properties) {
    # alert('selected nodes ' + this.body.data.nodes.get(properties.nodes[0]).id);}") %>%
    visPhysics(stabilization = T) 

}