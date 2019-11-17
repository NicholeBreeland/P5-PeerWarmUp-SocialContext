#P5 Peer Warm Up Social Context Reliability ---------------------------------
  
  all.data = read.csv("P5_WarmUp_SocialContext.csv", stringsAsFactors = FALSE)
  
  options(scipen = 999)
  
  all.data[all.data == 888] <- NA
  
  #999 = child missing from view, 
  #coded as 999 but recode as 0 as to not overinflate ICC's but to include for reliability.
  all.data[all.data == 999] <- 0
  
  table(all.data$Coders)
  
  names(all.data)
  
  #Coder categories for reliability check
  
  #BC_NB = 1
  #BC_RP = 2

  
  all.data$P5_WarmUp_CoderCategory[all.data$Coders == "BC_NB"] <- 1
  all.data$P5_WarmUp_CoderCategory[all.data$Coders == "BC_RP"] <- 2

  
  
  
  #Nichole Scores
  
  all.data$Child_A_Afill_NB <- rowMeans(all.data[,5:10], na.rm=TRUE)
  all.data$Child_A_Antag_NB <- rowMeans(all.data[,11:16], na.rm=TRUE)
  all.data$Child_A_Eng_NB <- rowMeans(all.data[,17:22], na.rm=TRUE)
  all.data$Child_A_JCE_NB <- rowMeans(all.data[,23:28], na.rm=TRUE)
  all.data$Child_A_Contrib_NB <- rowMeans(all.data[,29:31], na.rm=TRUE)
  
  all.data$Child_B_Afill_NB <- rowMeans(all.data[,113:118], na.rm=TRUE)
  all.data$Child_B_Antag_NB <- rowMeans(all.data[,119:124], na.rm=TRUE)
  all.data$Child_B_Eng_NB <- rowMeans(all.data[,125:130], na.rm=TRUE)
  all.data$Child_B_JCE_NB <- rowMeans(all.data[,131:136], na.rm=TRUE)
  all.data$Child_B_Contrib_NB <- rowMeans(all.data[,137:139], na.rm=TRUE)
  
  #Rebecca scores
  all.data$Child_A_Afill_RP <- rowMeans(all.data[,32:37], na.rm=TRUE)
  all.data$Child_A_Antag_RP <- rowMeans(all.data[,38:43], na.rm=TRUE)
  all.data$Child_A_Eng_RP <- rowMeans(all.data[,44:49], na.rm=TRUE)
  all.data$Child_A_JCE_RP <- rowMeans(all.data[,50:55], na.rm=TRUE)
  all.data$Child_A_Contrib_RP <- rowMeans(all.data[,56:58], na.rm=TRUE)
  
  all.data$Child_B_Afill_RP <- rowMeans(all.data[,140:145], na.rm=TRUE)
  all.data$Child_B_Antag_RP <- rowMeans(all.data[,146:151], na.rm=TRUE)
  all.data$Child_B_Eng_RP <- rowMeans(all.data[,152:157], na.rm=TRUE)
  all.data$Child_B_JCE_RP <- rowMeans(all.data[,158:163], na.rm=TRUE)
  all.data$Child_B_Contrib_RP <- rowMeans(all.data[,164:166], na.rm=TRUE)
  
  #Buket scores
  
  all.data$Child_A_Afill_BC <- rowMeans(all.data[,59:64], na.rm=TRUE)
  all.data$Child_A_Antag_BC <- rowMeans(all.data[,65:70], na.rm=TRUE)
  all.data$Child_A_Eng_BC <- rowMeans(all.data[,71:76], na.rm=TRUE)
  all.data$Child_A_JCE_BC <- rowMeans(all.data[,77:82], na.rm=TRUE)
  all.data$Child_A_Contrib_BC <- rowMeans(all.data[,83:85], na.rm=TRUE)
  
  all.data$Child_B_Afill_BC <- rowMeans(all.data[,167:172], na.rm=TRUE)
  all.data$Child_B_Antag_BC <- rowMeans(all.data[,173:178], na.rm=TRUE)
  all.data$Child_B_Eng_BC <- rowMeans(all.data[,179:184], na.rm=TRUE)
  all.data$Child_B_JCE_BC <- rowMeans(all.data[,185:190], na.rm=TRUE)
  all.data$Child_B_Contrib_BC <- rowMeans(all.data[,191:193], na.rm=TRUE)
  
  
  #data subsets for raters
  
  #subset for pair 1
  sc.df_BC_NB <- all.data[which(all.data$P4_WarmUp_CoderCategory == 1),]
  
  #subset for pair 2
  sc.df_BC_RP <- all.data[which(all.data$P4_WarmUp_CoderCategory == 2),]
  
  
  
  #pair 1 variable subsets
  
  sc.df_BC_NB_afill <- subset(sc.df_BC_NB, select = c("Child_A_Afill_BC", "Child_B_Afill_BC", 
                                                      "Child_A_Afill_NB", "Child_B_Afill_NB"))
  sc.df_BC_NB_afill <- data.frame(Child_A_Afill_BC = c(sc.df_BC_NB_afill[,"Child_A_Afill_BC"], 
                                                       sc.df_BC_NB_afill[,"Child_B_Afill_BC"]),
                                  Child_A_Afill_NB = c(sc.df_BC_NB_afill[,"Child_A_Afill_NB"], 
                                                       sc.df_BC_NB_afill[,"Child_B_Afill_NB"]))
  
  
  sc.df_BC_NB_antag <- subset(sc.df_BC_NB, select = c("Child_A_Antag_BC", "Child_B_Antag_BC", 
                                                      "Child_A_Antag_NB", "Child_B_Antag_NB"))
  sc.df_BC_NB_antag <- data.frame(Child_A_Antag_BC = c(sc.df_BC_NB_antag[,"Child_A_Antag_BC"], 
                                                       sc.df_BC_NB_antag[,"Child_B_Antag_BC"]),
                                  Child_A_Antag_NB = c(sc.df_BC_NB_antag[,"Child_A_Antag_NB"], 
                                                       sc.df_BC_NB_antag[,"Child_B_Antag_NB"]))
  
  sc.df_BC_NB_eng <- subset(sc.df_BC_NB, select = c("Child_A_Eng_BC", "Child_B_Eng_BC", 
                                                    "Child_A_Eng_NB", "Child_B_Eng_NB"))
  sc.df_BC_NB_eng <- data.frame(Child_A_Eng_BC = c(sc.df_BC_NB_eng[,"Child_A_Eng_BC"], 
                                                   sc.df_BC_NB_eng[,"Child_B_Eng_BC"]),
                                Child_A_Eng_NB = c(sc.df_BC_NB_eng[,"Child_A_Eng_NB"], 
                                                   sc.df_BC_NB_eng[,"Child_B_Eng_NB"]))
  
  sc.df_BC_NB_jce <- subset(sc.df_BC_NB, select = c("Child_A_JCE_BC", "Child_B_JCE_BC", 
                                                    "Child_A_JCE_NB", "Child_B_JCE_NB"))
  sc.df_BC_NB_jce <- data.frame(Child_A_JCE_BC = c(sc.df_BC_NB_jce[,"Child_A_JCE_BC"], 
                                                   sc.df_BC_NB_jce[,"Child_B_JCE_BC"]),
                                Child_A_JCE_NB = c(sc.df_BC_NB_jce[,"Child_A_JCE_NB"], 
                                                   sc.df_BC_NB_jce[,"Child_B_JCE_NB"]))
  
  sc.df_BC_NB_contrib <- subset(sc.df_BC_NB, select = c("Child_A_Contrib_BC", "Child_B_Contrib_BC", 
                                                        "Child_A_Contrib_NB", "Child_B_Contrib_NB"))
  sc.df_BC_NB_contrib <- data.frame(Child_A_Contrib_BC = c(sc.df_BC_NB_contrib[,"Child_A_Contrib_BC"], 
                                                           sc.df_BC_NB_contrib[,"Child_B_Contrib_BC"]),
                                    Child_A_Contrib_NB = c(sc.df_BC_NB_contrib[,"Child_A_Contrib_NB"], 
                                                           sc.df_BC_NB_contrib[,"Child_B_Contrib_NB"]))
  
  
  
  #pair 2 variable subsets
  
  sc.df_BC_RP_afill <- subset(sc.df_BC_RP, select = c("Child_A_Afill_BC", "Child_B_Afill_BC", 
                                                      "Child_A_Afill_RP", "Child_B_Afill_RP"))
  sc.df_BC_RP_afill <- data.frame(Child_A_Afill_BC = c(sc.df_BC_RP_afill[,"Child_A_Afill_BC"], 
                                                       sc.df_BC_RP_afill[,"Child_B_Afill_BC"]),
                                  Child_A_Afill_RP = c(sc.df_BC_RP_afill[,"Child_A_Afill_RP"], 
                                                       sc.df_BC_RP_afill[,"Child_B_Afill_RP"]))
  
  
  sc.df_BC_RP_antag <- subset(sc.df_BC_RP, select = c("Child_A_Antag_BC", "Child_B_Antag_BC", 
                                                      "Child_A_Antag_RP", "Child_B_Antag_RP"))
  sc.df_BC_RP_antag <- data.frame(Child_A_Antag_BC = c(sc.df_BC_RP_antag[,"Child_A_Antag_BC"], 
                                                       sc.df_BC_RP_antag[,"Child_B_Antag_BC"]),
                                  Child_A_Antag_RP = c(sc.df_BC_RP_antag[,"Child_A_Antag_RP"], 
                                                       sc.df_BC_RP_antag[,"Child_B_Antag_RP"]))
  
  sc.df_BC_RP_eng <- subset(sc.df_BC_RP, select = c("Child_A_Eng_BC", "Child_B_Eng_BC", 
                                                    "Child_A_Eng_RP", "Child_B_Eng_RP"))
  sc.df_BC_RP_eng <- data.frame(Child_A_Eng_BC = c(sc.df_BC_RP_eng[,"Child_A_Eng_BC"], 
                                                   sc.df_BC_RP_eng[,"Child_B_Eng_BC"]),
                                Child_A_Eng_RP = c(sc.df_BC_RP_eng[,"Child_A_Eng_RP"], 
                                                   sc.df_BC_RP_eng[,"Child_B_Eng_RP"]))
  
  sc.df_BC_RP_jce <- subset(sc.df_BC_RP, select = c("Child_A_JCE_BC", "Child_B_JCE_BC", 
                                                    "Child_A_JCE_RP", "Child_B_JCE_RP"))
  sc.df_BC_RP_jce <- data.frame(Child_A_JCE_BC = c(sc.df_BC_RP_jce[,"Child_A_JCE_BC"], 
                                                   sc.df_BC_RP_jce[,"Child_B_JCE_BC"]),
                                Child_A_JCE_RP = c(sc.df_BC_RP_jce[,"Child_A_JCE_RP"], 
                                                   sc.df_BC_RP_jce[,"Child_B_JCE_RP"]))
  
  sc.df_BC_RP_contrib <- subset(sc.df_BC_RP, select = c("Child_A_Contrib_BC", "Child_B_Contrib_BC", 
                                                        "Child_A_Contrib_RP", "Child_B_Contrib_RP"))
  sc.df_BC_RP_contrib <- data.frame(Child_A_Contrib_BC = c(sc.df_BC_RP_contrib[,"Child_A_Contrib_BC"], 
                                                           sc.df_BC_RP_contrib[,"Child_B_Contrib_BC"]),
                                    Child_A_Contrib_RP = c(sc.df_BC_RP_contrib[,"Child_A_Contrib_RP"], 
                                                           sc.df_BC_RP_contrib[,"Child_B_Contrib_RP"]))
  
  
  #ICC reliabilities for all pairs all variables
  library(irr)
  
  #Pair 1
  icc(sc.df_BC_NB_afill, model="twoway", type="consistency", unit = "average")
  icc(sc.df_BC_NB_antag, model="twoway", type="consistency", unit = "average")
  icc(sc.df_BC_NB_eng, model="twoway", type="consistency", unit = "average")
  icc(sc.df_BC_NB_jce, model="twoway", type="consistency", unit = "average")
  icc(sc.df_BC_NB_contrib, model="twoway", type="consistency", unit = "average")
  
  #Pair 2
  icc(sc.df_BC_RP_afill, model="twoway", type="consistency", unit = "average")
  icc(sc.df_BC_RP_antag, model="twoway", type="consistency", unit = "average")
  icc(sc.df_BC_RP_eng, model="twoway", type="consistency", unit = "average")
  icc(sc.df_BC_RP_jce, model="twoway", type="consistency", unit = "average")
  icc(sc.df_BC_RP_contrib, model="twoway", type="consistency", unit = "average")
  
  