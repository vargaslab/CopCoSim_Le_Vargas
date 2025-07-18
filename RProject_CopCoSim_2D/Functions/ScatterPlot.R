# ScatterPlot(X, Y, 9, 
#           Xmin = Pb_CrossValid_Sta[2,1], Xmax = Pb_CrossValid_Sta[7,1], 
#           Ymin = Pb_CrossValid_Sta[2,2],Ymax = Pb_CrossValid_Sta[7,2], 
#           XLAB = "Pb (ppm)", YLAB = "Pb* (ppm)")
# p1 = Ip
# p2 = Phie
# BREAKS = 20
# Xmin = Ip_Stat[2,2]
# Xmax = Ip_Stat[7,2]
# Ymin = Phie_Stat[2,2]
# Ymax = Phie_Stat[7,2]
# XLAB = expression(bold( "P-impedance (m/s.g/cm³)"))
# YLAB = expression(bold( "Effective porosity (v/v)"))

ScatterPlot<- function (p1, p2, BREAKS, Xmin, Xmax, Ymin, Ymax, XLAB, YLAB)
{
  if (Xmin < 0) {
    Xmin = Xmin * (1.1)
  }
  else if (Xmin > 0) {
    Xmin = Xmin * (0.9)
  }
  if (Xmax < 0) {
    Xmax = Xmax * (0.9)
  }
  else if (Xmax > 0) {
    Xmax = Xmax * (1.1)
  }
  if (Ymin < 0) {
    Ymin = Ymin * (1.1)
  }
  else if (Ymin > 0) {
    Ymin = Ymin * (0.9)
  }
  if (Ymax < 0) {
    Ymax = Ymax * (0.9)
  }
  else if (Ymax > 0) {
    Ymax = Ymax * (1.1)
  }
  DatosN <- as.data.frame(cbind(p1, p2))
  colnames(DatosN) <- c("P1", "P2")
  difhis1 <- Xmax - Xmin
  numclases <- BREAKS
  tamint1 <- difhis1/numclases
  valorhis1 <- 0
  vectorhis1 <- 0
  for (i in 1:numclases) {
    valorhis1[i] <- Xmin + i * tamint1
    vectorhis1 <- c(Xmin, valorhis1)
  }
  difhis2 <- Ymax - Ymin
  numclases <- BREAKS
  tamint2 <- difhis2/numclases
  valorhis2 <- 0
  vectorhis2 <- 0
  for (i in 1:numclases) {
    valorhis2[i] <- Ymin + i * tamint2
    vectorhis2 <- c(Ymin, valorhis2)
  }
  cual1 <- 0
  cuales1 <- 0
  for (i in 1:length(DatosN$P1)) {
    if (DatosN$P1[i] < min(vectorhis1) | DatosN$P1[i] > max(vectorhis1)) {
      cual1[i] <- i
      cuales1 <- c(cual1)
    }
    if (DatosN$P1[i] >= min(vectorhis1) & DatosN$P1[i] <= 
          max(vectorhis1)) {
      cual1[i] <- 0
      cuales1 <- c(cual1)
    }
  }
  if (sum(cuales1) > 0) {
    DatosN <- DatosN[-cuales1, ]
  }
  cual2 <- 0
  cuales2 <- 0
  for (i in 1:length(DatosN$P2)) {
    if (DatosN$P2[i] < min(vectorhis2) | DatosN$P2[i] > max(vectorhis2)) {
      cual2[i] <- i
      cuales2 <- c(cual2)
    }
    if (DatosN$P2[i] >= min(vectorhis2) & DatosN$P2[i] <= 
          max(vectorhis2)) {
      cual2[i] <- 0
      cuales2 <- c(cual2)
    }
  }
  if (sum(cuales2) > 0) {
    DatosN <- DatosN[-cuales2, ]
  }
  Corr <- cor(DatosN$P2, DatosN$P1, method = "pearson")
  tau_K <- cor(DatosN$P2, DatosN$P1, method = "kendall")
  rho_S <- cor(DatosN$P2, DatosN$P1, method = "spearman")
  orden <- matrix(c(2, 6, 6, 3, 6, 6, 1, 5, 4, 7, 7, 7), ncol = 3, 
                  nrow = 4, byrow = T)
  div <- layout(orden, widths = c(6, 0.5, 1.5, 2), heights = c(1.5, 0.5, 6, 2), TRUE)
  layout.show(div)
  par(mar = c(5, 5, 0, 0))
  plot(DatosN$P1, DatosN$P2, xlim = c(min(vectorhis1), max(vectorhis1)), 
       ylim = c(min(vectorhis2), max(vectorhis2)), xlab = XLAB, 
       ylab = YLAB, pch = 21, col = "black", bg = "black", 
       cex.lab = 1.2, cex = 1, cex.axis = 1)
  grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
  par(new=TRUE)
  plot(DatosN$P1, DatosN$P2, xlim = c(min(vectorhis1), max(vectorhis1)), 
       ylim = c(min(vectorhis2), max(vectorhis2)), xlab = XLAB, 
       ylab = YLAB, pch = 21, col = "black", bg = "black", 
       cex.lab = 1.2, cex = 1, cex.axis = 1)
  par(mar = c(0, 5, 1, 0))
  histo1 <- hist(DatosN$P1, breaks = vectorhis1 , plot = FALSE)            # vectorhis1 = "Sturges"
  top <- max(histo1$counts)
  barplot(histo1$counts, ylim = c(0, top + 4), 
           space = 0, col = "#00AAFFFF", ylab = "Frequency", 
          cex.lab = 1.2, cex.axis = 1, main = "")
  par(mar = c(0, 5, 0, 0))
  plot(0, 0, type = "n", xlim = c(min(vectorhis1), max(vectorhis1)), 
       ylim = c(0, 1.54), xaxt = "n", yaxt = "n", xlab = "", 
       ylab = "")
  boxplot(DatosN$P1, range = 1.5, ylim = c(min(vectorhis1), 
                                           max(vectorhis1)), horizontal = TRUE, col = "#00AAFFFF", 
          pch = 22, axes = FALSE, add = TRUE)
  par(mar = c(5, 0, 0, 1))
  histo2 <- hist(DatosN$P2, breaks = vectorhis2 , plot = FALSE) # vectorhis2 = "Sturges" 
  top <- max(histo2$counts)
  barplot(histo2$counts, xlim = c(0, top + 4), space = 0, col = "#CCFF00FF", 
          xlab = "Frequency", cex.lab = 1.2, cex.axis = 1, main = "", 
          horiz = TRUE)
  par(mar = c(5, 0, 0, 0))
  plot(0, 0, type = "n", ylim = c(min(vectorhis2), max(vectorhis2)), 
       xlim = c(0, 1.54), xaxt = "n", yaxt = "n", xlab = "", 
       ylab = "")
  boxplot(DatosN$P2, range = 1.5, ylim = c(min(vectorhis2), 
                                           max(vectorhis2)), col = "#CCFF00FF", pch = 22, axes = FALSE, 
          add = TRUE)
  par(mar = c(0, 0, 0, 0))
  plot(0, 0, type = "n", xlim = c(0, 60), ylim = c(0, 7), xaxt = "n", 
       yaxt = "n", xlab = "", ylab = "")
  
  text(30, 6, labels = "Dependence", cex = 1.3)
  text(31, 5, labels = "Measures", cex = 1.3)
  
  text(20, 3, labels = "Spearman = ", cex = 1.3)
  text(48, 3, labels = round(rho_S, 2), cex = 1.3)
  
  text(20, 2, labels = "Kendall = ", cex = 1.3)
  text(48, 2, labels = round(tau_K, 2), cex = 1.3)
  
  text(20, 1, labels = "Pearson = ", cex = 1.3)
  text(48, 1, labels = round(Corr, 2), cex = 1.3)
  box()
  PropertyST <- summary(p1)
  PropertyST[7] <- var(p1)
  PropertyST[8] <- sd(p1)
  summaryP1 <- t(t(PropertyST))
  PropertyST2 <- summary(p2)
  PropertyST2[7] <- var(p2)
  PropertyST2[8] <- sd(p2)
  summaryP2 <- t(t(PropertyST2))
  par(mar = c(0.5, 6, 0, 3))
  plot(0, 0, type = "n", xlim = c(0, 60), ylim = c(0, 7), xaxt = "n", 
       yaxt = "n", xlab = "", ylab = "")
  text(10, 5.8, labels = "Minimum     ", cex = 1.3)
  text(10, 5, labels = "1st Quartile"   , cex = 1.3)
  text(10, 4.2, labels = "Median     ", cex = 1.3)
  text(10, 3.4, labels = "Mean        ", cex = 1.3)
  text(10, 2.6, labels = "3rd Quartile"   , cex = 1.3)
  text(10, 1.8, labels = "Maximum     ", cex = 1.3)
  text(10, 1, labels = "Variance   ", cex = 1.3)
  text(10, 0.2, labels = "Std. Dev.   ", cex = 1.3)
  text(24, 6.9, labels = XLAB, cex = 1.3)
  text(24, 5.8, labels = sprintf("%.5f", summaryP1[1, 1]), cex = 1.3)
  text(24, 5, labels = sprintf("%.5f", summaryP1[2, 1]), cex = 1.3)
  text(24, 4.2, labels = sprintf("%.5f", summaryP1[3, 1]), 
       cex = 1.3)
  text(24, 3.4, labels = sprintf("%.5f", summaryP1[4, 1]), 
       cex = 1.3)
  text(24, 2.6, labels = sprintf("%.5f", summaryP1[5, 1]), 
       cex = 1.3)
  text(24, 1.8, labels = sprintf("%.5f", summaryP1[6, 1]), 
       cex = 1.3)
  text(24, 1, labels = sprintf("%.5f", summaryP1[7, 1]), cex = 1.3)
  text(24, 0.2, labels = sprintf("%.5f", summaryP1[8, 1]), 
       cex = 1.3)
  text(51, 6.9, labels = YLAB, cex = 1.3)
  text(51, 5.8, labels = sprintf("%.5f", summaryP2[1, 1]), 
       cex = 1.3)
  text(51, 5, labels = sprintf("%.5f", summaryP2[2, 1]), cex = 1.3)
  text(51, 4.2, labels = sprintf("%.5f", summaryP2[3, 1]), 
       cex = 1.3)
  text(51, 3.4, labels = sprintf("%.5f", summaryP2[4, 1]), 
       cex = 1.3)
  text(51, 2.6, labels = sprintf("%.5f", summaryP2[5, 1]), 
       cex = 1.3)
  text(51, 1.8, labels = sprintf("%.5f", summaryP2[6, 1]), 
       cex = 1.3)
  text(51, 1, labels = sprintf("%.5f", summaryP2[7, 1]), cex = 1.3)
  text(51, 0.2, labels = sprintf("%.5f", summaryP2[8, 1]), 
       cex = 1.3)
  box("outer", lty = "solid", col = "black")
}