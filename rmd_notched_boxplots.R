
par(mfrow=c(1,3))

for(n in c(10,40,90)){
    
    
    set.seed(123)
    
    x <- rnorm(n, 0)
    y <- rnorm(n, 0.55)
    z <- rnorm(n, -0.25)
    
    dat <- data.frame(outcome = c(x,y,z),
                      group = rep(c("X","Y","Z"), each = n))
    
    
    bp <- boxplot(outcome ~ group, data = dat, notch = TRUE, ylim = c(-1,1))
    
    rect(xleft = 0, ybottom = median(y)-IQR(y, type = 2)*1.58/sqrt(n), 
         xright = 4, ytop = median(y)+IQR(y, type = 2)*1.58/sqrt(n), 
         border = NA, col = "#00000066")
    
    fit <- aov(outcome ~ group, data = dat)
    print(TukeyHSD(fit, ordered = TRUE)$group)
    
    
}







par(mfrow=c(1,1))

    n=40
    
    set.seed(123)
    
    x <- rnorm(n, 0)
    y <- rnorm(n, 0.4)
    z1 <- rnorm(n, -0.25)
    z2 <- rnorm(n, -0.25)
    z3 <- rnorm(n, -0.25)
    z4 <- rnorm(n, -0.25)
    z5 <- rnorm(n, -0.25)
    z6 <- rnorm(n, -0.25)
    
    
    
    dat <- data.frame(outcome = c(x,y,z1,z2,z3,z4,z5,z6),
                      group = rep(c("X","Y","Z1", "Z2","Z3","Z4","Z5","Z6"), each = n))
    
    
    boxplot(outcome ~ group, data = dat, notch = TRUE, ylim = c(-1,1))
    
    rect(xleft = 0, ybottom = median(y)-IQR(y, type = 2)*1.58/sqrt(n), 
         xright = 9, ytop = median(y)+IQR(y, type = 2)*1.58/sqrt(n), 
         border = NA, col = "#00000066")
    
    
    fit <- aov(outcome ~ group, data = dat)
    print(TukeyHSD(fit, ordered = FALSE)$group)
    
    




#https://stats.stackexchange.com/questions/262495/reading-box-and-whisker-plots-possible-to-glean-significant-differences-between
#https://stats.stackexchange.com/questions/228719/box-plot-notches-vs-tukey-kramer-interval/228731#228731