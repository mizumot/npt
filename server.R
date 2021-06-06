library(shiny)
library(shinyAce)
library(psych)
library(beeswarm)



shinyServer(function(input, output) {


#-----------------------------------------------------------------
# Mann-Whitney U-test (Comparing two independent conditions)
#-----------------------------------------------------------------

    # Basic statistics

        MWU.bs <- reactive({

            dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
            describeBy(dat[,2], dat[,1])

        })

        output$MWU.bs.out <- renderPrint({
            MWU.bs()
        })



    # Rank

        MWU.ranking <- reactive({

            dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))

            ranked <- rank(dat[,2])
            data <- data.frame(dat[,1], ranked)

            n <- round(tapply(data[,2], data[,1], length),2)
            m <- round(tapply(data[,2], data[,1], mean),2)
            t <- round(tapply(data[,2], data[,1], sum),2)
            ranks <- data.frame(n, m, t)
            colnames(ranks) <- c("n","Rank Mean","Rank Sum")

            print(ranks)

        })

        output$MWU.ranking.out <- renderPrint({
            MWU.ranking()
        })



    # Box plot

        MWU.boxPlot <- function(){
            dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))

            boxplot(dat[,2] ~ dat[,1], las=1)
            beeswarm(dat[,2] ~ dat[,1], col = 4, pch = 16, vert = TRUE, add = TRUE)

        }

        output$MWU.boxPlot <- renderPlot({
        print(MWU.boxPlot())
        })



    # Mann-Whitney U-test

        MWU.test <- reactive({

            dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))

                dat2 <- split(dat, dat[,1])
                x <- dat2[[1]][,2]
                y <- dat2[[2]][,2]
                max.len = max(length(x), length(y))
                x <- c(x, rep(NA, max.len - length(x)))
                y <- c(y, rep(NA, max.len - length(y)))

                U.test <- function(x, y, correct = TRUE)
                {
                    x <- x[!is.na(x)]
                    y <- y[!is.na(y)]
                    n1 <- length(x)
                    n2 <- length(y)
                    n <- n1 + n2
                    xy <- c(x, y)
                    r <- rank(xy)
                    U1 <- n1*n2+n1*(n1+1)/2-sum(r[1:n1])
                    tie <- table(r)
                    U <- min(U1, n1*n2-U1) # U
                    V <- n1*n2*(n^3-n-sum(tie^3-tie))/12/(n^2-n) # variance ties considered
                    E <- n1*n2/2 # Expected
                    z <- ((U-E)-ifelse(correct, 0.5, 0))/sqrt(V) # z-value
                    EffectSize.r <- abs(z)/sqrt(n)
                    ESrCI <- r.con(EffectSize.r, n, p =.95, twotailed=TRUE)
                    P <- pnorm(abs(z), lower.tail=FALSE)*2
                    cat(" Mann-Whitney U-test", "\n",
                    "\n",
                    "U =", U, ",", "E(U) =", E, ",", "V(U) =", V, "\n",
                    "z-value =", round(z, 3), "\n",
                    "p-value =", P, "\n", "\n",
                    "Effect size r [95% CI]=", round(EffectSize.r, 3), "[", round(ESrCI, 3), "]", "\n"
                    )
                }
                U.test(x, y, correct = FALSE)
        })

        output$MWU.test.out <- renderPrint({
            MWU.test()
        })



    # Info
        MWU.info <- reactive({
            info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
            info2 <- paste("It was executed on ", date(), ".", sep = "")
            cat(sprintf(info1), "\n")
            cat(sprintf(info2), "\n")
        })

        output$MWU.info.out <- renderPrint({
            MWU.info()
        })





#-----------------------------------------------------------------
# Wilcoxon signed-rank test (Comparing two related conditions)
#-----------------------------------------------------------------

    # Basic statistics

        WSR.bs <- reactive({

            dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
            describe(dat)

        })

        output$WSR.bs.out <- renderPrint({
            WSR.bs()
        })



    # Rank

        WSR.ranking <- reactive({

            dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))

            dat$diff <- dat[,2] - dat[,1]
            dat$sign <- ifelse(dat$diff < 0, "Negative", ifelse((dat$diff > 0), "Positive", "Tie"))
            newdata <- subset(dat, dat$sign != "Tie") # Except Tie
            newdata$rank <- rank(abs(newdata$diff))
            n <- tapply(dat[,1], dat$sign, length)
            m <- tapply(newdata$rank, newdata$sign, mean)
            t <- tapply(newdata$rank, newdata$sign, sum)

            list(n = n, "Rank Mean" = round(m, 2), "Rank Sum" = round(t, 2))

        })

        output$WSR.ranking.out <- renderPrint({
            WSR.ranking()
        })



    # Box plot

       WSR.boxPlot <- function(){
            dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))

            boxplot(dat, las=1)
            beeswarm(dat, col = 4, pch = 16, vert = TRUE,  add = TRUE)

        }

        output$WSR.boxPlot <- renderPlot({
            print(WSR.boxPlot())
        })



    # Wilcoxon signed-rank test

        WSR.test <- reactive({

            dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))

            x <- dat[,1]
            x <- x[!is.na(x)]

            y <- dat[,2]
            y <- y[!is.na(y)]
          
            options(warn=-1) # Suppress a warning message
            result <- wilcox.test(x, y, paired=TRUE, correct=FALSE)

            pval <- result$p.value
            z <- qnorm(1-(pval/2))
            r1 <- z/sqrt(length(x)*2)
            esR.CI1 <- r.con(r1, length(x*2), p =.95, twotailed=TRUE)
            #r2 <- z/sqrt(length(x)-sum((y-x==0)))
            #esR.CI2 <- round(r.con(r2, length(x)-sum((y-x==0)), p =.95, twotailed=TRUE), 3)
            
            print(result)

            cat(" z-value =", round(z, 3), "\n",
            "\n",
            "Effect size r [95% CI] =", round(r1, 3), "[", round(esR.CI1, 3), "]", "\n"
            )
            #"Effect size r (without considering ties) [95% CI]=", round(r2, 3), "[", esR.CI1, "]", "\n")

        })

        output$WSR.test.out <- renderPrint({
            WSR.test()
        })



    # Info
        WSR.info <- reactive({
            info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
            info2 <- paste("It was executed on ", date(), ".", sep = "")
            cat(sprintf(info1), "\n")
            cat(sprintf(info2), "\n")
        })

        output$WSR.info.out <- renderPrint({
            WSR.info()
        })





#-----------------------------------------------------------------
# Kruskal-Wallis test (Differences between several independent groups)
#-----------------------------------------------------------------

    # Basic statistics

        KW.bs <- reactive({

            dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
            describeBy(dat[,2], dat[,1])

        })

        output$KW.bs.out <- renderPrint({
            KW.bs()
        })



    # Rank

        KW.ranking <- reactive({

            dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))

            ranked <- rank(dat[,2])
            data <- data.frame(dat[,1], ranked)

            n <- round(tapply(data[,2], data[,1], length),2)
            m <- round(tapply(data[,2], data[,1], mean),2)
            t <- round(tapply(data[,2], data[,1], sum),2)
            ranks <- data.frame(n, m, t)
            colnames(ranks) <- c("n","Rank Mean","Rank Sum")

            print(ranks)

        })

        output$KW.ranking.out <- renderPrint({
            KW.ranking()
        })



    # Box plot

       KW.boxPlot <- function(){
            dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))

            boxplot(dat[,2] ~ dat[,1], las=1)
            beeswarm(dat[,2] ~ dat[,1], col = 4, pch = 16, vert = TRUE,  add = TRUE)

        }

        output$KW.boxPlot <- renderPlot({
            print(KW.boxPlot())
        })



    # Kruskal-Wallis test

        KW.test <- reactive({

            dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))

            result <- kruskal.test(dat[,2] ~ dat[,1])
            print(result)
            
            z <- qnorm(1 - (result$p.value/2))    # p to z
            esR <- abs(z)/sqrt(nrow(dat))         # z to r
            # 95%CI of r
            esR.CI <- r.con(esR, nrow(dat), p =.95, twotailed=TRUE)
            
            cat("Effect size r [95% CI] =", round(esR, 3), "[", round(esR.CI, 3), "]", "\n")
            cat("  *Converted from p-value (p -> z -> r)", "\n", "\n", "\n")
            
            #eta2 <- result$statistic[[1]]/(length(dat[,1])-1)
            
            #cat("Effect size (eta-squared) =", sprintf("%.3f",round(eta2,4)), "\n")
            #cat("*Kruskal-Wallis chi-squared / (sample size - 1)", "\n", "\n")
            
        # pair-wise comparisons
            cat("=============================================================", "\n")
            cat("\n", "Pairwise comparisons (Test statistics and effect sizes)", "\n", "\n")
            cat("=============================================================", "\n", "\n")


            U.test <- function(x, y, correct = TRUE) # this is used in "pairWiseU"
            {
                x <- x[!is.na(x)]
                y <- y[!is.na(y)]
                n1 <- length(x)
                n2 <- length(y)
                n <- n1+n2
                xy <- c(x, y)
                r <- rank(xy)
                U1 <- n1*n2+n1*(n1+1)/2-sum(r[1:n1])
                tie <- table(r)
                U <- min(U1, n1*n2-U1) # U
                V <- n1*n2*(n^3-n-sum(tie^3-tie))/12/(n^2-n) # variance ties considered
                E <- n1*n2/2 # Expected
                z <- round(((U-E)-ifelse(correct, 0.5, 0))/sqrt(V),3)  # z-value
                EffectSize.r <- round(abs(z)/sqrt(n),3)
                P <- pnorm(abs(z), lower.tail=FALSE)*2
                return(structure(list(statistic=c(U=U, "E(U)"=E, "V(U)"=V, "Z-value"=z), p.value=P), class="htest"))
            }


            pairWiseU <- function(x,y) {

                uniqY <- unique(y)
                xx <- data.frame(x,y)
                yy <- unstack(xx)

                for (i in 1:length(uniqY)) {
                    for (j in 1:length(uniqY)) {
                        if (i >= j) {
                            next
                        } else {
                            x <- data.frame(yy[i])
                            y <- data.frame(yy[j])
                            n1 <- nrow(x)
                            n2 <- nrow(y)
                            n <- n1 + n2
                            resultU <- U.test(x, y, correct=FALSE)
                            r <- abs(resultU[[1]][[4]])/sqrt(n)
                            esR.CI <- r.con(r, n, p =.95, twotailed=TRUE)
                            
                            cat("Comparisons", colnames(x), "-", colnames(y), ":", "\n",
                            "Mann-Whitney's U:",sprintf("%.3f",round(resultU[[1]][1],4)),"\n",
                            "z-value:",sprintf("%.3f",round(resultU[[1]][4],4)), "\n",
                            #p-value (two-tailed without adjustment):",substr(sprintf("%.3f",round(resultU[[2]],4)),2,5), "\n",
                            "Effect size (r) [95% CI]=", sprintf("%.3f",r), "[", round(esR.CI, 3), "]", "\n",
                            "\n"
                            )

                        }
                    }
                }
            }

            pairWiseU(dat[,2], dat[,1])
            
            cat("\n")  
            cat("=============================================================", "\n")
            cat("\n", "Pairwise comparisons (p-values)", "\n", "\n")
            # cat("\n", "For details see, http://www.med.osaka-u.ac.jp/pub/kid/clinicaljournalclub1.html", "\n")
            cat("=============================================================", "\n")
            
            # Bonferroni
            cat("\n",
                "<< Bonferroni method >>", "\n")
            bon <- pairwise.wilcox.test(dat[,2], dat[,1], p.adj="bonferroni", exact=F, correct = F)
            print(bon)
            
            cat("\n",
                "--------------------------------------------------------", "\n")
            cat("\n",
                "<< Holm-Bonferroni method >>", "\n")
            # Holm
            holm <- pairwise.wilcox.test(dat[,2], dat[,1], p.adj="holm", exact=F, correct = F)
            print(holm)
            
            
            cat("\n",
                "--------------------------------------------------------", "\n")
            cat("\n",
                "<< False Discovery Rate >>", "\n")
            # false discovery rate
            fdr <- pairwise.wilcox.test(dat[,2], dat[,1], p.adj="fdr", exact=F, correct = F)
            print(fdr)
            
            
            cat("\n",
                "--------------------------------------------------------", "\n")
                
        })

        output$KW.test.out <- renderPrint({
            KW.test()
        })



    # Info
        KW.info <- reactive({
            info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
            info2 <- paste("It was executed on ", date(), ".", sep = "")
            cat(sprintf(info1), "\n")
            cat(sprintf(info2), "\n")
        })

        output$KW.info.out <- renderPrint({
            KW.info()
        })





#-----------------------------------------------------------------
# Friedman test (Differences between several related groups)
#-----------------------------------------------------------------
# For two treatments (k = 2), Friedman test is equivalent to the sign test (not "signed" rank test).
# Wilcoxon signed-rank test is better for k = 2 design.


    # Basic statistics

        Friedman.bs <- reactive({

            dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))
            describe(dat)

        })

        output$Friedman.bs.out <- renderPrint({
            Friedman.bs()
        })



    # Rank

        Friedman.ranking <- reactive({

            dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))

            dtf <- data.frame()
            for (i in 1:nrow(dat)){
                dtf <- rbind(dtf, rank(dat[i,]))
            }
            colnames(dtf) <- colnames(dat)

            Rank.Mean <- round(apply(dtf[,1:ncol(dtf)], 2, mean),2)
            print(Rank.Mean)

        })

        output$Friedman.ranking.out <- renderPrint({
            Friedman.ranking()
        })



    # Box plot

       Friedman.boxPlot <- function(){
            dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))

            boxplot(dat, las=1)
            beeswarm(dat, col = 4, pch = 16, vert = TRUE,  add = TRUE)

        }

        output$Friedman.boxPlot <- renderPlot({
            print(Friedman.boxPlot())
        })



    # Friedman test
    
        Friedman.test <- reactive({

            dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))

            result <-friedman.test(as.matrix(dat))

            print(result)

            z <- qnorm(1 - (result$p.value/2))     # p to z
            esR <- abs(z)/sqrt(nrow(dat))    # z to r
            # 95%CI of r
            esR.CI <- r.con(esR, nrow(dat), p =.95, twotailed=TRUE)
            
            cat("Effect size r [95% CI] =", round(esR, 3), "[", round(esR.CI, 3), "]", "\n")
            cat("  *Converted from p-value (p -> z -> r)", "\n", "\n", "\n")
            
            #eta2 <- result$statistic[[1]]/((length(dat[,1])*(length(dat[1,])-1)))
            #names(eta2) <- NULL

            #cat("Effect size (eta-squared) =", sprintf("%.3f",round(eta2,4)), "\n", "\n")
            

        # pair-wise comparisons
        cat("=============================================================", "\n")
        cat("\n", "Pairwise comparisons (Test statistics and effect sizes)", "\n", "\n")
        cat("=============================================================", "\n", "\n")
          
        pairWiseW <- function(x) {
          colnames(x) <- c(1:length(x))
          a <- as.numeric(colnames(x))
          
          for (i in 1:length(a)) {
            for (j in 1:length(a)) {
              if (i >= j) {
                next
              } else {
                
                res <- wilcox.test(x[,i], x[,j], paired=TRUE, correct=FALSE)
                pval <- res$p.value
                z <- qnorm(1-(pval/2))
                r1 <- z/sqrt(length(x[,i])*2)
                esR.CI1 <- r.con(r1, length(x[,i]*2), p =.95, twotailed=TRUE)
                
                #r2 <- z/sqrt(length(x[,1])-sum((x[,2]-x[,1])==0))
                
                cat("Comparisons", a[i], "-", a[j], ":", "\n",
                    "V =", res[[1]][1],"\n",
                    "z-value =",sprintf("%.3f",round(z,4)), "\n",
                    #"p-value =",sprintf("%.3f",round(pval,4)), "\n",
                    "Effect size r [95% CI] =", round(r1, 3), "[", round(esR.CI1, 3), "]", "\n", 
                    #"Effect eize r (without considering ties) =", round(r2, 3), "\n",
                    "\n")
              }
            }
          }
        }
        
        pairWiseW(dat)
        
        
        cat("\n") 
			  cat("=============================================================", "\n")
  			cat("\n", "Pairwise comparisons (p-values)", "\n", "\n")
            #cat("\n", "For details see, http://www.med.osaka-u.ac.jp/pub/kid/clinicaljournalclub1.html", "\n")
			  cat("=============================================================", "\n")

            x <- stack(dat)
            x1 <- x[,1]
            x2 <- x[,2]

         		# Bonferroni
         		cat("\n",
         		"<< Bonferroni method >>", "\n")
                  bon <- pairwise.wilcox.test(x1, x2, p.adj="bonferroni", exact=F, paired=T, correct = F)
            print(bon)
      
      			cat("\n",
        			"--------------------------------------------------------", "\n")
      			cat("\n",
      			"<< Holm-Bonferroni method >>", "\n")
      			# Holm
      			holm <- pairwise.wilcox.test(x1, x2, p.adj="holm", exact=F, paired=T, correct = F)
      			print(holm)
      
      
      			cat("\n",
        			"--------------------------------------------------------", "\n")
      			cat("\n",
      			"<< False Discovery Rate >>", "\n")
      			# false discovery rate
      			fdr <- pairwise.wilcox.test(x1, x2, p.adj="fdr", exact=F, paired=T, correct = F)
      			print(fdr)


            cat("\n",
            "--------------------------------------------------------", "\n", "\n")

 
        })

        output$Friedman.test.out <- renderPrint({
            Friedman.test()
        })



    # Info
        Friedman.info <- reactive({
            info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
            info2 <- paste("It was executed on ", date(), ".", sep = "")
            cat(sprintf(info1), "\n")
            cat(sprintf(info2), "\n")
        })

        output$Friedman.info.out <- renderPrint({
            Friedman.info()
        })



})
