library(shiny)
library(shinyAce)
library(psych)
library(car)
library(rpsychi)
library(boot)
library(plyr)
library(ggplot2)
library(randomForest)


shinyServer(function(input, output) {


#-----------------------------------------------------------------
# Relative Weight Analysis Using Raw Data
#-----------------------------------------------------------------

    # Basic statistics
        bs <- reactive({
            x <- read.csv(text=input$text, sep="\t")
            describe(x)[2:13]
        })
        
        output$textarea.out <- renderPrint({
            bs()
        })

        
    # Correlation
        makecorPlot <- function(){
            x <- read.csv(text=input$text, sep="\t")
            pairs.panels(x)
        }
        
        output$corPlot <- renderPlot({
            print(makecorPlot())
        })
 
 
    # Regression
        reg <- reactive({
            dat <- read.csv(text=input$text, sep="\t")

            colnames(dat) <- c("Criterion", c(colnames(dat)[2:ncol(dat)]))
            result <- lm(Criterion ~., dat)

            reslt <- summary(result)
            print(reslt)

            z <- scale(dat)     # standardize the data
            z <- data.frame(z)
            z.res <- summary(lm(Criterion ~ ., z))

            stdb <- data.frame(round((z.res$coefficients[,1][-1]),3))
            colnames(stdb)[1] <- "Standardized beta"

            cat("\n", "---", "\n", "Standardized beta estimates:", "\n")
            print(stdb)

        if (ncol(dat) >= 3) {

            VIF <- vif(result)
            Tolerance <- 1/VIF

            vif.res <- round(data.frame(VIF, Tolerance),3)

            cat("\n", "---", "\n", "VIF and tolerance statistic (1/VIF):", "\n")
            print(vif.res)
            cat("\n", "VIF should be smaller than 10 (clozer to 1 better);", "\n",
            "tolerance statistic (1/VIF) should be greater than 0.2.", "\n")
        }
        })
        
        output$reg.out <- renderPrint({
            reg()
        })
        
        
    # RWA
        rwacalc <- reactive({
            dat <- read.csv(text=input$text, sep="\t")
            thedata<-dat
            Labels<-names(thedata)[2:length(thedata)]
            multRegress<-function(mydata){
              numVar<<-NCOL(mydata)
              Variables<<- names(mydata)[2:numVar]
              mydata<-cor(mydata, use="complete.obs")
              RXX<-mydata[2:numVar,2:numVar]
              RXY<-mydata[2:numVar,1]
              RXX.eigen<-eigen(RXX)
              D<-diag(RXX.eigen$val)
              delta<-sqrt(D)
              lambda<-RXX.eigen$vec%*%delta%*%t(RXX.eigen$vec)
              lambdasq<-lambda^2
              beta<-solve(lambda)%*%RXY
              rsquare<<-sum(beta^2)
              RawWgt<-lambdasq%*%beta^2
              import<-(RawWgt/rsquare)*100
              result<<-data.frame(Variables, Raw.RelWeight=RawWgt, Rescaled.RelWeight=import)
            }
            multBootstrap<-function(mydata, indices){
              mydata<-mydata[indices,]
              multWeights<-multRegress(mydata)
              return(multWeights$Raw.RelWeight)
            }
            multBootrand<-function(mydata, indices){
              mydata<-mydata[indices,]
              multRWeights<-multRegress(mydata)
              multReps<-multRWeights$Raw.RelWeight
              randWeight<-multReps[length(multReps)]
              randStat<-multReps[-(length(multReps))]-randWeight
              return(randStat)
            }
            mybootci<-function(x){
              boot.ci(multBoot,conf=0.95, type="bca", index=x)
            }
            runBoot<-function(num){
              INDEX<-1:num
              test<-lapply(INDEX, FUN=mybootci)
              test2<-t(sapply(test,'[[',i=4)) #extracts confidence interval
              CIresult<<-data.frame(Variables, CI.Lower.Bound=test2[,4],CI.Upper.Bound=test2[,5])
            }
            myRbootci<-function(x){
              boot.ci(multRBoot,conf=0.95,type="bca",index=x)
            }
            runRBoot<-function(num){
              INDEX<-1:num
              test<-lapply(INDEX,FUN=myRbootci)
              test2<-t(sapply(test,'[[',i=4))
              CIresult<<-data.frame(Labels,CI.Lower.Bound=test2[,4],CI.Upper.Bound=test2[,5])
            }
            myCbootci<-function(x){
              boot.ci(multC2Boot,conf=0.95,type="bca",index=x)
            }
            runCBoot<-function(num){
              INDEX<-1:num
              test<-lapply(INDEX,FUN=myCbootci)
              test2<-t(sapply(test,'[[',i=4))
              CIresult<<-data.frame(Labels2,CI.Lower.Bound=test2[,4],CI.Upper.Bound=test2[,5])
            }
            myGbootci<-function(x){
              boot.ci(groupBoot,conf=0.95,type="bca",index=x)
            }
            runGBoot<-function(num){
              INDEX<-1:num
              test<-lapply(INDEX,FUN=myGbootci)
              test2<-t(sapply(test,'[[',i=4))
              CIresult<<-data.frame(Labels,CI.Lower.Bound=test2[,4],CI.Upper.Bound=test2[,5])
            }
            
            multRegress(thedata)
            RW.Results<-result
            
            RSQ.Results<-rsquare
            
            #Bootstrapped Confidence interval around the individual relative weights
            multBoot<-boot(thedata, multBootstrap, 10000)
            multci<-boot.ci(multBoot,conf=0.95, type="bca")
            runBoot(length(thedata[,2:numVar]))
            CI.Results<-CIresult
            
            #Bootstrapped Confidence interval tests of Significance
            randVar<-rnorm(length(thedata[,1]),0,1)
            randData<-cbind(thedata,randVar)
            multRBoot<-boot(randData,multBootrand, 10000)
            multRci<-boot.ci(multRBoot,conf=0.95, type="bca")
            runRBoot(length(randData[,2:(numVar-1)]))
            CI.Significance<-CIresult
            
            list(RSQ.Results=RSQ.Results, RW.Results = RW.Results, CI.Results = CI.Results, CI.Significance=CI.Significance) # 他で使うために定義
            
        })


        rwa <- reactive({
            cat("R-squared For the Model:", "\n")
            RSQ.Results <- rwacalc()$RSQ.Results
            print(RSQ.Results)
            
            cat("\n", "The Raw and Rescaled Weights:", "\n")
            RW.Results <- rwacalc()$RW.Results
            print(RW.Results)
            
            cat("\n", "BCa Confidence Intervals around the raw weights:", "\n")
            CI.Results <- rwacalc()$CI.Results
            print(CI.Results)
            
            cat("\n", "BCa Confidence Interval Tests of significance:", "\n")
            cat("  (If 0 is not included, weight is significant at p < .05)", "\n")
            CI.Significance <- rwacalc()$CI.Significance
            print(CI.Significance)
            
        })
        
        output$rwa.out <- renderPrint({
            rwa()
        })
    
    
    # 95% CI Plot
        confPlot <- function(){
            dat <- read.csv(text=input$text, sep="\t")
            
            RW.Results <- rwacalc()$RW.Results # 前で計算したものを利用
            CI.Results <- rwacalc()$CI.Results # 前で計算したものを利用
            RSQ.Results <- rwacalc()$RSQ.Results
            
            resOrigin <- join(RW.Results, CI.Results)
            is.num <- sapply(resOrigin, is.numeric)
            resOrigin[is.num] <- lapply(resOrigin[is.num], round, 3)
            resTable <- resOrigin[,c(1,2,4,5,3)]
            plotdat <- resTable[1:4]
            
            ggplot(plotdat, aes(reorder(Variables, Raw.RelWeight), Raw.RelWeight)) +
              #geom_col(fill = "#0066cc") + # for newer version of ggplot2
              geom_bar(stat="identity", fill="lightblue", colour = "blue") + # for older version of ggplot2
              coord_flip() +
              geom_text(aes(label = Raw.RelWeight), hjust = -0.1, vjust = -0.3) +
              geom_errorbar(aes(ymin=CI.Lower.Bound, ymax=CI.Upper.Bound), width=.2, position=position_dodge(.9)) +
              labs(title = "Variable Importance Estimates",
                   x = "Predictor Variables",
                   y = "Raw Relative Weights",
                   caption = paste0("Note: R-squared = ",
                                    round(RSQ.Results, 3),
                                    ". ",
                                    "Raw Relative Weights sum to R-squared. n = ",
                                    nrow(dat), ". "
                   )) +
              theme_classic()
        }
            
        output$confPlot <- renderPlot({
            print(confPlot())
        })
        
        
    # Variable Importance Plot (Random Forest)
        randomForestPlot <- function(){
            dat <- read.csv(text=input$text, sep="\t")
            colnames(dat) <- c("Criterion", c(colnames(dat)[2:ncol(dat)]))
            forest <- randomForest(Criterion~ ., data=dat)
            forest$importance
            varImpPlot(forest)
            }
                
            output$randomForestPlot <- renderPlot({
                print(randomForestPlot())
            })
            
    
    # Info
        info1 <- reactive({
            info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
            info2 <- paste("It was executed on ", date(), ".", sep = "")
            cat(sprintf(info1), "\n")
            cat(sprintf(info2), "\n")
        })
        
        output$info1.out <- renderPrint({
            info1()
        })





#-----------------------------------------------------------------
# Relative Weight Analysis Using Correlation Matrix
#-----------------------------------------------------------------

    # Check the Correlation Matrix
        inputData <- reactive({
            dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
            print(dat)

        })

        output$inputData.out <- renderPrint({
            inputData()
        })


    # Multiple Regression analysis
        mra2.result <- reactive({
            n <- input$n
            dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
            dat <- as.data.frame(dat)
            # source("http://mizumot.com/code/multreg.second2.R", encoding="UTF-8")
            multreg.second2 <-
            function (dat, n, m = NULL, sd = NULL, sig.level = 0.05,
                      digits = 3)
            {
              depname <- colnames(dat[1])
              indname <- colnames(dat[-1])
              x <- c(depname, indname)
              corr <- dat
              sd <- sd[x]
              m <- m[x]
              corr.lower <- tanh(atanh(corr) + qnorm(sig.level/2, lower.tail = TRUE)/sqrt(n -
                                                                                            3))
              corr.upper <- tanh(atanh(corr) + qnorm(sig.level/2, lower.tail = FALSE)/sqrt(n -
                                                                                             3))
              corr.conf <- corr.lower
              corr.conf[upper.tri(corr.conf)] <- corr.upper[upper.tri(corr.upper)]
              p <- ncol(corr)
              K <- solve(corr)
              a <- 1/sqrt(diag(K))
              K <- K * outer(a, a)
              partial.corr <- 2 * diag(p) - K
              dimnames(partial.corr) <- dimnames(partial.corr)
              cor.mat <- corr
              cor.mat[upper.tri(cor.mat)] <- partial.corr[upper.tri(partial.corr)]
              num <- which(depname == colnames(corr))
              rxy <- corr[, num][-num]
              Rind <- corr[-num, -num]
              bs <- solve(Rind) %*% rxy
              R.sq <- as.vector(t(as.matrix(rxy)) %*% solve(Rind) %*% as.matrix(rxy))
              k <- nrow(bs)
              bs.sem <- numeric(k)
              for (i in 1:k) {
                xname <- rownames(bs)[i]
                xdel <- setdiff(rownames(bs), xname)
                num <- which(xname == colnames(Rind))
                rxy <- Rind[, num][-num]
                Rind2 <- Rind[-num, -num]
                Ri <- as.vector(t(as.matrix(rxy)) %*% solve(Rind2) %*%
                                  as.matrix(rxy))
                bs.sem[i] <- sqrt((1 - R.sq)/(n - k - 1)) * sqrt(1/(1 -
                                                                      Ri))
              }
              standardized.estimates <- data.frame(matrix(NA, nrow = k,
                                                          ncol = 4))
              rownames(standardized.estimates) <- rownames(bs)
              colnames(standardized.estimates) <- c("estimates", "lower",
                                                    "upper", "std")
              standardized.estimates[, 1] <- as.numeric(bs)
              standardized.estimates[, 4] <- bs.sem
              standardized.estimates[, 2] <- standardized.estimates$estimates +
                qnorm(sig.level/2) * standardized.estimates$std
              standardized.estimates[, 3] <- standardized.estimates$estimates +
                qnorm(sig.level/2, lower.tail = FALSE) * standardized.estimates$std
              if (!is.null(sd)) {
                b <- sd[depname]/sd[rownames(bs)] * standardized.estimates$estimates
                b.sem <- sd[depname]/sd[rownames(bs)] * standardized.estimates$std
                b.lower <- b + qt(sig.level/2, n - k - 1) * b.sem
                b.upper <- b + qt(sig.level/2, n - k - 1, lower.tail = FALSE) *
                  b.sem
                Intercept <- m[depname] - sum(b * m[names(b)])
                raw.estimates <- data.frame(matrix(NA, nrow = k + 1,
                                                   ncol = 4))
                rownames(raw.estimates) <- c("Intercept", names(b))
                colnames(raw.estimates) <- c("estimates", "lower", "upper",
                                             "std")
                raw.estimates[, 1] <- c(Intercept, b)
                raw.estimates[, 4] <- c(NA, bs.sem)
                raw.estimates[, 2] <- c(NA, b.lower)
                raw.estimates[, 3] <- c(NA, b.upper)
              }
              u <- length(indname)
              nu <- n - u - 1
              f.sq <- R.sq/(1 - R.sq)
              f.value <- f.sq * (nu/u)
              delta.lower <- try(FNONCT(f.value, u, nu, prob = 1 - sig.level/2))
              delta.upper <- FNONCT(f.value, u, nu, prob = sig.level/2)
              if (is.character(delta.lower)) {
                delta.lower <- 0
              }
              R.sq.lower <- delta.lower/(delta.lower + u + nu + 1)
              R.sq.upper <- delta.upper/(delta.upper + u + nu + 1)
              omnibus.es <- c(Rsq = R.sq, lower = R.sq.lower, upper = R.sq.upper)
              criterion.power <- c(small = power.multi(n = n, n.ind = u,
                                                       delta = 0.02, sig.level = sig.level), medium = power.multi(n = n,
                                                                                                                  n.ind = u, delta = 0.15, sig.level = sig.level), large = power.multi(n = n,
                                                                                                                                                                                       n.ind = u, delta = 0.35, sig.level = sig.level))
              output <- list(corr.partial.corr = cor.mat, corr.confidence = corr.conf,
                             omnibus.es = omnibus.es, standardized.estimates = standardized.estimates,
                             power = criterion.power)
              if (!is.null(sd)) {
                output <- list(corr.partial.corr = cor.mat, corr.confidence = corr.conf,
                               omnibus.es = omnibus.es, raw.estimates = raw.estimates,
                               standardized.estimates = standardized.estimates,
                               power = criterion.power)
              }
              output <- sapply(output, round, digits)
              return(output)
            }

            multreg.second2(dat, n=n)

        })

        output$mra2.result.out <- renderPrint({
            mra2.result()
        })


    # Relative Weight Analysis
        rwacalc.cor <- reactive({
            dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
            thedata<- as.data.frame(dat)
            Labels <- names(thedata)[2:length(thedata)]
            multRegress <- function(mydata){
              numVar<<-ncol(mydata)
              Variables<<- names(mydata)[2:numVar]
              RXX<-mydata[2:numVar,2:numVar]
              RXY<-mydata[2:numVar,1]
              RXX.eigen<-eigen(RXX)
              D<-diag(RXX.eigen$val)
              delta<-sqrt(D)
              lambda<-RXX.eigen$vec%*%delta%*%t(RXX.eigen$vec)
              lambdasq<-lambda^2
              beta<-solve(lambda)%*%RXY
              rsquare<<-sum(beta^2)
              RawWgt<-lambdasq%*%beta^2
              import<-(RawWgt/rsquare)*100
              result<<-data.frame(Variables, Raw.RelWeight=RawWgt, Rescaled.RelWeight=import)
            }
            multRegress(thedata)
            RW.Results <- result
            RSQ.Results <- rsquare
            
            list(RSQ.Results2=RSQ.Results, RW.Results2 = RW.Results) # 他で使うために定義
        })
            
            
        wra.cor <- reactive({
            cat("R-squared For the Model:", "\n")
            RSQ.Results2 <- rwacalc.cor()$RSQ.Results2
            print(RSQ.Results2)
            
            cat("\n", "The Raw and Rescaled Weights:", "\n")
            RW.Results2 <- rwacalc.cor()$RW.Results2
            print(RW.Results2)
            
        })

        output$wra.cor.out <- renderPrint({
            wra.cor()
        })

        
    # Importance Plot
        imPlot <- function(){
            
            RW.Results <- rwacalc.cor()$RW.Results2 # 前で計算したものを利用
            RSQ.Results <- rwacalc.cor()$RSQ.Results2
            
            is.num <- sapply(RW.Results, is.numeric)
            RW.Results[is.num] <- lapply(RW.Results[is.num], round, 3)
            
            ggplot(RW.Results, aes(reorder(Variables, Raw.RelWeight), Raw.RelWeight)) +
              # geom_col(fill = "#0066cc") + # for newer version of ggplot2
              geom_bar(stat="identity", fill="lightblue", colour = "blue") + # for older version of ggplot2
              coord_flip() +
              scale_y_continuous(limits = c(0, max(RW.Results[2])*1.1)) +
              geom_text(aes(label = Raw.RelWeight), hjust = -0.1, vjust = -0.3) +
              labs(title = "Variable Importance Estimates",
                   x = "Predictor Variables",
                   y = "Raw Relative Weights",
                   caption = paste0("Note: R-squared = ",
                                    round(RSQ.Results, 3),
                                    ". ",
                                    "Raw Relative Weights sum to R-squared. n = ",
                                    input$n, ". "
                   )) +
              theme_classic()
              
        }
                
            output$imPlot <- renderPlot({
                print(imPlot())
            })


    # Info
        info2 <- reactive({
            info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
            info2 <- paste("It was executed on ", date(), ".", sep = "")
            cat(sprintf(info1), "\n")
            cat(sprintf(info2), "\n")
        })

        output$info2.out <- renderPrint({
            info2()
        })

})
