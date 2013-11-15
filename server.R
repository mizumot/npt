library(shiny)
library(shinyAce)
library(psych)





shinyServer(function(input, output) {
    
    options(warn=-1)
    
    
    
    bs <- reactive({
        
        
            dat <- read.csv(text=input$text, sep="\t")
        
        
        if (input$type == "mannwhitney") {

            describeBy(dat[,2], dat[,1])
            
        }
        
        else if (input$type == "wilcoxon") {
            
            describe(dat)
        
        }
        
        else if (input$type == "kruskalwallis") {
            
            describeBy(dat[,2], dat[,1])
        }
        
        else if (input$type == "friedman") {
                
            describe(dat)
            
        }


    })
    
    
    
    
    ranking <- reactive({
        
        
        dat <- read.csv(text=input$text, sep="\t")
        
        
        if (input$type == "mannwhitney") {
            
            ranked <- rank(dat[,2])
            data <- data.frame(dat[,1], ranked)
            
            n <- round(tapply(data[,2], data[,1], length),2)
            m <- round(tapply(data[,2], data[,1], mean),2)
            t <- round(tapply(data[,2], data[,1], sum),2)
            ranks <- data.frame(n, m, t)
            colnames(ranks) <- c("n","Rank Mean","Rank Sum")
            
            print(ranks)
            
        }
        
        else if (input$type == "wilcoxon") {
            
            dat$diff <- dat[,2] - dat[,1]
            dat$sign <- ifelse(dat$diff < 0, "Negative", ifelse((dat$diff > 0), "Positive", "Tie"))
            newdata <- subset(dat, dat$sign != "Tie") #Tie以外
            newdata$rank <- rank(abs(newdata$diff))
            n <- tapply(dat[,1], dat$sign, length)
            m <- tapply(newdata$rank, newdata$sign, mean)
            t <- tapply(newdata$rank, newdata$sign, sum)
            
            list(n = n, "Rank Mean" = round(m, 2), "Rank Sum" = round(t, 2))
            
        }
        
        else if (input$type == "kruskalwallis") {
            
            ranked <- rank(dat[,2])
            data <- data.frame(dat[,1], ranked)
            
            n <- round(tapply(data[,2], data[,1], length),2)
            m <- round(tapply(data[,2], data[,1], mean),2)
            t <- round(tapply(data[,2], data[,1], sum),2)
            ranks <- data.frame(n, m, t)
            colnames(ranks) <- c("n","Rank Mean","Rank Sum")
            
            print(ranks)
            
        }
        
        else if (input$type == "friedman") {
            
            dtf <- data.frame()
            for (i in 1:nrow(dat)){
                dtf <- rbind(dtf, rank(dat[i,]))
            }
            colnames(dtf) <- colnames(dat)
            
            Rank.Mean <- round(apply(dtf[,1:ncol(dtf)], 2, mean),2)
            print(Rank.Mean)
            
        }
        
        
    })

    
    
    
    
    output$boxPlot <- renderPlot({
        
            dat <- read.csv(text=input$text, sep="\t")
        

        if (input$type == "mannwhitney") {

            boxplot(dat[,2] ~ dat[,1], las=1)
            stripchart(dat[,2] ~ dat[,1], pch = 16, vert = TRUE,  add = TRUE)
        
        }
        
        
        if (input$type == "wilcoxon") {
            
            boxplot(dat, las=1)
            stripchart(dat, pch = 16, vert = TRUE,  add = TRUE)

        }
        
        if (input$type == "kruskalwallis") {
            
            boxplot(dat[,2] ~ dat[,1], las=1)
            stripchart(dat[,2] ~ dat[,1], pch = 16, vert = TRUE,  add = TRUE)
            
        }
        
        if (input$type == "friedman") {
            
            boxplot(dat, las=1)
            stripchart(dat, pch = 16, vert = TRUE,  add = TRUE)
            
        }

    })
    
    
    
    
    
    test <- reactive({
        
        
            dat <- read.csv(text=input$text, sep="\t")
       
       
        if (input$type == "mannwhitney") {
            
            d <- data.frame(dat[,2], dat[,1])
            d <- unstack(d)
        
            x <- d$X1
            y <- d$X2
        
            U.test <- function(x, y, correct = TRUE)
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
                cat(" Mann-Whitney U-test", "\n",
                "\n",
                "U =", U, ",", "E(U) =", E, ",", "V(U) =", V, "\n",
                "z-value =", z, "\n",
                "p-value =", P, "\n", "\n",
                "Effect eize r =", EffectSize.r)
                }
            U.test(x, y, correct = FALSE)
        }


        if (input$type == "wilcoxon") {
            x <- dat[,1]
            x <- x[!is.na(x)]
            
            y <- dat[,2]
            y <- y[!is.na(y)]
            
            result <- wilcox.test(x, y, paired=TRUE, correct=FALSE)

            
            pval <- result$p.value
            z <- qnorm(1-(pval/2))
            
            r1 <- z/sqrt(length(x*2))
            
            r2 <- z/sqrt(length(x)-sum((y-x==0)))
            
            print(result)
            
            cat(" z-value =", round(z, 3), "\n",
            "\n",
            "Effect eize r =", round(r1, 3), "\n",
            "Effect eize r (without considering ties) =", round(r2, 3), "\n")
        }
        
        
        if (input$type == "kruskalwallis") {
            
            result <- kruskal.test(dat[,2] ~ dat[,1])
			
			print(result)
			
            eta2 <- result$statistic/(length(dat[,1])-1)
            names(eta2) <- NULL
            
            cat("Effect size (eta-squared) =", sprintf("%.3f",round(eta2,4)), "\n")			
			
     # pair-wise comparisons
			cat("\n",
  			"=============================================================", "\n")
  			cat("\n", "Pairwise comparisons (p-values)", "\n")
  			cat("\n", "For details see, http://www.med.osaka-u.ac.jp/pub/kid/clinicaljournalclub1.html", "\n")
			cat("\n",
  			"------------------------", "\n")
            
   			# Bonferroni
   			cat("\n",
   			"<< Bonferroni method >>", "\n")    
            bon <- pairwise.wilcox.test(dat[,2], dat[,1], p.adj="bonferroni", exact=F)
            print(bon)	

			cat("\n",
  			"------------------------", "\n")
			cat("\n",
			"<< Holm-Bonferroni method >>", "\n") 
			# Holm
			holm <- pairwise.wilcox.test(dat[,2], dat[,1], p.adj="holm", exact=F)
			print(holm)
			
			
			cat("\n",
  			"------------------------", "\n")
			cat("\n",
			"<< False Discovery Rate >>", "\n") 
			# false discovery rate
			fdr <- pairwise.wilcox.test(dat[,2], dat[,1], p.adj="fdr", exact=F)
			print(fdr)
					

            cat("\n",
            "------------------------", "\n")
            
            cat("\n",
            "=============================================================", "\n")
            cat("\n", "Pairwise comparisons (Test statistics and effect sizes)", "\n")
            cat("\n",
            "------------------------", "\n")


	# マン・ホイットニーの U 検定関数定義 http://aoki2.si.gunma-u.ac.jp/R/u-test.html
			U.test <- function(	x,				# 第一群の観測値ベクトルまたは，分割表データ（y=NULL)
								y = NULL,			# 第二群の観測値ベクトル
								correct = TRUE)			# 連続性の補正を行うかどうか
			{
			method <- "マン・ホイットニーの U 検定"
			if (is.null(y)) {				# 2 × C 行列の分割表として与えられたとき
			if (nrow(x) != 2) stop("2 x C matrix is expected.")
			data.name <- paste(deparse(substitute(x)), "as 2 by C matrix")
			nc <- ncol(x)				# カテゴリー数
			y <- x[2,]				# 第二群の度数分布
			x <- x[1,]				# 第一群の度数分布
			tie <- x+y				# 合計した度数分布（同順位）
			n1 <- sum(x)				# 第一群のサンプルサイズ
			n2 <- sum(y)				# 第二群のサンプルサイズ
			n <- n1+n2				# 合計したサンプルサイズ
			rj <- c(0, cumsum(tie)[-nc])+(tie+1)/2	# カテゴリーに属するものの順位
			U1 <- n1*n2+n1*(n1+1)/2-sum(x*rj)	# 検定統計量
			}
			else {						# 2 つのデータベクトルとして与えられたとき
			data.name <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
			x <- x[!is.na(x)]			# 欠損値を持つケースを除く
			y <- y[!is.na(y)]			# 欠損値を持つケースを除く
			n1 <- length(x)				# 第一群のサンプルサイズ
			n2 <- length(y)				# 第二群のサンプルサイズ
			n <- n1+n2				# 合計したサンプルサイズ
			xy <- c(x, y)				# 両群のデータを結合したもの
			r <- rank(xy)				# 順位のベクトル
			U1 <- n1*n2+n1*(n1+1)/2-sum(r[1:n1])	# 検定統計量
			tie <- table(r)				# 同順位の集計
			}
			U <- min(U1, n1*n2-U1)				# U 統計量
			V <- n1*n2*(n^3-n-sum(tie^3-tie))/12/(n^2-n)	# 同順位を考慮した分散
			E <- n1*n2/2					# 期待値
			Z <- ((U-E)-ifelse(correct, 0.5, 0))/sqrt(V)	# Z 値
			P <- pnorm(abs(Z), lower.tail=FALSE)*2		# 両側 P 値
			return(structure(list(statistic=c(U=U, "E(U)"=E, "V(U)"=V, "Z-value"=Z), p.value=P,
			method=method, data.name=data.name), class="htest"))
			}
	# -------------------------------------------------------------------------
		         
		             
            pairWiseU <- function(x,y) {
  				uniqY <- unique(y)
 				a <- c()
  				xx <- data.frame(x,y)
  				yy <- unstack(xx)
  				
  				for (i in 1:length(uniqY)) {
    				for (j in 1:length(uniqY)) {
      					if (i >= j) {
        					next
      					} else {
        				resultU <- U.test(yy[,i], yy[,j], correct=FALSE)
                        r <- resultU[[1]][4]/sqrt(length(yy[,j]))
                        
                        cat("Comparisons", uniqY[i], "-", uniqY[j], ":", "\n",
                        "Mann-Whitney's U =",sprintf("%.3f",round(resultU[[1]][1],4)),"\n",
                        "z-value =",sprintf("%.3f",round(resultU[[1]][4],4)), "\n",
                        #"p-value (two-tailed without adjustment):",substr(sprintf("%.3f",round(resultU[[2]],4)),2,5), "\n",
                        "Effect size (r) =", sprintf("%.3f",r), "\n",
                        "\n"
                        )
      					}
    				}	
  				}
			}
			
			pairWiseU(dat[,2], dat[,1])
            
        }
        
        
        if (input$type == "friedman") {
            
            result <-friedman.test(as.matrix(dat))
			
			print(result)
			
            eta2 <- result$statistic/((length(dat[,1])*(length(dat[1,])-1)))
            names(eta2) <- NULL
            
            cat("Effect size (eta-squared) =", sprintf("%.3f",round(eta2,4)), "\n")
            
            
            # pair-wise comparisons
			cat("\n",
  			"=============================================================", "\n")
  			cat("\n", "Pairwise comparisons (p-values)", "\n")
  			cat("\n", "For details see, http://www.med.osaka-u.ac.jp/pub/kid/clinicaljournalclub1.html", "\n")
			cat("\n",
  			"------------------------", "\n")
            
    
            x <- stack(dat)
            x1 <- x[,1]
            x2 <- x[,2]
    
            
   			# Bonferroni
   			cat("\n",
   			"<< Bonferroni method >>", "\n")
            bon <- pairwise.wilcox.test(x1, x2, p.adj="bonferroni", exact=F, paired=T)
            print(bon)
            
			cat("\n",
  			"------------------------", "\n")
			cat("\n",
			"<< Holm-Bonferroni method >>", "\n")
			# Holm
			holm <- pairwise.wilcox.test(x1, x2, p.adj="holm", exact=F, paired=T)

			print(holm)
			
			
			cat("\n",
  			"------------------------", "\n")
			cat("\n",
			"<< False Discovery Rate >>", "\n")
			# false discovery rate
			fdr <- pairwise.wilcox.test(x1, x2, p.adj="fdr", exact=F, paired=T)
			print(fdr)
            
            
            cat("\n",
            "------------------------", "\n")
            
            cat("\n",
            "=============================================================", "\n")
            cat("\n", "Pairwise comparisons (Test statistics and effect sizes)", "\n")
            cat("\n",
            "------------------------", "\n")
        
            
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
                            r1 <- z/sqrt(length(x[,1]*2))
                            r2 <- z/sqrt(length(x[,1])-sum((x[,2]-x[,1])==0))
                            
                            cat("Comparisons", a[i], "-", a[j], ":", "\n",
                            "V =", res[[1]][1],"\n",
                            "z-value =",sprintf("%.3f",round(z,4)), "\n",
                            "Effect eize r =", round(r1, 3), "\n",
                            "Effect eize r (without considering ties) =", round(r2, 3), "\n",
                            "\n")
                        }
                    }	
                }
            }
            
            pairWiseW(dat)
            
        }
    })






    
    
    
    
    output$textarea.out <- renderPrint({
        bs()
    })
    
    output$ranking.out <- renderPrint({
        ranking()
    })
    
    output$test.out <- renderPrint({
        test()
    })


})
