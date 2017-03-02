library(shiny)
library(ggplot2)
library(survey)
library(tableone)


shinyServer(function(input, output) {
    pC1   <- reactive(input$probC)
    pA1C0   <- reactive(input$probA_C0)
    pA1C1   <- reactive(input$probA_C1)
    pM1A0C0 <- reactive(input$probM_A0C0)
    pM1A0C1 <- reactive(input$probM_A0C1)
    pM1A1C0 <- reactive(input$probM_A1C0)
    pM1A1C1 <- reactive(input$probM_A1C1)
    
    n <- 10^3
    
    dat <- reactive({
        # Take care of the random seed.
        #    Code from Cole in nbpMatching pkg
        if(exists(".Random.seed", envir = .GlobalEnv)) {
            save.seed <- get(".Random.seed", envir= .GlobalEnv)
            on.exit(assign(".Random.seed", save.seed, envir = .GlobalEnv))
        } else {
            on.exit(rm(".Random.seed", envir = .GlobalEnv))
        }
        set.seed(3517)
        
        id <- 1:n
        
        C <- rbinom(n, 1, pC1())
        A <- rbinom(n, 1, pA1C1() * C + pA1C0() * (1 - C))
        M <- rbinom(n, 1, 
            pM1A0C0() * (1 - A) * (1 - C) +
            pM1A0C1() * (1 - A) * C +
            pM1A1C0() * A * (1 - C) +
            pM1A1C1() * A * C 
        )
        dat <- data.frame(id, A, M, C)
        dat <- rbind(dat, dat)
        dat <- within(dat, {
            A_star <- c(A[1:n], 1 - A[1:n])
            AM <- paste0(A, M)
            A_starM <- paste0(A_star, M)
            
            A.factor <- factor(A, levels= 0:1, labels= c('A0', 'A1'))
            M.factor <- factor(M, levels= 0:1, labels= c('M0', 'M1'))
            C.factor <- factor(C, levels= 0:1, labels= c('C0', 'C1'))
            A_star.factor <- factor(A_star, levels= 0:1,
                labels= c('A*0', 'A*1'))
            AM.factor <- factor(AM)
            A_starM.factor <- factor(A_starM)
            
            type <- factor(rep(c("Orig", "Supp"), each= n))
            
            Ycont <- 1.5*A + 1*M + 0.5*C + rnorm(2*n, sd= 0.1)
            Ybin <- rbinom(2*n, 1, plogis(Ycont/2)) 
        })
        dat    
    })
    
    propTableAMC <- reactive({
        # A in rows, M in cols
        table(dat()$A, dat()$M, dat()$C) 
    })
    propTableAC <- reactive({
        # A in rows, C in cols
        table(dat()$A, dat()$C) 
    })
    propTableA <- reactive({
        table(dat()$A) / (n * 2)    
    })
    
    datWithWts <- reactive({
        datw <- dat()  
        
        wM_num <- apply(datw[, c("A_star", "M", "C")], 1,
            function(vec) {
                v1 <- propTableAMC()[vec[1] + 1, vec[2] + 1, vec[3] + 1]
                v2 <- propTableAMC()[vec[1] + 1, (1 - vec[2]) + 1, vec[3] + 1]
                v1 / (v1 + v2)
            }
        )
        wM_denom <- apply(datw[, c("A", "M", "C")], 1,
            function(vec) {
                v1 <- propTableAMC()[vec[1] + 1, vec[2] + 1, vec[3] + 1]
                v2 <- propTableAMC()[vec[1] + 1, (1 - vec[2]) + 1, vec[3] + 1]
                v1 / (v1 + v2)
            }
        )
        wA_num <- sapply(datw[, c("A")],
            function(x) propTableA()[x + 1]
        )
        wA_denom <- apply(datw[, c("A", "C")], 1,
            function(vec) {
                v1 <- propTableAC()[vec[1] + 1, vec[2] + 1]
                v2 <- propTableAC()[(1 - vec[1]) + 1, vec[2] + 1]
                v1 / (v1 + v2)
            }
        )
        
        datw <- within(datw, {
            wA <- wA_num / wA_denom
            wM <- wM_num / wM_denom
            W <- wA * wM
        })
        datw
    })
    datWithWtsWide <- reactive({
        datOrig <- datWithWts()[datWithWts()$type == "Orig", ]    
        datSupp <- datWithWts()[datWithWts()$type == "Supp", ]  
        cbind(datOrig, wM2 = datSupp$wM, W2= datSupp$W)
    })
    
    output$wtPlot <- renderPlot({
        #wMrange <- range(datWithWtsWide()$wM)
        #wM2range <- range(datWithWtsWide()$wM2)
        Wrange <- range(datWithWtsWide()$W)
        W2range <- range(datWithWtsWide()$W2)
        lims <- 
            #c(min(wMrange[1], wM2range[1]), max(wMrange[2], wM2range[2]))
            c(min(Wrange[1], W2range[1]), max(Wrange[2], W2range[2]))
        
        ggplot(data= datWithWtsWide(),
            mapping=  aes(
                #x= wM, y= wM2, 
                x= W, y= W2, 
                colour= C.factor)) +
        geom_abline(slope= 1, intercept= 0) +
        geom_point(size= 3, alpha= 0.1) +
        xlim(lims) +
        ylim(lims) + 
        #xlab("W_M on original row") +
        #ylab("W_M on supplemental row") +
        xlab("W on original row") +
        ylab("W on supplemental row") +
        facet_grid(M.factor ~ A.factor)    
    })
  
    
    vars1 <- c("C.factor", "M.factor", "A_star.factor", "AM.factor", "A_starM.factor")
    vars2 <- c("C.factor", "M.factor", "A.factor", "AM.factor", "A_starM.factor")
    tabOrig <- reactive({
        ## Create a TableOne object
        CreateTableOne(
            vars       = vars1,
            strata     = "A.factor",
            data       = datWithWts(),
            factorVars = vars1,
            includeNA  = FALSE,
            test       = FALSE,
            smd        = TRUE
        )
    })    
    svydatW <- reactive({
        svydesign(ids = ~ 0, data = datWithWts(), 
        weights = ~ W)
    })
    tabWtdW <- reactive({
        ## Create a TableOne object
        svyCreateTableOne(
            vars       = vars1,
            strata     = "A.factor",
            data       = svydatW(),
            factorVars = vars1,
            includeNA  = FALSE,
            test       = FALSE,
            smd        = TRUE
        )
    })    
    tabWtdW_byA_star <- reactive({
        ## Create a TableOne object
        svyCreateTableOne(
            vars       = vars2,
            strata     = "A_star.factor",
            data       = svydatW(),
            factorVars = vars2,
            includeNA  = FALSE,
            test       = FALSE,
            smd        = TRUE
        )
    })    
    tabWtdWa <- reactive({
        # Create a survey object
        svydat <- svydesign(ids = ~ 0, data = datWithWts(), 
            weights = ~ wA)

        ## Create a TableOne object
        svyCreateTableOne(
            vars       = vars1,
            strata     = "A.factor",
            data       = svydat,
            factorVars = vars1,
            includeNA  = FALSE,
            test       = FALSE,
            smd        = TRUE
        )
    })    
    tabWtdWm <- reactive({
        # Create a survey object
        svydat <- svydesign(ids = ~ 0, data = datWithWts(), 
            weights = ~ wM)

        ## Create a TableOne object
        svyCreateTableOne(
            vars       = vars1,
            strata     = "A.factor",
            data       = svydat,
            factorVars = vars1,
            includeNA  = FALSE,
            test       = FALSE,
            smd        = TRUE
        )
    })    
    output$showtabOrig <- renderPrint({
        print(tabOrig())    
    })
    output$showtabWtdW <- renderPrint({
        print(tabWtdW())    
    })
    output$showtabWtdW_byA_star <- renderPrint({
        print(tabWtdW_byA_star())    
    })
    output$showtabWtdWa <- renderPrint({
        print(tabWtdWa())    
    })
    output$showtabWtdWm <- renderPrint({
        print(tabWtdWm())    
    })
    
    
    # regressions
    output$lm1 <- renderPrint({
        print(svyglm(Ycont ~ A + A_star, svydatW()))    
    })
    
    output$lm1Unweighted <- renderPrint({
        print(lm(Ycont ~ A + A_star, data= dat()))    
    })
    
})
