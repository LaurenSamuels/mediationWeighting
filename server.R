library(shiny)
library(ggplot2)

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
            A.factor <- factor(A, levels= 0:1, labels= c('A0', 'A1'))
            M.factor <- factor(M, levels= 0:1, labels= c('M0', 'M1'))
            C.factor <- factor(C, levels= 0:1, labels= c('C0', 'C1'))
            A_star.factor <- factor(A_star)
            type <- factor(rep(c("Orig", "Supp"), each= n))
        })
        
        dat    
    })
    
    propTable <- reactive({
        # A in rows, M in cols
        table(dat()$A, dat()$M, dat()$C) 
    })
    propTableA <- reactive({
        table(dat()$A) / (n * 2)    
    })
    
    datWithWts <- reactive({
        datw <- dat()  
        
        wM_num <- apply(datw[, c("A_star", "M", "C")], 1,
            function(vec) {
                v1 <- propTable()[vec[1] + 1, vec[2] + 1, vec[3] + 1]
                v2 <- propTable()[(1 - vec[1]) + 1, vec[2] + 1, vec[3] + 1]
                v1 / (v1 + v2)
            }
        )
        wM_denom <- apply(datw[, c("A", "M", "C")], 1,
            function(vec) {
                v1 <- propTable()[vec[1] + 1, vec[2] + 1, vec[3] + 1]
                v2 <- propTable()[(1 - vec[1]) + 1, vec[2] + 1, vec[3] + 1]
                v1 / (v1 + v2)
            }
        )
        #wA_num <- sapply(datw[, c("A")],
        #    function(x) propTableA()[x + 1]
        #)
        #wA_denom <- apply(datw[, c("A", "M", "C")], 1,
                # wrong
        #    function(vec) propTable()[vec[1] + 1, vec[2] + 1, vec[3] + 1]
        #)
        
        datw <- within(datw, {
            #wA <- wA_num / wA_denom
            wM <- wM_num / wM_denom
        })
        datw
    })
    datWithWtsWide <- reactive({
        datOrig <- datWithWts()[datWithWts()$type == "Orig", ]    
        datSupp <- datWithWts()[datWithWts()$type == "Supp", ]  
        cbind(datOrig, wM2 = datSupp$wM)
    })
    
    output$wtPlot <- renderPlot({
        wMrange <- range(datWithWtsWide()$wM)
        wM2range <- range(datWithWtsWide()$wM2)
        lims <- 
            c(min(wMrange[1], wM2range[1]), max(wMrange[2], wM2range[2]))
        
        ggplot(data= datWithWtsWide(),
            mapping= aes(x= wM, y= wM2, colour= C.factor)) +
        geom_abline(slope= 1, intercept= 0) +
        geom_point(size= 3, alpha= 0.1) +
        xlim(lims) +
        ylim(lims) + 
        xlab("W_M on original row") +
        ylab("W_M on supplemental row") +
        facet_grid(A.factor ~ M.factor)    
    
    })
  
})
