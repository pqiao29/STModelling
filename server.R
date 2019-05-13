library(shiny)
library(dplyr)
library(copSTM)
library(ggplot2)
library(gridExtra)


shinyServer(function(input, output) {
  
  ## Data tab ====================================================
  get_data <- reactive({
    if(input $ExDat) return(readRDS("ExampleData.rds"))
    
                  req(input$file1)
                 if(tolower(tools::file_ext(input$file1$datapath)) == "csv"){
                     return(read.csv(input$file1$datapath, header = input$header))
                  }else if(tolower(tools::file_ext(input$file1$datapath)) == "txt"){
                     return(read.csv(input$file1$datapath, header = input$header, sep = ""))
                  }
      })
  
  output$indata <- renderDataTable({
    get_data()
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$summary <- renderPrint({
    dt <- get_data()
    br()
    h3(paste0( max(dt[, 4]), " groups, ", max(dt[, 1] + 1), " time points"))
  })
  
  ## Render UI's ------------------------------------------------
  output$sum <- renderUI({
    
    #reactive({ req(input$file1)})
    get_data()
    
    actionButton(inputId = "view_sum", "Show data summary", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 #"color: #F7FAF8; background-color: #0B6E32; border-color: #F7FAF8"
  })
  output$growth_t <- renderUI({
    
    dt <- get_data()
    t_max <- max(dt[, 1]) 
    
    sliderInput('growth_trange', '2-1. Choose a time range', 0, t_max, c(0, t_max), step = 1)
  })
  output$dist_n <- renderUI({
    
   get_data()

  sliderInput('nInput', '2-2. Choose the number of tiles', 5, 40, 5)
  })
  output$dist_t <- renderUI({
    dt <- get_data()
    t_max <- max(dt[, 1])
    
    sliderInput('sld_t', 'Time point for distribution plot', 0, t_max, 0, step = 1)
  })
  
  ## Summary tab ===============================================
   ## Functions -----------------------------
  get_count_data <- reactive({
    
    dt <- get_data()
    n <- input$nInput
    
    ## tilling ---------------------------------------------------------
    Mx <- max(dt[, 2]) + 1
    mx <- min(dt[, 2]) - 1
    My <- max(dt[, 3]) + 1
    my <- min(dt[, 3]) - 1
    l.x <- (Mx - mx)/n
    l.y <- (My - my)/n
    col_num <- findInterval(dt[, 2], seq(mx, Mx, l.x))
    row_num <- findInterval(dt[, 3], seq(my, My, l.y))
    grid <- as.integer((row_num - 1) * n + col_num)
    dt <- cbind(dt, grid)
    
    ## ---------------------------------------------------------
    t_max <- max(dt[, 1]) 
    t_size <- t_max + 1
    K <- max(dt[, 4])
    #count_data <- array(0, dim=c(K, (n^2), t_size))
    count_for_dist <- matrix(0, t_size, n*n)
    count_for_growth <- matrix(0, K, t_size)
    for( t in 0:t_max ){
      data_t <- dt[which(dt[, 1] == t), ]
      tem.y <- matrix(0, K, (n^2))
      for( i in 1:(n^2)){
        ind.i <- which(data_t[ , 5] == i)  ## The ith grid
        if(length(ind.i) > 1 ){
          data_it <- data_t[ind.i,]
          for( k in 1:K ){
            tem.y[k,i] <- sum(data_it[ ,4] == k)  ## Color group
          }
        }
        if(length(ind.i) == 1 ) {
          data_it <- data_t[ind.i,]
          for( k in 1:K ){
            tem.y[k,i] <- sum(data_it[4] == k)
          }
        }
      }
      #count_data[, , (t+1)] <- tem.y
      count_for_dist[t + 1, ] <- apply(tem.y, 2, sum)
      count_for_growth[, t + 1] <- apply(tem.y, 1, sum)
    } 
    
    ret <- list("dist" = count_for_dist, "growth" = count_for_growth, "n" = n)
    return(ret)
  })
  
  SummaryPlot <- function(){
    
    count_list <- get_count_data()
    t_range <- input$growth_trange + 1
    
    count_growth <- (count_list$growth)[, c(t_range[1]: t_range[2])]
    t_size <- ncol(count_growth)
    K <- nrow(count_growth)
    ylmt <- range(count_growth)
    set.seed(K)
    cl <- colours(TRUE)[sample(1:502, K)]
    
    
    fit_plot <- data.frame(Time = rep(c(1:t_size - 1), K), Count = c(t(count_growth))
                           , Group = as.factor(rep(1:K, each = t_size)) )
    
    gg_growth <- ggplot(data = fit_plot, aes(x = Time, y = Count, group = Group)) +
                 ggtitle("Growth Curves") +
                 geom_line(aes(color = Group), size = 1.5)  +
                 scale_color_manual(values = cl) +
                 theme(legend.title = element_blank(), 
                       legend.text=element_text(size= 17), 
                       aspect.ratio=1, 
                       plot.title = element_text(color= 1, size=24, face="bold"), 
                       axis.title.x = element_text(color=1, size = 24),
                       axis.title.y = element_text(color=1, size = 24)) +
                       scale_x_continuous(breaks=seq(1, t_size, 1))
    
    ### Distribution plot -----------------------------------------
    t <- input$sld_t + 1
    n <- count_list$n
    
    count <- count_list$dist[t, ]
    lmts <- range(count_list$dist)
    row_plot <- rep(n:1, n)
    col_plot <- rep(1:n, each = n)
    count_plot <- data.frame("count" = count, "row" = row_plot, "col" = col_plot)
    
    gg_dist <- ggplot(data = count_plot, aes(x = col, y = row)) +
                      ggtitle("Cell Distributions") +
                      geom_tile(aes(fill = count)) +
                      geom_text(aes(label = count)) + 
                      theme(legend.title = element_blank(), 
                            legend.text=element_text(size= 17), 
                            aspect.ratio=1, 
                            plot.title = element_text(color= 1, size=24, face="bold"), 
                            axis.title.x=element_blank(), axis.title.y=element_blank()) +
                            scale_fill_gradient2(low = "white", high = "green4", limit = lmts)
    
    grid.arrange(arrangeGrob(gg_growth, gg_dist, ncol=1, nrow=2))

  }

   ## App plot ------------------------------
  observeEvent(input$view_sum, {
   output$growth <- renderPlot({ SummaryPlot() }, height = 1250, width = 600)
  })
  
   ## Download ------------------------------
  output$downloadSum <- downloadHandler(
    filename = "Summary.pdf",
    content = function(file) {
      ggsave(file, plot = SummaryPlot(), width = 12, height = 24)
    }
  )
  
  ## Estimation tab ======================================================
  ## estimation -----------------------------------
  output$est_buttom <- renderUI({
    
    get_data()
    
    actionButton(inputId = "est", "Model Estimation", 
                 style="color: #fff; background-color: #FC8953; border-color: #B05F33")
  })
  
  EstPlot <- function() {
    
    t_range <- input$growth_trange 
    t_size <- t_range[2] - t_range[1] 
    if(t_size <= 0) stop("Please select time range that contains at least three time points")
    
    dt <- as.matrix(get_data())
    dt <- dt[between(dt[, 1], t_range[1], t_range[2]) , ]
    n <- input$nInput
    
    #########  Estimation ########
    res <- idpSTM(dt, n, 50, TRUE)
    ##############################
    
    ## Estimation plot -----------------------------------------------------------
    est <- res$coefficients$main_effects
    
    K <- max(dt[, 4])
    row_plot <- rep(K:1, K)
    col_plot <- rep(1:K, each = K)   
    beta_plot <- data.frame("beta" = c(est), "row" = row_plot, "col" = col_plot)
    
    gg_est <- ggplot(data = beta_plot, aes(x = col, y = row)) +
              ggtitle("Impact Estimates") +
              geom_tile(aes(fill = beta)) + 
              geom_text(aes(label = round(beta, 2))) + 
              theme(legend.title = element_blank(), axis.title.x=element_blank(), 
                    legend.text=element_text(size= 17), 
                    axis.title.y=element_blank(), aspect.ratio=1, 
                    plot.title = element_text(color= 1, size=24, face="bold")) +
              scale_fill_gradient2(low = "steelblue", high = "red1", mid = "white", na.value="white", midpoint = 0, limit = range(est))
    
    ## Goodness of fit plot ------------------------------------------------------- 
    fit <- res$fitted
    obs <- res$observed
    ylmt <- range(c(fit, obs))
    set.seed(K)
    cl <- colours(TRUE)[sample(1:502, K)]

    fit_plot <- data.frame(Time = rep(c(1:t_size), (2*K)), Count = c(t(fit), t(obs))
                           , Type = as.factor(rep(c(1:(2*K)), each = t_size))
                           , Group = as.factor(rep(rep(1:K, each = t_size), 2))
                           , l = as.factor(rep(c("observed", "fitted"), each = (K*t_size))))

    gg_good <- ggplot(data = fit_plot, aes(x = Time, y = Count, group = Type)) +
               ggtitle("Goodness-of-Fit Curves") +
               geom_line(aes(linetype = l, color = Group), size = 1.5)  +
               scale_color_manual(values = cl) +
               theme(legend.title = element_blank(), 
                     legend.text=element_text(size= 17), 
                     aspect.ratio=1, 
                     plot.title = element_text(color= 1, size=24, face="bold"), 
                     axis.title.x = element_text(color=1, size = 24),
                     axis.title.y = element_text(color=1, size = 24)) +
               scale_x_continuous(breaks=seq(1, t_size, 1))
    
    ## Plots layout ------------------------------------------------------------
    
    list("beta" = est, "plot_est" = gg_est, "plot_good" = gg_good)
     
  }
  
  observeEvent(input$est,{
    
    res <- EstPlot()
    
    Arrange <- function(gg1, gg2){
      grid.arrange(arrangeGrob(gg1, gg2, ncol=1, nrow=2))
    }
    
    output$beta_plot <- renderPlot({ Arrange(res$plot_est, res$plot_good) }, height = 1250, width = 600)

    output$downEst <- downloadHandler(
      filename = "Estimation.pdf",
      content = function(file) {
        ggsave(file, plot = Arrange(res$plot_est, res$plot_good), width = 12, height = 24)
      }
    )
    
    output$downEst_csv <- downloadHandler(
      filename = function(){"Estimation.csv"},
      content = function(fname){
        write.csv(res$beta, fname)
      }
    )
    
  })
  
  ## Selection tab ======================================================
  output$sel_buttom <- renderUI({
    
    get_data()
    
    actionButton(inputId = "select", "Model Selection", 
                 style="color: #fff; background-color: #6BE377; border-color: #6AB555")
  })
  
  Selection <- function(){
    t_range <- input$growth_trange 
    t_size <- t_range[2] - t_range[1] 
    if(t_size <= 0) stop("Please select time range that contains at least three time points")
    
    dt <- as.matrix(get_data())
    dt <- dt[between(dt[, 1], t_range[1], t_range[2]) , ]
    n <- input$nInput
    
    ######### Model Selection ########
    res <- idpSTMSelect(dt, n, 50, ModelCnt = 200, Message = F)
    est <- res$coefficients$main_effects
    ret <- list("beta" = est)
    est[!est] <- NA
    ##############################
    
    K <- max(dt[, 4])
    row_plot <- rep(K:1, K)
    col_plot <- rep(1:K, each = K)   
    beta_plot <- data.frame("beta" = c(est), "row" = row_plot, "col" = col_plot)
    
    gg <- ggplot(data = beta_plot, aes(x = col, y = row)) +
           ggtitle("Impact Selection") +
           geom_tile(aes(fill = beta)) + 
           geom_text(aes(label = round(beta, 2))) + 
           theme(legend.title = element_blank(), axis.title.x=element_blank(), 
                 legend.text=element_text(size= 17), 
                 axis.title.y=element_blank(), aspect.ratio=1, 
                 plot.title = element_text(color= 1, size=24, face="bold")) +
           scale_fill_gradient2(low = "steelblue", high = "red1", mid = "white", na.value="white", midpoint = 0, limit = range(est))
    ret$plot <- gg
    ret
  }
  
  
  observeEvent(input$select,{
    
    res <- Selection()
    
    Selection_plot <- res$plot
    
    output$sel_plot <- renderPlot({ Selection_plot }, height = 600, width = 600)
    
    output$downSel_pdf <- downloadHandler(
      filename = "Selection.pdf",
      content = function(file) {
        ggsave(file, plot = Selection_plot, width = 12, height = 12)
      }
    )
    
    output$downSel_csv <- downloadHandler(
      filename = function(){"Selection.csv"}, 
      content = function(fname){
        write.csv(res$beta, fname)
      }
    )
    
  })
  
})