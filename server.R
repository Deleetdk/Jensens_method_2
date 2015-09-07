
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


# functions ----------------------------------------------------------------
#string concatenation function
"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}


# settings ----------------------------------------------------------------

##initial loadings
#the initial setup of the loadings table
d = data.frame(c(.77, .81, .65, .75, .66, .65, .82, .45, .59),
               c(rep(.5, 3), rep(0, 6)),
               c(rep(0, 3), rep(.5, 3), rep(0, 3)),
               c(rep(0, 6), rep(.5, 3)))

num_gf = ncol(d) - 1 #subtract g
colnames(d) = c("g", str_c("F", 1:num_gf))
rownames(d) = str_c("I", 1:nrow(d))

##Initial differences
d2 = data.frame(d = c(1, -.5, .5, 0, 0))

shinyServer(function(input, output) {
  
  reac_d = reactive({
    #fetch updated
    if (!is.null(input$loadings_table)) {
      d = as.data.frame(hot_to_r(input$loadings_table)) #convert to df because the received is a chacracter matrix
      d$s = NULL
    }

    #calculate s
    d$s = apply(d, 1, function(x) {
      var_g_group = sum(x^2)
      var_remain = 1 - var_g_group
      loading_specificity = sqrt(var_remain)
      return(loading_specificity)
      }
    )
    
    num_gf = ncol(d)-2 #subtract g and s
    colnames(d) = c("g", str_c("F", 1:num_gf), "s")
    rownames(d) = str_c("I", 1:nrow(d))
    
    return(d)
  })
  
  reac_d_case = reactive({
    #update
    #this is to avoid spamming the server with generate data requests
    input$update
    
    
    #this code is only run when update is changed
    isolate({
      #settings
      n = 1e2
      set.seed(1)
      
      #fetch d
      d = reac_d()
      
      #how many LVs
      num_LV = ncol(d) - 1 #subtract g
      
      #factor scores
      d_fact_scores = data.frame(matrix(rnorm(num_LV * n), nrow = n))
      colnames(d_fact_scores) = colnames(d)[-length(colnames(d))]
      
      #indicator scores
      d_case = data.frame(matrix(ncol = nrow(d), nrow=n))
      colnames(d_case) = rownames(d)
      
      for (row_i in 1:nrow(d_case)) {
        if (row_i %% 100 == 0) {
          print(str_c(row_i, " of ", n))
        }
        for (col_i in 1:ncol(d_case)) {
          scores = c(d_fact_scores[row_i, ], rnorm(1)) #generate std scores
          weights = d[col_i, ] #get weights
          wtd_score = sum(scores * weights)
          d_case[row_i, col_i] = wtd_score
        }
      }
    })
    
    return(d_case)
  })
  
  reac_d_indicators = reactive({
    #fetch d
    d = reac_d()
    
    #make indicator list
    d_indicators = list()
    d_indicators$g = str_c(rownames(d)[d$g > 0], collapse = " + ")
    
    #group_factors = grep("F", colnames(d), value = T) #grep method
    group_factors <<- colnames(d)[str_detect(colnames(d), "F")]
    
    for (gf in group_factors) {
      #list with structure
      d_indicators[[gf]] = str_c(rownames(d)[d[gf] > 0], collapse = " + ")
    }
    
    print(d_indicators)
    return(d_indicators)
  })
  
  #Table input
  output$loadings_table = renderRHandsontable({
    #fetch table
    d = reac_d()
    d_case = reac_d_case()

    #change to hs table
    d = rhandsontable(d, useTypes = F) %>% 
      hot_table(highlightCol = T, highlightRow = T,
                allowRowEdit = T, allowColEdit = T)
    return(d)
  })
  
  output$SEM_bi = renderPlot({
    #update
    #this is to avoid spamming the server with generate data requests
    input$update
    
    isolate({
      #fetch data
      d = reac_d()
      d_case = reac_d_case()
      d_indicators = reac_d_indicators()
      
      #set up model
      model = ""
      for (LV_i in seq_along(d_indicators)) {
        LV_name = names(d_indicators[LV_i])
        model = model + LV_name + " =~ " + d_indicators[LV_i] + "\n"
      }
      
      #fit
      fit = sem(model, data = d_case, std.lv = T, orthogonal = T)
      
      #plot
      semPaths(fit, "model", "std", bifactor = "g", layout = "tree2", residuals = F, exoCov = F)
    })
    
  })
  
  output$SEM_hier = renderPlot({
    #update
    #this is to avoid spamming the server with generate data requests
    input$update
    
    isolate({
      #fetch data
      d = reac_d()
      d_case = reac_d_case()
      d_indicators = reac_d_indicators()
      
      #set up model
      model = "g =~ " + str_c(group_factors, collapse = " + ") + "\n"
      for (LV_i in seq_along(d_indicators)) {
        #skip g factor
        if (LV_i == 1) next
        
        #set loadings
        LV_name = names(d_indicators[LV_i])
        model = model + LV_name + " =~ " + d_indicators[LV_i] + "\n"
      }
      
      #fit
      fit = sem(model, data = d_case, std.lv = T, orthogonal = F)
      
      #plot
      semPaths(fit, "model", "std", layout = "tree2", residuals = F, exoCov = F)
    })
    
  })
  
  output$SEM_single = renderPlot({
    #update
    #this is to avoid spamming the server with generate data requests
    input$update
    
    isolate({
      #fetch data
      d = reac_d()
      d_case = reac_d_case()
      d_indicators = reac_d_indicators()
      
      #set up model
      model = "g =~ " + str_c(d_indicators, collapse = " + ")
      
      #fit
      fit = sem(model, data = d_case, std.lv = T, orthogonal = F)
      
      #plot
      semPaths(fit, "model", "std", layout = "tree2", residuals = F, exoCov = F)
    })
    
  })
  
  
  output$EFA_single = renderPlot({
    #update
    #this is to avoid spamming the server with generate data requests
    input$update
    
    isolate({
      #fetch data
      d = reac_d()
      d_case = reac_d_case()
      d_indicators = reac_d_indicators()

      #plot
      qgraph.efa(d_case, factor = 1, corMat = F, residuals = F)
#       fa = fa(d_case)
#       plot(fa)
      #semPaths(fit, "model", "std", layout = "tree2", residuals = F, exoCov = F)
    })
  })
  
})



