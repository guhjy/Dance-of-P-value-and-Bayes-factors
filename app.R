# 載入所需套件
library(shiny)
library(ggplot2)
library(dplyr)
library(BayesFactor)

# 定義使用者介面 (UI)
ui <- fluidPage(
    # 設定頁面語言為繁體中文
    tags$head(
        tags$meta(charset = "UTF-8"),
        tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
        tags$script('Shiny.addCustomMessageHandler("setLocale", function(message) { try { Intl.NumberFormat("zh-TW"); } catch (e) { console.error("zh-TW locale not supported"); } });')
    ),
    titlePanel("P 值與貝氏因子 BF10 的「跳舞」-為什麼單次試驗不可靠？"),

    sidebarLayout(
        sidebarPanel(
            helpText("在固定樣本數'n'下執行多次模擬，觀察P值與貝氏因子 BF10 的分佈，以及特定結果的長期頻率。https://www.esci-dances.thenewstatistics.com/"),
            numericInput("n_fixed", "固定樣本數 (每組 n):", value = 50, min = 5, max = 1000, step = 1) , # 
            numericInput("delta", "真實效果量 (效果量 Cohen’s d: 平均值差異 Delta=平均值/標準差, 0.2 小, 0.5 中等, 0.8 大):", value = 0.2, step = 0.1) , # 
            numericInput("sd", "標準差 (SD):", value = 1, min = 0.1, step = 0.1) , # 
            numericInput("r_scale", "H1 的先驗: 科西分佈的尺度 0.707, cauchy(0, 0.707) 有 50% 在 -0.707 至 0.707, 有 95% 在 -4.659 至 4.659, 比常態分布更能容忍極端值", value = sqrt(2)/2, min = 0.1, step = 0.1) , # # Changed label slightly
            numericInput("num_sims", "模擬次數:", value = 1000, min = 100, max = 20000, step = 100) , # 
            actionButton("run_sim", "執行模擬", icon = icon("play")) , # 
            hr(),
            tags$b("目標條件:"),
            tags$ul(
                tags$li("P值 < 0.05, p < 0.005 的頻率") , # 
                tags$li("95% 信賴區間包含真實平均值的頻率") , # 
                tags$li("貝氏因子 (BF10) > 3 (支持 H1 的中等證據) 的頻率") , # 
                tags$li("貝氏因子 (BF10) > 10 (支持 H1 的強證據) 的頻率") # 
            )
        ),

        mainPanel(
            h4("模擬摘要:"),
            verbatimTextOutput("summary_stats") , # 
            hr(),
            fluidRow(
                column(6, plotOutput("p_value_hist")),
                column(6, plotOutput("bf_hist"))
            )
        )
    )
)

# 定義伺服器邏輯 (Server)
server <- function(input, output, session) {

    session$onFlushed(function() {
        session$sendCustomMessage("setLocale", list())
    }, once = TRUE) # 

    simulation_results <- eventReactive(input$run_sim, {
        n_sims <- input$num_sims # 

        withProgress(message = '執行模擬中...', value = 0, {

            n <- input$n_fixed # 
            delta <- input$delta # 
            sd <- input$sd # 
            r_scale <- input$r_scale # 
            theoretical_se <- sd * sqrt(2 / n) # 

            p_values <- numeric(n_sims) # 
            log10_bfs <- numeric(n_sims) # 
            ci_covers_delta <- logical(n_sims) # 

            for (i in 1:n_sims) {
                if (i %% 100 == 0) { # 
                    incProgress(100/n_sims, detail = paste("模擬", i)) # 
                 }

                group1 <- rnorm(n, mean = 0, sd = sd) # 
                group2 <- rnorm(n, mean = delta, sd = sd) # 

                t_test_result <- tryCatch({
                     t.test(group2, group1, var.equal = TRUE, conf.level = 0.95) # 
                 }, error = function(e) NULL)

                log10_bf <- tryCatch({
                    bf_result <- BayesFactor::ttestBF(x = group2, y = group1, rscale = r_scale, paired=FALSE)
                    log10(exp(bf_result@bayesFactor$bf)) # 
                }, error = function(e) NA)

                if (!is.null(t_test_result) && !is.na(log10_bf) && is.finite(log10_bf)) {
                    p_values[i] <- t_test_result$p.value # 
                    log10_bfs[i] <- log10_bf # 
                    conf_int <- t_test_result$conf.int # 
                    ci_covers_delta[i] <- (conf_int[1] <= delta && conf_int[2] >= delta) # 
                } else {
                     p_values[i] <- NA # 
                     log10_bfs[i] <- NA # 
                     ci_covers_delta[i] <- NA # 
                }
            }

            valid_indices <- !is.na(p_values) & !is.na(log10_bfs) & !is.na(ci_covers_delta) # 
            p_values <- p_values[valid_indices] # 
            log10_bfs <- log10_bfs[valid_indices] # 
            ci_covers_delta <- ci_covers_delta[valid_indices] # 
            actual_sims_run <- length(p_values) # 

            if (actual_sims_run == 0) {
              return(list(error = "所有模擬均失敗。請檢查參數（例如，n >= 2）。")) # 
            }

            # *** 新增：計算 BF 圖的範圍並檢查超出範圍的點 ***
            num_outside_bf_range <- 0
            if(actual_sims_run > 0) {
                bf_boundaries_vals <- c(log10(10), log10(3), 0, log10(1/3), log10(1/10)) # Values from bf_boundaries 
                plot_min_calc <- min(c(log10_bfs, bf_boundaries_vals), na.rm = TRUE) - 0.5 # Similar logic to plot 
                plot_max_calc <- max(c(log10_bfs, bf_boundaries_vals), na.rm = TRUE) + 0.5 # Similar logic to plot 
                num_outside_bf_range <- sum(log10_bfs < plot_min_calc | log10_bfs > plot_max_calc, na.rm = TRUE)
            }
            # *** 新增結束 ***


            percent_p_less_05 <- mean(p_values < 0.05) * 100 # 
            percent_ci_covers <- mean(ci_covers_delta) * 100 # 
            percent_bf_greater_3 <- mean(log10_bfs > log10(3)) * 100 # 
            percent_bf_greater_10 <- mean(log10_bfs > 1) * 100 #
  
            percent_p_less_005 <- mean(p_values < 0.005, na.rm = TRUE) * 100 # Calculate percentage for p < 0.005

            failed_sims <- n_sims - actual_sims_run # 

            list(
                p_values = p_values, # 
                log10_bfs = log10_bfs, # 
                percent_p_less_05 = percent_p_less_05, # 
                percent_ci_covers = percent_ci_covers, # 
                percent_bf_greater_3 = percent_bf_greater_3, # 
                percent_bf_greater_10 = percent_bf_greater_10, # 
                percent_p_less_005 = percent_p_less_005, #

                n = n, # 
                delta = delta, # 
                sd = sd, # 
                theoretical_se = theoretical_se, # 
                num_sims_requested = n_sims, # 
                num_sims_completed = actual_sims_run, # 
                failed_sims = failed_sims, # 
                num_outside_bf_range = num_outside_bf_range # *** 新增回傳值 ***
            )
        })
    })

    output$summary_stats <- renderPrint({
        results <- simulation_results()

        if (!is.null(results$error)) { # 
          cat("錯誤:", results$error, "\n") # 
          return() # 
        }

         cat("貝氏定理：H1 的後驗機率 p(H1|D)= p(H1) x p(D|H1)/p(D)\n")
         cat("=先驗機率 x 似然率/邊際似然率\n")
         cat("H1 的後驗勝算= H1 的先驗勝算 x 貝氏因子 BF10\n")
         cat("勝算= 機率/(1-機率)，機率= 勝算/(勝算+1)\n")
          cat("若先驗勝算 1, 則 BF10=3, 10 時後驗機率=0.75, 0.91\n")
        cat("\n--- 模擬參數 ---\n")
        cat("固定樣本數 (n 每組):", results$n, "\n") # 
        cat("真實平均值差異 (Delta):", results$delta, "\n") # 
        cat("標準差 (SD):", results$sd, "\n") # 
        cat("理論標準誤 (SE):", sprintf("%.4f", results$theoretical_se), "\n") # 
        cat("要求模擬次數:", results$num_sims_requested, "\n") # 
        cat("完成模擬次數:", results$num_sims_completed, "\n") # 
        if(results$failed_sims > 0) { # 
            cat("失敗模擬次數:", results$failed_sims, "\n") # 
        }
        cat("\n--- 結果 ---\n")
        cat(sprintf("P值 < 0.05 的模擬百分比: %.2f%%\n", results$percent_p_less_05)) # 
        cat(sprintf("P值 < 0.005 的模擬百分比: %.2f%%\n", results$percent_p_less_005)) # Display the new percentage

        cat(sprintf("95%% CI 包含真實 Delta 的模擬百分比: %.2f%%\n", results$percent_ci_covers)) # 
        cat(" (注意: 這是模擬 n 次之後的結果, 但是單次 CI 包含真值的機率是 0 或 1）\n")
        cat(sprintf("BF10 > 3 (中等證據) 的模擬百分比: %.2f%%\n", results$percent_bf_greater_3)) # 
        cat(sprintf("BF10 > 10 (強證據) 的模擬百分比: %.2f%%\n", results$percent_bf_greater_10)) # 

        # *** 新增：顯示超出範圍的點的訊息 ***
        if (results$num_outside_bf_range > 0) {
           cat(sprintf("\n注意：有 %d 個模擬的 Log10(BF10) 值超出圖表範圍，未顯示在貝氏因子分佈圖中。\n", results$num_outside_bf_range))
        }
        # *** 新增結束 ***

    }) # # Closing renderPrint

    output$p_value_hist <- renderPlot({
        results <- simulation_results()
        req(results$p_values)
        df <- data.frame(p_value = results$p_values)
        ggplot(df, aes(x = p_value)) +
            geom_histogram(aes(y = ..density..), binwidth = 0.02, boundary = 0, fill = "lightblue", color = "black") + # 
            geom_density(color = "blue", alpha = 0.5) + # 
            geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") + # 
            scale_x_continuous(breaks = seq(0, 1, 0.1)) + # 
            labs(
                title = "P值分佈", # 
                subtitle = paste(results$num_sims_completed, "次模擬 (n =", results$n, ", Delta =", results$delta, ")"), # 
                x = "P值", # 
                y = "密度" # 
            ) +
            theme_minimal(base_family = "sans") + # 
            annotate("text", x = 0.06, y = Inf, label = "p = 0.05", color = "red", hjust = 0, vjust = 1.5, family="sans") # 
    })

    output$bf_hist <- renderPlot({
        results <- simulation_results()
        req(results$log10_bfs)
        df <- data.frame(log10_bf10 = results$log10_bfs)
        bf_boundaries <- data.frame( # 
            log10_val = c(log10(10), log10(3), 0, log10(1/3), log10(1/10)), # 
            label = c("強 H1", "中等 H1", "軼事", "中等 H0", "強 H0") # 
        )
        target_bf3_log10 <- log10(3) # 
        target_bf10_log10 <- log10(10) # 

        # *** 注意：這裡的 plot_min/plot_max 計算邏輯需要與上面 eventReactive 中的一致 ***
        bf_boundaries_vals <- bf_boundaries$log10_val # 
        plot_min <- min(c(df$log10_bf10, bf_boundaries_vals), na.rm = TRUE) - 0.5 # 
        plot_max <- max(c(df$log10_bf10, bf_boundaries_vals), na.rm = TRUE) + 0.5 # 


        ggplot(df, aes(x = log10_bf10)) +
            geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "lightgreen", color = "black") + # 
             geom_density(color = "darkgreen", alpha = 0.5) + # 
            geom_vline(data=bf_boundaries, aes(xintercept = log10_val), linetype = "dotted", color = "grey40") + # 
            geom_vline(xintercept = target_bf3_log10, linetype = "dashed", color = "darkorange", size=0.8) + # 
            geom_vline(xintercept = target_bf10_log10, linetype = "dashed", color = "darkred", size=1) + # 
            # 使用 coord_cartesian 替代 scale_x_continuous(limits=...) 來 "放大" 而非移除數據
            coord_cartesian(xlim = c(plot_min, plot_max)) + # *** 修改：使用 coord_cartesian ***
            labs(
                title = "Log10(貝氏因子)分佈", # 
                subtitle = paste(results$num_sims_completed, "次模擬 (n =", results$n, ", Delta =", results$delta, ", H1 vs H0)"), # 
                x = "Log10(BF10)", # 
                y = "密度" # 
            ) +
            theme_minimal(base_family = "sans") + # 
            geom_text(data=bf_boundaries, aes(x = log10_val, y = -Inf, label=label), vjust=-0.5, size=3, angle=90, hjust=0, color="grey40", family="sans") + # 
            annotate("text", x = target_bf3_log10 + 0.05, y = Inf, label = "BF=3", color = "darkorange", hjust = 0, vjust = 1.5, family="sans") + # 
             annotate("text", x = target_bf10_log10 + 0.05, y = Inf, label = "BF=10", color = "darkred", hjust = 0, vjust = 1.5, family="sans") # 
    })

} # Closing server function 

shinyApp(ui = ui, server = server) # 
