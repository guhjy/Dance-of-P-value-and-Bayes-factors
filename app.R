# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(BayesFactor)

# Define User Interface (UI)
ui <- fluidPage(
    # Set page language to Traditional Chinese
    tags$head(
        tags$meta(charset = "UTF-8"),
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
        tags$script('Shiny.addCustomMessageHandler("setLocale", function(message) {
            try { Intl.NumberFormat("zh-TW"); }
            catch (e) { console.error("zh-TW locale not supported"); }
        });'),
        tags$style(HTML("
          .selectize-dropdown-content .option {
            white-space: normal;
            word-break: break-word;
          }
        "))
    ),
    titlePanel("P 值與貝氏因子 BF10 的「跳舞」與序列更新 -為什麼單次試驗不可靠？"), # Updated title

    sidebarLayout(
        sidebarPanel(
            helpText("在固定樣本數'n'下執行多次模擬，觀察P值與貝氏因子 BF10 的分佈，以及特定結果的長期頻率。此版本會將前一次模擬的後驗機率作為下一次模擬的先驗機率。https://www.esci-dances.thenewstatistics.com/"), # Updated help text

            # --- 可折疊的驚訝值說明 ---
            tags$details(
                tags$summary("顯示/隱藏：驚訝值 (S-value) 說明"),
                tags$pre(
"驚訝值 S 提供另一種解釋 p 值的方式，用「位元 (bits)」量化相對於某個假設（通常是 H0）的「驚訝程度」或「資訊量」。
定義：S = -log₂(p)，其中 p 是 p 值。反過來說，p = (1/2)ˢ。
用「位元 (bits)」量化相對於某個假設（通常是 H0）的「驚訝程度」或「資訊量」。
定義：S = -log₂(p)，其中 p 是 p 值。反過來說，p = (1/2)ˢ。
直觀解釋：S 值為 'x' 位元，相當於連續擲一枚公平硬幣 'x' 次，每次都出現正面（或反面）的驚訝程度。

範例驚訝值：
  S = 4 位元 (p = 1/16 = 0.0625)：連續擲 4 次硬幣皆同面的驚訝程度。
  S = 7 位元 (p ≈ 0.008)：連續擲 7 次硬幣皆同面的驚訝程度。
  S = 8 位元 (p ≈ 0.004)：連續擲 8 次硬幣皆同面的驚訝程度。
  S = 10 位元 (p ≈ 0.001)：連續擲 10 次硬幣皆同面的驚訝程度。

主要特性：
  S 值是等距變項 (Interval Variable)，例如 S=8 的資訊量確實是 S=4 的 2 倍。
  針對同一主題之不同獨立研究的 S 值可以相加。

與 p 值的對比：
  p 值並非等距變項，例如 p=0.05 並非 p=0.01 資訊量的 5 倍。
  p 值不能直接相加來整合證據。

大約的 S 值與 p 值對照關係：
  S 值   |  p 值
  -------|---------
  ≤ 1    | ≥ 0.36- 幾乎沒有提供反對 H0 的資訊
  ≈ 2    | 0.18 - 0.35
  ≈ 3    | 0.1 - 0.17
  ≈ 4    | 0.05 - 0.09 (p=0.05 約為 S=4.3)
  ≈ 7    | 0.006 - 0.01
  ≈ 8    | 0.003 - 0.005
  ≥ 10   | ≤ 0.001

假設檢定：
  S 值越高，與虛無假設 H0 的相容性越低，越能提供排除 H0 的證據。
  無論 S 值多低（p 值多高），都無法用來「接受」或「證實」H0 為真。
  低 S 值僅代表缺乏反對 H0 的證據，而非 H0 為真證據。"
                )
            ),
            hr(),

            numericInput("n_fixed", "固定樣本數 (每組 n):", value = 50, min = 5, max = 1000, step = 1),
            numericInput("delta", "真實效果量 (效果量 Cohen’s d: 平均值差異 Delta=平均值/標準差, 0.2 小, 0.5 中等, 0.8 大):", value = 0.2, step = 0.1),
            numericInput("sd", "標準差 (SD):", value = 1, min = 0.1, step = 0.1),
            numericInput("r_scale", "H1 的先驗: 科西分佈的尺度 0.707, cauchy(0, 0.707) 有 50% 在 -0.707 至 0.707, 有 95% 在 -4.659 至 4.659, 比常態分布更能容忍極端值", value = sqrt(2)/2, min = 0.1, step = 0.1),
            numericInput("num_sims", "序列模擬步數:", value = 1000, min = 100, max = 20000, step = 100), # Changed label
            actionButton("run_sim", "執行序列模擬", icon = icon("play")), # Changed label
            sliderInput("prior_prob", "初始 H1 的先驗機率 (p[H1]):", min = 0.01, max = 0.99, value = 0.5, step = 0.01), # Changed label
            hr(),
            tags$b("目標條件 (基於所有序列步驟的頻率):"), # Updated label
            tags$ul(
                tags$li("P值 < 0.05, p < 0.005 的頻率"),
                tags$li("95% 信賴區間包含真實平均值的頻率"),
                tags$li("貝氏因子 (BF10) > 3, 10 (支持 H1 的中等, 強證據) 的頻率"),
                tags$li("貝氏因子 (BF10) < 1/3, 1/10 (支持 H0 的中等, 強證據) 的頻率")
            )
        ),

        mainPanel(
            h4("序列模擬摘要:"), # Updated title
            verbatimTextOutput("summary_stats"),
            hr(),
            fluidRow(
                column(6, plotOutput("p_value_hist")),
                column(6, plotOutput("bf_hist"))
            ),
            fluidRow(
                 column(6, plotOutput("p_value_dance")),
                 column(6, plotOutput("bf_dance"))
            ),
            fluidRow(
                 column(12, plotOutput("posterior_prob_dance")) # New plot for posterior probability
            )
        )
    )
)

# Define Server Logic
server <- function(input, output, session) {

    session$onFlushed(function() {
        session$sendCustomMessage("setLocale", list())
    }, once = TRUE)

    simulation_results <- eventReactive(input$run_sim, {
        n_sims <- input$num_sims

        withProgress(message = '執行序列模擬中...', value = 0, { # Updated message

            n <- input$n_fixed
            delta <- input$delta
            sd <- input$sd
            r_scale <- input$r_scale
            theoretical_se <- sd * sqrt(2 / n)

            # Initialize lists to store results for each step in the sequence
            p_values <- list()
            log10_bfs <- list()
            ci_covers_delta <- list()
            posterior_probs_h1 <- list() # Store posterior probability after each step

            # Initialize the prior probability for the first simulation
            current_prior_prob_h1 <- input$prior_prob

            for (i in 1:n_sims) {
                if (i %% 100 == 0) {
                    incProgress(100/n_sims, detail = paste("模擬步驟", i)) # Updated message
                 }

                # Generate data for one study (one step in the sequence)
                group1 <- rnorm(n, mean = 0, sd = sd)
                group2 <- rnorm(n, mean = delta, sd = sd)

                # Calculate t-test and p-value
                t_test_result <- tryCatch({
                     t.test(group2, group1, var.equal = TRUE, conf.level = 0.95)
                 }, error = function(e) NULL)

                # Calculate BF10 using the fixed rscale
                log10_bf <- tryCatch({
                    bf_result <- BayesFactor::ttestBF(x = group2, y = group1, rscale = r_scale, paired=FALSE)
                    log10(exp(bf_result@bayesFactor$bf))
                }, error = function(e) NA)

                # Store results and perform sequential update if calculation was successful
                if (!is.null(t_test_result) && !is.na(log10_bf) && is.finite(log10_bf)) {
                    p_values[[i]] <- t_test_result$p.value
                    log10_bfs[[i]] <- log10_bf
                    conf_int <- t_test_result$conf.int
                    ci_covers_delta[[i]] <- (conf_int[1] <= delta && conf_int[2] >= delta)

                    # Calculate posterior probability using current prior and BF10
                    current_bf10 <- exp(log(10) * log10_bf) # Convert log10(BF10) back to BF10
                    # Ensure prior is not 0 or 1 to avoid division by zero or log(0)
                    current_prior_prob_h1_clamped <- max(1e-9, min(1 - 1e-9, current_prior_prob_h1))
                    current_prior_odds <- current_prior_prob_h1_clamped / (1 - current_prior_prob_h1_clamped)

                    current_posterior_odds <- current_prior_odds * current_bf10
                    current_posterior_prob_h1 <- current_posterior_odds / (1 + current_posterior_odds)

                    # Store the calculated posterior probability for this step
                    posterior_probs_h1[[i]] <- current_posterior_prob_h1

                    # Update the prior probability for the next simulation step
                    current_prior_prob_h1 <- current_posterior_prob_h1

                } else {
                    # Handle failed simulation step
                    p_values[[i]] <- NA
                    log10_bfs[[i]] <- NA
                    ci_covers_delta[[i]] <- NA
                    # If calculation failed, use the posterior from the previous successful step as the prior for the next step.
                    # The value in posterior_probs_h1[[i]] will be NA, but current_prior_prob_h1 is not updated.
                    posterior_probs_h1[[i]] <- NA # Store NA for posterior if calculation failed
                }
            }

            # Convert lists to vectors for easier processing, removing NAs for summary stats
            # Keep NAs in vectors for dance plots to show gaps if steps failed
            p_values_vec <- unlist(p_values)
            log10_bfs_vec <- unlist(log10_bfs)
            ci_covers_delta_vec <- unlist(ci_covers_delta)
            posterior_probs_h1_vec <- unlist(posterior_probs_h1)

            # Filter out NAs for summary statistics calculations
            valid_indices_for_summary <- !is.na(p_values_vec) & !is.na(log10_bfs_vec) & !is.na(ci_covers_delta_vec) & !is.na(posterior_probs_h1_vec)

            p_values_summary <- p_values_vec[valid_indices_for_summary]
            log10_bfs_summary <- log10_bfs_vec[valid_indices_for_summary]
            ci_covers_delta_summary <- ci_covers_delta_vec[valid_indices_for_summary]
            posterior_probs_h1_summary <- posterior_probs_h1_vec[valid_indices_for_summary]


            actual_sim_steps_completed <- length(p_values_summary)

            if (actual_sim_steps_completed == 0) {
              return(list(error = "所有序列模擬步驟均失敗。請檢查參數（例如，n >= 2）。"))
            }

            # Calculate summary statistics based on completed steps
            percent_p_less_05 <- mean(p_values_summary < 0.05) * 100
            percent_p_less_005 <- mean(p_values_summary < 0.005) * 100
            percent_ci_covers <- mean(ci_covers_delta_summary) * 100
            percent_bf_greater_3 <- mean(log10_bfs_summary > log10(3)) * 100
            percent_bf_greater_10 <- mean(log10_bfs_summary > 1) * 100
            percent_bf_less_1over3 <- mean(log10_bfs_summary < log10(1/3)) * 100
            percent_bf_less_1over10 <- mean(log10_bfs_summary < log10(1/10)) * 100

            # Calculate number of points outside BF plot range based on summary data
            bf_boundaries_vals <- c(log10(10), log10(3), 0, log10(1/3), log10(1/10))
            plot_min_calc <- min(c(log10_bfs_summary, bf_boundaries_vals), na.rm = TRUE) - 0.5
            plot_max_calc <- max(c(log10_bfs_summary, bf_boundaries_vals), na.rm = TRUE) + 0.5
            num_outside_bf_range <- sum(log10_bfs_summary < plot_min_calc | log10_bfs_summary > plot_max_calc, na.rm = TRUE)

            # --- Calculate steps to reach posterior probability thresholds ---
            steps_to_05 <- NA
            steps_to_95 <- NA

            # Find the first step where posterior prob is <= 0.05
            idx_le_05 <- which(posterior_probs_h1_vec <= 0.05)
            if (length(idx_le_05) > 0) {
                steps_to_05 <- min(idx_le_05)
            }

            # Find the first step where posterior prob is >= 0.95
            idx_ge_95 <- which(posterior_probs_h1_vec >= 0.95)
            if (length(idx_ge_95) > 0) {
                steps_to_95 <- min(idx_ge_95)
            }
            # --- End of threshold calculation ---


            # Get the final posterior probability after the last successful step
            # Need to find the last non-NA value in the original vector
            last_valid_posterior <- tail(posterior_probs_h1_vec[!is.na(posterior_probs_h1_vec)], 1)
            final_posterior_prob_h1 <- if (length(last_valid_posterior) > 0) last_valid_posterior else NA


            list(
                p_values = p_values_vec, # Original vector with NAs for dance plot
                log10_bfs = log10_bfs_vec, # Original vector with NAs for dance plot
                ci_covers_delta = ci_covers_delta_vec, # Original vector with NAs
                posterior_probs_h1 = posterior_probs_h1_vec, # Original vector with NAs for dance plot

                percent_p_less_05 = percent_p_less_05,
                percent_p_less_005 = percent_p_less_005,
                percent_ci_covers = percent_ci_covers,
                percent_bf_greater_3 = percent_bf_greater_3,
                percent_bf_greater_10 = percent_bf_greater_10,
                percent_bf_less_1over3 = percent_bf_less_1over3,
                percent_bf_less_1over10 = percent_bf_less_1over10,

                n = n,
                delta = delta,
                sd = sd,
                theoretical_se = theoretical_se,
                r_scale = r_scale, # Include r_scale in results
                initial_prior_prob = input$prior_prob, # Include initial prior
                final_posterior_prob_h1 = final_posterior_prob_h1, # Include final posterior

                num_sims_requested = n_sims,
                num_sims_completed = actual_sim_steps_completed, # Completed steps used for summary
                failed_sims = n_sims - actual_sim_steps_completed, # Failed steps
                num_outside_bf_range = num_outside_bf_range,

                # --- Add steps to reach thresholds ---
                steps_to_05 = steps_to_05,
                steps_to_95 = steps_to_95
                # --- End of added results ---
            )
        })
    })

    output$summary_stats <- renderPrint({
        results <- simulation_results()

        if (!is.null(results$error)) {
          cat("錯誤:", results$error, "\n")
          return()
        }

        cat("貝氏定理：H1 的後驗機率 p(H1|D)= p(H1) x p(D|H1)/p(D)\n")
        cat("=先驗機率 x 似然率/邊際似然率\n")
        cat("H1 的後驗勝算= H1 的先驗勝算 x 貝氏因子 BF10\n")
        cat("勝算= 機率/(1-機率)，機率= 勝算/(勝算+1)\n")

        # Display the initial and final posterior probabilities from the simulation sequence
        cat(sprintf("\n--- 序列更新結果 ---\n"))
        cat(sprintf("初始 H1 的先驗機率: %.4f\n", results$initial_prior_prob))
        # Find the last non-NA posterior probability
        if (!is.na(results$final_posterior_prob_h1)) {
             cat(sprintf("經過 %d 個模擬步驟後，H1 的最終後驗機率: %.4f\n", results$num_sims_requested, results$final_posterior_prob_h1))
        } else {
             cat(sprintf("經過 %d 個模擬步驟後，無法計算最終後驗機率 (所有步驟均失敗)。\n", results$num_sims_requested))
        }

        # --- Display steps to reach thresholds ---
        cat(sprintf("\n--- 達到後驗機率閾值的步驟數 ---\n"))
        if (!is.na(results$steps_to_05)) {
            cat(sprintf("達到 H1 後驗機率 <= 0.05 所需的最小步驟數: %d\n", results$steps_to_05))
        } else {
            cat(sprintf("在 %d 個模擬步驟內未達到 H1 後驗機率 <= 0.05。\n", results$num_sims_requested))
        }

        if (!is.na(results$steps_to_95)) {
            cat(sprintf("達到 H1 後驗機率 >= 0.95 所需的最小步驟數: %d\n", results$steps_to_95))
        } else {
            cat(sprintf("在 %d 個模擬步驟內未達到 H1 後驗機率 >= 0.95。\n", results$num_sims_requested))
        }
        # --- End of displaying thresholds ---


        cat("\n--- 序列模擬參數 ---\n") # Updated title
        cat("固定樣本數 (每組):", results$n, "\n")
        cat("真實平均值差異 (Delta):", results$delta, "\n")
        cat("標準差 (SD):", results$sd, "\n")
        cat("H1 的先驗 (rscale):", results$r_scale, "\n") # Display r_scale
        cat("理論標準誤 (SE):", sprintf("%.4f", results$theoretical_se), "\n")
        cat("要求序列模擬步數:", results$num_sims_requested, "\n")
        cat("完成序列模擬步數 (用於摘要統計):", results$num_sims_completed, "\n") # Updated label
        if(results$failed_sims > 0) {
            cat("失敗序列模擬步數:", results$failed_sims, "\n") # Updated label
        }
        cat("\n--- 序列結果 (基於完成的步驟) ---\n") # Updated title
        cat(sprintf("P值 < 0.05 的模擬百分比: %.2f%%\n", results$percent_p_less_05))
        cat(sprintf("P值 < 0.005 的模擬百分比: %.2f%%\n", results$percent_p_less_005))

        cat(sprintf("95%% CI 包含真實 Delta 的模擬百分比: %.2f%%\n", results$percent_ci_covers))
        cat(" (注意: 這是序列模擬中單一步驟 CI 包含真值的頻率)\n") # Updated note
        cat(sprintf("BF10 > 3 (中等證據) 的模擬百分比: %.2f%%\n", results$percent_bf_greater_3))
        cat(sprintf("BF10 > 10 (強證據) 的模擬百分比: %.2f%%\n", results$percent_bf_greater_10))
        cat(sprintf("BF10 < 1/3 (中等支持 H0) 的模擬百分比: %.2f%%\n", results$percent_bf_less_1over3))
        cat(sprintf("BF10 < 1/10 (強支持 H0) 的模擬百分比: %.2f%%\n", results$percent_bf_less_1over10))

        if (results$num_outside_bf_range > 0) {
           cat(sprintf("\n注意：有 %d 個模擬步驟的 Log10(BF10) 值超出圖表範圍，未顯示在貝氏因子分佈圖中。\n", results$num_outside_bf_range))
        }

    }) # Closing renderPrint

    output$p_value_hist <- renderPlot({
        results <- simulation_results()
        # Use filtered data for histograms
        p_values_summary <- results$p_values[!is.na(results$p_values)]
        req(p_values_summary)
        df <- data.frame(p_value = p_values_summary)
        ggplot(df, aes(x = p_value)) +
            geom_histogram(aes(y = after_stat(density)), binwidth = 0.02, boundary = 0, fill = "lightblue", color = "black") +
            geom_density(color = "blue", alpha = 0.5) +
            geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
            scale_x_continuous(breaks = seq(0, 1, 0.1)) +
            labs(
                title = "P值分佈 (序列步驟)", # Updated title
                subtitle = paste(results$num_sims_completed, "個完成步驟 (n =", results$n, ", Delta =", results$delta, ")"), # Updated subtitle
                x = "P值",
                y = "密度"
            ) +
            theme_minimal(base_family = "sans") +
            annotate("text", x = 0.06, y = Inf, label = "p = 0.05", color = "red", hjust = 0, vjust = 1.5, family="sans")
    })

    output$bf_hist <- renderPlot({
        results <- simulation_results()
        # Use filtered data for histograms
        log10_bfs_summary <- results$log10_bfs[!is.na(results$log10_bfs)]
        req(log10_bfs_summary)
        df <- data.frame(log10_bf10 = log10_bfs_summary)
        bf_boundaries <- data.frame(
            log10_val = c(log10(10), log10(3), 0, log10(1/3), log10(1/10)),
            label = c("強 H1", "中等 H1", "軼事", "中等 H0", "強 H0")
        )
        target_bf3_log10 <- log10(3)
        target_bf10_log10 <- log10(10)

        # Calculate plot limits based on summary data
        bf_boundaries_vals <- bf_boundaries$log10_val
        plot_min <- min(c(df$log10_bf10, bf_boundaries_vals), na.rm = TRUE) - 0.5
        plot_max <- max(c(df$log10_bf10, bf_boundaries_vals), na.rm = TRUE) + 0.5

        ggplot(df, aes(x = log10_bf10)) +
            geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "lightgreen", color = "black") +
             geom_density(color = "darkgreen", alpha = 0.5) +
            geom_vline(data=bf_boundaries, aes(xintercept = log10_val), linetype = "dotted", color = "grey40") +
            geom_vline(xintercept = target_bf3_log10, linetype = "dashed", color = "darkorange", size=0.8) +
            geom_vline(xintercept = target_bf10_log10, linetype = "dashed", color = "darkred", size=1) +
            coord_cartesian(xlim = c(plot_min, plot_max)) +
            labs(
                title = "Log10(貝氏因子)分佈 (序列步驟)", # Updated title
                subtitle = paste(results$num_sims_completed, "個完成步驟 (n =", results$n, ", Delta =", results$delta, ", H1 vs H0)"), # Updated subtitle
                x = "Log10(BF10)",
                y = "密度"
            ) +
            theme_minimal(base_family = "sans") +
            geom_text(data=bf_boundaries, aes(x = log10_val, y = -Inf, label=label), vjust=-0.5, size=3, angle=90, hjust=0, color="grey40", family="sans") +
            annotate("text", x = target_bf3_log10 + 0.05, y = Inf, label = "BF=3", color = "darkorange", hjust = 0, vjust = 1.5, family="sans") +
             annotate("text", x = target_bf10_log10 + 0.05, y = Inf, label = "BF=10", color = "darkred", hjust = 0, vjust = 1.5, family="sans")
    })

    output$p_value_dance <- renderPlot({
        results <- simulation_results()
        # Use original vector with NAs for dance plot
        df <- data.frame(sim = 1:results$num_sims_requested, p = results$p_values)
        ggplot(df, aes(x = sim, y = p)) +
            geom_line(color = "steelblue") +
            geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
            labs(title = "P 值之舞 (序列步驟)", x = "序列步驟", y = "P 值") + # Updated labels
            theme_minimal()
    })

    output$bf_dance <- renderPlot({
        results <- simulation_results()
        # Use original vector with NAs for dance plot
        df <- data.frame(sim = 1:results$num_sims_requested, log10bf = results$log10_bfs)
        ggplot(df, aes(x = sim, y = log10bf)) +
            geom_line(color = "darkgreen") +
            geom_hline(yintercept = log10(3), linetype = "dashed", color = "orange") +
            geom_hline(yintercept = log10(10), linetype = "dashed", color = "red") +
            labs(title = "BF10 之舞 (log10) (序列步驟)", x = "序列步驟", y = "log10(BF10)") + # Updated labels
            theme_minimal()
    })

    # New plot for Posterior Probability of H1 over the sequence
    output$posterior_prob_dance <- renderPlot({
        results <- simulation_results()
        # Use original vector with NAs for dance plot
        df <- data.frame(sim = 1:results$num_sims_requested, posterior_prob = results$posterior_probs_h1)
        ggplot(df, aes(x = sim, y = posterior_prob)) +
            geom_line(color = "purple") +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey") + # Add a line at 0.5 for reference
             geom_hline(yintercept = 0.05, linetype = "dotted", color = "red") + # Add line for 0.05 threshold
             geom_hline(yintercept = 0.95, linetype = "dotted", color = "blue") + # Add line for 0.95 threshold
            labs(
                title = "H1 後驗機率之舞 (序列更新)", # Title for the new plot
                x = "序列步驟", # X-axis label
                y = "H1 後驗機率" # Y-axis label
            ) +
            theme_minimal() +
            scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) # Ensure y-axis is 0 to 1
    })

} # Closing server function

shinyApp(ui = ui, server = server)
