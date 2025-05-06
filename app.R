# Load necessary libraries
library(shiny)
library(ggplot2)
library(gganimate) # For creating animations
library(dplyr)    # For data manipulation
library(gifski)   # For rendering GIF animations
library(DT)       # For creating interactive tables
library(showtext) # For displaying UTF-8 characters (like Chinese) in plots

# ==== UI Definition ====
# Defines the user interface of the Shiny app
ui <- fluidPage(
  # Set the title of the application window/tab
  titlePanel("p 值之舞 與 log(BF₁₀) 之舞"), # Updated title reflecting dual plots

  # Define the layout: sidebar and main panel
  sidebarLayout(
    # Sidebar panel for user inputs
    sidebarPanel(
      # Input for true effect size (Cohen's d)
      numericInput("d_true", "真實效應量 (Cohen's d)", value = 0.3, step = 0.1),
      # Input for the prior standard deviation for Bayes Factor calculation
      numericInput("prior_sd", "Prior SD (for BF10)", value = 0.5, step = 0.1),
      # Input for the sample size per group
      numericInput("n_per_group", "每組樣本數", value = 30, min = 5),
      # Input for the number of simulations to run
      # *** MODIFIED: Default value changed to 2000 for the table ***
      numericInput("n_sim", "模擬次數 (表格用)", value = 2000, min = 100, step = 50), # Min set to 100 as graphs use 100
      # Dropdown to select the style of the animation (applies to both plots)
      selectInput("anim_style", "動畫風格",
                  choices = c("線條動畫" = "reveal",
                              "跳點動畫" = "states",
                              "淡入動畫" = "time")),
      # Button to trigger the simulation
      actionButton("go", "開始模擬")
    ),

    # Main panel for displaying outputs
    mainPanel(
      # Use a tabset panel to organize outputs
      tabsetPanel(
        # First tab: Displays the animations and summary table
        tabPanel("動態比較圖", # Renamed tab slightly
                 # Area for the p-value animation
                 # *** MODIFIED: Title indicates graph shows 100 simulations ***
                 h4("p-value 之舞 (顯示前 100 次模擬)"), # Title for p-value plot
                 imageOutput("animated_plot_p", height = "350px"), # Output for p-value plot
                 hr(), # Separator line
                 # Area for the log(BF10) animation
                 # *** MODIFIED: Title indicates graph shows 100 simulations ***
                 h4("log(BF₁₀) 之舞 (顯示前 100 次模擬)"), # Title for log(BF10) plot
                 imageOutput("animated_plot_logbf", height = "350px"), # Output for log(BF10) plot
                 hr(), # Separator line
                 # Output for the summary data table
                 h4("模擬結果摘要 (基於所有模擬次數)"), # Title for summary table
                 DTOutput("summary_table")),
        # Second tab: Displays explanations of statistical indicators
        tabPanel("說明與解釋", # Explanation tab
                 HTML("
                   <h4>統計指標解釋</h4>
                   <p><b>1. p-value：</b>傳統統計中，p 值代表在 H₀ (虛無假設，例如兩組無差異) 成立下，觀察到目前或更極端資料的機率。常見閾值 (alpha, α) 如 0.05 或 0.005。</p>
                   <p><b>   p-value 分佈特性：</b></p>
                   <ul>
                     <li><b>當 H₀ 為真時 (真實效應量 d = 0)：</b> 在多次重複實驗下，p 值的分布會是<b>均勻分佈 (Uniform Distribution)</b> 在 0 到 1 之間。這意味著，如果 H₀ 真的成立，有 α% 的實驗會錯誤地得到 p < α 的結果 (即 Type I error rate)。例如，當 α = 0.05，即使沒有真實效應，也有 5% 的機率會觀察到 p < 0.05。</li>
                     <li><b>當 H₁ 為真時 (真實效應量 d ≠ 0)：</b> 在多次重複實驗下，p 值的分布會偏向 0。真實效應越大、樣本數越多，p 值就越容易小於 α。在這種情況下，<b>統計檢定力 (Power)</b> 就是 p < α 的比例，代表「當 H₁ 為真時，能正確偵測到效果 (即拒絕 H₀) 的機率」。</li>
                   </ul>
                   <p><b>2. Bayes Factor (BF<sub>10</sub>)：</b>衡量資料支持 H₁（有差異）相對於 H₀（無差異）的強度。</p>
                   <ul>
                     <li>BF<sub>10</sub> > 3：中等證據支持 H₁ (log(BF₁₀) > log(3) ≈ 1.1)</li>
                     <li>BF<sub>10</sub> > 10：強證據支持 H₁ (log(BF₁₀) > log(10) ≈ 2.3)</li>
                     <li>BF<sub>10</sub> < 1/3：中等證據支持 H₀ (log(BF₁₀) < log(1/3) ≈ -1.1)</li>
                     <li>BF<sub>10</sub> < 1/10：強證據支持 H₀ (log(BF₁₀) < log(1/10) ≈ -2.3)</li>
                     <li>1/3 < BF<sub>10</sub> < 3：證據不明確或薄弱 (-1.1 < log(BF₁₀) < 1.1)</li>
                   </ul>
                   <p><b>3. Posterior P(H₁)：</b>後驗機率，表示給定資料後，H₁ 為真的機率（假設 H₀ 和 H₁ 的先驗機率相等，即 P(H₀)=P(H₁)=0.5）。可直接用來決策。</p>
                   <p><b>4. S-value：</b>以資訊量觀點轉換 p 值，公式為 -log<sub>2</sub>(p)，越大代表對 H₀ 的否定越強。<b>（注意：此指標已從摘要表中移除）</b></p>
                   <p><b>5. ROPE (Region of Practical Equivalence)：</b>在貝氏分析中，ROPE 指的是效應量小到可以被視為「實務上等同於無差異」的一個區間（例如 Cohen's d 在 -0.1 到 0.1 之間）。分析時會計算效應量後驗分佈落在 ROPE 內的機率。<b>（注意：本 App 先前的「比較圖」分頁曾計算 BF<sub>10</sub> 介於 1/3 到 3 之間的比例，這代表證據不明確的比例，概念上與 ROPE 不同，請勿混淆。）</b></p>
                   <p><b>6. P(H₁) > 0.95：</b>這表示根據觀察到的數據和設定的先驗（Prior），計算出的「H₁ 為真」的後驗機率超過 95%。這是一個常用的決策閾值，表示有很高的信心認為 H₁ 是成立的（例如，存在真實的組間差異）。</p>
                   <hr>
                   <h4>延伸說明：log(S-value) 與 log(BF<sub>10</sub>) 的變異性比較</h4>
                   <p><b>1. 穩定性差異：</b>log(S-value) 在多次模擬下的變異性（SD）通常小於 log(BF<sub>10</sub>)，表示它對樣本波動與 prior 選擇較不敏感。</p>
                   <p><b>2. 實務意涵：</b>當我們希望評估研究設計或證據穩定性時，log(S-value) 的標準差可作為衡量「一致性」的參考指標。</p>
                   <p><b>3. 單次試驗的解釋穩健性：</b>Bayes factor 容易受 prior 與樣本數影響，單次結果可能變動極大；相對地，S-value 或 log(S-value) 更適合做穩健初步判讀。</p>
                   <p><b>4. 推薦用途：</b>在設計試驗、評估 replication 成功率、教學解釋中，log(S-value) 可輔助或替代 BF<sub>10</sub>，尤其在資料量較少時。</p>
                   <p><b>5. 小提醒：</b>log(S-value) 非為標準 Bayesian 指標，但實務上在小樣本設計中表現穩健，值得納入比較。</p>
                   <hr>

                   <h4>理解區間：信賴區間 vs. 可信區間</h4>
                   <p>信賴區間 (Confidence Intervals, CIs) 和可信區間 (Credible Intervals, CrIs) 都提供了一個未知母體參數（如平均數差異或效應量）的可能值範圍。然而，它們源於不同的統計哲學（頻率學派 vs. 貝氏學派），並且有著不同的解釋。</p>

                   <p><b>1. 95% 信賴區間 (CI) - 頻率學派方法</b></p>
                   <ul>
                     <li><b>來源：</b>頻率學派統計學，將機率視為事件的長期頻率。母體參數被認為是固定的、未知的常數。</li>
                     <li><b>計算：</b>僅基於觀察到的樣本資料。</li>
                     <li><b>解釋：</b>如果我們在相同條件下重複我們的研究或抽樣過程無數次，並為每次重複計算一個 95% CI，我們預期<b>這些計算出的區間中有 95%</b> 會包含真實的、固定的母體參數。</li>
                     <li><b>它*不*代表的意思：</b>它<b>不</b>代表您從*單一樣本*計算出的*特定* CI 有 95% 的機率包含真實參數。一旦計算出來，您的特定區間要麼包含真實值，要麼不包含；我們只是不知道是哪種情況。「95%」指的是用於生成區間的*程序*在多次假設性重複中的可靠性。</li>
                     <li><b>類比 (套圈圈遊戲)：</b>想像真實參數是地上的一個固定樁子。每次您收集樣本並計算 CI，就像是丟出一個圈圈。CI 的計算程序被校準過，使得（在多次投擲中）95% 的圈圈會落在樁子周圍。對於您已經丟出的任何一個圈圈，它要麼套中了樁子，要麼沒有。</li>
                   </ul>

                   <p><b>2. 95% 可信區間 (CrI) - 貝氏學派方法</b></p>
                   <ul>
                     <li><b>來源：</b>貝氏統計學，將機率視為信念或確定性的程度。參數被視為隨機變數，我們對其不確定性會隨著數據的加入而更新。</li>
                     <li><b>計算：</b>基於觀察到的樣本資料<b>以及</b>一個先驗機率分佈（代表在看到數據*之前*對參數的信念）。結果是參數的後驗機率分佈。</li>
                     <li><b>解釋：</b>給定觀察到的數據和選擇的先驗，有 <b>95% 的機率真實母體參數落在此特定計算出的區間內</b>。</li>
                     <li><b>它*確實*代表的意思：</b>它根據當前的知識狀態（先驗 + 數據）對參數的位置做出了直接的機率陳述。</li>
                     <li><b>類比 (找鑰匙)：</b>想像您在一個房間裡弄丟了鑰匙。您的先驗信念可能是它們很可能在門附近。在搜索了某些區域（收集數據）後，您更新了您的信念。一個 95% CrI 就像在房間地圖上畫一個邊界，並說：「根據我認為它們可能在哪裡以及我已經找過的地方，我有 95% 的把握鑰匙在這個邊界內。」</li>
                   </ul>

                   <p><b>主要差異總結</b></p>
                   <table border='1' style='border-collapse: collapse; width: 100%;'>
                     <thead>
                       <tr>
                         <th style='padding: 5px; text-align: left;'>特徵</th>
                         <th style='padding: 5px; text-align: left;'>95% 信賴區間 (CI)</th>
                         <th style='padding: 5px; text-align: left;'>95% 可信區間 (CrI)</th>
                       </tr>
                     </thead>
                     <tbody>
                       <tr>
                         <td style='padding: 5px;'><b>哲學</b></td>
                         <td style='padding: 5px;'>頻率學派</td>
                         <td style='padding: 5px;'>貝氏學派</td>
                       </tr>
                       <tr>
                         <td style='padding: 5px;'><b>參數</b></td>
                         <td style='padding: 5px;'>固定的、未知的常數</td>
                         <td style='padding: 5px;'>隨機變數（反映不確定性）</td>
                       </tr>
                       <tr>
                         <td style='padding: 5px;'><b>機率</b></td>
                         <td style='padding: 5px;'>指*方法*的長期成功率</td>
                         <td style='padding: 5px;'>指對*此區間*的信念程度</td>
                       </tr>
                       <tr>
                         <td style='padding: 5px;'><b>輸入</b></td>
                         <td style='padding: 5px;'>僅樣本資料</td>
                         <td style='padding: 5px;'>樣本資料 + 先驗分佈</td>
                       </tr>
                       <tr>
                         <td style='padding: 5px;'><b>解釋</b></td>
                         <td style='padding: 5px;'>「以此方式計算的區間中，有 95% 會捕捉到真實值。」</td>
                         <td style='padding: 5px;'>「真實值落在此區間內的機率為 95%。」</td>
                       </tr>
                     </tbody>
                   </table>

                   <p><b>實務上：</b></p>
                   <ul>
                     <li>通常，對於簡單模型、大樣本和無資訊先驗，CI 和 CrI 在數值上可能很相似。然而，它們的解釋仍然有根本上的不同。</li>
                     <li>CrI 的解釋通常更直觀，更接近研究人員*希望*能對 CI 所做的陳述。</li>
                     <li>兩者之間的選擇取決於研究問題和哲學方法。貝氏方法（如產生 CrI 和貝氏因子，如本 App 所用）允許納入先驗知識，並對假設或參數做出直接的機率陳述。頻率學派方法（如 p 值和 CI）則關注長期錯誤率和程序的可靠性。</li>
                   </ul>
                   ")) # End of HTML content
      ) # End of tabsetPanel
    ) # End of mainPanel
  ) # End of sidebarLayout
) # End of fluidPage (UI)

# ==== Server Logic ====
# Defines the server-side logic of the Shiny app
server <- function(input, output, session) { # Added session argument

  # *** ADDED: Enable showtext for UTF-8 character display in plots ***
  showtext_auto()
  # Optional: Specify a font that supports Chinese characters if needed
  # Example: font_add_google("Noto Sans TC", "notosans") # Noto Sans Traditional Chinese
  # Then you might need to add theme(text = element_text(family = "notosans")) to ggplot objects

  # --- Simulation Data Generation ---

  # Reactive expression to run the simulation when the 'go' button is pressed
  # This generates data based on the user's input$n_sim (default 2000)
  simulate_data <- eventReactive(input$go, {

    # Helper function to compute Bayes Factor (BF10)
    compute_bf10 <- function(t_stat, n1, n2, prior_sd) {
       # Basic check for valid inputs
      if (is.na(t_stat) || is.na(n1) || is.na(n2) || is.na(prior_sd) || n1 <= 0 || n2 <= 0 || prior_sd <= 0) {
        return(NA_real_)
      }
      # Attempt calculation using BayesFactor package if available, otherwise use approximation
      if (requireNamespace("BayesFactor", quietly = TRUE)) {
        bf_result <- tryCatch({
          # Using rnorm to simulate data matching t-stat is an approximation for BF calculation
          # A more direct calculation might be preferred if possible, but ttestBF needs data or summary stats
          # Let's generate data that would *approximately* yield the t-stat
          # We need to solve for the mean difference (md) given t = md / (sd * sqrt(1/n1 + 1/n2))
          # Assuming pooled sd = 1 for simplicity here, md = t_stat * sqrt(1/n1 + 1/n2)
          mean_diff_approx <- t_stat * sqrt(1/n1 + 1/n2)
          # Generate data with this mean difference
          x_sim <- rnorm(n1, mean = 0, sd = 1) # Group 1 centered at 0
          y_sim <- rnorm(n2, mean = mean_diff_approx, sd = 1) # Group 2 with the mean difference
          BayesFactor::ttestBF(x = x_sim, y = y_sim, rscale = prior_sd)
        }, error = function(e) NULL)

        if (!is.null(bf_result) && inherits(bf_result, "BFBayesFactor")) {
           # Extract BF10 value correctly
           bf_matrix <- BayesFactor::extractBF(bf_result)
           bf10 <- bf_matrix$bf[1] # Assuming the first BF is BF10
           # Handle potential Inf/NaN from exp() - BF is already on natural scale from extractBF
           if (is.infinite(bf10) || is.nan(bf10)) return(NA_real_)
           return(bf10)
        }
      }
      # Fallback approximation if BayesFactor package is not available or fails
      # This approximation is likely less accurate than using the package
      # Using a placeholder NA if package fails, as approximations can be unreliable.
      # warning("BayesFactor package failed or not installed. BF10 calculation skipped.")
      return(NA_real_) # Return NA if package method fails

    }

    # Helper function to compute posterior probability P(H1) from BF10
    compute_post_prob <- function(bf10) {
      if (is.na(bf10)) return(NA_real_)
      # Assume equal prior odds P(H1)/P(H0) = 1
      prior_odds <- 1
      post_odds <- bf10 * prior_odds
      # Handle case where post_odds might be Inf
      if (is.infinite(post_odds)) return(1.0) # If BF is Inf, posterior prob is 1
      return(post_odds / (1 + post_odds))
    }

    # Helper function to compute S-value from p-value (kept for potential future use, but not displayed)
    compute_svalue <- function(p) {
      if (is.na(p) || p < 0 || p > 1) return(NA_real_) # Add checks for valid p-value
      # Handle p=0 and p=1 specifically
      if (p == 0) return(Inf)
      if (p == 1) return(0)
      -log2(p)
    }

    # Set seed for reproducibility
    set.seed(123)
    # Run the simulation 'n_sim' times (user input, default 2000)
    results <- lapply(1:input$n_sim, function(i) {
      # Generate data based on d_true
      g1 <- rnorm(input$n_per_group, mean = 0, sd = 1)
      g2 <- rnorm(input$n_per_group, mean = input$d_true, sd = 1)

      # Perform t-test
      t_res <- tryCatch(t.test(g1, g2), error = function(e) NULL)

      # Handle t-test failure
      if (is.null(t_res) || !is.finite(t_res$statistic) || !is.finite(t_res$p.value)) {
         return(data.frame(sim = i, p = NA_real_, bf10 = NA_real_, log_bf10 = NA_real_, post_p = NA_real_, s_value = NA_real_))
      }

      t_stat <- t_res$statistic
      p_val <- t_res$p.value

      # Compute BF10
      bf10 <- compute_bf10(t_stat, input$n_per_group, input$n_per_group, input$prior_sd)

      # Calculate log(BF10), handle non-positive or NA BF10 values
      log_bf10 <- if (!is.na(bf10) && bf10 > 0) log(bf10) else NA_real_

      # Compute Posterior Probability P(H1)
      post_p <- compute_post_prob(bf10)

      # Compute S-value
      s_val <- compute_svalue(p_val)

      # Return results
      data.frame(sim = i, p = p_val, bf10 = bf10, log_bf10 = log_bf10, post_p = post_p, s_value = s_val)
    })
    # Combine results into one data frame
    df_final <- do.call(rbind, results)

    # Ensure columns that should be numeric are numeric
    df_final$p <- as.numeric(df_final$p)
    df_final$bf10 <- as.numeric(df_final$bf10)
    df_final$log_bf10 <- as.numeric(df_final$log_bf10)
    df_final$post_p <- as.numeric(df_final$post_p)
    df_final$s_value <- as.numeric(df_final$s_value)

    return(df_final)
  })

  # --- Render p-value Animation ---
  output$animated_plot_p <- renderImage({
    df_full <- simulate_data() # Get potentially large dataset (e.g., 2000 sims)
    if (!is.data.frame(df_full) || nrow(df_full) == 0) {
         return(list(src = "", contentType = 'image/gif', alt = "請點擊「開始模擬」產生資料"))
    }

    # *** MODIFIED: Subset to first 100 simulations for the graph ***
    n_plot_sims <- 100
    df <- head(df_full, n_plot_sims)
    actual_sims_in_df <- nrow(df) # How many sims are we actually plotting (<= 100)

    # Clean data for p-value plot
    df_clean_p <- df[!is.na(df$p), ]
    if (nrow(df_clean_p) == 0) {
         return(list(src = "", contentType = 'image/gif', alt = "前 100 次模擬中無有效的 p-value 資料可繪圖"))
    }

    # Select animation style
    anim_style <- switch(input$anim_style,
                         "reveal" = transition_reveal(sim),
                         "states" = transition_states(sim, transition_length = 2, state_length = 1),
                         "time" = transition_time(sim))

    # Create p-value plot
    # *** MODIFIED: Added group = 1 to aes() to fix geom_line warning ***
    # *** MODIFIED: Update title to show actual number plotted vs. requested (100) ***
    p_plot <- ggplot(df_clean_p, aes(x = sim, y = p, group = 1)) + # Added group = 1
      geom_line(color = "steelblue") +
      geom_point(size = 2, color = "orange") +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") + # Significance threshold
      geom_hline(yintercept = 0.005, linetype = "dotted", color = "darkred") + # Add 0.005 threshold
      labs(x = "模擬次數 (前 100 次)", y = "p-value", # Modified x-axis label
           title = paste("p-value 之舞 (顯示 ", nrow(df_clean_p), "/", actual_sims_in_df, " 次有效模擬)", sep="")) + # Modified title
      scale_y_continuous(limits = c(0, 1)) + # Ensure y-axis is 0 to 1
      anim_style +
      theme_minimal() # showtext will handle font rendering

    # Save and return GIF
    anim_file_p <- tempfile(fileext = ".gif")
    anim_result_p <- tryCatch({
        if (!requireNamespace("gifski", quietly = TRUE)) stop("gifski package needed.")
        # *** ADDED: Use showtext device wrapper for saving ***
        showtext_opts(dpi = 96) # Set DPI for consistency if needed
        anim_save(anim_file_p, animate(p_plot, fps = 15, width = 600, height = 350, renderer = gifski_renderer()))
        list(src = anim_file_p, contentType = 'image/gif')
    }, error = function(e) {
        # Provide more informative error message
        msg <- paste("p-value 動畫渲染失敗:", e$message)
        if (!requireNamespace("gifski", quietly = TRUE)) {
            msg <- paste(msg, "請安裝 'gifski' 套件。")
        }
        # *** ADDED: Mention showtext in error if relevant ***
        if (grepl("showtext", e$message, ignore.case = TRUE)) {
           msg <- paste(msg, "請確認 'showtext' 套件已安裝且字體可用。")
        }
        list(src = "", contentType = 'image/gif', alt = msg)
    })
    return(anim_result_p)
  }, deleteFile = TRUE)

  # --- Render log(BF10) Animation ---
  output$animated_plot_logbf <- renderImage({
    df_full <- simulate_data() # Get potentially large dataset (e.g., 2000 sims)
     if (!is.data.frame(df_full) || nrow(df_full) == 0) {
         return(list(src = "", contentType = 'image/gif', alt = "請點擊「開始模擬」產生資料"))
    }

    # *** MODIFIED: Subset to first 100 simulations for the graph ***
    n_plot_sims <- 100
    df <- head(df_full, n_plot_sims)
    actual_sims_in_df <- nrow(df) # How many sims are we actually plotting (<= 100)

    # Clean data for log(BF10) plot (remove NA and non-finite values)
    df_clean_logbf <- df[!is.na(df$log_bf10) & is.finite(df$log_bf10), ]
     if (nrow(df_clean_logbf) == 0) {
         return(list(src = "", contentType = 'image/gif', alt = "前 100 次模擬中無有效的 log(BF10) 資料可繪圖"))
     }

    # Select animation style (same as p-value plot)
    anim_style <- switch(input$anim_style,
                         "reveal" = transition_reveal(sim),
                         "states" = transition_states(sim, transition_length = 2, state_length = 1),
                         "time" = transition_time(sim))

    # Determine y-axis limits dynamically but capped for readability (based on the 100 sims)
    y_min <- min(df_clean_logbf$log_bf10, na.rm = TRUE)
    y_max <- max(df_clean_logbf$log_bf10, na.rm = TRUE)
    # Cap the limits to avoid extreme scales if there are outliers
    y_lim_lower <- max(y_min, log(1/1000)) # Cap at BF=1/1000
    y_lim_upper <- min(y_max, log(1000))  # Cap at BF=1000
    # Ensure lower limit is less than upper limit
    if(y_lim_lower >= y_lim_upper) {
        y_lim_lower <- log(1/100)
        y_lim_upper <- log(100)
    }


    # Create log(BF10) plot
    # *** MODIFIED: Added group = 1 to aes() to fix geom_line warning ***
    # *** MODIFIED: Update title and x-axis label ***
    logbf_plot <- ggplot(df_clean_logbf, aes(x = sim, y = log_bf10, group = 1)) + # Added group = 1
      geom_line(color = "darkgreen") + # Different color
      geom_point(size = 2, color = "purple") + # Different color
      geom_hline(yintercept = log(10), linetype = "dotted", color = "darkblue", alpha=0.7) + # Strong Evidence H1
      geom_hline(yintercept = log(3), linetype = "dashed", color = "blue", alpha=0.7) +  # Moderate Evidence H1
      geom_hline(yintercept = 0, linetype = "solid", color = "grey50", alpha=0.5) + # BF = 1 line
      geom_hline(yintercept = log(1/3), linetype = "dashed", color = "red", alpha=0.7) + # Moderate Evidence H0
      geom_hline(yintercept = log(1/10), linetype = "dotted", color = "darkred", alpha=0.7) + # Strong Evidence H0
      scale_y_continuous(limits = c(y_lim_lower, y_lim_upper)) + # Apply dynamic limits
      labs(x = "模擬次數 (前 100 次)", y = "log(BF₁₀)", # Modified x-axis label
           title = paste("log(BF₁₀) 之舞 (顯示 ", nrow(df_clean_logbf), "/", actual_sims_in_df, " 次有效模擬)", sep=""), # Modified title
           caption = "水平線: log(10), log(3), 0, log(1/3), log(1/10)") +
      anim_style +
      theme_minimal() + # showtext will handle font rendering
      theme(plot.caption = element_text(hjust = 0.5, size=9, color="gray30")) # Add caption style


    # Save and return GIF
    anim_file_logbf <- tempfile(fileext = ".gif")
    anim_result_logbf <- tryCatch({
        if (!requireNamespace("gifski", quietly = TRUE)) stop("gifski package needed.")
        # *** ADDED: Use showtext device wrapper for saving ***
        showtext_opts(dpi = 96) # Set DPI for consistency if needed
        anim_save(anim_file_logbf, animate(logbf_plot, fps = 15, width = 600, height = 350, renderer = gifski_renderer()))
        list(src = anim_file_logbf, contentType = 'image/gif')
    }, error = function(e) {
        # Provide more informative error message
        msg <- paste("log(BF10) 動畫渲染失敗:", e$message)
        if (!requireNamespace("gifski", quietly = TRUE)) {
            msg <- paste(msg, "請安裝 'gifski' 套件。")
        }
         # *** ADDED: Mention showtext in error if relevant ***
        if (grepl("showtext", e$message, ignore.case = TRUE)) {
           msg <- paste(msg, "請確認 'showtext' 套件已安裝且字體可用。")
        }
        list(src = "", contentType = 'image/gif', alt = msg)
    })
    return(anim_result_logbf)
  }, deleteFile = TRUE)

  # --- Render Summary Table ---
  # This uses the full dataset from simulate_data() (default 2000 sims)
  output$summary_table <- renderDT({
    df <- simulate_data() # Uses full simulation data
    # Ensure df is a data frame before proceeding
    if (!is.data.frame(df) || nrow(df) == 0) {
        return(datatable(data.frame(Message = "請點擊「開始模擬」產生資料"), options = list(dom = 't', searching = FALSE, info = FALSE), rownames = FALSE))
    }

    # Determine if H0 is true (d=0) or H1 is true (d!=0)
    is_h0_true <- isTRUE(all.equal(input$d_true, 0)) # Use all.equal for float comparison

    # Define column names with HTML tooltips - UPDATED for Power/Type I Error
    colnames_vec <- c(
      # Updated tooltip for p < 0.05
      if (is_h0_true) {
        '<span title="當真實效應 d=0 時，此為 Type I error rate (偽陽性率)">p < 0.05<br>(Type I Error)</span>'
      } else {
        '<span title="當真實效應 d≠0 時，此為統計檢定力 (Power)">p < 0.05<br>(Power)</span>'
      },
      '<span title="p-value < 0.005 表示更嚴格的顯著性">p < 0.005</span>',
      '<span title="BF10 > 3 表示中等證據支持 H₁">BF10 > 3<br>(中等支持 H₁)</span>',
      '<span title="BF10 > 10 表示強證據支持 H₁">BF10 > 10<br>(強支持 H₁)</span>',
      '<span title="BF10 < 1/3 表示中等證據支持 H₀">BF10 < 1/3<br>(中等支持 H₀)</span>',
      '<span title="BF10 < 1/10 表示強證據支持 H₀">BF10 < 1/10<br>(強支持 H₀)</span>',
      '<span title="Posterior P(H₁) > 0.95 表示後驗機率很高">P(H₁) > 0.95</span>'
    )

    # --- Robust calculation of summary statistics ---
    # Helper function to safely calculate mean proportion
    calc_mean_prop <- function(condition_vector) {
      # Ensure input is logical or can be coerced safely
      logical_vector <- tryCatch(as.logical(condition_vector),
                                 warning = function(w) rep(NA, length(condition_vector)),
                                 error = function(e) rep(NA, length(condition_vector)))
      # Handle cases where all results are NA after coercion
      if (all(is.na(logical_vector))) return(NA_real_)
      mean(logical_vector, na.rm = TRUE)
    }

    # Calculate proportions safely using the full dataset
    p_prop_05 <- calc_mean_prop(df$p < 0.05)
    p_prop_005 <- calc_mean_prop(df$p < 0.005)
    bf10_pos_3 <- calc_mean_prop(df$bf10 > 3)
    bf10_pos_10 <- calc_mean_prop(df$bf10 > 10)
    bf10_neg_1_3 <- calc_mean_prop(df$bf10 < 1/3)
    bf10_neg_1_10 <- calc_mean_prop(df$bf10 < 1/10)
    post_prop <- calc_mean_prop(df$post_p > 0.95)

    # Create the summary data frame
    summary_df <- data.frame(
      p_05 = round(p_prop_05, 3),
      p_005 = round(p_prop_005, 3),
      bf10_gt3 = round(bf10_pos_3, 3),
      bf10_gt10 = round(bf10_pos_10, 3),
      bf10_lt1_3 = round(bf10_neg_1_3, 3),
      bf10_lt1_10 = round(bf10_neg_1_10, 3),
      post = round(post_prop, 3)
    )

    # Create the DataTable
    datatable(summary_df,
              colnames = lapply(colnames_vec, HTML), # Use the updated vector
              escape = FALSE, # Allow HTML in headers
              options = list(dom = 't', # Show only table ('t')
                             paging = FALSE,
                             searching = FALSE,
                             info = FALSE,
                             ordering = FALSE), # Disable column sorting
              rownames = FALSE,
              class = 'cell-border stripe') # Add some basic styling
  })

} # End of server function

# ==== Run the Application ====
shinyApp(ui = ui, server = server)
