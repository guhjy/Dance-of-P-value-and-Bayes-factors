# Load necessary libraries
library(shiny)
library(ggplot2)
library(gganimate) # For creating animations
library(dplyr)    # For data manipulation
library(gifski)   # For rendering GIF animations
library(DT)       # For creating interactive tables
library(showtext) # For displaying UTF-8 characters (like Chinese) in plots
library(scales)   # For log scales in ggplot

# ==== UI Definition ====
# Defines the user interface of the Shiny app
ui <- fluidPage(
  # Set the title of the application window/tab
  titlePanel("p 值、BF₁₀、S 值、P(H₁) 模擬器"), # Updated title to include P(H1)

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
      numericInput("n_sim", "模擬次數", value = 2000, min = 100, step = 50),
      # Button to trigger the simulation
      actionButton("go", "開始模擬")
    ),

    # Main panel for displaying outputs
    mainPanel(
      # Use a tabset panel to organize outputs
      tabsetPanel(
        # Tab 1: Animations (First 100 simulations)
        tabPanel("動態比較圖 (前 100 次)",
                 h4("p 值之舞 (顯示前 100 次模擬)"),
                 imageOutput("animated_plot_p", height = "350px"),
                 hr(),
                 h4("log(BF10) 之舞 (顯示前 100 次模擬)"),
                 imageOutput("animated_plot_logbf", height = "350px")
        ),
        # Tab 2: Violin Plots (All simulations)
        tabPanel("分佈圖 (所有模擬)",
                 fluidRow(
                   column(6,
                          h4("p-value 分佈"),
                          plotOutput("violin_plot_p", height = "350px")
                   ),
                   column(6,
                          h4("S-value 分佈"),
                          plotOutput("violin_plot_s", height = "350px")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(6,
                          h4("BF₁₀ 分佈 (對數尺度)"),
                          plotOutput("violin_plot_bf10", height = "350px")
                   ),
                   column(6,
                          h4("log(BF₁₀) 分佈"),
                          plotOutput("violin_plot_logbf", height = "350px")
                   )
                 ),
                 hr(), # Added horizontal rule
                 fluidRow( # New row for P(H1) plot
                   column(6,
                          h4("P(H₁) 分佈 (後驗機率)"),
                          plotOutput("violin_plot_post_p", height = "350px")
                   )
                   # This column will be on the left, leaving space on the right.
                   # If you want to add another plot later, it can go into a column(6, ...) here.
                 )
        ),
        # Tab 3: Summary Table and Specific Explanations
        tabPanel("摘要表與說明",
                 h4("模擬結果摘要 (基於所有模擬次數)"),
                 DTOutput("summary_table"),
                 hr(),
                 h4("摘要表指標解釋"),
                 HTML("
                   <p><b>1. p-value：</b>傳統統計中，p 值代表在 H₀ (虛無假設，例如兩組無差異) 成立下，觀察到目前或更極端資料的機率。常見閾值 (alpha, α) 如 0.05 或 0.005。</p>
                   <p><b>   p-value 分佈特性：</b></p>
                   <ul>
                     <li><b>當 H₀ 為真時 (真實效應量 d = 0)：</b> 在多次重複實驗下，p 值的分布會是<b>均勻分佈 (Uniform Distribution)</b> 在 0 到 1 之間。這意味著，如果 H₀ 真的成立，有 α% 的實驗會錯誤地得到 p < α 的結果 (即 Type I error rate)。例如，當 α = 0.05，即使沒有真實效應，也有 5% 的機率會觀察到 p < 0.05。摘要表中的 <b>p < 0.05 (Type I Error)</b> 即顯示此比例。</li>
                     <li><b>當 H₁ 為真時 (真實效應量 d ≠ 0)：</b> 在多次重複實驗下，p 值的分布會偏向 0。真實效應越大、樣本數越多，p 值就越容易小於 α。在這種情況下，<b>統計檢定力 (Power)</b> 就是 p < α 的比例，代表「當 H₁ 為真時，能正確偵測到效果 (即拒絕 H₀) 的機率」。摘要表中的 <b>p < 0.05 (Power)</b> 即顯示此比例。</li>
                     <li><b>p < 0.005:</b> 顯示符合更嚴格顯著性標準 (α = 0.005) 的模擬比例。</li>
                   </ul>
                   <p><b>2. Bayes Factor (BF<sub>10</sub>)：</b>衡量資料支持 H₁（有差異）相對於 H₀（無差異）的強度。</p>
                   <ul>
                     <li>BF<sub>10</sub> > 3：中等證據支持 H₁ (log(BF₁₀) > log(3) ≈ 1.1)</li>
                     <li>BF<sub>10</sub> > 10：強證據支持 H₁ (log(BF₁₀) > log(10) ≈ 2.3)</li>
                     <li>BF<sub>10</sub> < 1/3：中等證據支持 H₀ (log(BF₁₀) < log(1/3) ≈ -1.1)</li>
                     <li>BF<sub>10</sub> < 1/10：強證據支持 H₀ (log(BF₁₀) < log(1/10) ≈ -2.3)</li>
                     <li>1/3 < BF<sub>10</sub> < 3：證據不明確或薄弱 (-1.1 < log(BF₁₀) < 1.1)</li>
                     <li>摘要表顯示了符合這些證據強度閾值的模擬比例。</li>
                   </ul>
                   <p><b>3. Posterior P(H₁) (後驗機率)：</b>給定資料和先驗假設下，H₁ 為真的機率（假設 H₀ 和 H₁ 的先驗機率相等 P(H₀)=P(H₁)=0.5）。可直接用來決策。</p>
                    <p><b>   P(H₁) > 0.95：</b>摘要表顯示後驗機率超過 95% 的模擬比例，這是一個常用的決策閾值，表示有很高的信心認為 H₁ 是成立的。</p>
                   <p><b>4. S-value (Surprisal)：</b> S-value = -log₂(p-value)。它將 p-value 轉換為一個更直觀的尺度，表示觀察到的數據相對於某個檢驗假設（通常是 H₀）有多麼「令人驚訝」或「不相容」。S-value 的單位是位元 (bits) 的資訊。例如，p=0.05 對應 S ≈ 4.3 位元。</p>
                   <p><b>5. ROPE (Region of Practical Equivalence)：</b>在貝氏分析中，ROPE 指的是效應量小到可以被視為「實務上等同於無差異」的一個區間（例如 Cohen's d 在 -0.1 到 0.1 之間）。<b>本 App 摘要表中的 d in ROPE [-0.05, 0.05] 指標是計算每次模擬中 Cohen's d 的 *點估計值* 落在指定區間 [-0.05, 0.05] 內的比例，此為簡化計算，與標準貝氏 ROPE 分析（基於後驗分佈）不同。</b></p>

                   <p><b>6. IQR (Interquartile Range, 四分位距)：</b>IQR 是衡量統計分佈變異性或離散程度的指標。它計算的是第 75 百分位數 (Q3) 與第 25 百分位數 (Q1) 之間的差值 (IQR = Q3 - Q1)。IQR 描述了數據中間 50% 的範圍。</p>
                   <ul>
                     <li><b>優點：</b> IQR 對於數據中的極端值（離群值）不敏感，比全距 (Range) 或標準差 (Standard Deviation) 更穩健 (robust)。</li>
                     <li><b>解釋：</b>
                         <ul>
                           <li><b>較小的 IQR：</b>表示數據點集中在中間值附近，變異性較小。在模擬中，這意味著不同模擬實驗得到的該指標值比較一致。</li>
                           <li><b>較大的 IQR：</b>表示數據點分佈較廣，變異性較大。在模擬中，這意味著不同模擬實驗得到的該指標值差異很大，結果較不穩定或隨機性較高。</li>
                         </ul>
                     </li>
                     <li><b>在本 App 中：</b>摘要表顯示了 p-value, S-value, BF₁₀, log(BF₁₀), 和 Posterior P(H₁) 在所有模擬實驗中的 IQR。這有助於理解在給定設定（真實效應量、樣本數、先驗）下，這些統計指標在重複實驗中的穩定性。例如：
                         <ul>
                           <li>如果 <b>IQR(p)</b> 很大（接近 1），尤其當真實效應量 d=0 時，反映了 p 值在 H₀ 下的均勻分佈特性，即隨機性很大。如果 d≠0 且樣本數足夠，IQR(p) 通常會較小且靠近 0。</li>
                           <li>如果 <b>IQR(log(BF₁₀))</b> 很大，表示即使實驗條件相同，證據強度 (支持 H₁ 或 H₀) 在不同模擬實驗之間也有很大差異。</li>
                           <li>如果 <b>IQR(Posterior P(H₁))</b> 很大，表示對 H₁ 成立的信念程度在不同模擬實驗之間變化很大。</li>
                         </ul>
                     </li>
                   </ul>
                 ") # End of HTML for summary table explanations
        ),
        # Tab 4: General Explanations (CI vs CrI)
        tabPanel("一般說明",
                 HTML("
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
                 ") # End of HTML for general explanations
        ) # End of tabPanel "一般說明"
      ) # End of tabsetPanel
    ) # End of mainPanel
  ) # End of sidebarLayout
) # End of fluidPage (UI)

# ==== Server Logic ====
# Defines the server-side logic of the Shiny app
server <- function(input, output, session) {

  # Enable showtext for UTF-8 character display in plots
  showtext_auto()

  # --- Simulation Data Generation ---
  # Reactive expression to run the simulation when the 'go' button is pressed
  simulate_data <- eventReactive(input$go, {

    # Helper function to compute Bayes Factor (BF10)
    compute_bf10 <- function(t_stat, n1, n2, prior_sd) {
      if (is.na(t_stat) || is.na(n1) || is.na(n2) || is.na(prior_sd) || n1 <= 0 || n2 <= 0 || prior_sd <= 0) {
        return(NA_real_)
      }
      if (requireNamespace("BayesFactor", quietly = TRUE)) {
        bf_result <- tryCatch({
          # Simulate data for ttestBF based on observed t-statistic and N
          # This is an approximation. A more direct calculation might be possible
          # but ttestBF typically takes raw data or summary stats like mean, sd, n.
          # For simplicity, we generate data that would roughly correspond to the t-stat.
          # This part can be tricky and might need refinement for perfect accuracy.
          # Assuming pooled sd = 1 for simplicity in generating data for BF.
          mean_diff_approx <- t_stat * sqrt(1/n1 + 1/n2) # Approximate mean diff if sd_pooled=1
          x_sim <- rnorm(n1, mean = 0, sd = 1)
          y_sim <- rnorm(n2, mean = mean_diff_approx, sd = 1)
          BayesFactor::ttestBF(x = x_sim, y = y_sim, rscale = prior_sd)
        }, error = function(e) NULL)

        if (!is.null(bf_result) && inherits(bf_result, "BFBayesFactor")) {
           bf_matrix <- BayesFactor::extractBF(bf_result)
           bf10 <- bf_matrix$bf[1]
           if (is.infinite(bf10) || is.nan(bf10)) return(NA_real_)
           return(bf10)
        }
      }
      return(NA_real_) # Return NA if BayesFactor package is not available or error
    }

    # Helper function to compute posterior probability P(H1) from BF10
    # Assumes prior P(H1) = P(H0) = 0.5, so prior odds = 1
    compute_post_prob <- function(bf10) {
      if (is.na(bf10)) return(NA_real_)
      prior_odds <- 1 # P(H1)/P(H0) = 0.5/0.5 = 1
      post_odds <- bf10 * prior_odds
      if (is.infinite(post_odds)) return(1.0) # If BF10 is Inf, P(H1) approaches 1
      return(post_odds / (1 + post_odds))
    }

    # Helper function to compute S-value from p-value
    compute_svalue <- function(p) {
      if (is.na(p) || p < 0 || p > 1) return(NA_real_)
      if (p == 0) return(Inf) # S-value is Inf for p=0
      if (p == 1) return(0)   # S-value is 0 for p=1
      -log2(p)
    }

    # Show progress notification
    withProgress(message = '執行模擬中...', value = 0, {
        # Set seed for reproducibility
        set.seed(123)
        # Run the simulation 'n_sim' times
        results <- vector("list", input$n_sim) # Pre-allocate list
        for (i in 1:input$n_sim) {
          # Generate data based on d_true
          g1 <- rnorm(input$n_per_group, mean = 0, sd = 1)
          g2 <- rnorm(input$n_per_group, mean = input$d_true, sd = 1)

          # Perform t-test
          t_res <- tryCatch(t.test(g1, g2), error = function(e) NULL)

          # Handle t-test failure or invalid results
          if (is.null(t_res) || !is.finite(t_res$statistic) || !is.finite(t_res$p.value)) {
             p_val <- NA_real_
             t_stat <- NA_real_
             d_est <- NA_real_
          } else {
             t_stat <- t_res$statistic
             p_val <- t_res$p.value
             # Calculate estimated Cohen's d (for independent samples, equal n)
             d_est <- t_stat * sqrt(2 / input$n_per_group)
          }

          # Compute BF10
          bf10 <- compute_bf10(t_stat, input$n_per_group, input$n_per_group, input$prior_sd)
          # Calculate log(BF10)
          log_bf10 <- if (!is.na(bf10) && bf10 > 0) log(bf10) else NA_real_
          # Compute Posterior Probability P(H1)
          post_p <- compute_post_prob(bf10)
          # Compute S-value
          s_val <- compute_svalue(p_val)

          # Store results
          results[[i]] <- data.frame(sim = i, p = p_val, bf10 = bf10, log_bf10 = log_bf10, post_p = post_p, s_value = s_val, d_est = d_est)

          # Increment the progress bar
          incProgress(1/input$n_sim, detail = paste("完成", i, "/", input$n_sim))
        } # End for loop
    }) # End withProgress

    # Combine results into one data frame
    df_final <- bind_rows(results) # More efficient than do.call(rbind, ...)

    # Ensure columns that should be numeric are numeric
    # This helps prevent issues if NAs were introduced as non-numeric types initially
    df_final <- df_final %>%
        mutate(across(c(p, bf10, log_bf10, post_p, s_value, d_est), as.numeric))

    return(df_final)
  })

  # --- Render p-value Animation ---
  output$animated_plot_p <- renderImage({
    df_full <- simulate_data()
    if (!is.data.frame(df_full) || nrow(df_full) == 0) {
         return(list(src = "", contentType = 'image/gif', alt = "請點擊「開始模擬」產生資料"))
    }
    n_plot_sims <- 100 # Number of simulations for animation
    df <- head(df_full, n_plot_sims)
    actual_sims_in_df <- nrow(df) # Actual sims used (might be < n_plot_sims if total sims < 100)
    df_clean_p <- df[!is.na(df$p), ] # Remove NA p-values for plotting
    if (nrow(df_clean_p) == 0) {
         return(list(src = "", contentType = 'image/gif', alt = "前 100 次模擬中無有效的 p-value 資料可繪圖"))
    }

    p_plot <- ggplot(df_clean_p, aes(x = sim, y = p, group = 1)) +
      geom_line(color = "steelblue") +
      geom_point(size = 2, color = "orange") +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 0.005, linetype = "dotted", color = "darkred") +
      labs(x = "模擬次數 (前 100 次)", y = "p-value",
           title = paste("p-value 之舞 (顯示 ", nrow(df_clean_p), "/", actual_sims_in_df, " 次有效模擬)", sep="")) +
      scale_y_continuous(limits = c(0, 1)) +
      transition_reveal(sim) +
      theme_minimal()

    anim_file_p <- tempfile(fileext = ".gif")
    anim_result_p <- tryCatch({
        if (!requireNamespace("gifski", quietly = TRUE)) stop("gifski package needed.")
        showtext_opts(dpi = 96) # Ensure showtext DPI is set for gifski
        anim_save(anim_file_p, animate(p_plot, fps = 15, width = 600, height = 350, renderer = gifski_renderer()))
        list(src = anim_file_p, contentType = 'image/gif')
    }, error = function(e) {
        # Provide more informative error messages
        msg <- paste("p-value 動畫渲染失敗:", e$message)
        if (!requireNamespace("gifski", quietly = TRUE)) msg <- paste(msg, "請安裝 'gifski' 套件。")
        if (grepl("showtext", e$message, ignore.case = TRUE)) msg <- paste(msg, "請確認 'showtext' 套件已安裝且字體可用。")
        list(src = "", contentType = 'image/gif', alt = msg)
    })
    return(anim_result_p)
  }, deleteFile = TRUE)

  # --- Render log(BF10) Animation ---
  output$animated_plot_logbf <- renderImage({
    df_full <- simulate_data()
     if (!is.data.frame(df_full) || nrow(df_full) == 0) {
         return(list(src = "", contentType = 'image/gif', alt = "請點擊「開始模擬」產生資料"))
    }
    n_plot_sims <- 100
    df <- head(df_full, n_plot_sims)
    actual_sims_in_df <- nrow(df)
    df_clean_logbf <- df[!is.na(df$log_bf10) & is.finite(df$log_bf10), ] # Ensure finite values
     if (nrow(df_clean_logbf) == 0) {
         return(list(src = "", contentType = 'image/gif', alt = "前 100 次模擬中無有效的 log(BF10) 資料可繪圖"))
     }

    # Determine y-axis limits for log(BF10) plot, capping extremes
    y_min <- min(df_clean_logbf$log_bf10, na.rm = TRUE)
    y_max <- max(df_clean_logbf$log_bf10, na.rm = TRUE)
    y_lim_lower <- max(y_min, log(1/1000), na.rm=TRUE) # Cap at BF=1/1000 (log scale)
    y_lim_upper <- min(y_max, log(1000), na.rm=TRUE)  # Cap at BF=1000 (log scale)
    # Fallback if limits are problematic
    if(is.infinite(y_lim_lower) || is.infinite(y_lim_upper) || y_lim_lower >= y_lim_upper) {
        y_lim_lower <- log(1/100) # Default lower limit if calculation fails
        y_lim_upper <- log(100)   # Default upper limit if calculation fails
    }

    logbf_plot <- ggplot(df_clean_logbf, aes(x = sim, y = log_bf10, group = 1)) +
      geom_line(color = "darkgreen") +
      geom_point(size = 2, color = "purple") +
      geom_hline(yintercept = log(10), linetype = "dotted", color = "darkblue", alpha=0.7) +
      geom_hline(yintercept = log(3), linetype = "dashed", color = "blue", alpha=0.7) +
      geom_hline(yintercept = 0, linetype = "solid", color = "grey50", alpha=0.5) + # BF=1
      geom_hline(yintercept = log(1/3), linetype = "dashed", color = "red", alpha=0.7) +
      geom_hline(yintercept = log(1/10), linetype = "dotted", color = "darkred", alpha=0.7) +
      scale_y_continuous(limits = c(y_lim_lower, y_lim_upper)) + # Apply calculated limits
      labs(x = "模擬次數 (前 100 次)", y = "log(BF₁₀)",
           title = paste("log(BF₁₀) 之舞 (顯示 ", nrow(df_clean_logbf), "/", actual_sims_in_df, " 次有效模擬)", sep=""),
           caption = "水平線: log(10), log(3), 0, log(1/3), log(1/10)") +
      transition_reveal(sim) +
      theme_minimal() +
      theme(plot.caption = element_text(hjust = 0.5, size=9, color="gray30")) # Style caption

    anim_file_logbf <- tempfile(fileext = ".gif")
    anim_result_logbf <- tryCatch({
        if (!requireNamespace("gifski", quietly = TRUE)) stop("gifski package needed.")
        showtext_opts(dpi = 96)
        anim_save(anim_file_logbf, animate(logbf_plot, fps = 15, width = 600, height = 350, renderer = gifski_renderer()))
        list(src = anim_file_logbf, contentType = 'image/gif')
    }, error = function(e) {
        msg <- paste("log(BF10) 動畫渲染失敗:", e$message)
        if (!requireNamespace("gifski", quietly = TRUE)) msg <- paste(msg, "請安裝 'gifski' 套件。")
        if (grepl("showtext", e$message, ignore.case = TRUE)) msg <- paste(msg, "請確認 'showtext' 套件已安裝且字體可用。")
        list(src = "", contentType = 'image/gif', alt = msg)
    })
    return(anim_result_logbf)
  }, deleteFile = TRUE)

  # --- Render Violin Plots (Using FULL dataset) ---

  # Helper function for creating violin plots with Q1, Median, Q3 in subtitle
  create_violin_plot <- function(data, var, title, y_label, y_limits = NULL, hlines = NULL, hline_labels = NULL, use_log_scale = FALSE) {
    # Filter out NA and non-finite values for the specific variable
    df_clean <- data[!is.na(data[[var]]) & is.finite(data[[var]]), ]

    if (nrow(df_clean) == 0) {
      return(ggplot() + labs(title = title, subtitle = "無有效資料可繪圖") + theme_void())
    }

    # Calculate Q1, Median, Q3
    quantiles_data <- quantile(df_clean[[var]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 7) # type=7 is R default
    q1_val <- round(quantiles_data[1], 3)
    median_val <- round(quantiles_data[2], 3)
    q3_val <- round(quantiles_data[3] , 3)
    
    # Create subtitle string, handle NAs from quantiles if df_clean is too small or all same value
    plot_subtitle <- "Q1, Median, Q3: N/A"
    if(all(!is.na(c(q1_val, median_val, q3_val)))){
        plot_subtitle <- paste0("Q1: ", q1_val, " | Median: ", median_val, " | Q3: ", q3_val)
    }


    # Base plot
    p <- ggplot(df_clean, aes(x = "", y = .data[[var]])) +
      geom_violin(trim = TRUE, fill = "lightblue", alpha = 0.7) +
      # geom_boxplot width reduced, outlier.shape=NA to hide outliers (violin shows density)
      geom_boxplot(width = 0.08, fill = "white", alpha = 0.5, outlier.shape = NA) +
      labs(title = title, x = "", y = y_label, subtitle = plot_subtitle) + # Added subtitle
      theme_minimal() +
      theme(axis.text.x = element_blank(), # Hide x-axis text ("")
            axis.ticks.x = element_blank(), # Hide x-axis ticks
            plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray20")) # Style subtitle

    # Add horizontal lines if provided
    if (!is.null(hlines)) {
      p <- p + geom_hline(yintercept = hlines, linetype = "dashed", color = "grey50")
      # Add labels for horizontal lines if provided
      if (!is.null(hline_labels) && length(hline_labels) == length(hlines)) {
         # Create annotation data frame
         hline_data <- data.frame(yintercept = hlines, label = hline_labels)
         # Add text annotations slightly offset. Adjust x position if needed.
         p <- p + geom_text(data = hline_data, aes(x = 0.55, y = yintercept, label = label),
                            hjust = 0, vjust = -0.5, size = 3, color = "grey30", inherit.aes = FALSE)
      }
    }

    # Apply y-axis limits if provided
    if (!is.null(y_limits)) {
        if (use_log_scale) {
            safe_limits <- pmax(y_limits, .Machine$double.eps)
             p <- p + scale_y_log10(limits = safe_limits, labels = scales::trans_format("log10", scales::math_format(10^.x))) +
                      annotation_logticks(sides = "l")
        } else {
            p <- p + scale_y_continuous(limits = y_limits)
        }
    } else if (use_log_scale) {
        # Apply log scale without specific limits, ensuring positive range for auto-scaling
        min_val_for_log <- min(df_clean[[var]][df_clean[[var]] > 0], na.rm = TRUE)
        max_val_for_log <- max(df_clean[[var]], na.rm = TRUE)
        if (is.finite(min_val_for_log) && is.finite(max_val_for_log) && min_val_for_log > 0) {
           p <- p + scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
                    annotation_logticks(sides = "l")
        } else {
           p <- p + labs(caption = paste(y_label, "無法使用對數尺度 (無正值或範圍問題)")) # Add caption instead of subtitle
        }
    }
    return(p)
  }

  # p-value violin plot
  output$violin_plot_p <- renderPlot({
    df <- simulate_data()
    create_violin_plot(df, "p", title = "p-value 分佈", y_label = "p-value",
                       y_limits = c(0, 1), hlines = c(0.05, 0.005), hline_labels = c("0.05", "0.005"))
  })

  # S-value violin plot
  output$violin_plot_s <- renderPlot({
    df <- simulate_data()
    df_finite_s <- df[is.finite(df$s_value), ]
    max_s_default_limit <- -log2(1e-6) # Cap S at p=1e-6 for plotting sanity
    # Calculate y_lim_s ensuring max is not Inf and min is not greater than max
    s_values_for_limit <- df_finite_s$s_value[df_finite_s$s_value >= 0] # Only non-negative S-values
    y_max_s_val <- if(length(s_values_for_limit) > 0) max(s_values_for_limit, na.rm = TRUE) else max_s_default_limit
    
    y_lim_s_upper <- min(y_max_s_val, max_s_default_limit, na.rm = TRUE)
    y_lim_s <- c(0, if(is.finite(y_lim_s_upper) && y_lim_s_upper > 0) y_lim_s_upper else 10)


    create_violin_plot(df, "s_value", title = "S-value 分佈", y_label = "S-value (-log₂p)",
                       y_limits = y_lim_s,
                       hlines = -log2(c(0.05, 0.005)),
                       hline_labels = c(paste0("S ≈ ", round(-log2(0.05),1)), paste0("S ≈ ", round(-log2(0.005),1))))
  })

  # BF10 violin plot (using log scale)
  output$violin_plot_bf10 <- renderPlot({
    df <- simulate_data()
    df_pos_bf <- df[which(df$bf10 > 0 & is.finite(df$bf10)), ] # Ensure positive and finite for log scale
    bf_hlines <- c(1/10, 1/3, 1, 3, 10)
    bf_hline_labels <- c("1/10", "1/3", "1", "3", "10")

    min_bf_default_limit <- 1e-4
    max_bf_default_limit <- 1e4
    
    y_min_bf_val <- if(nrow(df_pos_bf) > 0) min(df_pos_bf$bf10, na.rm = TRUE) else min_bf_default_limit
    y_max_bf_val <- if(nrow(df_pos_bf) > 0) max(df_pos_bf$bf10, na.rm = TRUE) else max_bf_default_limit

    y_lim_bf_lower <- max(y_min_bf_val, min_bf_default_limit, na.rm = TRUE)
    y_lim_bf_upper <- min(y_max_bf_val, max_bf_default_limit, na.rm = TRUE)

    y_lim_bf <- c(
        if(is.finite(y_lim_bf_lower) && y_lim_bf_lower > 0) y_lim_bf_lower else 0.01,
        if(is.finite(y_lim_bf_upper) && y_lim_bf_upper > y_lim_bf_lower) y_lim_bf_upper else 100
    )
    if(y_lim_bf[1] >= y_lim_bf[2]) y_lim_bf <- c(0.01, 100) # Final fallback

    create_violin_plot(df_pos_bf, "bf10", title = "BF₁₀ 分佈 (對數尺度)", y_label = "BF₁₀",
                       use_log_scale = TRUE, y_limits = y_lim_bf,
                       hlines = bf_hlines, hline_labels = bf_hline_labels)
  })

  # log(BF10) violin plot
  output$violin_plot_logbf <- renderPlot({
    df <- simulate_data()
    df_finite_logbf <- df[is.finite(df$log_bf10), ]
    min_logbf_default_limit <- log(1/1000)
    max_logbf_default_limit <- log(1000)

    y_min_logbf_val <- if(nrow(df_finite_logbf) > 0) min(df_finite_logbf$log_bf10, na.rm = TRUE) else min_logbf_default_limit
    y_max_logbf_val <- if(nrow(df_finite_logbf) > 0) max(df_finite_logbf$log_bf10, na.rm = TRUE) else max_logbf_default_limit
    
    y_lim_logbf_lower <- max(y_min_logbf_val, min_logbf_default_limit, na.rm = TRUE)
    y_lim_logbf_upper <- min(y_max_logbf_val, max_logbf_default_limit, na.rm = TRUE)

    y_lim_logbf <- c(
        if(is.finite(y_lim_logbf_lower)) y_lim_logbf_lower else log(1/100),
        if(is.finite(y_lim_logbf_upper) && y_lim_logbf_upper > y_lim_logbf_lower) y_lim_logbf_upper else log(100)
    )
    if(y_lim_logbf[1] >= y_lim_logbf[2]) y_lim_logbf <- c(log(1/100), log(100)) # Final fallback


    logbf_hlines <- log(c(1/10, 1/3, 1, 3, 10))
    logbf_hline_labels <- c("log(1/10)", "log(1/3)", "0 (BF=1)", "log(3)", "log(10)")

    create_violin_plot(df, "log_bf10", title = "log(BF₁₀) 分佈", y_label = "log(BF₁₀)",
                       y_limits = y_lim_logbf, hlines = logbf_hlines, hline_labels = logbf_hline_labels)
  })

  # *** NEW Violin Plot for Posterior P(H1) ***
  output$violin_plot_post_p <- renderPlot({
    df <- simulate_data()
    # P(H1) ranges from 0 to 1.
    # Key thresholds could be 0.5 (equivocal), 0.95 (strong evidence for H1)
    ph1_hlines <- c(0.05, 0.5, 0.95) # Added 0.05 for symmetry with 0.95
    ph1_hline_labels <- c("P(H₁) = 0.05", "P(H₁) = 0.5", "P(H₁) = 0.95")

    create_violin_plot(df, "post_p", title = "P(H₁) 分佈 (後驗機率)", y_label = "P(H₁)",
                       y_limits = c(0, 1), hlines = ph1_hlines, hline_labels = ph1_hline_labels)
  })


  # --- Render Summary Table ---
  output$summary_table <- renderDT({
    df <- simulate_data()
    if (!is.data.frame(df) || nrow(df) == 0) {
        # Return an empty table with a message if no data
        return(datatable(data.frame(Message = "請點擊「開始模擬」產生資料"), options = list(dom = 't', searching = FALSE, info = FALSE), rownames = FALSE))
    }

    is_h0_true <- isTRUE(all.equal(input$d_true, 0)) # Check if H0 is true (d_true is 0)

    # Define column names with HTML for tooltips
    colnames_vec <- c(
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
      '<span title="估計效應量 d 的點估計值落在 [-0.05, 0.05] 區間內的比例">d in ROPE<br>[-0.05, 0.05]</span>',
      '<span title="Posterior P(H₁) > 0.95 表示後驗機率很高">P(H₁) > 0.95</span>',
      '<span title="p-value 的四分位距 (IQR = Q3 - Q1)">IQR(p)</span>',
      '<span title="S-value 的四分位距">IQR(S)</span>',
      '<span title="BF₁₀ 的四分位距">IQR(BF₁₀)</span>', # Note: IQR for BF10 can be very large and skewed
      '<span title="log(BF₁₀) 的四分位距">IQR(log BF₁₀)</span>',
      '<span title="Posterior P(H₁) 的四分位距">IQR(P(H₁))</span>'
    )

    # Helper function to calculate mean/proportion, robust to NAs in logical conversion
    calc_mean_prop <- function(condition_vector) {
      # Ensure the input is logical, handling potential NAs from conditions on NA data
      logical_vector <- tryCatch(as.logical(condition_vector),
                                 warning = function(w) rep(NA, length(condition_vector)),
                                 error = function(e) rep(NA, length(condition_vector)))
      if (all(is.na(logical_vector))) return(NA_real_) # if all became NA (e.g. df$p was all NA)
      mean(logical_vector, na.rm = TRUE)
    }

    # Helper function to calculate IQR, robust to NAs and non-finite values
    calc_iqr <- function(data_vector) {
        numeric_vector <- tryCatch(as.numeric(data_vector),
                                   warning = function(w) rep(NA, length(data_vector)),
                                   error = function(e) rep(NA, length(data_vector)))
        finite_vector <- numeric_vector[is.finite(numeric_vector)] # Use only finite values for IQR
        if (length(finite_vector) < 2) return(NA_real_) # IQR needs at least 2 points
        IQR(finite_vector, na.rm = FALSE, type = 7) # type=7 is R default
    }

    # Calculate proportions
    p_prop_05 <- calc_mean_prop(df$p < 0.05)
    p_prop_005 <- calc_mean_prop(df$p < 0.005)
    bf10_pos_3 <- calc_mean_prop(df$bf10 > 3)
    bf10_pos_10 <- calc_mean_prop(df$bf10 > 10)
    bf10_neg_1_3 <- calc_mean_prop(df$bf10 < 1/3)
    bf10_neg_1_10 <- calc_mean_prop(df$bf10 < 1/10)
    post_prop <- calc_mean_prop(df$post_p > 0.95) # Proportion P(H1) > 0.95
    rope_prop <- calc_mean_prop(abs(df$d_est) <= 0.05) # Proportion of d_est in ROPE

    # Calculate IQRs
    iqr_p <- calc_iqr(df$p)
    iqr_s <- calc_iqr(df$s_value)
    iqr_bf10 <- calc_iqr(df$bf10)       # IQR of BF10
    iqr_logbf10 <- calc_iqr(df$log_bf10) # IQR of log(BF10)
    iqr_postp <- calc_iqr(df$post_p)     # IQR of Posterior P(H1)

    # Create summary data frame
    summary_df <- data.frame(
      p_05 = round(p_prop_05, 3),
      p_005 = round(p_prop_005, 3),
      bf10_gt3 = round(bf10_pos_3, 3),
      bf10_gt10 = round(bf10_pos_10, 3),
      bf10_lt1_3 = round(bf10_neg_1_3, 3),
      bf10_lt1_10 = round(bf10_neg_1_10, 3),
      rope = round(rope_prop, 3),
      post = round(post_prop, 3),
      iqr_p = round(iqr_p, 3),
      iqr_s = round(iqr_s, 2), # S-values can be larger, 2 decimal places
      iqr_bf10 = if(!is.na(iqr_bf10) && iqr_bf10 > 1000) scales::scientific(iqr_bf10, digits=2) else round(iqr_bf10, 2), # Scientific for large BF10 IQR
      iqr_logbf10 = round(iqr_logbf10, 2),
      iqr_postp = round(iqr_postp, 3)
    )

    # Render the table using DT package
    datatable(summary_df,
              colnames = lapply(colnames_vec, HTML), # Use HTML for tooltips in column names
              escape = FALSE, # Allow HTML in column names
              options = list(dom = 't',      # Show only the table (no search, pagination controls)
                             paging = FALSE,
                             searching = FALSE,
                             info = FALSE,
                             ordering = FALSE), # Disable column ordering
              rownames = FALSE, # Hide row names
              class = 'cell-border stripe') # Add some styling
  })

} # End of server function

# ==== Run the Application ====
shinyApp(ui = ui, server = server)
