# ==========================================
# STOCKOUT RISK DETECTION SYSTEM (CIA 3)
# ==========================================

# Install if not already installed
# install.packages("DiagrammeR")
# install.packages("htmlwidgets")

library(DiagrammeR)
library(htmlwidgets)

# ==========================================
# 1. FLOW DIAGRAM FUNCTION (DFD)
# ==========================================

create_flow_diagram <- function() {
  
  d <- grViz("
  digraph flowchart {

    graph [layout = dot]

    node [shape=rectangle, style=filled, fillcolor=lightpink]

    A [label='Inventory CSV File']
    B [label='Load Data']
    C [label='Calculate Days of Inventory']
    D [label='Apply IF-ELSE Rules']
    E [label='Risk Classification']
    F [label='Output + Visualization']

    A -> B -> C -> D -> E -> F
  }
  ")
  
  # Try to display in viewer
  print(d)
  
  # ALSO save & open in browser (works everywhere)
  saveWidget(d, "flow_diagram.html", selfcontained = TRUE)
  browseURL("flow_diagram.html")
}

# ==========================================
# 2. FILE HANDLING FUNCTION
# ==========================================

load_inventory_data <- function(file_path) {
  
  tryCatch({
    
    if (!file.exists(file_path)) {
      stop("File not found! Check the path.")
    }
    data <- read.csv("data/sample_inventory.csv")
    
    required_cols <- c("ProductName", "CurrentStock", "ReorderLevel", "DailySales")
    
    if (!all(required_cols %in% colnames(data))) {
      stop("Missing required columns in dataset!")
    }
    
    return(data)
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

# ==========================================
# 3. CALCULATE DAYS OF INVENTORY
# ==========================================

calculate_days_inventory <- function(stock, sales) {
  
  if (sales == 0) {
    return(Inf)
  } else {
    return(stock / sales)
  }
}

# ==========================================
# 4. RISK CLASSIFICATION FUNCTION
# ==========================================

classify_risk <- function(stock, reorder, days) {
  
  if (stock <= reorder) {
    return("HIGH")
    
  } else if (days < 3) {
    return("HIGH")
    
  } else if (days >= 3 & days <= 7) {
    return("MEDIUM")
    
  } else {
    return("LOW")
  }
}

# ==========================================
# 5. MAIN PROCESSING FUNCTION
# ==========================================

process_inventory <- function(data) {
  
  stockout_risk <- c()
  days_list <- c()
  
  cat("\n===== STOCKOUT RISK ANALYSIS =====\n\n")
  
  for (i in 1:nrow(data)) {
    
    product <- data$ProductName[i]
    stock <- data$CurrentStock[i]
    reorder <- data$ReorderLevel[i]
    sales <- data$DailySales[i]
    
    days <- calculate_days_inventory(stock, sales)
    risk <- classify_risk(stock, reorder, days)
    
    stockout_risk <- c(stockout_risk, risk)
    days_list <- c(days_list, days)
    
    cat("Product:", product,
        "| Days:", round(days, 2),
        "| Risk:", risk, "\n")
  }
  
  data$DaysInventory <- days_list
  data$StockoutRisk <- stockout_risk
  
  return(data)
}

# ==========================================
# 6. ARRAY CREATION FUNCTION
# ==========================================

create_inventory_array <- function(data) {
  
  arr <- array(
    c(data$CurrentStock,
      data$DailySales,
      data$DaysInventory),
    dim = c(nrow(data), 3, 1)
  )
  
  dimnames(arr) <- list(
    data$ProductName,
    c("Stock", "Sales", "Days"),
    "Analysis"
  )
  
  return(arr)
}

# ==========================================
# 7. SAVE OUTPUT FUNCTION
# ==========================================

save_output <- function(data, path = "output_inventory.csv") {
  
  tryCatch({
    write.csv(data, path, row.names = FALSE)
    cat("\nOutput saved successfully at:", path, "\n")
    
  }, error = function(e) {
    cat("Error saving file:", e$message, "\n")
  })
}

# ==========================================
# 8. VISUALIZATION FUNCTION
# ==========================================

plot_risk <- function(data) {
  
  risk_table <- table(data$StockoutRisk)
  
  barplot(risk_table,
          main = "Stockout Risk Distribution",
          xlab = "Risk Level",
          ylab = "Number of Products",
          col = c("red", "orange", "green"))
}

# ==========================================
# 9. MAIN EXECUTION FUNCTION
# ==========================================

run_system <- function() {
  
  # Step 1: Show DFD
  create_flow_diagram()
  
  # Step 2: File path (CHANGE THIS)
  file_path <- "C://Users//allab//OneDrive//Documents//3rd Trimester B.A//R for Managers//CIA 2 Stockout Problem//inventory.csv"
  
  data <- load_inventory_data(file_path)
  
  if (is.null(data)) {
    cat("Execution stopped due to error.\n")
    return()
  }
  
  # Step 3: Process data
  processed_data <- process_inventory(data)
  
  # Step 4: Create array
  inventory_array <- create_inventory_array(processed_data)
  cat("\nArray Created Successfully\n")
  
  # Step 5: Save output
  save_output(processed_data)
  
  # Step 6: Plot graph
  plot_risk(processed_data)
  
  cat("\nPROCESS COMPLETED SUCCESSFULLY\n")
}

# ==========================================
# RUN PROGRAM
# ==========================================

run_system()