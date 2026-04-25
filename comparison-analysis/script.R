#!/usr/bin/env Rscript

# ============================================================
#  Consolidated Statistical & Comparative Analysis
# ============================================================

# This script finds all eval_*.csv files in the project root,
# merges them adding a 'Tool' column based on the filename,
# and performs comparative analysis.

required_packages <- c("dplyr", "ggplot2", "tidyr", "forcats", "psych", "stringr", "readr", "fmsb")
to_install <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(forcats)
  library(psych)
  library(stringr)
  library(readr)
  library(fmsb)
})

# --- 1. Load Data ---

# Set project root directory
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  project_root <- args[1]
} else {
  # Default to user's known project root if no argument provided
  project_root <- "$home/evalevm"
}

cat("📂 Project Root:", project_root, "\n")

# Find CSV files matching the pattern in the project root
csv_files <- list.files(path = project_root, pattern = "^evalevm_.*\\.csv$", full.names = TRUE)

if(length(csv_files) == 0) {
  stop(paste("No CSV files found matching 'evalevm_*.csv' in directory:", project_root))
}

cat("📂 Found", length(csv_files), "CSV files. Loading and merging...\n")

# Function to read and label each CSV
read_tool_csv <- function(file_path) {
  # Extract tool name from filename (e.g., ./evalevm_evmole.csv -> evmole)
  file_name <- basename(file_path)
  tool_name <- str_match(file_name, "^evalevm_(.*)\\.csv$")[,2]
  
  cat("   - Reading:", file_name, "as tool:", tool_name, "\n")
  
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(Tool = tool_name) %>%
    mutate(across(everything(), as.character)) # Force consistent types for binding
  
  return(df)
}

# Bind all dataframes
raw_data <- bind_rows(lapply(csv_files, read_tool_csv))

# --- 2. Normalize Columns ---

# Map snake_case CSV columns to PascalCase expected by analysis logic
# Assumes standard output format from evalevm CSV writer
data <- raw_data %>%
  # Update status: items with exit_status=1 or failed_to_parse=TRUE are considered as status="failed"
  mutate(status = ifelse(exit_status %in% c("1") | as.logical(failed_to_parse) %in% c(TRUE), "failed", status)) %>%
  mutate(
    SampleID = sample_id,
    FileName = sample_id, # Using sample_id as filename
    Status = status,
    
    # Explicit Type Conversion
    MaxRAMKB = as.numeric(max_ram_kb),
    ExecTimeMs = as.numeric(exec_time_ms),
    ExecTimeS = as.numeric(exec_time_ms) / 1000,
    AvgCPUPercent = as.numeric(avg_cpu_percent),
    Nodes = as.numeric(nodes),
    UniqueNodes = as.numeric(unique_blocks),
    Edges = as.numeric(edges),
    Density = as.numeric(density),
    OutDegree = as.numeric(avg_out_degree), 
    Reachability = as.numeric(coverage_percent), 
    GraphComplexity = as.numeric(mccabe),
    MaxDepth = as.numeric(max_depth),
    OrphanNodes = as.numeric(orphan_nodes),
    Islands = as.numeric(islands),
    failed_to_parse = as.logical(failed_to_parse),
    IsInvalid = as.logical(is_invalid),
    InvalidPercentage = as.numeric(invalid_percentage),
    BlockCount = as.numeric(block_count)
  ) %>% 
  # Derived metrics
  mutate(
    ConnectedNodes = Nodes - OrphanNodes,
    RAMPerNodeKB = ifelse(Nodes > 0, MaxRAMKB / Nodes, 0),
    ExecTimePerNode = ifelse(Nodes > 0, ExecTimeMs / Nodes, 0),
    ExecTimePerEdge = ifelse(Edges > 0, ExecTimeMs / Edges, 0),
    CPUTimeRatio = AvgCPUPercent, # Simple mapping, could be more complex
    PerformanceScore = (Nodes + Edges) / (ExecTimeS + 0.001) # Example metric
  ) 

# --- 3. Compute Node Redundancy Metric ---

cat("\n📊 Computing Node Redundancy (relative to Paper's BlockCount)...\n")
# Extract BlockCount from 'paper-algorithm' tool to serve as ground truth for all others
paper_blocks <- data %>%
  filter(Tool == "paper") %>%
  select(SampleID, PaperBlockCount = BlockCount) %>%
  distinct()

data <- data %>%
  left_join(paper_blocks, by = "SampleID") %>%
  mutate(
    # Special case: ByteSpector uses its own UniqueNodes as ground truth
    # All other tools use Paper's BlockCount
    BaselineNodes = ifelse(Tool == "bytespector", UniqueNodes, PaperBlockCount),
    
    # Avoid division by zero
    NodeRedundancyPerc = ifelse(!is.na(BaselineNodes) & BaselineNodes > 0, 
                                ((Nodes - BaselineNodes) / BaselineNodes) * 100, 
                                0)
  ) %>%
  select(-BaselineNodes) # Remove temporary column

# Clean & Factors
data$Tool <- as.factor(data$Tool)
data$Status <- as.factor(data$Status)
data$Success <- ifelse(data$Status == "ok", 1, 0) # 'ok' seems to be the success status in headers
data$CFGFailure <- ifelse(data$failed_to_parse, 1, 0)

# Function to compute CFG Quality Score
compute_cfg_quality <- function(reachability, nodes, edges, avg_out_degree) {
  if (is.na(nodes) || nodes <= 1) return(0)
  
  reach_norm <- ifelse(is.na(reachability), 0, reachability / 100)
  density <- ifelse(is.na(edges), 0, edges / (nodes * (nodes - 1)))
  # Handle NaN density
  if(is.nan(density) || is.infinite(density)) density <- 0
  
  outdeg_norm <- ifelse(is.na(avg_out_degree), 0, min(avg_out_degree / (nodes - 1), 1))
  if(is.nan(outdeg_norm)) outdeg_norm <- 0
  
  cqs <- (reach_norm + density + outdeg_norm) / 3
  return(cqs)
}

# Creates a dataset of items with orphan nodes for the 'paper-algorithm' tool
get_paper_orphan_nodes <- function(df) {
  df %>%
    filter(Tool == "paper") %>%
    filter(OrphanNodes > 0) %>%
    select(SampleID, OrphanNodes, Nodes, Edges, Reachability, Status)
}

data$CQS <- mapply(compute_cfg_quality,
                   reachability = data$Reachability,
                   nodes = data$Nodes,
                   edges = data$Edges,
                   avg_out_degree = data$OutDegree)

# --- Summary Calculation (Before NA replacement) ---
# We calculate averages IGNORING NAs (na.rm=TRUE) but essentially on the raw data
# where failed runs have NAs. This ensures "Average Time" is average time of *successful/valid* runs.

summary_by_tool <- data %>%
  group_by(Tool) %>%
  summarise(
    n = n(),
    success_rate = mean(Success, na.rm = TRUE),
    
    # Time & Resources
    mean_exec_time_s = mean(ExecTimeS, na.rm = TRUE),
    max_ram_kb = mean(MaxRAMKB, na.rm = TRUE),
    mean_cpu = mean(AvgCPUPercent, na.rm = TRUE),
    
    # Graph Topology
    mean_nodes = mean(Nodes, na.rm = TRUE),
    mean_unique_nodes = mean(UniqueNodes, na.rm = TRUE),
    mean_connected_nodes = mean(ConnectedNodes, na.rm = TRUE),
    mean_orphan_nodes = mean(OrphanNodes, na.rm = TRUE),
    mean_edges = mean(Edges, na.rm = TRUE),
    mean_density = mean(Density, na.rm = TRUE),
    mean_depth = mean(MaxDepth, na.rm = TRUE),
    mean_out_degree = mean(OutDegree, na.rm = TRUE),
    mean_islands = mean(Islands, na.rm = TRUE),
    
    # Complexity & Quality
    mean_cyclomatic = mean(GraphComplexity, na.rm = TRUE),
    mean_cqs = mean(CQS, na.rm = TRUE),
    mean_reachability = mean(Reachability, na.rm = TRUE), # Explicit original reachability
    mean_node_redundancy = mean(NodeRedundancyPerc, na.rm = TRUE),
    
    # Efficiency Ratios
    mean_ram_per_node_kb = mean(RAMPerNodeKB, na.rm = TRUE),
    mean_time_per_node_ms = mean(ExecTimePerNode, na.rm = TRUE),
    mean_time_per_edge_ms = mean(ExecTimePerEdge, na.rm = TRUE),
    
    # Percentiles
    p95_time = quantile(ExecTimeS, 0.95, na.rm = TRUE)
  )

cat("\n🔹 Tool Comparison Summary:\n")
print(summary_by_tool)
write.csv(summary_by_tool, "consolidated_tool_summary.csv", row.names = FALSE)

# Generate specific coverage summary table
coverage_summary <- summary_by_tool %>%
  select(
    Tool,
    mean_nodes,
    mean_connected_nodes,
    mean_edges,
    mean_reachability, # Use explicit Reachability
    mean_depth,
    mean_islands,
    mean_orphan_nodes,
    mean_node_redundancy
  ) %>%
  rename(
    Nodes = mean_nodes,
    Edges = mean_edges,
    Reachability = mean_reachability,
    CFG_Depth_Max = mean_depth,
    Islands = mean_islands,
    OrphanNodes = mean_orphan_nodes,
    Node_Redundancy_Perc = mean_node_redundancy
  )

cat("\n🔹 Coverage Summary Table:\n")
print(coverage_summary)
write.csv(coverage_summary, "coverage_summary.csv", row.names = FALSE)


# Filter out empty contracts/anomalies if needed
# data <- data %>% filter(Nodes > 0 | Status != "ok") # User requested to keep everything

# Replace NA in numeric columns with 0 to include failures in statistics for PLOTS
# For plots, we might want 0s instead of NAs to show "no result"
data <- data %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# --- 3. Analysis & Visualization ---

cat("\n📈 Overall summary:\n")
print(summary(data %>% select(Tool, ExecTimeS, MaxRAMKB, Nodes, Edges, Success)))

cat("\n📉 Generating comparison plots...\n")

# Execution Time Boxplot
p1 <- ggplot(data, aes(x = Tool, y = ExecTimeS, fill = Tool)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10() +
  labs(title = "Execution Time Distribution (Log Scale)", y = "Time (s)", x = "Tool") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("consolidated_exec_time.png", plot = p1, width = 10, height = 6)

# RAM Usage Boxplot
p2 <- ggplot(data, aes(x = Tool, y = MaxRAMKB/1024, fill = Tool)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10() +
  labs(title = "RAM Usage Distribution (Log Scale)", y = "Memory (MB)", x = "Tool") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("consolidated_ram_usage.png", plot = p2, width = 10, height = 6)

# Nodes vs Time Scatter
p3 <- ggplot(data, aes(x = Nodes, y = ExecTimeS, color = Tool)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_y_log10() +
  scale_x_log10() +
  labs(title = "Nodes Detected vs Execution Time", x = "Nodes (log)", y = "Time (s, log)") +
  theme_minimal()
ggsave("consolidated_nodes_vs_time.png", plot = p3, width = 10, height = 6)

# CQS Bar Chart
p4 <- ggplot(summary_by_tool, aes(x = reorder(Tool, -mean_cqs), y = mean_cqs, fill = Tool)) +
  geom_bar(stat = "identity") +
  labs(title = "Average CFG Quality Score (CQS)", x = "Tool", y = "Score (0-1)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
ggsave("consolidated_cqs.png", plot = p4, width = 8, height = 6)

# Bubble Chart: Nodes vs Edges vs MaxDepth
p5 <- ggplot(summary_by_tool, aes(x = mean_nodes, y = mean_edges, size = mean_depth, color = Tool, label = Tool)) +
  geom_point(alpha = 0.7) +
  geom_text(vjust = -1.5, size = 3, show.legend = FALSE) +
  scale_size(range = c(5, 15), name = "Avg Max Depth") +
  labs(title = "Tool Complexity Comparison",
       subtitle = "Nodes vs Edges (Position) vs Max Depth (Size)",
       x = "Average Nodes Detected",
       y = "Average Edges Detected") +
  theme_minimal() +
  theme(legend.position = "right")
ggsave("consolidated_complexity_bubble.png", plot = p5, width = 10, height = 7)

# Stacked Bar Chart: Connected vs Orphan Nodes
# Pivot summary data to long format for stacking
orphan_data <- summary_by_tool %>%
  select(Tool, mean_connected_nodes, mean_orphan_nodes) %>%
  pivot_longer(cols = c(mean_connected_nodes, mean_orphan_nodes), 
               names_to = "NodeType", 
               values_to = "Count")

# Rename for legend
orphan_data$NodeType <- recode(orphan_data$NodeType, 
                               mean_connected_nodes = "Connected (Good)", 
                               mean_orphan_nodes = "Orphan (Bad)")

p6 <- ggplot(orphan_data, aes(x = reorder(Tool, -Count), y = Count, fill = NodeType)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Connected (Good)" = "#4CAF50", "Orphan (Bad)" = "#F44336")) +
  labs(title = "Node Connectivity Analysis",
       subtitle = "Total Nodes Breakdown: Connected vs Orphan (Lower Orphan ratio is better)",
       x = "Tool",
       y = "Average Node Count",
       fill = "Node Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("consolidated_orphan_nodes.png", plot = p6, width = 10, height = 6)

# --- Radar Chart (Tool Profiling) ---
# Select metrics for the pentagon (Nodes, Connected, Edges, Quality, Success)
radar_coords <- summary_by_tool %>%
  select(
    Tool,
    mean_nodes,
    mean_connected_nodes,
    mean_edges,
    mean_cqs,
    success_rate
  ) %>%
  as.data.frame()

# Normalize data (0-1 scale) for radar chart
# For radar chart in fmsb, we need max and min rows first
max_vals <- apply(radar_coords[,-1], 2, max, na.rm=TRUE)
min_vals <- apply(radar_coords[,-1], 2, min, na.rm=TRUE)

# Handle case where max == min ( avoid div/0 )
max_vals[max_vals == min_vals] <- max_vals[max_vals == min_vals] + 0.0001

# Scale data manually to ensure robust plotting
scaled_data <- as.data.frame(scale(radar_coords[,-1], center = min_vals, scale = max_vals - min_vals))
scaled_data$Tool <- radar_coords$Tool

# Setup grid for small multiples
png("consolidated_radar_chart.png", width = 1200, height = 1200, res = 150)
par(mfrow = c(3, 4), mar = c(1, 1, 2, 1)) 

# Iterate and plot for each tool
tools_list <- as.character(unique(scaled_data$Tool))

for (t in tools_list) {
  tool_row <- scaled_data[scaled_data$Tool == t, -6] # exclude Tool name column
  
  # Bind max (1), min (0), and actual data row
  plot_data <- rbind(rep(1, 5), rep(0, 5), tool_row)
  colnames(plot_data) <- c("Total Nodes", "Connected", "Edges", "Quality", "Success")
  
  radarchart(plot_data,
             axistype = 1,
             pcol = rgb(0.2, 0.6, 0.9, 0.9),
             pfcol = rgb(0.2, 0.6, 0.9, 0.4),
             plwd = 2,
             cglcol = "grey",
             cglty = 1,
             axislabcol = "grey",
             caxislabels = seq(0, 1, 0.25), 
             title = t,
             vlcex = 0.8
  )
}
dev.off()

cat("\n✅ Processing complete. Results saved to:\n")
cat("  - consolidated_tool_summary.csv\n")
cat("  - consolidated_exec_time.png\n")
cat("  - consolidated_ram_usage.png\n")
cat("  - consolidated_nodes_vs_time.png\n")
cat("  - consolidated_cqs.png\n")
cat("  - consolidated_complexity_bubble.png\n")
cat("  - consolidated_orphan_nodes.png\n")
cat("  - consolidated_radar_chart.png\n")

# --- 4. Anomaly Detection for 'Paper-algorithm' Tool ---

cat("\n🔍 Extracting anomalies for 'paper-algorithm' tool (Failures, Islands, or Orphans)...\n")

paper_anomalies <- data %>%
  filter(Tool == "paper") %>%
  filter(Status != "ok" | Islands > 0 | OrphanNodes > 0 | failed_to_parse == TRUE) %>%
  select(SampleID, Status, Islands, OrphanNodes, Nodes, Edges, Reachability, failed_to_parse)

if (nrow(paper_anomalies) > 0) {
  cat("⚠️ Found", nrow(paper_anomalies), "anomalies in 'paper-algorithm' results.\n")
  print(paper_anomalies)
  write.csv(paper_anomalies, "paper_anomalies.csv", row.names = FALSE)
  cat("📁 Details saved to: paper_anomalies.csv\n")
} else {
  cat("✅ No anomalies found for 'paper-algorithm' tool. Perfect integrity!\n")
}

# --- 4b. Orphan Nodes Specifically ---
cat("\n🕵️ Generating focused report for 'paper-algorithm' orphan nodes...\n")
paper_orphans <- get_paper_orphan_nodes(data)
if (nrow(paper_orphans) > 0) {
  cat("⚠️ Found", nrow(paper_orphans), "contracts with orphan nodes in 'paper-algorithm'.\n")
  write.csv(paper_orphans, "paper_orphans.csv", row.names = FALSE)
  cat("📁 Results saved to: paper_orphans.csv\n")
} else {
  cat("✅ No orphan nodes detected in 'paper-algorithm' results.\n")
}

# --- 5. Underperformance Analysis (Paper vs Competitors) ---

cat("\n🔍 Checking for cases where 'paper-algorithm' detects fewer Nodes/Edges than key competitors...\n")

# Select relevant columns and pivot
comparison_data <- data %>%
  filter(Tool %in% c("paper", "ethersolve", "bytespector", "evmole")) %>%
  select(SampleID, Tool, Nodes, Edges) %>%
  pivot_wider(names_from = Tool, values_from = c(Nodes, Edges))

# Function to check underperformance
check_underperformance <- function(df, competitor_tool) {
  paper_nodes_col <- "Nodes_paper"
  comp_nodes_col <- paste0("Nodes_", competitor_tool)
  
  if(!comp_nodes_col %in% names(df)) return(NULL)
  
  underperforming <- df %>%
    filter(.data[[paper_nodes_col]] < .data[[comp_nodes_col]]) %>%
    mutate(
      Competitor = competitor_tool,
      PaperNodes = .data[[paper_nodes_col]],
      CompetitorNodes = .data[[comp_nodes_col]],
      Diff = .data[[comp_nodes_col]] - .data[[paper_nodes_col]]
    ) %>%
    select(SampleID, Competitor, PaperNodes, CompetitorNodes, Diff)
  
  return(underperforming)
}

# Check against specific tools
competitors <- c("ethersolve", "evmole")
underperf_list <- list()

for (comp in competitors) {
  res <- check_underperformance(comparison_data, comp)
  if (!is.null(res) && nrow(res) > 0) {
    underperf_list[[comp]] <- res
  }
}

if (length(underperf_list) > 0) {
  all_underperf <- bind_rows(underperf_list)
  cat("⚠️ 'Paper-algorithm' detected fewer nodes in", nrow(all_underperf), "comparisons.\n")
  print(head(all_underperf))
  write.csv(all_underperf, "paper_underperformance.csv", row.names = FALSE)
  cat("📁 Details saved to: paper_underperformance.csv\n")
} else {
  cat("✅ 'Paper-algorithm' matched or outperformed all key competitors in Node count!\n")
}

