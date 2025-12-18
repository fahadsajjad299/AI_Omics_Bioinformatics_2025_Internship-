######################## Assignment No 3C ########################
######### AI and Biotechonlogy Bioinformatics 2025 ##########################

#following steps were followed 

#Download the Packages first

#setting the working directory
setwd("C:/Users/DELL/Documents/AI_Omics_Internship_2025/Parkinson_DEG_Project")
getwd()


#  step 1: creating folders
dir.create("Scripts", showWarnings = FALSE)
dir.create("RawData", showWarnings = FALSE)
dir.create("Results", showWarnings = FALSE)
dir.create("Plots", showWarnings = FALSE)


# Step 2: Load required packages
library(GEOquery)
library(limma)
library(AnnotationDbi)
library(hgu133a.db)
library(pheatmap)
library(ggplot2)
library(dplyr)

# Step 3: Data stes
gse <- getGEO("GSE8397", GSEMatrix = TRUE)
exprSet <- exprs(gse[[1]])
pdata <- pData(gse[[1]])

# Step 4: Define groups ----
group <- ifelse(grepl("Parkinson", pdata$characteristics_ch1), "Disease", "Control")
group <- factor(group, levels = c("Control", "Disease"))
design <- model.matrix(~ group)

#Step 4: Map probe IDs to gene symbols ----
  gene_symbols <- mapIds(hgu133a.db,
                         keys = rownames(exprSet),
                         column = "SYMBOL",
                         keytype = "PROBEID",
                         multiVals = "first")

exprSet <- as.data.frame(exprSet)
exprSet$GeneSymbol <- gene_symbols

# Remove probes without gene symbols
exprSet <- exprSet[!is.na(exprSet$GeneSymbol), ]

# Handle duplicates: keep average expression for duplicate genes
exprSet_avg <- exprSet %>%
  group_by(GeneSymbol) %>%
  summarise(across(where(is.numeric), mean))

# Set GeneSymbol as row names
exprSet_mat <- as.matrix(exprSet_avg[, -1])
rownames(exprSet_mat) <- exprSet_avg$GeneSymbol

# Step 5: Differential expression using limma ----
fit <- lmFit(exprSet_mat, design)
fit <- eBayes(fit)
deg_results <- topTable(fit, coef = 2, number = Inf, adjust.method = "fdr")

# Step 6: Save DEG results ----
write.csv(deg_results, "Results/DEG_results_complete.csv", row.names = TRUE)

# Separate upregulated and downregulated
upregulated <- deg_results %>% filter(logFC > 1 & adj.P.Val < 0.05)
downregulated <- deg_results %>% filter(logFC < -1 & adj.P.Val < 0.05)

write.csv(upregulated, "Results/DEG_upregulated.csv", row.names = TRUE)
write.csv(downregulated, "Results/DEG_downregulated.csv", row.names = TRUE)

# Step 7: Volcano plot ----
deg_results$Significance <- ifelse(deg_results$adj.P.Val < 0.05 & abs(deg_results$logFC) > 1,
                                   ifelse(deg_results$logFC > 1, "Upregulated", "Downregulated"), "Not Sig")

volcano <- ggplot(deg_results, aes(x = logFC, y = -log10(adj.P.Val), color = Significance)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c("blue", "grey", "red")) +
  theme_minimal() +
  labs(title = "Volcano Plot: Parkinson's vs Control",
       x = "Log2 Fold Change", y = "-Log10 Adjusted P-Value")

ggsave("Plots/Volcano_Plot.png", volcano, width = 6, height = 5)

# Step 8: Heatmap of top 25 DEGs ----
top25 <- deg_results %>% arrange(adj.P.Val) %>% head(25)
pheatmap(exprSet_mat[rownames(exprSet_mat) %in% rownames(top25), ],
         scale = "row",
         show_rownames = TRUE,
         show_colnames = FALSE,
         main = "Top 25 Differentially Expressed Genes",
         filename = "Plots/Heatmap_Top25.png")

# Step 9: Short summary ----
cat("
Result Summary:
- Multiple probes mapped to the same gene; duplicates were averaged by mean expression.
- Comparison: Parkinson’s disease vs Healthy controls.
- Total DEGs (adj.P.Val < 0.05 & |logFC| > 1):", nrow(upregulated) + nrow(downregulated), "
   • Upregulated:", nrow(upregulated), "
   • Downregulated:", nrow(downregulated), "
Results and plots saved in 'Results' and 'Plots' folders.
")



