# =============================================================================
# Teachers' Media Competence and Its Classroom Integration: A Transposition
# Gap Shared by Women and Men in Basic Education in the Dominican Republic
#
# Analysis script. Reproduces Tables I-IV, Fig. 1 and the statistics reported
# in the text of the article.
#
# Input : data/ami_alfamed_dr_teachers.csv  (see README.md for the codebook)
# Output: output/tables   Tables I-IV (.docx, IEEE format)
#         output/figures  Fig. 1 (.png and .tiff, 600 dpi)
#         output/results  unrounded results (.csv), reported statistics (.txt)
#                         and session information
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------
required <- c("ggplot2", "flextable", "officer", "car", "MASS", "psych", "GPArotation", "pwr")
missing  <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
suppressPackageStartupMessages({
  library(ggplot2); library(flextable); library(officer)
})

# Run from the directory that contains this script
script_dir <- function() {
  d <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "")
  if (!nzchar(d)) d <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) "")
  if (!nzchar(d)) {
    a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(a)) d <- dirname(normalizePath(sub("^--file=", "", a[1])))
  }
  if (!nzchar(d) || d == ".") d <- getwd()
  d
}
setwd(script_dir())

DATA_FILE <- file.path("data", "ami_alfamed_dr_teachers.csv")
OUT_TAB   <- file.path("output", "tables")
OUT_FIG   <- file.path("output", "figures")
OUT_RES   <- file.path("output", "results")
for (d in c(OUT_TAB, OUT_FIG, OUT_RES)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

options(contrasts = c("contr.sum", "contr.poly"))   # type III sums of squares
ALPHA <- 0.05


# -----------------------------------------------------------------------------
# 1. Data
# -----------------------------------------------------------------------------
raw <- read.csv(DATA_FILE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

LABELS <- list(
  age_group        = c("<25", "26–34", "35–44", "45–54", ">54"),
  experience       = c("<5", "5–15", "16–25", ">25"),
  degree           = c("Bachelor’s", "Postgraduate", "Other"),
  institution_type = c("Public", "Private", "Other"),
  mil_courses      = c("None", "1", "2", "3", ">3")
)
TITLES <- c(age_group = "Age group (years)", experience = "Teaching experience (years)",
            degree = "Highest degree", institution_type = "Type of institution",
            mil_courses = "MIL training courses")

raw$gender <- factor(raw$gender, levels = c("female", "male"), labels = c("Women", "Men"))

# Analytic sample: responses retained after the data-quality screening
dat <- raw[raw$excluded == 0, ]
gender <- dat$gender
n_w <- sum(gender == "Women"); n_m <- sum(gender == "Men"); N <- nrow(dat)

# Covariates for ANCOVA: ordinal (1, 2, 3, ...) and categorical versions
age <- dat$age_group
exp_years <- dat$experience
age_f <- factor(age)
exp_f <- factor(exp_years)


# -----------------------------------------------------------------------------
# 2. Instrument structure and scores
# -----------------------------------------------------------------------------
SUBDIMENSIONS <- data.frame(
  code  = c("B1-D1", "B1-D2", "B1-D3", "B1-D4", "B1-D5", "B2-D1", "B2-D2", "B3-D1", "B3-D2", "B3-D3"),
  name  = c("Access to information", "Language", "Technology", "Production processes",
            "Politics and media industry", "Ideology and values", "Reception and understanding",
            "Civic participation", "Creation", "Communication"),
  short = c("Access", "Language", "Technology", "Production", "Politics", "Ideology",
            "Reception", "Participation", "Creation", "Communication"),
  stringsAsFactors = FALSE)
SUBDIMENSIONS$prefix <- sub("-", "_", SUBDIMENSIONS$code)
BLOCKS <- c(B1 = "Knowledge (B1)", B2 = "Understanding (B2)", B3 = "Expression (B3)")

personal_items <- function(prefix) grep(paste0("^", prefix, "_P[0-9]+$"), names(dat), value = TRUE)
integration_item <- function(prefix) paste0(prefix, "_INT")

# Personal competence: mean of the personal items of each subdimension / block
PERSONAL <- sapply(SUBDIMENSIONS$prefix, function(p) rowMeans(dat[, personal_items(p)], na.rm = TRUE))
colnames(PERSONAL) <- SUBDIMENSIONS$code
BLOCK_SCORE <- sapply(names(BLOCKS), function(b)
  rowMeans(dat[, unlist(lapply(SUBDIMENSIONS$prefix[startsWith(SUBDIMENSIONS$code, b)], personal_items))], na.rm = TRUE))

# Classroom integration: final item of each subdimension
INTEGRATION <- sapply(SUBDIMENSIONS$prefix, function(p) dat[[integration_item(p)]])
colnames(INTEGRATION) <- SUBDIMENSIONS$code

# Gap = integration - personal competence (per participant and subdimension)
GAP <- INTEGRATION - PERSONAL


# -----------------------------------------------------------------------------
# 3. Helper functions
# -----------------------------------------------------------------------------
minus <- function(x) gsub("-", "−", x, fixed = TRUE)
f2    <- function(x, d = 2) minus(formatC(x, format = "f", digits = d))
fsign <- function(x, d = 2) minus(ifelse(x > 0, paste0("+", formatC(x, format = "f", digits = d)),
                                          formatC(x, format = "f", digits = d)))
fp    <- function(p) ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))
fpeq  <- function(p) ifelse(p < 0.001, "< 0.001", paste("=", formatC(p, format = "f", digits = 3)))
fci   <- function(lo, hi, d_lo = 2, d_hi = 2) paste0("[", f2(lo, d_lo), ", ", f2(hi, d_hi), "]")
msd   <- function(m, s) paste0(f2(m), " (", f2(s), ")")
stars <- function(p, dagger = TRUE) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*",
                                  ifelse(dagger & p < 0.10, "†", ""))))

pooled_sd <- function(a, b) sqrt(((length(a) - 1) * var(a) + (length(b) - 1) * var(b)) / (length(a) + length(b) - 2))

# Independent-samples comparison (Student's t, pooled variance).
# Differences and CIs are expressed as men - women; Cohen's d is positive when men score higher.
compare_gender <- function(y) {
  ok <- !is.na(y); w <- y[ok & gender == "Women"]; m <- y[ok & gender == "Men"]
  tt <- t.test(w, m, var.equal = TRUE)
  data.frame(n_w = length(w), n_m = length(m),
             mean_w = mean(w), sd_w = sd(w), mean_m = mean(m), sd_m = sd(m),
             t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
             d = (mean(m) - mean(w)) / pooled_sd(w, m),
             ci_low = -tt$conf.int[2], ci_high = -tt$conf.int[1],
             p_mann_whitney = suppressWarnings(wilcox.test(w, m)$p.value))
}

ieee_table <- function(df, number, title, note, body_size = 8) {
  ft <- flextable(df)
  ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- fontsize(ft, size = 8, part = "header")
  ft <- fontsize(ft, size = body_size, part = "body")
  ft <- border_remove(ft)
  ft <- hline_top(ft, border = fp_border(width = 1), part = "header")
  ft <- hline_bottom(ft, border = fp_border(width = 0.5), part = "header")
  ft <- hline_bottom(ft, border = fp_border(width = 1), part = "body")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- align(ft, j = 2:ncol(df), align = "center", part = "all")
  ft <- padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
  ft <- autofit(ft)
  ft <- add_footer_lines(ft, values = as_paragraph(as_i("Note. "), note))
  ft <- font(ft, fontname = "Times New Roman", part = "footer")
  ft <- fontsize(ft, size = 7, part = "footer")
  cap <- fp_text(font.family = "Times New Roman", font.size = 8)
  set_caption(ft, caption = as_paragraph(as_chunk(paste0("TABLE ", number, "\n", title), props = cap)),
              align_with_table = FALSE, fp_p = fp_par(text.align = "center"))
}
save_table <- function(ft, file) {
  doc <- body_add_flextable(read_docx(), ft)
  print(doc, target = file.path(OUT_TAB, file))
}

REPORT <- character()
report <- function(...) REPORT <<- c(REPORT, sprintf(...))
section <- function(title) REPORT <<- c(REPORT, "", paste0("== ", title, " =="))


# -----------------------------------------------------------------------------
# 4. Sample (Table I)
# -----------------------------------------------------------------------------
section("Sample")
excl_tab <- table(raw$excluded, raw$gender)
excl_chi <- suppressWarnings(chisq.test(excl_tab, correct = FALSE))
report("Initial respondents = %d; excluded = %d; analytic sample N = %d (women = %d, %.1f%%; men = %d)",
       nrow(raw), sum(raw$excluded), N, n_w, 100 * n_w / N, n_m)
reasons <- table(raw$exclusion_reason[raw$excluded == 1])
report("Exclusions by reason: %s", paste(names(reasons), reasons, sep = " = ", collapse = "; "))
report("Excluded vs. retained, gender distribution: chi2(%d) = %.2f, p %s",
       excl_chi$parameter, excl_chi$statistic, fpeq(excl_chi$p.value))

cramer_v <- function(tab, chi) sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
rows <- list(); t1_values <- list()
for (v in names(LABELS)) {
  x <- factor(dat[[v]], levels = seq_along(LABELS[[v]]) - (v == "mil_courses"), labels = LABELS[[v]])
  tab <- table(x, gender)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  V <- cramer_v(tab, chi)
  t1_values[[v]] <- data.frame(variable = v, chi2 = unname(chi$statistic), df = unname(chi$parameter),
                               p = chi$p.value, cramer_v = V)
  rows[[length(rows) + 1]] <- data.frame(
    Variable = sprintf("%s; χ²(%d) = %s, p %s, V = %s", TITLES[[v]], unname(chi$parameter),
                       f2(unname(chi$statistic)), fpeq(chi$p.value), formatC(V, format = "f", digits = 3)),
    W = "", M = "", T = "")
  rows[[length(rows) + 1]] <- data.frame(
    Variable = paste0("   ", LABELS[[v]]),
    W = sprintf("%d (%.1f%%)", tab[, 1], 100 * tab[, 1] / n_w),
    M = sprintf("%d (%.1f%%)", tab[, 2], 100 * tab[, 2] / n_m),
    T = sprintf("%d (%.1f%%)", rowSums(tab), 100 * rowSums(tab) / N))
}
table1 <- do.call(rbind, rows)
names(table1) <- c("Variable", sprintf("Women (n = %d)", n_w), sprintf("Men (n = %d)", n_m), sprintf("Total (N = %d)", N))
write.csv(do.call(rbind, t1_values), file.path(OUT_RES, "table1_chisquare.csv"), row.names = FALSE)

hdr <- which(table1[[2]] == "")
ft1 <- ieee_table(table1, "I", "Sample Characteristics by Gender",
  "MIL = media and information literacy; V = Cramér’s V.")
ft1 <- merge_h_range(italic(ft1, i = hdr, j = 1), i = hdr, j1 = 1, j2 = 4)
save_table(ft1, "table1.docx")

report("University teachers = %.1f%%", 100 * mean(dat$university_teacher))
devices <- grep("^(home|work)_", names(dat), value = TRUE)
for (v in devices) {
  tab <- table(factor(dat[[v]], levels = 0:1), gender)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  report("Device %-16s total = %5.1f%%; women = %5.1f%%; men = %5.1f%%; chi2(1) = %5.2f, p %s, V = %.3f",
         v, 100 * mean(dat[[v]]), 100 * mean(dat[[v]][gender == "Women"]), 100 * mean(dat[[v]][gender == "Men"]),
         chi$statistic, fpeq(chi$p.value), cramer_v(tab, chi))
}


# -----------------------------------------------------------------------------
# 5. Reliability (all items of each block, listwise deletion)
# -----------------------------------------------------------------------------
section("Reliability")
for (b in names(BLOCKS)) {
  items <- unlist(lapply(SUBDIMENSIONS$prefix[startsWith(SUBDIMENSIONS$code, b)],
                         function(p) c(personal_items(p), integration_item(p))))
  x <- na.omit(dat[, items])
  a <- suppressWarnings(psych::alpha(x, warnings = FALSE)$total$raw_alpha)
  w <- suppressWarnings(suppressMessages(psych::omega(x, nfactors = 1, plot = FALSE)$omega.tot))
  report("%s (%d items, n = %d): alpha = %.3f, omega = %.3f", BLOCKS[[b]], length(items), nrow(x), a, w)
}


# -----------------------------------------------------------------------------
# 6. RQ1 - Personal media competence by gender (Table II)
# -----------------------------------------------------------------------------
section("RQ1: personal media competence")
res_sub   <- do.call(rbind, lapply(SUBDIMENSIONS$code, function(k) compare_gender(PERSONAL[, k])))
res_block <- do.call(rbind, lapply(names(BLOCKS), function(b) compare_gender(BLOCK_SCORE[, b])))
res_sub$p_bh   <- p.adjust(res_sub$p, "BH")     # k = 10
res_block$p_bh <- p.adjust(res_block$p, "BH")   # k = 3
res_sub$label   <- paste0("   ", sub(".*-", "", SUBDIMENSIONS$code), " ", SUBDIMENSIONS$name)
res_block$label <- BLOCKS
res_sub$block   <- substr(SUBDIMENSIONS$code, 1, 2)
res_block$block <- names(BLOCKS)
res_block$order <- 0; res_sub$order <- seq_len(nrow(res_sub))
table2_values <- rbind(res_block, res_sub)
table2_values <- table2_values[order(table2_values$block, table2_values$order), ]
write.csv(table2_values, file.path(OUT_RES, "table2_personal_competence.csv"), row.names = FALSE)

lo_digits <- ifelse(abs(table2_values$ci_low) < 0.005, 3, 2)
table2 <- data.frame(
  `Block / subdimension` = table2_values$label,
  `Women M (SD)` = msd(table2_values$mean_w, table2_values$sd_w),
  `Men M (SD)`   = msd(table2_values$mean_m, table2_values$sd_m),
  t  = f2(table2_values$t),
  p  = paste0(fp(table2_values$p), sub("\\*+", "*", stars(table2_values$p))),
  d  = f2(table2_values$d),
  `95% CI` = mapply(function(a, b, k) fci(a, b, d_lo = k), table2_values$ci_low, table2_values$ci_high, lo_digits),
  check.names = FALSE)
names(table2)[4] <- sprintf("t(%d)", N - 2)
save_table(ieee_table(table2, "II",
  sprintf("Gender Differences in Personal Media Competence by Block and Subdimension (N = %d)", N),
  "M = mean of all non-integration items in each block or subdimension (1–4 scale). d = Cohen’s d (positive values indicate higher scores among men). CI = confidence interval for the difference (men − women). BH-FDR correction was applied separately to subdimensions (k = 10) and blocks (k = 3). †p < 0.10; *p < 0.05 (uncorrected)."),
  "table2.docx")

report("Smallest BH-adjusted p: subdimensions = %s; blocks = %s", fp(min(res_sub$p_bh)), fp(min(res_block$p_bh)))
report("Mann-Whitney U, p < 0.05 (uncorrected): %s",
       paste(c(SUBDIMENSIONS$code[res_sub$p_mann_whitney < ALPHA], names(BLOCKS)[res_block$p_mann_whitney < ALPHA]),
             collapse = ", "))

# Statistical power for the observed group sizes (two-tailed, alpha = 0.05)
pw_med <- pwr::pwr.t2n.test(n1 = n_w, n2 = n_m, d = 0.5, sig.level = ALPHA)$power
pw_d80 <- pwr::pwr.t2n.test(n1 = n_w, n2 = n_m, power = 0.80, sig.level = ALPHA)$d
report("Power: d = 0.50 -> %.3f; minimum detectable d with 80%% power = %.2f", pw_med, pw_d80)

# ANCOVA by block (covariates: age group and teaching experience)
eta_p <- function(a, term) a[term, "Sum Sq"] / (a[term, "Sum Sq"] + a["Residuals", "Sum Sq"])
for (b in names(BLOCKS)) {
  y <- BLOCK_SCORE[, b]
  slopes <- car::Anova(lm(y ~ gender * age + gender * exp_years), type = 3)
  fit    <- lm(y ~ gender + age + exp_years)
  anc    <- car::Anova(fit, type = 3)
  anc_f  <- car::Anova(lm(y ~ gender + age_f + exp_f), type = 3)
  lev    <- car::leveneTest(residuals(fit) ~ gender, center = mean)
  sw     <- shapiro.test(residuals(fit))
  report("%s | slopes: gender x age F(1, %d) = %.2f, p %s, etap2 = %.3f; gender x experience F(1, %d) = %.2f, p %s, etap2 = %.3f",
         BLOCKS[[b]], slopes["Residuals", "Df"], slopes["gender:age", "F value"], fpeq(slopes["gender:age", "Pr(>F)"]),
         eta_p(slopes, "gender:age"), slopes["Residuals", "Df"], slopes["gender:exp_years", "F value"],
         fpeq(slopes["gender:exp_years", "Pr(>F)"]), eta_p(slopes, "gender:exp_years"))
  report("%s | ANCOVA gender F(1, %d) = %.2f, p %s, etap2 = %.3f; age p %s; experience p %s; Levene p %s; Shapiro-Wilk residuals p %s",
         BLOCKS[[b]], anc["Residuals", "Df"], anc["gender", "F value"], fpeq(anc["gender", "Pr(>F)"]), eta_p(anc, "gender"),
         fpeq(anc["age", "Pr(>F)"]), fpeq(anc["exp_years", "Pr(>F)"]), fpeq(lev[1, "Pr(>F)"]), fpeq(sw$p.value))
  report("%s | ANCOVA with categorical covariates: gender F(%d, %d) = %.2f, p %s",
         BLOCKS[[b]], anc_f["gender", "Df"], anc_f["Residuals", "Df"], anc_f["gender", "F value"], fpeq(anc_f["gender", "Pr(>F)"]))
}


# -----------------------------------------------------------------------------
# 7. RQ2 - Classroom integration by gender (Table III)
# -----------------------------------------------------------------------------
section("RQ2: classroom integration")
table3_values <- cbind(code = SUBDIMENSIONS$code,
                       do.call(rbind, lapply(SUBDIMENSIONS$code, function(k) compare_gender(INTEGRATION[, k]))))
table3_values$p_bh <- p.adjust(table3_values$p, "BH")
write.csv(table3_values, file.path(OUT_RES, "table3_classroom_integration.csv"), row.names = FALSE)

n_missing <- colSums(is.na(INTEGRATION))
listwise  <- which(n_missing > 0)
mark <- ifelse(n_missing > 0, "ᵃ", "")
table3 <- data.frame(
  Subdimension   = paste0(SUBDIMENSIONS$code, " ", SUBDIMENSIONS$name, mark),
  `Women M (SD)` = msd(table3_values$mean_w, table3_values$sd_w),
  `Men M (SD)`   = msd(table3_values$mean_m, table3_values$sd_m),
  t = f2(table3_values$t),
  p = paste0(fp(table3_values$p), sub("\\*+", "*", stars(table3_values$p, dagger = FALSE))),
  d = f2(table3_values$d),
  `95% CI` = fci(table3_values$ci_low, table3_values$ci_high),
  check.names = FALSE)
save_table(ieee_table(table3, "III", "Gender Differences in the Classroom Integration Item by Subdimension",
  paste0("M = mean of the final (integration) item of each subdimension (1–4 scale). df = ", N - 2,
         ", except ᵃlistwise deletion of missing integration responses: ",
         paste(sprintf("%s (n = %d/%d; df = %d)", SUBDIMENSIONS$code[listwise], table3_values$n_w[listwise],
                       table3_values$n_m[listwise], round(table3_values$df[listwise])), collapse = ", "),
         ". BH-FDR correction applied across the ten integration comparisons. *p < 0.05 (uncorrected).")),
  "table3.docx")

report("Missing integration responses: %s",
       paste(sprintf("%s n = %d (%.1f%%)", SUBDIMENSIONS$code[listwise], n_missing[listwise],
                     100 * n_missing[listwise] / N), collapse = "; "))
report("Smallest BH-adjusted p = %s", fp(min(table3_values$p_bh)))
report("Largest upper CI bound (men - women): personal competence = %.3f; classroom integration = %.3f",
       max(res_sub$ci_high, res_block$ci_high), max(table3_values$ci_high))

# Ordinal logistic regression for B3-D3 (gender, age group, teaching experience)
ord <- na.omit(data.frame(y = factor(INTEGRATION[, "B3-D3"], ordered = TRUE),
                          male = as.numeric(gender == "Men"), age = age, experience = exp_years))
m_full <- MASS::polr(y ~ male + age + experience, data = ord, Hess = TRUE)
m_null <- MASS::polr(y ~ 1, data = ord, Hess = TRUE)
report("Ordinal regression B3-D3: N = %d, McFadden R2 = %.3f", nrow(ord),
       1 - as.numeric(logLik(m_full)) / as.numeric(logLik(m_null)))
for (v in c("male", "age", "experience")) {
  lr <- as.numeric(2 * (logLik(m_full) - logLik(update(m_full, as.formula(paste(". ~ . -", v))))))
  ci <- exp(suppressMessages(confint(m_full, v)))
  report("   %-10s chi2(1) = %.2f, p %s, OR = %.2f, 95%% CI [%.2f, %.2f]",
         v, lr, fpeq(pchisq(lr, 1, lower.tail = FALSE)), exp(coef(m_full)[v]), ci[1], ci[2])
}


# -----------------------------------------------------------------------------
# 8. RQ3 - Gap between classroom integration and personal competence
#    (Table IV and Fig. 1)
# -----------------------------------------------------------------------------
section("RQ3: integration - personal competence gap")
table4_values <- do.call(rbind, lapply(SUBDIMENSIONS$code, function(k) {
  y <- GAP[, k]; ok <- !is.na(y)
  w <- y[ok & gender == "Women"]; m <- y[ok & gender == "Men"]
  tt <- t.test(w, m, var.equal = TRUE)                     # delta gap = women - men
  data.frame(code = k, gap_w = mean(w), gap_m = mean(m),
             p_zero_w = t.test(w)$p.value, p_zero_m = t.test(m)$p.value,
             delta_gap = mean(w) - mean(m), ci_low = tt$conf.int[1], ci_high = tt$conf.int[2],
             t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
             d = (mean(w) - mean(m)) / pooled_sd(w, m),
             p_mann_whitney = suppressWarnings(wilcox.test(w, m)$p.value),
             p_ancova = car::Anova(lm(y ~ gender + age + exp_years), type = 3)["gender", "Pr(>F)"],
             p_ancova_categorical = car::Anova(lm(y ~ gender + age_f + exp_f), type = 3)["gender", "Pr(>F)"])
}))
table4_values$p_bh <- p.adjust(table4_values$p, "BH")
write.csv(table4_values, file.path(OUT_RES, "table4_gap.csv"), row.names = FALSE)

table4 <- data.frame(
  Subdimension = paste0(SUBDIMENSIONS$code, " ", SUBDIMENSIONS$short, mark),
  `Gap, women` = paste0(fsign(table4_values$gap_w), stars(table4_values$p_zero_w)),
  `Gap, men`   = paste0(fsign(table4_values$gap_m), stars(table4_values$p_zero_m)),
  delta = fsign(table4_values$delta_gap),
  `95% CI` = fci(table4_values$ci_low, table4_values$ci_high),
  t = f2(table4_values$t), p = fp(table4_values$p), d = f2(table4_values$d),
  check.names = FALSE)
names(table4)[4] <- "ΔGap"
save_table(ieee_table(table4, "IV", "Gap Between Classroom Integration and Personal Competence by Gender",
  paste0("Gap = M(Integration) − M(Personal), computed for each participant. ΔGap = women’s mean gap − men’s mean gap. Symbols in the gap columns indicate that the gap differs from zero within each gender (one-sample t test): †p < 0.10; *p < 0.05; **p < 0.01; ***p < 0.001. d = Cohen’s d for ΔGap. df = ",
         N - 2, ", except ᵃlistwise deletion: ",
         paste(sprintf("%s (df = %d)", SUBDIMENSIONS$code[listwise], round(table4_values$df[listwise])), collapse = ", "), "."),
  body_size = 7), "table4.docx")

report("Delta gap > 0 in %d of 10 subdimensions", sum(table4_values$delta_gap > 0))
report("Gap different from zero (p < 0.05): women = %d, men = %d",
       sum(table4_values$p_zero_w < ALPHA), sum(table4_values$p_zero_m < ALPHA))
report("Delta gap: p range = %s-%s; |d| max = %.2f; smallest BH-adjusted p = %s",
       fp(min(table4_values$p)), fp(max(table4_values$p)), max(abs(table4_values$d)), fp(min(table4_values$p_bh)))
report("Robustness: smallest Mann-Whitney p = %s; smallest ANCOVA p = %s (ordinal covariates), %s (categorical covariates)",
       fp(min(table4_values$p_mann_whitney)), fp(min(table4_values$p_ancova)), fp(min(table4_values$p_ancova_categorical)))

# Moderated regression: integration ~ personal competence (mean-centered) x gender
moderation <- do.call(rbind, lapply(SUBDIMENSIONS$code, function(k) {
  pc  <- PERSONAL[, k] - mean(PERSONAL[, k])
  fit <- lm(INTEGRATION[, k] ~ pc * gender)
  a   <- car::Anova(fit, type = 3)
  data.frame(code = k, n = nobs(fit), r_personal_integration = cor(PERSONAL[, k], INTEGRATION[, k], use = "complete.obs"),
             F_interaction = a["pc:gender", "F value"], df_residual = a["Residuals", "Df"],
             p_interaction = a["pc:gender", "Pr(>F)"])
}))
moderation$p_bh <- p.adjust(moderation$p_interaction, "BH")
write.csv(moderation, file.path(OUT_RES, "moderation_personal_x_gender.csv"), row.names = FALSE)
report("Moderation: r(personal, integration) = %.2f-%.2f; personal x gender interaction p = %s-%s (smallest: %s); smallest BH-adjusted p = %s",
       min(moderation$r_personal_integration), max(moderation$r_personal_integration),
       fp(min(moderation$p_interaction)), fp(max(moderation$p_interaction)),
       moderation$code[which.min(moderation$p_interaction)], fp(min(moderation$p_bh)))

# Fig. 1
axis_labels <- c("Access to info.", "Language", "Technology", "Production\nprocesses",
                 "Politics and media\nindustry", "Ideology and\nvalues", "Reception and\nunderst.",
                 "Civic participation", "Creation", "Communication")
axis_labels <- ifelse(grepl("\n", axis_labels),
                      paste0(axis_labels, " (", SUBDIMENSIONS$code, ")"),
                      paste0(axis_labels, "\n(", SUBDIMENSIONS$code, ")"))
groups <- c(sprintf("Women (n = %d)", n_w), sprintf("Men (n = %d)", n_m))
fig_data <- rbind(data.frame(sub = axis_labels, gap = table4_values$gap_w, group = groups[1]),
                  data.frame(sub = axis_labels, gap = table4_values$gap_m, group = groups[2]))
fig_data$sub   <- factor(fig_data$sub, levels = rev(axis_labels))
fig_data$group <- factor(fig_data$group, levels = groups)
fig_data$dodge <- factor(fig_data$group, levels = rev(groups))

fig1 <- ggplot(fig_data, aes(x = gap, y = sub, fill = group, group = dodge)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.76, colour = "black", linewidth = 0.2) +
  geom_text(aes(label = fsign(gap), hjust = ifelse(gap >= 0, -0.15, 1.15)),
            position = position_dodge(width = 0.8), size = 6 / .pt, family = "serif") +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = setNames(c("#404040", "#c8c8c8"), groups)) +
  scale_x_continuous(limits = c(-0.82, 0.32), breaks = c(-0.75, -0.5, -0.25, 0, 0.25),
                     labels = function(x) minus(formatC(x, format = "f", digits = 2))) +
  labs(x = "Mean gap (integration − personal)", y = NULL, fill = NULL) +
  theme_classic(base_size = 7, base_family = "serif") +
  theme(legend.position = "top", legend.key.size = unit(7, "pt"),
        panel.grid.major.x = element_line(linetype = "dotted", linewidth = 0.2, colour = "grey60"),
        axis.text = element_text(colour = "black"))

ggsave(file.path(OUT_FIG, "fig1.png"),  fig1, width = 3.5, height = 4.0, units = "in", dpi = 600)
ggsave(file.path(OUT_FIG, "fig1.tiff"), fig1, width = 3.5, height = 4.0, units = "in", dpi = 600, compression = "lzw")


# -----------------------------------------------------------------------------
# 9. Reported statistics and session information
# -----------------------------------------------------------------------------
writeLines(enc2utf8(REPORT[-1]), file.path(OUT_RES, "reported_statistics.txt"), useBytes = TRUE)
writeLines(capture.output(sessionInfo()), file.path(OUT_RES, "session_info.txt"))
message("Done. Outputs written to ", normalizePath("output"))
