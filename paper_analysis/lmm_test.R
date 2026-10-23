#' Run check_model() diagnostics across parameters
#'
#' Fits the same full model used by [lmm_eval()] for each parameter
#' (Gamma GLMM for parameters in `gamma_params`, log-normal LMM otherwise) and
#' returns `check_model()` diagnostic plots for each, without computing contrasts.
#' Intended as a pre-check step before running [lmm_eval()] on the same
#' df/formula/params, to confirm model fit is reasonable before interpreting results.
#'
#' @param df Data frame in long format with columns: param, value, and
#'   whatever grouping/fixed-effect variables are in the formula
#' @param params Character vector of parameter names to loop over (values of `param` column)
#' @param full_formula_rhs Right-hand side of the full model formula, as a string,
#'   e.g. "burn_group + (1 | site_code)"
#' @param gamma_params Character vector of parameter names that should use a
#'   Gamma(link = "log") GLMM instead of a log-normal LMM (e.g. "NO3")
#'
#' @return A named list of check_model() diagnostic outputs, one per parameter in params
check_lmm_models <- function(df, params, full_formula_rhs, gamma_params = "NO3") {
  
  map(params, function(param_name) {
    
    param_df <- df %>% filter(param == param_name & value > 0)
    
    is_gamma <- param_name %in% gamma_params
    
    if (is_gamma) {
      full_fmla <- as.formula(paste("value ~", full_formula_rhs))
      model_full <- glmmTMB(full_fmla, data = param_df, family = Gamma(link = "log"))
    } else {
      param_df <- param_df %>% mutate(log_value = log(value))
      full_fmla <- as.formula(paste("log_value ~", full_formula_rhs))
      model_full <- lmer(full_fmla, data = param_df, REML = FALSE)
    }
    
    diag_plot <- plot(check_model(model_full))
    diag_plot + patchwork::plot_annotation(
      title = param_name,
      theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
    )    
  }) %>% set_names(params)
}

#' Distribution-free 95% CI for a median
#'
#' Order-statistic (binomial) confidence interval for the sample median.
#' Computed on the raw (untransformed) data and ignores any model structure.
#'
#' @param x Numeric vector.
#' @param conf Confidence level. Default 0.95.
#'
#' @return Named numeric vector with `lower` and `upper`. Both are `NA` when n is
#'   too small to form an interval at the requested confidence level.
median_ci <- function(x, conf = 0.95) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  r <- qbinom((1 - conf) / 2, n, 0.5)
  if (r < 1) return(c(lower = NA_real_, upper = NA_real_))  # n too small for a 95% CI
  c(lower = x[r], upper = x[n - r + 1])
}

#' Back-transform model-estimated marginal means to the original data scale
#'
#' Extracts estimated marginal means and 95% CIs from an `emmeans` object on the
#' model's link/log scale and exponentiates them. Both model types used in this
#' file are fit on a log scale, so `exp()` is the correct inverse in each case:
#' \itemize{
#'   \item **Log-normal LMM** (`log_value ~ ...`): `exp(emmean)` is the
#'     geometric mean on the original scale, which estimates the median
#'     under log-normality. No bias correction (sigma^2 / 2) is applied, so it is
#'     not an estimate of the arithmetic mean.
#'   \item **Gamma GLMM, log link** (`value ~ ...`): `exp(emmean)` is the
#'     estimated marginal mean on the response scale (a mean, not a median).
#' }
#' The log scale is requested explicitly (`type = "link"`) and the transformation
#' is applied manually, because `log_value` is created with `mutate()` and
#' `emmeans` cannot detect it. CI limits are exponentiated rather than
#' recomputed, so they are asymmetric on the original scale. Ratios of these
#' estimates equal `exp(contrast)`, so they agree with the reported percent
#' differences.
#'
#' @param group_means An `emmGrid` object from `emmeans()`.
#'
#' @return A data frame with the grouping columns of `group_means` plus `emm`,
#'   `emm_low` and `emm_high` (estimate and 95% CI on the original data scale).
emm_to_original <- function(group_means) {
  as.data.frame(summary(group_means, infer = c(TRUE, FALSE), type = "link")) %>%
    # glmmTMB (asymptotic) output names the limits asymp.LCL / asymp.UCL
    rename(any_of(c(lower.CL = "asymp.LCL", upper.CL = "asymp.UCL"))) %>%
    mutate(emm      = exp(emmean),
           emm_low  = exp(lower.CL),
           emm_high = exp(upper.CL)) %>%
    select(-any_of(c("emmean", "SE", "df", "lower.CL", "upper.CL")))
}

#' Evaluate mixed model contrasts across parameters
#'
#' Fits a full and null mixed model for each parameter in `params`, then
#' computes pairwise contrasts of the levels of `group_var`. Parameters are
#' log-transformed and fit with `lmer()` (log-normal LMM); parameters listed in
#' `gamma_params` are fit with `glmmTMB()` using a Gamma family and log link.
#' A likelihood ratio test (full vs. null model) is returned for each parameter.
#'
#' For each group, two sets of summaries are returned:
#' \itemize{
#'   \item **Raw (descriptive)**: median and distribution-free 95% CI
#'     ([median_ci()]) of the observed data, ignoring random effects and covariates.
#'   \item **Model-estimated**: estimated marginal mean and 95% CI from the full
#'     model, back-transformed to the original scale with [emm_to_original()]
#'     (`exp()` of the log-scale estimate). For LMMs this is the geometric mean
#'     (estimated median under log-normality); for Gamma GLMMs it is the
#'     estimated mean. Estimates are averaged over the other fixed effects in the
#'     model (equal weights) and use random effects at zero (population level).
#' }
#' Percent differences are `(exp(contrast) - 1) * 100`, i.e. the ratio of the
#' model-estimated values minus one, so they are consistent with `emm`, not with
#' the ratio of raw medians.
#'
#' @param df Data frame in long format with columns: param, value, and
#'   whatever grouping/fixed-effect variables are in the formula
#' @param params Character vector of parameter names to loop over (values of `param` column)
#' @param group_var Name (string) of the variable to compute contrasts over (e.g. "site_code", "Year")
#' @param full_formula_rhs Right-hand side of the full model formula, as a string,
#'   e.g. "site_code + season + (1 | Year)"
#' @param null_formula_rhs Right-hand side of the null model formula, as a string,
#'   e.g. "season + (1 | Year)"  (i.e. full formula minus group_var)
#' @param gamma_params Character vector of parameter names that should use a
#'   Gamma(link = "log") GLMM instead of a log-normal LMM (e.g. "NO3")
#'
#' @return A list with two data frames:
#' \describe{
#'   \item{contrasts}{One row per parameter x pairwise contrast, with
#'     `median_*`, `ci_low_*`, `ci_high_*` (raw) and `emm_*`, `emm_low_*`,
#'     `emm_high_*` (model-estimated) for the first (`_1`) and second (`_2`)
#'     group; `pct_est`, `pct_low`, `pct_high` (percent difference and 95% CI from
#'     the model); contrast p-value (`p_value_contrast`) and likelihood ratio test
#'     p-value (`p_value_anova`) with significance flags.}
#'   \item{medians}{One row per parameter x group, with `n`; raw `median`,
#'     `ci_low`, `ci_high`; and model-estimated `emm`, `emm_low`, `emm_high`.
#'     All values are on the original data scale and rounded to 3 decimals.}
#' }
lmm_eval <- function(df, params, group_var, 
                     full_formula_rhs, null_formula_rhs,
                     gamma_params = "NO3") {
  
  results_list <- map(params, function(param_name) {
    
    param_df <- df %>% filter(param == param_name & value > 0)
    
    is_gamma <- param_name %in% gamma_params
    
    if (is_gamma) {
      full_fmla <- as.formula(paste("value ~", full_formula_rhs))
      null_fmla <- as.formula(paste("value ~", null_formula_rhs))
      
      model_full <- glmmTMB(full_fmla, data = param_df, family = Gamma(link = "log"))
      model_null <- glmmTMB(null_fmla, data = param_df, family = Gamma(link = "log"))
      
      group_means <- emmeans(model_full, group_var)
      contrasts <- summary(pairs(group_means), infer = TRUE) %>%
        as.data.frame() %>%
        rename(p_value_contrast = p.value,
               lower.CL = asymp.LCL,
               upper.CL = asymp.UCL)
      
    } else {
      param_df <- param_df %>% mutate(log_value = log(value))
      full_fmla <- as.formula(paste("log_value ~", full_formula_rhs))
      null_fmla <- as.formula(paste("log_value ~", null_formula_rhs))
      
      model_full <- lmer(full_fmla, data = param_df, REML = FALSE)
      model_null <- lmer(null_fmla, data = param_df, REML = FALSE)
      
      group_means <- emmeans(model_full, group_var)
      contrasts <- summary(pairs(group_means), infer = TRUE) %>%
        as.data.frame() %>%
        rename(p_value_contrast = p.value)
    }
    
    anova_res <- anova(model_null, model_full)
    p_anova <- anova_res$`Pr(>Chisq)`[2]
    
    contrasts <- contrasts %>%
      mutate(p_value_anova = p_anova,
             parameter = param_name)
    
    # model-estimated group values, back-transformed (exp) to the original scale
    estimates <- emm_to_original(group_means) %>%
      rename(group = all_of(group_var))
    
    medians <- param_df %>%
      group_by(.data[[group_var]]) %>%
      summarize(n = n(),
                median = median(value, na.rm = TRUE),
                ci_low = median_ci(value)[["lower"]],
                ci_high = median_ci(value)[["upper"]],
                .groups = "drop") %>%
      rename(group = all_of(group_var)) %>%
      left_join(estimates, by = "group") %>%
      mutate(parameter = param_name,
             across(c(median, ci_low, ci_high, emm, emm_low, emm_high), ~ round(.x, 3)))
    
    list(contrasts = contrasts, medians = medians)
  }) %>% set_names(params)
  
  contrasts_all <- map_dfr(results_list, "contrasts")
  medians_all   <- map_dfr(results_list, "medians")
  
  # Join contrast's two group-level summaries by parsing "contrast" (e.g. "grp1 - grp2")
  contrasts_with_means <- contrasts_all %>%
    mutate(
      pct_est  = round((exp(estimate) - 1) * 100, 2),
      pct_low  = round((exp(lower.CL) - 1) * 100, 2),
      pct_high = round((exp(upper.CL) - 1) * 100, 2),
      p_sig_anova    = ifelse(p_value_anova < 0.05, "*", ""),
      #if p_val is < 0.0001, display as "0.0001"; otherwise round to 4 decimal places
      p_value_anova = ifelse(p_value_anova < 0.0001, 0.0001, round(p_value_anova, 4)), 
      p_value_contrast = ifelse(p_value_contrast < 0.0001, 0.0001, round(p_value_contrast, 4)),
      p_sig_contrast = ifelse(p_value_contrast < 0.05, "*", ""),
      group_1 = str_trim(str_split_fixed(contrast, " - ", 2)[, 1]),
      group_2 = str_trim(str_split_fixed(contrast, " - ", 2)[, 2])
    ) %>%
    mutate(group_1 = gsub("Year", "", group_1), 
           group_2 = gsub("Year", "", group_2))%>%
    left_join(medians_all %>% select(parameter, group,
                                     median_1 = median, ci_low_1 = ci_low, ci_high_1 = ci_high,
                                     emm_1 = emm, emm_low_1 = emm_low, emm_high_1 = emm_high),
              by = c("parameter", "group_1" = "group")) %>%
    left_join(medians_all %>% select(parameter, group,
                                     median_2 = median, ci_low_2 = ci_low, ci_high_2 = ci_high,
                                     emm_2 = emm, emm_low_2 = emm_low, emm_high_2 = emm_high),
              by = c("parameter", "group_2" = "group")) %>%
    select(parameter, contrast,
           median_1, ci_low_1, ci_high_1, emm_1, emm_low_1, emm_high_1,
           median_2, ci_low_2, ci_high_2, emm_2, emm_low_2, emm_high_2,
           pct_est, pct_low, pct_high,
           p_value_contrast, p_sig_contrast, p_value_anova, p_sig_anova)
  
  list(contrasts = contrasts_with_means, medians = medians_all)
}


#' Flip the direction of pairwise contrasts (e.g. "A - B" -> "B - A")
#'
#' Allows for more natural comparisons (e.g. an 80% decrease from A - B rather
#' than a 400% increase from B - A). The contrast label is reversed, the group 1
#' and group 2 summaries (raw median/CI and model estimate/CI) are swapped, and
#' the percent difference and its CI are inverted via the ratio:
#' `pct_flipped = (1 / (1 + pct / 100) - 1) * 100`, with the lower and upper
#' limits exchanged. P-values are unchanged. All other columns are preserved.
#'
#' @param contrasts_df The `contrasts` element returned by [lmm_eval()] or
#'   [lmm_eval_group()].
#'
#' @return A data frame with the same columns as `contrasts_df`.
flip_contrasts <- function(contrasts_df) {
  out <- contrasts_df
  
  # swap every *_1 / *_2 pair that exists (median, raw CI, model estimate and CI)
  stems <- intersect(c("median", "ci_low", "ci_high", "emm", "emm_low", "emm_high"),
                     sub("_1$", "", grep("_1$", names(contrasts_df), value = TRUE)))
  for (s in stems) {
    a <- paste0(s, "_1"); b <- paste0(s, "_2")
    out[c(a, b)] <- contrasts_df[c(b, a)]
  }
  
  ratio_est  <- 1 + contrasts_df$pct_est  / 100
  ratio_low  <- 1 + contrasts_df$pct_low  / 100
  ratio_high <- 1 + contrasts_df$pct_high / 100
  
  out$contrast <- str_replace(contrasts_df$contrast, "^(\\S+) - (\\S+)$", "\\2 - \\1")
  out$pct_est  <- round((1 / ratio_est  - 1) * 100, 2)
  out$pct_low  <- round((1 / ratio_high - 1) * 100, 2)
  out$pct_high <- round((1 / ratio_low  - 1) * 100, 2)
  out
}


#' Evaluate longitudinal mixed model contrasts within groups
#'
#' Fits a full and null linear (or Gamma) mixed model for each parameter in
#' \code{params}, then computes pairwise contrasts of \code{within_var}
#' levels separately within each level of \code{group_var} (e.g., does a
#' site's values differ between fill statuses?). Continuous parameters are
#' log-transformed and fit with \code{lmer()}; parameters listed in
#' \code{gamma_params} are fit with \code{glmmTMB()} using a Gamma family and
#' log link. A likelihood ratio test (full vs. null model) is also returned
#' for each parameter.
#'
#' For each \code{within_var} x \code{group_var} combination, two sets of
#' summaries are returned:
#' \itemize{
#'   \item \strong{Raw (descriptive)}: median and distribution-free 95\% CI
#'     (\code{\link{median_ci}}) of the observed data, ignoring random effects
#'     and covariates.
#'   \item \strong{Model-estimated}: estimated marginal mean and 95\% CI from the
#'     full model, back-transformed to the original scale with
#'     \code{\link{emm_to_original}} (\code{exp()} of the log-scale estimate).
#'     For LMMs this is the geometric mean (estimated median under
#'     log-normality); for Gamma GLMMs it is the estimated mean. Estimates are
#'     averaged over any other fixed effects (equal weights) and use random
#'     effects at zero (population level).
#' }
#' Percent differences are \code{(exp(contrast) - 1) * 100}, i.e. the ratio of
#' the model-estimated values minus one, so they are consistent with
#' \code{emm}, not with the ratio of raw medians.
#'
#' @param df A data frame in long format, containing at minimum the columns
#'   \code{param} (parameter name), \code{value} (numeric measurement),
#'   \code{within_var}, and \code{group_var}.
#' @param params Character vector of parameter names (values of the
#'   \code{param} column) to evaluate, e.g. \code{c("DOC", "NO3", "ChlA")}.
#' @param within_var Character string giving the name of the column whose
#'   levels will be pairwise-contrasted (e.g. \code{"fill_status"}).
#' @param group_var Character string giving the name of the column defining
#'   the groups within which \code{within_var} contrasts are computed (e.g.
#'   \code{"location"}).
#' @param full_formula_rhs Character string giving the right-hand side of the
#'   full model formula (e.g. \code{"location * fill_status + (1 | season)"}).
#'   Combined with \code{value} (Gamma parameters) or \code{log_value}
#'   (all other parameters) as the response.
#' @param null_formula_rhs Character string giving the right-hand side of the
#'   null (reduced) model formula used for the likelihood ratio test. Should
#'   typically include \code{group_var} so the test isolates the effect of
#'   \code{within_var} rather than an omnibus effect (e.g.
#'   \code{"location + (1 | season)"}).
#' @param gamma_params Character vector of parameter names to fit with a
#'   Gamma GLMM (\code{value ~ ...}, log link) instead of a log-transformed
#'   linear mixed model (\code{log_value ~ ...}). Defaults to \code{"NO3"}.
#'
#' @return A list with two data frames:
#' \describe{
#'   \item{contrasts}{One row per parameter x \code{group_var} level x
#'     \code{within_var} contrast, with \code{median_*}, \code{ci_low_*},
#'     \code{ci_high_*} (raw) and \code{emm_*}, \code{emm_low_*},
#'     \code{emm_high_*} (model-estimated) for the first (\code{_1}) and second
#'     (\code{_2}) level; \code{pct_est}, \code{pct_low}, \code{pct_high}
#'     (percent difference and 95\% CI from the model); contrast p-value
#'     (\code{p_value_contrast}) and likelihood ratio test p-value
#'     (\code{p_value_anova}) with significance flags.}
#'   \item{medians}{One row per parameter x \code{within_var} x \code{group_var}
#'     combination, with \code{n}; raw \code{median}, \code{ci_low},
#'     \code{ci_high}; and model-estimated \code{emm}, \code{emm_low},
#'     \code{emm_high}. All values are on the original data scale and rounded to
#'     3 decimals.}
#' }
lmm_eval_group <- function(df, params, 
                           within_var,      # e.g. "fill_status" — the variable being contrasted
                           group_var,       # e.g. "location" — the variable contrasts are computed within
                           full_formula_rhs,
                           null_formula_rhs,   # should typically include group_var, e.g. "location + (1 | season)"
                           gamma_params = "NO3") {
  
  emmeans_spec <- as.formula(paste("~", within_var, "|", group_var))
  
  results_list <- map(params, function(param_name) {
    
    param_df <- df %>% filter(param == param_name & value > 0)
    
    is_gamma <- param_name %in% gamma_params
    
    if (is_gamma) {
      full_fmla <- as.formula(paste("value ~", full_formula_rhs))
      null_fmla <- as.formula(paste("value ~", null_formula_rhs))
      
      model_full <- glmmTMB(full_fmla, data = param_df, family = Gamma(link = "log"))
      model_null <- glmmTMB(null_fmla, data = param_df, family = Gamma(link = "log"))
      
      group_means <- emmeans(model_full, emmeans_spec)
      contrasts <- summary(pairs(group_means), infer = TRUE) %>%
        as.data.frame() %>%
        rename(p_value_contrast = p.value,
               lower.CL = asymp.LCL,
               upper.CL = asymp.UCL)
      
    } else {
      param_df <- param_df %>% mutate(log_value = log(value))
      full_fmla <- as.formula(paste("log_value ~", full_formula_rhs))
      null_fmla <- as.formula(paste("log_value ~", null_formula_rhs))
      
      model_full <- lmer(full_fmla, data = param_df, REML = FALSE)
      model_null <- lmer(null_fmla, data = param_df, REML = FALSE)
      
      group_means <- emmeans(model_full, emmeans_spec)
      contrasts <- summary(pairs(group_means), infer = TRUE) %>%
        as.data.frame() %>%
        rename(p_value_contrast = p.value)
    }
    
    # contrasts will have one row per level of group_var, each showing
    # a within_var pairwise contrast (e.g. "full - empty") plus the group_var column
    anova_res <- anova(model_null, model_full)
    p_anova <- anova_res$`Pr(>Chisq)`[2]
    
    contrasts <- contrasts %>%
      mutate(p_value_anova = p_anova,
             parameter = param_name)
    
    # model-estimated values per within_var x group_var, back-transformed (exp)
    estimates <- emm_to_original(group_means)
    
    # raw medians + CIs and model estimates per within_var x group_var combination
    medians <- param_df %>%
      group_by(across(all_of(c(within_var, group_var)))) %>%
      summarize(n = n(),
                median = median(value, na.rm = TRUE),
                ci_low = median_ci(value)[["lower"]],
                ci_high = median_ci(value)[["upper"]],
                .groups = "drop") %>%
      left_join(estimates, by = c(within_var, group_var)) %>%
      mutate(parameter = param_name,
             across(c(median, ci_low, ci_high, emm, emm_low, emm_high), ~ round(.x, 3)))
    
    list(contrasts = contrasts, medians = medians)
  }) %>% set_names(params)
  
  contrasts_all <- map_dfr(results_list, "contrasts")
  medians_all   <- map_dfr(results_list, "medians")
  
  contrasts_with_means <- contrasts_all %>%
    mutate(
      pct_est  = round((exp(estimate) - 1) * 100, 2),
      pct_low  = round((exp(lower.CL) - 1) * 100, 2),
      pct_high = round((exp(upper.CL) - 1) * 100, 2),
      p_sig_anova    = ifelse(p_value_anova < 0.05, "*", ""),
      p_value_anova = ifelse(p_value_anova < 0.0001, 0.0001, round(p_value_anova, 4)),
      p_value_contrast = ifelse(p_value_contrast < 0.0001, 0.0001, round(p_value_contrast, 4)),
      p_sig_contrast = ifelse(p_value_contrast < 0.05, "*", ""),
      # parse the within_var pairwise labels, e.g. "full - empty"
      level_1 = str_trim(str_split_fixed(contrast, " - ", 2)[, 1]),
      level_2 = str_trim(str_split_fixed(contrast, " - ", 2)[, 2])
    ) %>%
    # join summaries using within_var level + group_var value (already its own column from emmeans output)
    left_join(
      medians_all %>%
        select(parameter, all_of(c(within_var, group_var)),
               median_1 = median, ci_low_1 = ci_low, ci_high_1 = ci_high,
               emm_1 = emm, emm_low_1 = emm_low, emm_high_1 = emm_high),
      by = c("parameter", group_var, "level_1" = within_var)
    ) %>%
    left_join(
      medians_all %>%
        select(parameter, all_of(c(within_var, group_var)),
               median_2 = median, ci_low_2 = ci_low, ci_high_2 = ci_high,
               emm_2 = emm, emm_low_2 = emm_low, emm_high_2 = emm_high),
      by = c("parameter", group_var, "level_2" = within_var)
    ) %>%
    select(parameter, all_of(group_var), contrast,
           median_1, ci_low_1, ci_high_1, emm_1, emm_low_1, emm_high_1,
           median_2, ci_low_2, ci_high_2, emm_2, emm_low_2, emm_high_2,
           pct_est, pct_low, pct_high,
           p_value_contrast, p_sig_contrast, p_value_anova, p_sig_anova)
  
  list(contrasts = contrasts_with_means, medians = medians_all)
}