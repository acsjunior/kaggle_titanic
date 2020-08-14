# Cor padrão dos gráficos:
GG_COLOR <- "#1da4d1"


# Quantifica os valores missing
count_missing_values <- function(df) {
  df <- as.data.frame(df)
  n_df <- nrow(df)
  out <- data.frame(var = names(df))
  out$n_missing <- NA
  out$f_missing <- NA
  out$var <- as.character(out$var)
  for (i in 1:nrow(out)) {
    var_name <- out[i,1]
    out$n_missing[i] <- length(df[is.na(df[var_name]),1])
    out$f_missing[i] <- round(out$n_missing[i] / n_df,4)
  }
  out <- out %>%
    arrange(desc(n_missing))
  
  return(out)
}


# Função head customizada
custom_head <- function(df, rows=6) {
  head(df, rows) %>%
    kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)
}


# Gráfico de barras customizado
custom_barPlot <- function(df, var_x, lab_x=var_x, lab_y="Freq", title="", log=NULL, angle_x=NULL) {
  plt <- df %>%
    ggplot(aes_string(x = var_x)) +
    geom_bar(fill = GG_COLOR, alpha = 0.7) +
    labs(x = lab_x, y = lab_y, title = title) +
    theme_bw()
  
  if(!is.null(log)) {
    plt <- plt + scale_y_log10()
  }
  
  if(!is.null(angle_x)) {
    plt <- plt + theme(axis.text.x = element_text(angle = angle_x, hjust = 1, vjust = 0.5))
  }
  
  return(plt)
}


# Gráfico de colunas customizado
custom_colPlot <- function(df, var_x, var_filter, criteria, lab_x=var_x, lab_y="Freq", title="") {
  tb <- as.data.frame(prop.table(table(df[[var_filter]], df[[var_x]]),2))
  names(tb) <- c(var_filter, var_x, "Freq")
  tb <- tb[tb[[var_filter]] == criteria,]
  
  plt <- ggplot(tb, aes_string(x = var_x, y = "Freq")) +
    geom_col(fill = GG_COLOR, alpha = 0.7) +
    labs(x = lab_x, y = lab_y, title = title) +
    theme_bw()
  
  return(plt)
}


# Boxplot customizado
custom_boxPlot <- function(df, var_x, lab_x=var_x, var_y, lab_y=var_y, title="", y_lim=NULL) {
  df <- df[!is.na(df[,var_x]), ]
  df[, var_x] <- as.factor(df[, var_x])
  
  plt <- ggplot(df, aes_string(x = var_x, y = var_y)) +
    geom_boxplot(fill = GG_COLOR, alpha = 0.7) + 
    labs(x = lab_x, y = lab_y, title = title) +
    theme_bw()
  
  if(!is.null(y_lim)) {
    plt <- plt + coord_cartesian(ylim = c(0,y_lim))
  }
  
  return(plt)
}