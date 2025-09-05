library(utils)
package <- function(p, version = NULL, remove = FALSE, github = '') {
  if (remove) {
    detach(paste0("package:", p), unload = T)
    remove.packages(p)
  }
  if (!is.element(p, installed.packages()[,1])) {
    if (missing(version)) {
      if (github != '') {
        package("devtools")
        install_github(paste0(github, '/', p))
      } else install.packages(p) # , repos='https://cran.rstudio.com/', type = 'source' may help in case of bug
    } else {
      try({detach("package:devtools", unload = T)})
      package("remotes")
      install_version(p, version = version, repos = "http://cran.us.r-project.org", upgrade = "never", dependencies = TRUE)
      package("devtools")
    }
  }
  else { if(!missing(version)) warning(paste("'", p, "' is already installed with a (potentially) newer version. You may want to install the required version (", version, ") to avoid bugs.", sep=""))}
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

package("readr")
package("tidyverse")
package("openxlsx")
package("memisc")
package("Hmisc")
package("dplyr")
package("stats")
package("DescTools")
package("broom")
package("relaimpo")
package("ggplot2")
package("ggrepel")
package("tidyr")
package("beepr")
package("kableExtra") # kbl (latex table)
package("RColorBrewer") # used in barres
package("plotly")

decrit <- function(variable, data = e, miss = TRUE, weights = NULL, numbers = FALSE, which = NULL, weight = T) { # TODO!: allow for boolean weights
  # if (!missing(data)) variable <- data[[variable]]
  if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
  if (!missing(which)) variable <- variable[which]
  if (weight) {
    # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
    if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else {
    if (!missing(which)) weights <- weights[which]
    if (length(weights)!=length(variable)) {
      warning("Lengths of weight and variable differ, non-weighted results are provided")
      weights <- NULL
    } }
  if (length(annotation(variable))>0 & !numbers) {
    if (!miss) {
      # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
    }
    else {
      if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else describe(as.factor(include.missings(variable)[include.missings(variable)!="" & !is.na(variable)]), weights = weights[include.missings(variable)!="" & !is.na(variable)], descript=Label(variable)) }
  }
  else {
    if (length(annotation(variable))>0) {
      if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
      else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
    } else describe(variable[variable!=""], weights = weights[variable!=""])  }
}

no.na <- function(vec, num_as_char = T, rep = "na") {
  if (num_as_char) {
    if (is.numeric(vec) | is.logical(vec)) return(replace_na(as.character(as.vector(vec)), rep))
    else return(replace_na(as.vector(vec), rep))
  } else if (is.logical(c(vec, rep))) { replace_na(as.vector(vec), rep)
  } else return(vec)
}
reg_formula <- function(dep_var, indep_vars, as_factor = FALSE) {
  if (as_factor) return(as.formula(paste(dep_var, "~ factor(", paste(indep_vars, collapse = ') + factor('), ")")))
  else return(as.formula(paste(dep_var, "~", paste(indep_vars, collapse = '+'))))
}
barres <- function(data, vars, file, title="", labels, color=c(), rev_color = FALSE, hover=legend, nsp=TRUE, sort=TRUE, legend=hover, showLegend=T,
                   margin_r=0, margin_l=NULL, share_labels = NULL, online=FALSE, export_xls = F, digits = 0, add_means = FALSE, show_legend_means = T, transform_mean = NULL, name_mean = "Mean",
                   display_values=T, thin=T, legend_x=NA, show_ticks=T, xrange=NA, save = FALSE, df=e, miss=T, weight_non_na = T, width = dev.size('px')[1], height = dev.size('px')[2],
                   weights = T, fr=F, rev=T, grouped = F, error_margin = F, color_margin = '#00000033', N = NA, font = 'Arial') { # default: Arial (also: Times, Latin Modern Sans, Computer Modern) # OECD: Computer Modern
  if (missing(vars) & missing(legend) & missing(hover)) warning('hover or legend must be given')
  if (!missing(miss)) nsp <- miss
  labels <- rev(unname(labels))
  if (!missing(vars)) vars <- rev(vars)
  if (!missing(data)) data <- data[, ncol(data):1, drop = FALSE]
  else if (!missing(vars)) {
    data <- dataKN(vars, data=df, miss=miss, weights = weights, return = "", fr=fr, rev=rev, weight_non_na = weight_non_na)
    N <- dataN(vars[1], data=df, miss=miss, weights = weights, return = "N")
    if ((missing(legend) || is.null(legend)) & missing(hover)) {
      if (is.logical(df[[vars[1]]])) hover <- legend <- labels # data1(var = vars[1], data=df, weights = weights)
      else hover <- legend <- dataN(var = vars[1], data=df, miss=miss, weights = weights, return = "legend", fr=fr, rev_legend = rev) } }
  if (length(color)==0) color <- color(data, nsp, rev_color = rev_color)
  if (identical(legend, TRUE) & missing(error_margin)) error_margin <- T
  margin_t <- 0 + 25*(!(thin))
  if (title!="") { margin_t <- 100 }
  if (grepl("<br>", title)) { margin_t <- 150 }
  legendSize <- 15+2 # 10, 13
  legendY <- 1 #1.1  + 0.3*thin/(ncol(data)-1) # last term may be problematic
  legendX <- 0.2
  legendFont = font #'Open Sans'
  if (is.null(margin_l)) margin_l <- 0 # 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>')))
  if (is.null(share_labels)) share_labels <- 0.01 + 0.49*(!(" " %in% labels)) # 0.14
  if (max(nchar(labels)) > 25) { legendSize <- 15 } # 9, 13
  # if (max(nchar(labels)) > 50) { legendSize <- 8 }
  # if (max(nchar(labels)) > 60) { legendSize <- 7 }
  if (max(nchar(labels)) > 50) { # 70
    legendSize <- 13 # 11
    # legendY = 1.2
    legendX= -0.2 # 1
    # if (ncol(data)>1) margin_t = 170
  }
  legendX <- .96 # overwrites the previous legendX that was defined with xanchor = 'left'
  if (!is.na(legend_x)) legendX <- legend_x
  if (!showLegend) { margin_t <- max(0, margin_t - 70) }
  if (ncol(data) == 1) legendY <- 1 # 1.5 + 0.3*thin
  if (!is.null(add_means) && any(add_means) && is.null(transform_mean)) transform_mean <- identity
  if (!is.null(add_means) && any(add_means)) means <- if (is.numeric(add_means)) add_means else sapply(vars, function(v) return(transform_mean(wtd.mean(df[[sub("_agg", "", v)]], weights = df[["weight"]]))))
  if (sort) {
    order <- order_agree(data = data, miss = miss, rev = rev, n = length(labels))
    labels <- labels[order]
    data <- matrix(data[, order], nrow=nrow(data))
    if (!is.null(add_means) && any(add_means)) means <- means[order]
  }
  if (is.na(xrange)) xrange <- c(0, max(colSums(data))*1.099)
  if (nrow(data)==1 & (sort | !showLegend)) {  # new: add !showLegend to manage responsable_CC i.e. comparisons of a multiple answer question
    if (!sort) order <- 1:length(labels)
    hover <- hover[order]
    value <- c()
    for (i in 1:length(hover)) {
      hover[i] <- paste(hover[i], "<br>Choisi dans ", round(100*data[1, i]), "% des réponses", sep="")
      value[i] <- paste(ifelse(data[1, i] > 0.01, round(100*data[1, i]), round(100*data[1, i], 1)), '%', sep='')
      value[i] <- paste(round(100*data[1, i], digits = digits), '%', sep='') } # '%  '
    hovers <- matrix(hover, nrow=length(hover))
    values <- matrix(value, nrow=length(hover))
  }
  else {
    hovers <- values <- c()
    if (nsp) {
      for (i in 1:(length(hover)-1)) {
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]/(1+data[length(hover), j])), '% des réponses<br>', round(100*data[i, j]), '% des réponses exprimées') )
          values <- c(values, paste(round(100*data[i, j]/(1+data[length(hover), j]), digits = digits), '%', sep='')) # '%  '
        }
      }
      for (j in 1:length(labels)) {
        hovers <- c(hovers, paste(hover[length(hover)], '<br>', round(100*data[length(hover), j]/(1+data[length(hover), j])), '% des réponses<br>') )
        values <- c(values, paste(round(100*data[length(hover), j]/(1+data[length(hover), j]), digits = digits), '%', sep='')) # '%  '
      }
    }
    else {
      if (is.element(hover[length(hover)],c("PNR", "PNR or other", "NSP"))) hover <- hover[1:(length(hover)-1)]
      if (is.element(legend[length(legend)],c("PNR", "PNR or other", "NSP"))) legend <- legend[1:(length(legend)-1)]
      for (i in 1:length(hover)) {
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]), '% des réponses exprimées<br>') )
          values <- c(values, paste(round(100*data[i, j], digits = digits), '%', sep='')) # '%  '
        }
      }
    }
    hovers <- matrix(hovers, ncol=length(hover))
    values <- matrix(values, ncol=length(hover))
  }
  if (!(display_values)) values <- replace(values, T, '')
  
  bars <- plot_ly(x = data[1,], y = labels, type = 'bar', orientation = 'h', text = values[,1], textposition = 'auto',
                  error_x = list(visible = error_margin, array=qnorm(1-0.05/2)*sqrt(data[1,]*(1-data[1,])/(N-1)), color = color_margin), # sort=FALSE,
                  hoverinfo = hovers[,1], name=legend[1], marker = list(color = color[1], line = list(color = 'white'))) %>% # , width = 0
    
    plotly::layout(xaxis = list(title = "",
                                showgrid = show_ticks,
                                showline = FALSE,
                                showticklabels = show_ticks,
                                gridcolor = toRGB("gray70"), # + noir, + proche de 0
                                gridwidth = 1,
                                griddash = "dot",
                                autotick = FALSE,
                                ticks = "outside",
                                tick0 = 0,
                                dtick = 0.1,
                                ticklen = 5*show_ticks,
                                tickwidth = 1,
                                tickcolor = toRGB("gray70"),
                                zeroline = T,
                                range = xrange,
                                domain = c(share_labels, 1)
    ),
    yaxis = list(title = "",
                 showgrid = FALSE,
                 showline = FALSE,
                 showticklabels = FALSE,
                 categoryorder = "trace",
                 # automargin = T,
                 zeroline = FALSE),
    hovermode = 'closest',
    barmode = ifelse(grouped, 'group', 'stack'),
    title = list(text = title, font = list(color = 'black')),
    # title = title,
    # titlefont = list(color='black'),
    font = list(color='black', size=legendSize-1),
    # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
    margin = list(l = margin_l, r = margin_r, t = margin_t, b = 24, autoexpand = thin), # 21, autoexpand=FALSE removes useless margin at bottom but creates bug with legend
    # margin = list(b = 20, t = margin_t),
    legend = list(orientation='h', y=legendY, x=legendX, xanchor = 'right', yanchor = 'bottom', traceorder='normal', font=list(size=legendSize+2, color='black', family = font)), # family='Balto',  , family=legendFont
    # showlegend = (showLegend & !((("Yes" %in% legend) | ("Oui" %in% legend)) & (length(legend)<4)))) %>%
    showlegend = showLegend # (showLegend & !(setequal(legend, c('Yes', 'No', 'PNR')) | setequal(legend, c('Oui', 'Non', 'NSP')) | setequal(legend, c('Yes', 'No')) | setequal(legend, c('Oui', 'Non'))))
    ) %>%
    
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = share_labels - 0.01, y = labels,
                    xanchor = 'right',
                    text = labels,
                    font = list(family = font, size = 14+2, color = 'black',
                                type = if (length(labels) == 2) 'bold' else '' # to avoid the last item being more black (for real bold, use 'Arial Black')
                    ),
                    showarrow = FALSE, align = 'right') # %>%
  # Legend in the Yes/No case
  if (showLegend == FALSE) {
    if ((setequal(legend, c('Yes', 'No', 'PNR')) | setequal(legend, c('Oui', 'Non', 'NSP')))) {
      bars <- bars %>% add_annotations(xref = 'x', yref = 'paper',
                                       x = c(0.1, 0.9, 1.1),
                                       y = 1.5,
                                       text = legend,
                                       font = list(family = font, size = 16, color = 'black'),
                                       showarrow = FALSE) } # %>%
    if ((setequal(legend, c('Yes', 'No')) | setequal(legend, c('Oui', 'Non')))) {
      bars <- bars %>% add_annotations(xref = 'x', yref = 'paper',
                                       x = c(0.1, 0.9),
                                       y = 1.5,
                                       text = legend,
                                       font = list(family = font, size = 16, color = 'black'),
                                       showarrow = FALSE) } # %>%
  }
  
  # print(nrow(data))
  # print(hover)
  # print(nrow(hovers))
  # print(ncol(hovers))
  if (nrow(data)>1) { for (i in 2:nrow(data)) { # evaluate=TRUE,
    bars <- add_trace(bars, x = data[i,], name=legend[i], text = values[,i], hoverinfo = 'text', hovertext = hovers[,i], marker = list(color = color[i]),
                      error_x = list(visible = error_margin, array=qnorm(1-0.05/2)*sqrt(data[i,]*(1-data[i,])/(N-1)), color = color_margin)) # width thickness (in px)
  } } # /!\ When data and vars are not provided, N cannot be computed, but error_margin=T still returns a (zero) confidence interval
  if (!is.null(add_means) && any(add_means))  bars <- add_trace(bars, x = means, name = name_mean, marker = list(color = 'black', size = 10, symbol = 'diamond'), showlegend = (!is.null(show_legend_means) && show_legend_means), type = 'scatter', mode = 'markers')
  if (online) { api_create(bars, filename=file, sharing="public") }
  if (!missing(file) & save) save_plotly(bars, filename = file, width = width, height = height) # new
  if (export_xls) {
    table <- as.data.frame(data, row.names = legend)
    names(table) <- labels
    if (!is.null(file)) save_plot(table, filename = sub(".*/", "", file), folder = sub("/[^/]*$", "/", file))
    print(table)
    # return(table) # old
  }
  # else return(bars) # old
  return(bars)
}
color <- function(v, grey=FALSE, grey_replaces_last = T, rev_color = FALSE, theme='RdBu') { # used in barres # TODO! whitout white
  if (is.matrix(v)) n <- nrow(v)
  else if (length(v) > 1) n <- length(v)
  else n <- v # cf. http://research.stowers.org/mcm/efg/R/Color/Chart/ColorChart.pdf
  if (grey & grey_replaces_last & n > 1) n <- n-1
  if (theme=='rainbow') {
    if (n == 1) cols <- c("#66B3B3") # "brown": #A52A2A Presentation Teal: #008096 (title) #1A8C8C (dark) #66B3B3 #99CCCC (light)
    else if (n == 2) cols <- c("#66B3B3", "#A52A2A") # c("lightgreen", "plum") = c("#90EE90", "#DDA0DD")
    else if (n == 3) cols <- color5[c(1,3,5)]
    else if (n == 4) cols <- c(rainbow(4, end=4/15)[1:3], "#228B22")
    else if (n == 5) cols <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
    else if (n == 6) cols <- rainbow(6)
    else if (n == 7) cols <- c("#000000", rainbow(7)[c(1:3,5:7)])
    else cols <- rainbow(n) # diverge_hcl green2red brewer.pal(n, Spectral/RdBu...)  https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
  } else if (theme=='default') {
    cols <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(n)
  } else {
    cols <- rev(brewer.pal(max(n, 3), theme))
    if (n == 1) cols <- cols[1]
    # if (n == 2) cols <- cols[c(1,3)]
    else if (n %% 2 == 0) cols <- rev(brewer.pal(n+2, theme))[c(1:(n/2),(n/2+2):(n+1))] }
  if (n > 10) cols <- colorRampPalette(cols)(n)
  if (rev_color) cols <- rev(cols)
  if (grey & n > 1) return(c(cols, "#D3D3D3")) # lightgrey
  else return(cols)
}
order_agree <- function(data, miss, rev = T, n = ncol(data)) { # used in barres
  agree <- c()
  if (!missing(miss)) {
    if (miss) for (i in 1:n) agree <- c(agree, sum(data[floor(nrow(data)/2+1):max(1,(nrow(data)-1)),i]))
    else for (i in 1:n) agree <- c(agree, sum(data[ifelse(nrow(data)==1,1,ceiling(nrow(data)/2+1)):nrow(data),i]))
  } else {
    if (nrow(data)==5 | nrow(data)==6) { for (i in 1:n) { agree <- c(agree, data[4, i] + data[5, i]) } }
    else if (nrow(data)==7) { for (i in 1:n) { agree <- c(agree, data[6, i] + data[7, i]) } }
    else { for (i in 1:n) { agree <- c(agree, data[1, i]) } } }
  return(order(agree, decreasing = rev)) }
save_plotly <- function(plot, filename = deparse(substitute(plot)), folder = '../figures/', width = dev.size('px')[1], height = dev.size('px')[2], method='orca', format = 'pdf', trim = T) { # used in barres; in case connection refused, turn off Windows Defender on private networks
  if (any(class(plot) == "data.frame")) {
    # file <- paste(folder, "xls/", filename, ".xlsx", sep='')
    file <- paste(sub("figures", "xlsx", folder), filename, ".xlsx", sep='')
    write.xlsx(plot, file, row.names = T, overwrite = T)
    print(file)
  } else {
    file <- paste(folder, filename, ".", format, sep='')
    # print(file)
    if (grepl('webshot', method)) { # four times faster: 2.5s (vs. 10s) but saves useless widgets and doesn't exactly respect the display
      saveWidget(plot, 'temp.html')
      webshot('temp.html', file, delay = 0.1, vwidth = width, vheight = height)
      file.remove('temp.html')}
    # else orca(plot, file = file, width = width, height = height, format = format) # bug with encoding in Windows
    else {
      server <- orca_serve() # doesn't work within a function because requires admin rights
      server$export(plot, file = file, width = width, height = height)
      server$close()
    }
    if (trim & format == 'png') image_write(image_trim(image_read(file)), file)
    if (trim & format == 'pdf') pdf_crop(file) }
}
pdf_crop <- function(file) { # used in barres and save_plotly
  if (Sys.which("pdfcrop") != "") system2("pdfcrop", args = shQuote(c(file, file)), stdout = "nul")
  else plot_crop(file)
}
