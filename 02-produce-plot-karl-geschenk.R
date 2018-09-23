## ######################################################################### ##
## karl geschenk
## --------------------------------------------------------------------------
## Ingo Nader
## Jan 2017
## ######################################################################### ##

## ========================================================================= ##
## karl geschenk
## ========================================================================= ##
## http://docs.ggplot2.org/dev/geom_curve.html

## [[to do]]
## * choose format 
## * (?) change dates to continuous values (convert) -- no, just use "+days()" instead of "%m+%"
## * parametrize font color and font size
## * (?) parametrize month shift in start / end dates (and back), experiment @Papp and @Hudik
## * add karenz in grau?
## * (?) fix arrow points (using alpha)
## * stretch axis?
## * geom_labels: light-grey background, dark-grey line?
## * Parametrize everything that is needed to change format?

## ========================================================================= ##
## load packages
## ========================================================================= ##

rm(list = ls(), inherits = TRUE)

library(tibble)
library(magrittr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(grid)

library(extrafont)  ## https://cran.r-project.org/web/packages/extrafont/README.html

options(tibble.width = Inf)

## ========================================================================= ##
## add fonts using extrafont package 
## ========================================================================= ##

#font_import() ## needs to be done once.

fonts()
fonttable()
loadfonts() ## necessary per session? For PostScript output, use loadfonts(device="postscript")
## to add fonts to pdf:
#embed_fonts("font_ggplot.pdf", outfile="font_ggplot_embed.pdf")  # If outfile is not specified, it will overwrite the original file; needs ghostscript installed on the system
## To check if the fonts have been properly embedded, open each of the PDF files with Adobe Reader, and go to File->Properties->Fonts. If a font is embedded, it will say "Embedded Subset" by the font's name; otherwise it will say nothing next to the name.

## ========================================================================= ##
## load data
## ========================================================================= ##

## load data:
source("./01-data-load.R")

# ## note: data is not in public repository, as it contains personal data
# ## format of data:
# dat <- tribble(
#   ~first_name,    ~last_name,     ~start_date,  ~end_date,    ~label_pos_x, ~y_pos, ~textbox_x, ~textbox_y, ~color_ind, ~textbox_text_buildingblock,
#   "Blossom",      "Powerpuff",   "2000-01-01", "2017-01-31", "2015-06-24",  6,     "2001-01-01",       5.5,  1,         "Leader in Pink", 
#   "Butercup",     "Powerpuff",   "2009-03-01", "2012-04-01", NA,            8,     "2008-03-01",      12.0,  4,         "Fighing Fury",
#   "Bubbles",      "Powerpuff",        "2012-09-03", "2016-06-30", "2015-06-24",  4,     NA,                 2.5,  5,    "Unicorn Lover"  
# )
# dat_karenz <- tribble(
#   ~start_date, ~end_date, ~y_pos, 
#   "2014-02-28", "2015-03-31", 6, 
#   "2015-07-18", "2015-09-29", 4 
# )

## ========================================================================= ##
## define sizes
## ========================================================================= ##

size_line <- 20 * .78
size_arrow <- size_line / 6
length_arrow_points <- 22*2.5*.78
alpha_val <- 1
font_val <- "Consolas"
font_name_val <- "Consolas"
font_textbox_val <- "Consolas"
date_shift_days <- 40
arrow_shift_x <- days(6*30)
arrow_shift_y <- .5

alpha_val_karenz <- 0.3
color_karenz <- "black" #"grey"

base_size <- 12   ## doesn't doo much #http://stat545.com/block017_write-figure-to-file.html
text_size_names <- 10 * .78
text_size_textbox <- 5.0 * .78 ## maybe a bit smaller here?
text_size_aabox <- 16 * .78
text_size_axes <- 24 * .78
text_size_signature <- 3 * .78
textbox_fill <- "lightgrey"
textbox_label_padding <- unit(6 * .78, "points")
textbox_label_radius <- unit(6 * .78, "points")
aabox_label_padding <- unit(20 * .78, "points")
aabox_label_radius <- unit(20 * .78, "points")

img_width <- 50
img_height <- 23
img_units <- "cm"
img_dpi <- 300

img_margin_mm <- 4

## ========================================================================= ##
## transform data
## ========================================================================= ##

dat_karenz %<>% mutate(
  start_date = as.Date(start_date),
  end_date = as.Date(end_date)
)

dat %<>% mutate(
  start_date = as.Date(start_date),
  start_date_visual = start_date,
  end_date = as.Date(end_date),
  end_date_na = end_date,   ## removed later for non-quitters
  end_date_visual = end_date ## for longer arrows than real working time (karl)
)

## change visual length for karl:
dat[dat$first_name %in% "Karl", "end_date_visual"] <- "2018-05-31"
dat[dat$first_name %in% "Karl", "start_date_visual"] <- "1999-10-01"

dat %<>% mutate(
  work_duration = as.numeric(end_date_visual - start_date_visual),
  start_date_visual_mod = start_date_visual + days(ifelse(first_name %in% c("Karl"), 0, date_shift_days)), ## 4 months later for late-starters 
  end_date_visual_mod = end_date_visual - days(ifelse(first_name %in% c("Karl", "Christiane", "Rita"), 0, date_shift_days)),      ## for quitters (both modified later, too, for non-quitters)
  label_pos_x = as.Date(label_pos_x),
  arc_y_start = ifelse(y_pos > 6, y_pos + .5, y_pos - .5),
  startbar_y_start = ifelse(y_pos > 6, max(y_pos) + 3, min(y_pos) - 3),
  startarc_x_start = start_date_visual_mod - days(123), #as.Date(arc_x_pos), ## 15 months for width:height = 20:5 and +/- 6 
  endarc_x_start   = end_date_visual_mod   + days(123),
  endbar_y_start = startbar_y_start,
  endbar_y_arrowstart = ifelse(y_pos > 6, endbar_y_start + arrow_shift_y, endbar_y_start - arrow_shift_y),
  endbar_y_arrowend = ifelse(y_pos > 6, endbar_y_arrowstart + 0.00001, endbar_y_arrowstart - 0.00001),
  textbox_x = as.Date(textbox_x),
  #textbox_x = startarc_x_start,
  #textbox_y = ifelse(y_pos > 6, max(y_pos) + 2, min(y_pos) - 2),
  textbox_text = paste0("[", first_name, " ", last_name, "]\n", 
    textbox_text_buildingblock),
  row_num = row_number(),
  color_val = as.factor(rainbow(23)[color_ind]) #as.factor("yellow")
)
## replace starting values with NA for:
dat %<>% mutate(
  startbar_y_start = replace(startbar_y_start, which(first_name %in% "Karl"), NA)
)
#dat[dat$first_name == "Karl", c("startarc_y_start")] <- NA

## replace ending values with NA for:
dat[dat$first_name %in% c("Karl", "Christiane", "Rita"), "endbar_y_start"] <- NA
dat[dat$first_name %in% c("Karl", "Christiane", "Rita"), "endarc_x_start"] <- NA
dat[dat$first_name %in% c("Christiane", "Rita"), "end_date_na"] <- NA
idx <- is.na(dat$label_pos_x)  ## get default label_pos_x if nothing is specified
dat[idx, ][["label_pos_x"]] <- with(dat[idx, ], (abs(start_date_visual - end_date_visual)/2 + start_date_visual))
idx <- is.na(dat$textbox_x)  ## get default textbox_x position if nothing is specified
dat[idx, ][["textbox_x"]] <- with(dat[idx, ], start_date_visual - days(400))
idx <- is.na(dat$textbox_y)  ## get default textbox_< position if nothing is specified
dat[idx, ][["textbox_y"]] <- with(dat[idx, ], ifelse(y_pos > 6, y_pos + 2, y_pos - 2))


## ========================================================================= ##
## produce plot
## ========================================================================= ##

p <- ggplot(dat) + 
  
  ## add vertical start bars:
  geom_segment(aes(x = startarc_x_start, y = startbar_y_start, xend = startarc_x_start, yend = arc_y_start, color = color_val), 
    alpha = alpha_val, size = size_line) +
  
  ## add vertical end bars:
  geom_segment(aes(x = endarc_x_start, y = arc_y_start, xend = endarc_x_start, yend = endbar_y_start, color = color_val), 
    alpha = alpha_val, size = size_line) +
  
  ## add end bar arrows for quitters:
  geom_segment(aes(x = endarc_x_start, y = endbar_y_arrowstart, xend = endarc_x_start, yend = endbar_y_arrowend, color = color_val),
    alpha = alpha_val, size = size_arrow,
    arrow = arrow(length = unit(length_arrow_points, "points"), type = "closed", angle = 40) ) +
    
  ## add start curves (top and to bottom):
  geom_curve(data = subset(dat, y_pos > 6), aes(x = startarc_x_start, y = arc_y_start, xend = start_date_visual_mod, yend = y_pos, color = color_val), 
    curvature = .5, angle = 90, alpha = alpha_val, size = size_line, lineend = "round") +  ## use "butt" if alpha < 1
  geom_curve(data = subset(dat, y_pos < 6), aes(x = startarc_x_start, y = arc_y_start, xend = start_date_visual_mod, yend = y_pos, color = color_val), 
    curvature = -.5, angle = 90, alpha = alpha_val, size = size_line, lineend = "round") +  ## use "butt" if alpha < 1
  
  ## add end and curves (top and bottom):
  geom_curve(data = subset(dat, y_pos > 6), aes(x = endarc_x_start, y = arc_y_start, xend = end_date_visual_mod, yend = y_pos, color = color_val), 
    curvature = -.5, angle = 90, alpha = alpha_val, size = size_line, lineend = "round") +  ## use "butt" if alpha < 1
  geom_curve(data = subset(dat, y_pos < 6), aes(x = endarc_x_start, y = arc_y_start, xend = end_date_visual_mod, yend = y_pos, color = color_val), 
    curvature = +.5, angle = 90, alpha = alpha_val, size = size_line, lineend = "round") +  ## use "butt" if alpha < 1
  
  ## add work segments:
  geom_segment(aes(x = start_date_visual_mod, xend = end_date_visual_mod, y = y_pos, yend = y_pos, color = color_val), alpha = alpha_val, size = size_line) +
  
  ## add karenz:
  geom_segment(data = dat_karenz, aes(x = start_date, xend = end_date, y = y_pos, yend = y_pos), color = color_karenz, alpha = alpha_val_karenz, size = size_line) +
  
  ## add arrows to non-quitting members:
  geom_segment(data = subset(dat, is.na(endbar_y_start)), aes(x = end_date_visual + arrow_shift_x, xend = end_date_visual + arrow_shift_x + .5, y = y_pos, yend = y_pos, color = color_val), 
    alpha = alpha_val, size = size_arrow,
    arrow = arrow(length = unit(length_arrow_points, "points"), type = "closed", angle = 40) ) +
  
  ## add starting date points and end date points, as well as lines:
  geom_segment(aes(x = start_date, y = y_pos, xend = end_date, yend = y_pos), 
    color = "black", alpha = .25, size = size_line / 2, lineend = "round") + 
  geom_point(aes(x = start_date, y = y_pos), color = "black", size = 4) + 
  geom_point(aes(x = end_date_na,   y = y_pos), color = "black", size = 4) +
  
  ## add names:
  #geom_text(aes(label = first_name, x = label_pos_x, y = y_pos), fontface = "bold", family = font_name_val) +
  geom_label(aes(label = first_name, x = label_pos_x, y = y_pos), fill = dat$color_val, fontface = "bold", family = font_name_val, size = text_size_names) +
  
  ## add text boxes:
  geom_label(aes(label = textbox_text, x = textbox_x, y = textbox_y), size = text_size_textbox, hjust = 0, family = font_textbox_val, fill = textbox_fill, label.padding = textbox_label_padding, label.r = textbox_label_radius) +

  ## add AA:
  annotate("label", label = "Advanced Analytics\nTeam Vienna", 
    x = as.Date("2002-06-01"), y = 11.5, fontface = "bold", family = font_name_val, 
    size = text_size_aabox, label.padding = aabox_label_padding, label.r = aabox_label_radius) +
  
  ## add signature:
  annotate("label", label = "Initiated by Ilka\nReviewed by Christiane\nDesigned and implemented by Ingo\nCreated using R and ggplot2", 
    x = as.Date("2000-02-01"), y = -0.6, hjust = 0,
    fontface = "bold", family = font_name_val, size = text_size_signature, 
    color = "black", fill = "black") +
  annotate("text", label = "Initiated by Ilka\nReviewed by Christiane\nDesigned and implemented by Ingo\nCreated using R and ggplot2", 
    x = as.Date("2000-02-01"), y = -0.6, hjust = 0,
    fontface = "bold", family = font_name_val, size = text_size_signature, 
    color = "lightgrey") + #, fill = "black") +
  
  ## change x scale:
  #scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2000-01-01", "2019-01-01"))) + #, sec.axis = dup_axis())
  scale_x_date(breaks = c(as.Date(paste0(2000:2017, "-01-01"))), 
    date_labels = "%Y", limits = as.Date(c("1999-10-01", "2019-01-01"))) + #, sec.axis = dup_axis())
  #scale_x_log10() + 
  
  ## change color:
  scale_color_manual(values = levels(dat$color_val), labels = levels(dat$color_val)) +
  
  ## change theme:
  theme_dark(base_size = base_size) + #theme_void()
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank()) +
  theme(panel.background = element_rect(fill = 'black', colour = 'black')) + 
  theme(plot.background = element_rect(fill = 'black', colour = 'black')) +
  theme(axis.text = element_text(color = "white", size = text_size_axes, face = "bold", angle = 20, family = font_val)) + 
  theme(legend.position="none") + ## no legends
  theme(plot.margin =   margin(t = img_margin_mm, r = img_margin_mm, b = img_margin_mm, l = img_margin_mm, unit = "mm"))
  

### change shape of errors (ony works in plotting window, not file):
#grid.force()
## change shape of arrows
#grid.gedit("segments", gp=gpar(linejoin ='mitre'))

## save plot to file:

img_scale <- 1
ggsave(filename = "karl-advanced-analytics-chronicles.png", 
  width = img_width, height = img_height, scale = img_scale, 
  units = img_units, dpi = img_dpi,
  plot = p)


# ## R_GSCMD
# ## /usr/local/bin/gs
# Sys.getenv("R_GSCMD")
# Sys.setenv(R_GSCMD = "/usr/local/bin/gs")



