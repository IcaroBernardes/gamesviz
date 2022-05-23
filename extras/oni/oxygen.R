# 0. Library and fonts management
library(tidyverse)
library(readxl)
library(glue)
library(ragg)
library(ggtext)
library(ggborderline)
library(ggfx)
library(nflplotR)
library(colorspace)

## Defines some layout constants
lineheight <- 1.2
font_title <- "GRAYSTROKE"
font_body <- "Liberation Sans"

# 1. Data download, load and handling
oxygen <- readxl::read_excel("extras/oni/oni_data.xlsx")
dupli <- readxl::read_excel("extras/oni/oni_data.xlsx", sheet = "dupli")
gameseed <- readxl::read_excel("extras/oni/oni_data.xlsx", sheet = "seed") %>% 
  dplyr::pull(seed)

## Select variables of interest and rearranges the data
oxygen <- oxygen %>% 
  dplyr::select(-duplis) %>% 
  tidyr::pivot_longer(cols = -cycle)

## Eliminates lines with absent values
oxygen <- oxygen %>% 
  dplyr::filter(!is.na(value))

## Adds a base color for the categories
categ <- tibble(
  name = c("oxyl", "difsr", "algcoln", "deod", "electro", "consume"),
  base = c("#008599", "#003d99", "#619900", "#999100", "#996e00", "#990029")
)
oxygen <- oxygen %>% dplyr::left_join(categ)

## Defines order of categories
oxygen <- oxygen %>% 
  dplyr::mutate(name = factor(name, levels = c("consume", "electro", "deod",
                                               "algcoln", "difsr", "oxyl")))

## Defines coordinates for the background colors
bg <- oxygen %>% 
  dplyr::filter(name != "consume") %>% 
  dplyr::group_by(cycle) %>% 
  dplyr::slice_max(order_by = value) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-value)

## Adds extra values to close the gaps
bg <- bg %>% 
  dplyr::group_by(name) %>% 
  dplyr::slice(1L) %>% 
  dplyr::mutate(cycle = cycle-2) %>% 
  rbind(bg) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(cycle, name)

## Desaturates and lightens the base colors
bg <- bg %>% 
  dplyr::mutate(fill = colorspace::desaturate(colorspace::lighten(base, 0.3), 0.5))

## Defines breaks of the x-axis
xbrks <- oxygen %>% 
  dplyr::filter(name != "consume") %>% 
  dplyr::group_by(cycle) %>% 
  dplyr::slice_max(order_by = value) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(name) %>% 
  dplyr::slice_max(order_by = cycle) %>% 
  dplyr::pull(cycle)

## Defines breaks of the y-axis
ybrks <- seq(0, 550, 50)

## Defines coordinates for the images of the sources
sources <- tibble(
  x = (xbrks + lead(xbrks, default = 0))/2,
  y = 30,
  height = c(0.1, 0.05, 0.1, 0.1, 0.05),
  name = c("electro", "deod", "algcoln", "difsr", "oxyl"),
  label = c("Electrolyzer", "Deodorizer", "Algae Terrarium",
            "Oxygen Diffuser", "Oxylite")
)

## Defines paths to the icons
sources <- sources %>% 
  dplyr::mutate(image = glue::glue("extras/oni/icons/{name}.png"))

## Defines coordinates for the images of the duplicants
dupli <- dupli %>% 
  dplyr::mutate(image = glue::glue("extras/oni/icons/{dupli}.png"),
                x = c(-0.5,4.5,2,12,39,43,73,94,128),
                y = c(235,235,235,269,377,377,445,507,533),
                height = c(rep(0.05, 3), rep(0.1, 6)))

## Defines coordinates for the labels of the arrival of duplicants
arrival <- tibble(
  x = c(2,12,41,73,94,128),
  y = c(231,272,380,450,510,536),
  label = c(
    glue::glue("**{dupli$dupli[1]}**, **{dupli$dupli[2]}** and **{dupli$dupli[3]}**<br>start the Colony at Cycle 1"),
    glue::glue("**{dupli$dupli[4]}** arrives at<br>the Colony at Cycle 12"),
    glue::glue("**{dupli$dupli[5]}** and **{dupli$dupli[6]}** arrive at<br>the Colony at Cycles 40 and 43"),
    glue::glue("**{dupli$dupli[7]}** arrives at<br>the Colony at Cycle 73"),
    glue::glue("**{dupli$dupli[8]}** arrives at<br>the Colony at Cycle 94"),
    glue::glue("**{dupli$dupli[9]}** arrives at<br>the Colony at Cycle 128")
  )
)

## Defines coordinates for the title
title <- tibble(
  x = 3,
  y = 877,
  size = 10,
  label = c(
    "<span style='font-size:205px;'>OXYGEN</span><br>
    <span style='font-size:80px;'><span style='color:#ef5c5a;'>ALMOST</span> INCLUDED?</span>"
  )
)

## Defines coordinates for the subtitle
subtitle <- tibble(
  x = 44,
  y = 877,
  size = 9.1,
  label = glue::glue(
    "**Oxygen Not Included** is a space-colony simulation game developed by Klei Entertainment.<br>
    In it, you give tasks to your **duplicants** while balancing their needs for food, comfort, toilets and, specially, oxygen.<br>
    The lines bellow show 150 cycles of a game played on a Terra asteroid with seed {gameseed}.<br>
    The <span style='color:#990029;'>**red line**</span> shows the <span style='color:#990029;'>**consumed oxygen**</span>, while the other lines show oxygen added by different sources in each cycle.<br>
    The graphic is divided in sections with different colors that correspond to the main source of oxygen of a given period of cycles.<br><br>
    **Graphic by Ícaro Bernardes (@IcaroBSC) | Sprites belong to Klei Entertainment**"
  )
)

## Defines coordinates for the commentary on the sources
comment <- tibble(
  x = xbrks-1,
  y = 700,
  label = c(
    '**Electrolyzers** use electric energy to break water<br>
    into oxygen and hydrogen. It introduces copious<br>
    amounts of heat to the base, but is a more regular<br>
    and easy to automate source of oxygen.',
    
    'A **Deodorizer** uses sand to filter poluted oxygen. It can be coupled with Algae<br>
    Terrariums which produce a lot of poluted water. That water vaporizes into gas<br>
    and is filtered by the Deodorizer. It is a solution that is low-cost in energy and<br>
    resources and that also helps the player to deal both with polution and breathability.',
    
    '**Algae Terrariums** are colonies<br>
    of algae that do some kind of<br>
    photosynthesis. They consume water<br>
    and carbon dioxide (when available)<br>
    and produce oxygen. They become<br>
    slightly more efficient when under<br>
    light. From time to time the colony<br>
    needs to be renovated, thus it needs<br>
    more algae and expels poluted water',
    
    'The **Oxygen Diffuser** burns algae to produce<br>
    oxygen in a very inefficient manner. It can be<br>
    an emergential solution if the base has lots<br>
    of algae, but it produces too much heat and<br>
    consumes some electric power.',
    
    '**Oxylite** is a mineral<br>
    that liberates oxygen as<br>
    it sublimates. It usually<br>
    appears in and around the<br>
    starting position of the<br>
    player. It helps them to<br>
    start constructing their<br>
    base without immediate<br>
    concern with oxygen.'
  )
)

# 2. Generates the plot 
## Creates the main plot
p <- ggplot(oxygen) + 
  
  ### Places the backgrounds
  geom_area(
    aes(x = cycle, y = Inf, group = name, fill = I(fill)),
    position = "identity", data = bg
  ) +
  
  ### Places the lines
  ggborderline::geom_borderline(
    aes(x = cycle, y = value, group = name, color = I(base)),
    bordersize = 1.5, size = 3, bordercolour = "white"
  ) +
  
  ### Places the icons of the sources
  ggfx::with_outer_glow(
    x = nflplotR::geom_from_path(aes(x = x, y = y, path = image, height = height),
                                 vjust = 0, data = sources),
    colour = "white", sigma = 0, expand = 15
  ) +

  ### Places the icons of the duplicants
  ggfx::with_outer_glow(
    x = nflplotR::geom_from_path(aes(x = x, y = y, path = image, height = height),
                                 vjust = 0, data = dupli),
    colour = "white", sigma = 0, expand = 15
  ) +

  ### Places the sources names
  ggfx::with_outer_glow(
    x = geom_text(aes(x = x, y = y-5, label = label), family = font_title,
                  color = "white", size = 8, vjust = 1, data = sources),
    colour = "black", sigma = 0, expand = 10
  ) +

  ### Places the title and adds a black contour, followed by a shadow
  ggfx::with_outer_glow(
    x = ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                              label.colour = NA, fill = NA, colour = "white",
                              lineheight = lineheight, hjust = 0, vjust = 1,
                              family = font_title, data = title),
    colour = "black",
    sigma = 0,
    expand = 20,
    id = "titled"
  ) +
  ggfx::with_shadow(
    x = "titled",
    colour = "gray",
    x_offset = -10,
    y_offset = 10,
  ) +

  ### Places the subtitles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        label.colour = NA, fill = NA, colour = "black",
                        lineheight = lineheight, hjust = 0, vjust = 1,
                        family = font_body, data = subtitle) +

  ### Places the labels that inform in
  ### which cycle the duplicants arrived at the Colony
  ggtext::geom_richtext(aes(x = x, y = y, label = label), size = 5,
                        label.colour = NA, fill = NA, colour = "white",
                        lineheight = lineheight, vjust = 1,
                        family = font_body, data = arrival) +

  ### Places commentaries on the sources
  ggtext::geom_richtext(aes(x = x, y = y, label = label), size = 6,
                        label.colour = NA, fill = NA, colour = "black",
                        lineheight = lineheight, vjust = 1, hjust = 1,
                        family = font_body, data = comment) +
  
  ### Defines many axes elements
  scale_x_continuous(breaks = xbrks, name = "Cycle",
                     expand = expansion(mult = 0.03)) +
  scale_y_continuous(breaks = ybrks,
                     name = "Oxygen (kg)<br><span style='font-size:18px;'>PRODUCED AND CONSUMED</span>",
                     limits = c(0, 900), expand = expansion(mult = c(0.03, 0)),
                     sec.axis = dup_axis(name = NULL)) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    text = element_text(color = "white", family = font_title),
    
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(50, 50, 50, 50),
    
    axis.title = element_text(size = 35),
    axis.title.x = element_text(margin = margin(30,0,0,0)),
    axis.title.y = ggtext::element_markdown(margin = margin(0,30,0,0), angle = 90),
    axis.text = element_text(size = 20)
    
  )

## Saves the plot
ggsave("extras/oni/oxygen.png", plot = p, dpi = "retina",
       width = 35, height = 20)

