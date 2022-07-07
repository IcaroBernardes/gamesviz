# 0. Library and fonts management
library(tidyverse)
library(readxl)
library(ragg)
library(systemfonts)
library(junebug)
library(scales)
library(glue)
library(stringr)
library(colorspace)
library(ggpath)
library(ggtext)

## Defines some layout constants
lineheight <- 1.2 ### Height of linebreaks
width <- 15 ### Image width
height <- 60 ### Image height
left_clr <- "#212C33" ### Color of left background and glyph

## Makes special styled fonts available to R (e.g.: Medium, Solid, etc)
### Lists fonts visible to {systemfonts}
fonts_list <- systemfonts::system_fonts()

### Takes all font styles that share that exact family name and
### registers them (makes them visible to {systemfonts})
junebug::font_hoist("Futura Condensed")
junebug::font_hoist("Futura-Condensed-Bold")
junebug::font_hoist("Font Awesome 6 Free")

### Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

## Defines the fonts 
font_title <- "Planewalker"
font_body <- "Futura Condensed Regular"
font_body_bold <- "Futura-Condensed-Bold Regular"
font_decor <- "Font Awesome 6 Free Solid"

# 1. Data download, load and handling
df1 <- readxl::read_excel("2022/05/data.xlsx", sheet = "df1")
df2 <- readxl::read_excel("2022/05/data.xlsx", sheet = "df2")
df3 <- readxl::read_excel("2022/05/data.xlsx", sheet = "df3")
df4 <- readxl::read_excel("2022/05/data.xlsx", sheet = "df4")
df5 <- readxl::read_excel("2022/05/data.xlsx", sheet = "df5")

## Adds path to the images of the races
details_races <- df1 %>% 
  dplyr::mutate(path = stringr::str_replace_all(race, "[:space:]", "_"),
                path = tolower(path),
                path = glue::glue("2022/05/images/{path}.png"))

## Inserts breaks in the "about" text
details_races <- details_races %>%
  dplyr::mutate(breaker = stringr::str_length(about),
                breaker = scales::rescale(breaker, to = c(72,91)),
                breaker = round(breaker)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(about = stringr::str_wrap(about, width = breaker)) %>% 
  dplyr::ungroup()

## Makes a lower case version of the names of the races
details_races <- details_races %>% dplyr::mutate(label = tolower(race))

## Defines coordinates for the name of the races and "about" text
details_races <- details_races %>% 
  dplyr::mutate(xr = -1, yr = 0.47,
                xa = -1, ya = 0.35)

## Defines coordinates for the description of the characteristics of the races
char_races <- df5 %>%
  dplyr::left_join(df4) %>% 
  dplyr::mutate(x = -1, y = 0.29,
                label = glue::glue("Dragonborn power: {power} - {effect}\nRace effect: {buff}"))

## Defines coordinates for the skill images
skill_images <- tibble(
  skill = c("Illusion", "Conjuration", "Destruction", "Restoration", "Alteration", "Enchanting",
            "Smithing", "Heavy Armor", "Block", "Two-Handed", "One-Handed", "Archery",
            "Light Armor", "Sneak", "Lockpicking", "Pickpocket", "Speech", "Alchemy"),
  x = rep(seq(-1, 0.1, length.out = 6), 3),
  y = c(rep(0.20,6), rep(0.14,6), rep(0.08,6)),
  path = glue::glue("2022/05/images/{tolower(skill)}.png"),
  color = c(rep("#00AEFF",6), rep("#FF3300",6), rep("#59FF00",6))
)

## Unites the data about skill images and defines color for them
skill_images <- df2 %>%
  dplyr::left_join(skill_images) %>% 
  dplyr::mutate(scaled = case_when(value == 25 ~ 0,
                                   value == 20 ~ 0.45,
                                   value == 15 ~ 0.9,
                                   TRUE ~ 2),
                color = colorspace::desaturate(color, amount = scaled),
                color = colorspace::darken(color, amount = scaled))

## Defines coordinates for the spell images
spells_images <- tibble(
  spell = c("Healing", "Flames", "Fury", "Conjure Familiar", "Sparks"),
  x = seq(-1, -0.12, length.out = 5),
  y = 0.01
)

## Adds a dummy for color scaling and reshapes the data
spells_images <- df3 %>% 
  dplyr::mutate(dummy = 1) %>% 
  tidyr::complete(race, spell) %>% 
  dplyr::mutate(color = ifelse(is.na(dummy), "#1A2533", NA)) %>% 
  dplyr::mutate(path = glue::glue("2022/05/images/{tolower(spell)}.png")) %>% 
  dplyr::left_join(spells_images)

## Define coordinates for the glyph
n_glyphs <- 40
dragon_border <- tibble(
  x = c(rep(0.504, n_glyphs), rep(0.499, n_glyphs)),
  y = rep(seq(0.995, 0.015, length.out = n_glyphs), 2) - c(rep(0, n_glyphs), rep(0.012, n_glyphs)),
  label = c(rep("\uf6d5", n_glyphs), rep("\uf06d", n_glyphs)),
  angle = c(rep(-90, n_glyphs), rep(0, n_glyphs)),
  size = c(rep(10, n_glyphs), rep(7, n_glyphs))
)

## Defines coordinates for the texts in the left
left_text <- tibble(
  x = 0.04,
  y = c(0.925, 0.825, 0.745,
        0.665, 0.610, 0.470,
        0.330, 0.190, 0.120,
        0.020),
  size = c(rep(9, 4), rep(14, 3), 9, 9, 8),
  family = c(rep(font_body, 4), rep(font_body_bold, 3), font_body, font_body, font_body_bold),
  label = c(
    "The Elder Scrolls V takes place in Skyrim, north of the Tamriel continent.
     The continent homes all ten playable races in the game.",
    
    "The cards on the right show the races names, appearance, description
    and abilities. Some of these abilities are unique powers associated with each
    race. Examples are the Magicka affinity of the High Elf, the luck for gold of
    the Imperial or the animal allegiance to the Wood Elf.",
    
    "Other abilites are spells and skills bonuses. Each race has a starting set,
    but a player may acquire any spells and develop any skills as the game
    progresses. This means that even if a player chooses a race akin to
    melee combat they still may become a berserker mage or a sneaky boxer!",
    
    "In truth, freedom is the strongest point of Skyrim. Many elements
    of the game help the player to get into role-playing. One of them is the
    skills system. Each skill becomes more powerful as the player uses it.
    There are 18 skills in Skyrim which are divided between three sigils.",
    
    "The Mage",
    
    "The Warrior",
    
    "The Thief",
    
    "The cards on the right show the initial skill level given to each race.
    Bright symbols represent a start at level 25. The desaturated ones, level 20.
    Those barely visible, level 15.",
    
    "Finally, on the right, the initial spells are shown. Those available for a
    given race are shown in bright colors.",
    
    "Data from The Elder Scrolls Wiki | Images from Bethesda | Graphic by Ícaro Bernardes (@IcaroBSC)"
    
  )
)

## Defines coordinates for the illustrative glyph
ex_glyphs <- tibble(
  x = rep(c(0.115,0.240,0.365), 2),
  y = c(rep(0.765, 3), rep(0.685, 3)),
  label = c("\ue05d","\uf51e","\ue573",
            "\ue4dc","\uf6e8","\uf504")
)

## Defines coordinates for the skill description
skill_texts <- tibble(
  x = 0.12,
  y = c(
    seq(0.59, 0.49, length.out = 6),
    seq(0.45, 0.35, length.out = 6),
    seq(0.31, 0.21, length.out = 6)
  ),
  size = 10,
  text = c(
    "Spellcasting that manipulates perception",
    "Spellcasting that summons entities and weapons",
    "Spellcasting that damages enemies",
    "Spellcasting that protects and heals",
    "Spellcasting that detects and transmutates",
    "Adding effects to equipment",
    
    "Refining weapons and armor with raw materials",
    "Enhancing bonuses of heavy armor",
    "Enhancing bonuses of blocking weapons",
    "Enhancing bonuses of two-handed weapons",
    "Enhancing bonuses of one-handed weapons",
    "Enhancing bonuses of bows",
    
    "Enhancing bonuses of light armor",
    "Moving and attacking stealthily",
    "Increasing bonuses at lockpicking",
    "Increasing bonuses at pickpocketing",
    "Increasing bonuses at conversations",
    "Creating potions and poisons"
  ),
  skill = c("Illusion","Conjuration","Destruction","Restoration","Alteration","Enchanting",
            "Smithing","Heavy Armor","Block","Two-Handed","One-Handed","Archery",
            "Light Armor","Sneak","Lockpicking","Pickpocket","Speech","Alchemy")
) %>% 
  dplyr::mutate(label = glue::glue("{skill} - {text}"),
                breaker = stringr::str_length(label),
                breaker = scales::rescale(breaker, to = c(26,35)),
                breaker = round(breaker)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(label = stringr::str_wrap(label, width = breaker),
                label = stringr::str_replace_all(label, "\n", "<br>")) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    styled = glue::glue("<span style='font-family:\"{font_body_bold}\";'>{skill}</span>"),
    label = stringr::str_replace(label, skill, styled),
    path = glue::glue("2022/05/images/{tolower(skill)}.png")
  )

## Defines coordinates for the skill legends
skill_legends <- skill_images %>% 
  dplyr::filter(skill == "Conjuration") %>% 
  dplyr::arrange(desc(value)) %>% 
  dplyr::distinct(path, color, value) %>% 
  dplyr::mutate(y = 0.15,
                x = c(0.115,0.240,0.365))

## Defines coordinates for the spell legends
spells_legends <- tibble(
  spell = c("Healing", "Flames", "Fury", "Conjure Familiar", "Sparks"),
  x = 0.04,
  y = seq(0.088, 0.033, length.out = 5),
  text = c(
    "Heals the caster",
    "Harms Health",
    "Makes everyone fight",
    "Summons a familiar",
    "Harms Health and Magicka"
  )
) %>% 
  dplyr::mutate(
    path = glue::glue("2022/05/images/{tolower(spell)}.png"),
    styled = glue::glue("<span style='font-family:\"{font_body_bold}\";'>{spell}</span>"),
    label = glue::glue("{styled} - {text}")
  )

## Makes the plot with the data of the races
races <- details_races %>% 
  ggplot() +
  
  ### Places the image of the races
  ggpath::geom_from_path(aes(x = 1, y = -0.02, path = path),
                         hjust = 1, vjust = 0, width = 0.5, height = 1.1) +
  
  ### Places the name of the races
  geom_text(aes(x = xr, y = yr, label = label), color = "white", size = 30,
            hjust = 0, vjust = 1, family = font_title) +
  
  ### Places the text about the races
  geom_text(aes(x = xa, y = ya, label = about), color = "white", size = 4,
            hjust = 0, vjust = 1, family = font_body, lineheight = lineheight) +
  
  ### Places the description of the characteristics of the races
  geom_text(aes(x = x, y = y, label = label), color = "white", size = 2.5,
            hjust = 0, vjust = 1, family = font_body, data = char_races) +
  
  ### Places the icons for the skills
  ggpath::geom_from_path(aes(x = x, y = y, path = path, colour = I(color)),
                         vjust = 0, hjust = 0, width = 0.1, height = 0.1,
                         data = skill_images) +
  
  ### Places the image of the spells
  ggpath::geom_from_path(aes(x = x, y = y, path = path, color = I(color)),
                         hjust = 0, vjust = 0, width = 0.1, height = 0.1,
                         data = spells_images) +
  
  ### Defines limits for the plot
  coord_cartesian(xlim = c(-1,1), ylim = c(0,0.5), expand = FALSE) +
  
  ### Facets by race
  facet_grid(race ~ .) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    strip.text = element_blank()
  )

## Creates the main plot
p <- ggplot(NULL) +
  
  ### Places the gray background in the left
  annotate("rect", xmin = -Inf, xmax = 0.49,
           ymin = -Inf, ymax = Inf,
           fill = left_clr, color = NA) +
  
  ### Places the decorative glyph in the background border
  geom_text(aes(x = x, y = y, label = label, angle = angle, size = I(size)),
            color = left_clr, hjust = 0, family = font_decor,
            data = dragon_border) +
  
  ### Places the title
  annotate("text", x = 0.04, y = 0.99, label = "Faces of\nTamriel", size = 47,
           color = "white", family = font_title, hjust = 0, vjust = 1, lineheight = 0.8) +
  
  ### Places the texts in the left
  ggtext::geom_textbox(aes(x = x, y = y, label = label,
                           size = I(size), family = family),
                       fill = NA, box.colour = NA, color = "white",
                       box.padding = unit(rep(0,4), "pt"),
                       width = unit(0.4, "npc"), lineheight = lineheight,
                       hjust = 0, vjust = 1, data = left_text) +
  
  ### Places the Tamriel map
  ggpath::geom_from_path(aes(x = 0.245, y = 0.865, path = "2022/05/images/tamriel.png"),
                         width = 0.38, height = 0.3) +
  
  ### Places the illustrative glyph
  geom_text(aes(x = x, y = y, label = label), color = "white",
            family = font_decor, size = 27, data = ex_glyphs) +
  
  ### Places the skill icons
  ggpath::geom_from_path(aes(x = 0.04, y = y, path = path), hjust = 0,
                         width = 0.06, height = 0.06, data = skill_texts) +
  
  ### Places the skill descriptions
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        fill = NA, label.colour = NA, color = "white",
                        lineheight = lineheight, family = font_body,
                        hjust = 0, data = skill_texts) +
  
  ### Places the skill legends
  ggpath::geom_from_path(aes(x = x, y = y, path = path, color = I(color)),
                         vjust = 1, width = 0.1, height = 0.025,
                         data = skill_legends) +
  
  ### Places the texts of the skill legends
  geom_text(aes(x = x, y = y, label = value), color = "white", vjust = 0,
            family = font_body_bold, size = 14, data = skill_legends) +
  
  ### Places the spells legends
  ggpath::geom_from_path(aes(x = x, y = y, path = path),
                         hjust = 0, width = 0.05, height = 0.05,
                         data = spells_legends) +
  
  ### Places the texts of the spells legends
  ggtext::geom_richtext(aes(x = x, y = y, label = label), hjust = 0, size = 9,
                        color = "white", label.colour = NA, fill = NA,
                        family = font_body, nudge_x = 0.05,
                        data = spells_legends) +
  
  ### Places the plot
  cowplot::draw_plot(races,
                     x = 1, y = 0,
                     width = 0.45, height = 0.997,
                     hjust = 1, vjust = 0,
                     halign = 1, valign = 0) +
  
  ### Defines limits for the plot
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(0,0,0,0, "pt")
  )

## Saves the plot
ggsave("2022/05/skyrim.png", plot = p, dpi = "retina", limitsize = FALSE,
       width = width, height = height, device = ragg::agg_png, res = 320)

