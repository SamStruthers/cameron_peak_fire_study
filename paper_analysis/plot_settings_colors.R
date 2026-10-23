# Create the lookup table with expression syntax
# Define as a named list
chem_expressions <- list(
  "Turbidity" = expression(Turbidity ~ (NTU)),
  "TSS"       = expression(TSS ~ (mg ~ L^{-1})),
  "ChlA"      = expression(Chlorophyll ~ italic("a") ~ (mu*g ~ L^{-1})),
  "TC"        = expression(TC ~ (mg ~ L^{-1})),
  "TN"        = expression(TN ~ (mg ~ L^{-1})),
  "DOC"       = expression(DOC ~ (mg ~ L^{-1})),
  "DTN"       = expression(TDN ~ (mg ~ L^{-1})),
  "pH"        = expression(pH),
  "ANC"       = expression(ANC ~ (mu*eq ~ L^{-1})),
  "SC"        = expression(SC ~ (mu*S ~ cm^{-1})),
  "Na"        = expression(Na^{"+"} ~ (mg ~ L^{-1})),
  "NH4"       = expression(NH[4]^{"+"} ~ (mg ~ L^{-1})),
  "K"         = expression(K^{"+"} ~ (mg ~ L^{-1})),
  "Mg"        = expression(Mg^{"2+"} ~ (mg ~ L^{-1})),
  "Ca"        = expression(Ca^{"2+"} ~ (mg ~ L^{-1})),
  "F"         = expression(F^{"-"} ~ (mg ~ L^{-1})),
  "Cl"        = expression(Cl^{"-"} ~ (mg ~ L^{-1})),
  "NO3"       = expression(NO[3]^{"-"} ~ (mg ~ L^{-1})),
  "PO4"       = expression(PO[4]^{"3-"} ~ (mg ~ L^{-1})),
  "SO4"       = expression(SO[4]^{"2-"} ~ (mg ~ L^{-1})), 
  "N_NO3" = expression(NO[3]^'-'-N~(mg~L^-1))
)
#Color for reservoirs, matches burn severity by site code

colorsBS_site_code <- c("BRNR" = "#d71a1d", "PTRR" ="#F3722C", "COMR" = "#F8961E", "CBRR" = "#F9C74F", "HORR" = "#F0E442", "JOER" = "#56B4E9", "LNGR" = "#0072B2")
#Seasonal Color values
season_clp_color_vals <- c("Winter" = "#03045e",  "Rising Limb" = '#047E82',  "Peak Flow" = '#59B851', "Falling Limb"='#FA850E', "Fall Baseflow" = "#772f1a")

color_burned <- c("unburned" = "#59B851", "burned" = "#d71a1d")
#ROSS theme plotting settings
if ({
  require(systemfonts)
  ("Roboto" %in% system_fonts()$family)
}) {
  ROSS_theme <- theme_bw() + #or theme_few()
    theme(plot.title = element_text(hjust = 0.5, face = 'bold', family = "Roboto"),
          plot.subtitle = element_text(hjust = 0.5, family = "Roboto"))
} else {
  message("You do not have the Roboto font family installed on your computer, currenly using ggplot default text family.
          See ROSS_themes.R for directions to install the font family on your computer.")
  ROSS_theme <- theme_bw() + #or theme_few()
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5))
}

#ROSS Colors

ROSS_lt_pal <- c("#002EA3", "#E70870", "#256BF5",
                 "#745CFB", "#1E4D2B", "#56104E")

ROSS_dk_pal <- c("#1E4D2B", "#E70870", "#256BF5",
                 "#FFCA3A", "#745CFB", "#C3F73A")

ROSS_acc_pal <- c("#56104E", "#745CFB", "#FFCA3A", "#1E4D2B")