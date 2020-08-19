# Prairie strips CV figures
# B Van Deynze
# June 2020

# Prepare environment ====
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggthemes)
library(pracma)
library(here)

theme_set(theme_clean())
theme_update(
  legend.position = "bottom",
  legend.background = element_rect(color = NA),
  plot.background = element_rect(color= NA),
  
)


# Read data
df_supply_logit <- read_csv(here("data/predictedsupply_logit.csv"))
df_supply_logit

df_supply_probit <- read_csv(here("data/predictedsupply_probit.csv"))
df_supply_probit

df_cost2 <- read_csv(here("data/predictedcost_integration.csv"))

df_probadopt <- read_csv(here("data/predictedadopt.csv"))
df_probadopt # Too wide
df_probadopt <-
  df_probadopt %>%
  pivot_longer(
    starts_with("pr_"),
    names_to = c("weights", "model", "state"),
    names_prefix = c("pr_"),
    names_sep = c("_"),
    values_to = "pr",
    values_drop_na = FALSE
  ) %>%
  mutate(
    state = case_when(
      is.na(state) ~ "all",
      TRUE ~ state
    )
  ) %>%
  drop_na()
df_probadopt

# Summarize data
df_supply_logit <-
  df_supply_logit %>%
  group_by(
    offer
  ) %>%
  summarize(
    acres = sum(acres),
    cost = sum(cost)
  ) %>%
  mutate(
    state = "total"
  ) %>%
  bind_rows(
    df_supply_logit
  ) %>%
  drop_na()
df_supply_logit %>% print(n = Inf)
# df_supply_logit <-
#   df_supply_logit %>% 
#   add_row(offer = rep(0, 5), acres = c(183187, 66422, 33904, 52598, 30263), cost = rep(0, 5), state = c("total", "illinois", "indiana", "ohio", "michigan")) %>%
#   group_by(state) %>% 
#   arrange(state, offer) %>%
#   mutate(cost2 = cumtrapz(acres, offer))

df_supply_probit <-
  df_supply_probit %>%
  group_by(
    offer
  ) %>%
  summarize(
    acres = sum(acres),
    cost = sum(cost)
  ) %>%
  mutate(
    state = "total"
  ) %>%
  bind_rows(
    df_supply_probit
  ) %>%
  drop_na()
df_supply_probit %>% print(n = Inf)
# df_supply_probit <-
#   df_supply_probit %>% 
#   add_row(offer = rep(0, 5), acres = c(183187, 66422, 33904, 52598, 30263), cost = rep(0, 5), state = c("total", "illinois", "indiana", "ohio", "michigan")) %>%
#   group_by(state) %>% 
#   arrange(state, offer) %>%
#   mutate(cost2 = cumtrapz(acres, offer))


# Build draft figure ====
# Figure 1
(
  plot_fig1_logit<-
    ggplot(
      df_probadopt %>% filter(model == "logit", state == "all"),
      aes(
        x = pr, 
        y = offer, 
        # color = state
      )
    ) + 
    geom_line(
      aes(
        linetype = weights
      ),
      size = 0.75,
      # color = RColorBrewer::brewer.pal(3, "Set1")[2]
    ) +
    # scale_color_brewer(palette = "Set1", labels = str_to_sentence, name = NULL) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
    scale_linetype_discrete(labels = c("Unweighted", "Weighted"), name = "Weights", guide = guide_legend()) +
    labs(
      x = "Probability of Prairie Strip Contract Enrollment",
      y = "Payment Offer [USD/Acre]"
    ) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.title.y = element_text(margin = margin(1,10,1,1)),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
)
ggsave("figs/fig1.png", width = 6, height = 4, dpi = 600)

# Figure 2
(
  plot_fig2_logit <-
    ggplot(
      df_supply_logit,
      aes(
        x = acres, 
        y = offer,
        linetype = state,
        shape = state,
        # color = state
      )
    ) + 
    geom_line(
      aes(
        # linetype = I(state != "total") %>% as.character()
      ),
      size = 0.5
    ) +
    geom_point(
      aes(
        # shape = state
      ),
      size = 1.5,
      fill = "white"
    ) +
    # scale_color_brewer(palette = "Set1", labels = str_to_sentence, name = NULL) +
    scale_x_continuous(labels = scales::label_comma()) +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, 540), breaks = c(0, 100, 200, 300, 400, 500)) +
    scale_shape_manual(values = c(22, 21, 23, 24, 19), name = NULL, labels = str_to_sentence) +
    scale_linetype_manual(values = rev(1:5), name = NULL, labels = str_to_sentence) +
    # scale_linetype_discrete(breaks = c("solid", "dotted")) +
    labs(
      x = "Cropland Converted to Prairie Strips [Acres]",
      y = "Payment Offer [USD/Acre]"
    ) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.title.y = element_text(margin = margin(1,10,1,1)),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
)
ggsave("figs/fig2.png", width = 6, height = 4, dpi = 600)


# Figure 3
(
  plot_fig3_logit <-
    ggplot(df_cost2 %>% filter(mwta < 540)) + 
    geom_ribbon(
      aes(
        ymin = cost_lb, ymax = cost_ub, x = acres_add
      ),
      # fill = "red",
      fill = "grey50",
      alpha = 0.4,
      color = NA
    ) +
    geom_line(
      aes(
        linetype = "Targeted Rate Program",
        x = acres_add,
        y = cost_lb
      ),
      size = 1.2,
      # color = "#E41A1C"
    ) +
    geom_line(
      aes(
        linetype = "Single Rate Program",
        x = acres_add,
        y = cost_ub
      ),
      size = 1.2,
      # color = "#E41A1C"
    ) +
    # scale_color_brewer(palette = "Set1", labels = str_to_sentence, name = NULL, guide = NULL) +
    scale_x_continuous(labels = scales::label_comma(), breaks = 5e4*c(0:6)) +
    scale_y_continuous(labels = scales::label_dollar(scale = 0.000001), breaks = 2.5e7*c(0:6)) +
    scale_linetype_discrete(NULL) +
    labs(
      x = "Additional Cropland Converted to Prairie Strips [Acres]",
      y = "Annual Program Cost [Millions USD]"
    ) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.title.y = element_text(margin = margin(1,10,1,1)),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
)
ggsave("figs/fig3.png", width = 6, height = 4, dpi = 600)


# Build draft figure ====
# Figure 1a
(
  plot_fig1_probit<-
    ggplot(
      df_probadopt %>% filter(model == "probit", state == "all"),
      aes(
        x = pr, 
        y = offer, 
        # color = state
      )
    ) + 
    geom_line(
      aes(
        linetype = weights
      ),
      size = 1.5,
      # color = RColorBrewer::brewer.pal(3, "Set1")[2]
    ) +
    # scale_color_brewer(palette = "Set1", labels = str_to_sentence, name = NULL) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
    scale_linetype_discrete(labels = c("Unweighted", "Weighted"), name = "Weights", guide = guide_legend()) +
    labs(
      x = "Probability of Prairie Strip Contract Enrollment",
      y = "Payment Offer [USD/Acre]"
    ) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.title.y = element_text(margin = margin(1,10,1,1)),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
)
ggsave("figs/figa1.png", width = 6, height = 4, dpi = 600)


# Figure 2a
(
  plot_fig2_probit <-
    ggplot(
      df_supply_probit %>% filter(cost != 0),
      aes(
        x = acres, 
        y = offer, 
        # color = state
      )
    ) + 
    geom_line(
      aes(
        # linetype = I(state != "total") %>% as.character()
        linetype = state
      ),
      size = 1
    ) +
    geom_point(
      aes(
        shape = state
      ),
      size = 2,
      fill = "white"
    ) +
    # scale_color_brewer(palette = "Set1", labels = str_to_sentence, name = NULL) +
    scale_x_continuous(labels = scales::label_comma()) +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, 540), breaks = c(0, 100, 200, 300, 400, 500)) +
    scale_shape_manual(values = c(22, 21, 23, 24, 19), name = NULL, labels = str_to_sentence) +
    scale_linetype_manual(values = rev(1:5), name = NULL, labels = str_to_sentence) +
    # scale_linetype_discrete(breaks = c("solid", "dotted")) +
    labs(
      x = "Cropland Converted to Prairie Strips [Acres]",
      y = "Payment Offer [USD/Acre]"
    ) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.title.y = element_text(margin = margin(1,10,1,1)),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
)
ggsave("figs/figa2.png", width = 6, height = 4, dpi = 600)


# Figure 3a
(
  plot_fig3_probit <-
    ggplot(
      df_supply_probit %>% 
        filter(
          state == "total", 
          cost != 0
        ),
      aes(
        x = acres - min(acres), 
        # color = state
      )
    ) + 
    # geom_ribbon(
    #   aes(
    #     ymin = cost2, ymax = cost, fill = state
    #   ),
    #   alpha = 0.6,
    #   color = NA
    # ) +
    geom_line(
      aes(
        y = cost
      ),
      size = 1.2,
      linetype = "solid"
    ) +
    # geom_line(
    #   aes(
    #     y = cost2
    #   ),
    #   size = 1.2,
    #   linetype = "dashed"
    # ) +
    # scale_color_brewer(palette = "Set1", labels = str_to_sentence, name = NULL, guide = NULL) +
    scale_x_continuous(labels = scales::label_comma(), breaks = 5e4*c(0:8)) +
    scale_y_continuous(labels = scales::label_dollar(scale = 0.000001), breaks = 2.5e7*c(0:7)) +
    scale_linetype_discrete(breaks = c("solid", "dotted")) +
    labs(
      x = "Cropland Converted to Prairie Strips [Acres]",
      y = "Annual Program Cost [Millions USD]"
    ) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.title.y = element_text(margin = margin(1,10,1,1)),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
)

ggsave("figs/figa3.png", width = 6, height = 4, dpi = 600)

