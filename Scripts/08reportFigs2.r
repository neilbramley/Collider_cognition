###########################################################
########### Other plots not using functions  ##############
###########################################################

library(tidyverse)
library(ggnewscale) # Load these if you don't have them
library(here)
library(RColorBrewer)
library(ggplot2)


load(here('Data', 'modelData', 'fitforplot4par.rda')) # 288 of 31
source(here('Scripts', 'plotUtils.R'))
#load(here('Data', 'modelData', 'goOptim.rda')) can't load this cos it is also called df

cor(df$prop, df$full) # .876
cor.test(df$prop, df$full, method = "pearson")


# ------------- A version of the previous, Fig.5 in papar, stacked bar chart but not combining A and Au -------------

# ------------  Just 111, for compound decision AvB . Just pgroup 1, noKind -----------------

# Add a column called node2 for whether node3 starts with A, Au, B or Bu
df <- df |>
  mutate(
    node2 = case_when(
      str_starts(node3, "A=") ~ "A",
      str_starts(node3, "Au=") ~ "Au",
      str_starts(node3, "B=") ~ "B",
      str_starts(node3, "Bu=") ~ "Bu",
      TRUE ~ NA_character_
    )
  )

df6 <- df |>
  filter(
    trial_structure_type %in%
      c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"),
    pgroup == "A=.1,Au=.5,B=.8,Bu=.5"
  ) |>
  select(
    trial_id,
    pgroup,
    noKind,
    prop,
    trial_structure_type,
    Variable,
    node2,
    Actual,
    SE
  )

# Summarize mean proportions by group (A, B)
df_bar6 <- df6 |>
  group_by(trial_structure_type, Variable, node2) |>
  summarise(Participants = sum(prop), Model = sum(noKind), .groups = 'drop')


my_colors <- c(
  "A" = brewer.pal(9, "Oranges")[4],
  "Au" = brewer.pal(9, "Oranges")[7],
  "B" = brewer.pal(9, "Greens")[4],
  "Bu" = brewer.pal(9, "Greens")[7]
)


# NEW: Total summary for placing error bars
df_error6_total <- df6 |>
  group_by(trial_structure_type, Variable) |>
  summarise(
    mean_prop = sum(prop),
    se = sqrt(sum(SE^2)),
    .groups = "drop"
  )

# NEW: Total summary for model dots
df_model6_total <- df6 |>
  group_by(trial_structure_type, Variable) |>
  summarise(
    Model = sum(noKind),
    .groups = "drop"
  )

# The old version with model dots (afterwards we will do one with no model)
p111abf6 <- ggplot() +
  geom_col(
    data = df_bar6,
    aes(x = Variable, y = Participants, fill = node2),
    position = position_stack()
  ) +

  # ---- ERROR BARS (total response) ----
  geom_errorbar(
    data = df_error6_total,
    aes(
      x = Variable,
      ymin = mean_prop - se,
      ymax = mean_prop + se
    ),
    width = 0.25,
    size = 0.8
  ) +

  # ---- MODEL DOTS (total prediction) ----
  geom_point(
    data = df_model6_total,
    aes(
      x = Variable,
      y = Model,
      shape = "Model",
      color = "Model"
    ),
    size = 5
  ) +
  scale_fill_manual(values = my_colors, name = "Participants \nand variable") +
  #scale_fill_brewer(palette = "Spectral", name = "Participants \nand variable") +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_color_manual(name = "", values = c("Model" = "blue")) +

  labs(
    x = 'Response',
    y = 'Proportion/Prediction',
    title = 'A=.1,Au=.5,B=.8,Bu=.5'
  ) +

  facet_wrap(~trial_structure_type) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(vjust = 0.5, hjust = 1),
    legend.margin = margin(c(0, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(t = 1)),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 10)
  )


print(p111abf6)

ggsave(
  filename = "p111abnokst.pdf",
  plot = p111abf6,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)


# The old version with model dots (afterwards we will do one with no model)
p111abf6 <- ggplot() +
  geom_col(
    data = df_bar6,
    aes(x = Variable, y = Participants, fill = node2),
    position = position_stack()
  ) +

  # ---- ERROR BARS (total response) ----
  geom_errorbar(
    data = df_error6_total,
    aes(
      x = Variable,
      ymin = mean_prop - se,
      ymax = mean_prop + se
    ),
    width = 0.25,
    size = 0.8
  ) +

  # ---- MODEL DOTS (total prediction) ----
  # geom_point(
  #   data = df_model6_total,
  #   aes(
  #     x = Variable,
  #     y = Model,
  #     shape = "Model",
  #     color = "Model"
  #   ),
  #   size = 5
  # ) +
  scale_fill_manual(values = my_colors, name = "Participants \nand variable") +
  #scale_fill_brewer(palette = "Spectral", name = "Participants \nand variable") +
  #scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_color_manual(name = "", values = c("Model" = "blue")) +

  labs(
    x = 'Response',
    y = 'Proportion/Prediction',
    title = 'A=.1,Au=.5,B=.8,Bu=.5'
  ) +

  facet_wrap(~trial_structure_type) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(vjust = 0.5, hjust = 1),
    legend.margin = margin(c(0, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(t = 1)),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 10)
  )


# ------------ Version for Tadeg: could the ppt pattern be modeled by a model with just Inference, Kindness + ig?

# ------------- A version of the previous, Fig.5 in papar, stacked bar chart but not combining A and Au -------------

# ------------  Just 111, for compound decision AvB . Just pgroup 1, noKindnoSelect -----------------

df7 <- df |>
  filter(
    trial_structure_type %in%
      c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"),
    pgroup == "A=.1,Au=.5,B=.8,Bu=.5"
  ) |>
  select(
    trial_id,
    pgroup,
    noKindnoSelect,
    prop,
    trial_structure_type,
    Variable,
    node2,
    Actual,
    SE
  )

my_colors <- c(
  "A" = brewer.pal(9, "Oranges")[4],
  "Au" = brewer.pal(9, "Oranges")[7],
  "B" = brewer.pal(9, "Greens")[4],
  "Bu" = brewer.pal(9, "Greens")[7]
)


# Summaries for stacked bars (unchanged)
df_bar7 <- df7 |>
  group_by(trial_structure_type, Variable, node2) |>
  summarise(
    Participants = sum(prop),
    Model = sum(noKindnoSelect),
    .groups = 'drop'
  )

# NEW: Total summary for placing error bars
df_error7_total <- df7 |>
  group_by(trial_structure_type, Variable) |>
  summarise(
    mean_prop = sum(prop),
    se = sqrt(sum(SE^2)),
    .groups = "drop"
  )

# NEW: Total summary for model dots
df_model7_total <- df7 |>
  group_by(trial_structure_type, Variable) |>
  summarise(
    Model = sum(noKindnoSelect),
    .groups = "drop"
  )


p111abf7 <- ggplot() +
  geom_col(
    data = df_bar7,
    aes(x = Variable, y = Participants, fill = node2),
    position = position_stack()
  ) +

  # ---- ERROR BARS (total response) ----
  geom_errorbar(
    data = df_error7_total,
    aes(
      x = Variable,
      ymin = mean_prop - se,
      ymax = mean_prop + se
    ),
    width = 0.25,
    size = 0.8
  ) +

  # ---- MODEL DOTS (total prediction) ----
  geom_point(
    data = df_model7_total,
    aes(
      x = Variable,
      y = Model,
      shape = "Model",
      color = "Model"
    ),
    size = 5
  ) +
  scale_fill_manual(values = my_colors, name = "Participants \nand variable") +
  #scale_fill_brewer(palette = "Spectral", name = "Participants \nand variable") +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_color_manual(name = "", values = c("Model" = "blue")) +

  labs(
    x = 'Response',
    y = 'Proportion/Prediction',
    title = 'A=.1,Au=.5,B=.8,Bu=.5'
  ) +

  facet_wrap(~trial_structure_type) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(vjust = 0.5, hjust = 1),
    legend.margin = margin(c(0, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(t = 1)),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 10)
  )


print(p111abf7)

ggsave(
  filename = "p111abnokst3.pdf",
  plot = p111abf6,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)


# ----------- Combine the two -------- keep this separate til we decide how to present

df6 <- df |>
  filter(
    trial_structure_type %in%
      c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"),
    pgroup == "A=.1,Au=.5,B=.8,Bu=.5"
  ) |>
  select(
    trial_id,
    pgroup,
    noKind,
    prop,
    trial_structure_type,
    Variable,
    node2,
    Actual,
    SE
  )

# Summarize mean proportions by group (A, B)
df_bar6 <- df6 |>
  group_by(trial_structure_type, Variable, node2) |>
  summarise(Participants = sum(prop), Model = sum(noKind), .groups = 'drop')


my_colors <- c(
  "A" = brewer.pal(9, "Oranges")[4],
  "Au" = brewer.pal(9, "Oranges")[7],
  "B" = brewer.pal(9, "Greens")[4],
  "Bu" = brewer.pal(9, "Greens")[7]
)


# NEW: Total summary for placing error bars
df_error6_total <- df6 |>
  group_by(trial_structure_type, Variable) |>
  summarise(
    mean_prop = sum(prop),
    se = sqrt(sum(SE^2)),
    .groups = "drop"
  )

# NEW: Total summary for model dots
# df_model6_total <- df6 |>
#   group_by(trial_structure_type, Variable) |>
#   summarise(
#     Model = sum(noKind),
#     .groups = "drop"
#   )

df_model_total <- df |>
  filter(
    trial_structure_type %in%
      c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"),
    pgroup == "A=.1,Au=.5,B=.8,Bu=.5"
  ) |>
  group_by(trial_structure_type, Variable) |>
  summarise(
    noKind = sum(noKind),
    noKindnoSelect = sum(noKindnoSelect),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(noKind, noKindnoSelect),
    names_to = "Model",
    values_to = "Prediction"
  )


p111abf6 <- ggplot() +
  geom_col(
    data = df_bar6,
    aes(x = Variable, y = Participants, fill = node2),
    position = position_stack()
  ) +

  # ---- ERROR BARS (total response) ----
  geom_errorbar(
    data = df_error6_total,
    aes(
      x = Variable,
      ymin = mean_prop - se,
      ymax = mean_prop + se
    ),
    width = 0.25,
    size = 0.8
  ) +

  # ---- MODEL DOTS (total prediction) ----
  geom_point(
    data = df_model_total,
    aes(
      x = Variable,
      y = Prediction,
      shape = Model,
      color = Model
    ),
    size = 5,
    position = position_dodge(width = 0.25)
  ) +
  scale_fill_manual(values = my_colors, name = "Participants \nand variable") +
  #scale_fill_brewer(palette = "Spectral", name = "Participants \nand variable") +
  scale_shape_manual(
    values = c(noKind = 16, noKindnoSelect = 17)
  ) +
  scale_color_manual(
    values = c(noKind = "blue", noKindnoSelect = "red")
  ) +

  labs(
    x = 'Response',
    y = 'Proportion/Prediction',
    title = 'A=.1,Au=.5,B=.8,Bu=.5'
  ) +

  facet_wrap(~trial_structure_type) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(vjust = 0.5, hjust = 1),
    legend.margin = margin(c(0, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(t = 1)),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 10)
  )


print(p111abf6)

ggsave(
  filename = "p111abnokst2.pdf",
  plot = p111abf6,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)
