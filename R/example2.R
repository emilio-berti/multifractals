library(tidyverse)
library(sf)
library(foreach)


data <- read_delim("../Poaceae.csv", delim = "\t") %>% 
  mutate(Species = modify(scientificName, function(x) {
    paste(str_split(x, " ", simplify = TRUE)[1],
          str_split(x, " ", simplify = TRUE)[2])
  }))

sp <- data %>% 
  filter(genus == "Poa")

sp %<>% 
  filter(countryCode == "DE",
         grepl("Poa", Species)) %>% 
  mutate(geometry = map2(decimalLongitude, decimalLatitude, function(x, y) {
    st_point(c(x, y))
  })) %>% 
  st_as_sf() 
  # st_crop(st_bbox(c(xmin = -125, xmax = -67, ymin = 23, ymax = 50))) #continental US

e <- st_bbox(sp)

loc <- tibble(x = runif(1, e["xmin"] + 1, e["xmax"] - 1),
              y = runif(1, e["ymin"] + 1, e["ymax"] - 1)) %>% 
  mutate(geometry = map2(x, y, function(x, y) {
    st_point(c(x, y))
  })) %>% 
  st_as_sf()

plot(sp$geometry, pch = 20)
plot(loc$geometry, add = TRUE, pch = 20, col = "tomato")

D <- seq(0.5, 2.5, by = 0.25)
Q <- seq(-10, 10)
prg <- txtProgressBar(style = 3)
foreach (d = D, .combine = "rbind") %do% {
  setTxtProgressBar(prg, which(D == d) / length(D))
  foreach (q = Q, .combine = "rbind") %do% {
    loc <- tibble(x = runif(1, e["xmin"] + 1, e["xmax"] - 1),
                  y = runif(1, e["ymin"] + 1, e["ymax"] - 1)) %>% 
      mutate(geometry = map2(x, y, function(x, y) {
        st_point(c(x, y))
      })) %>% 
      st_as_sf()
    buff <- st_buffer(loc, d)
    p <- st_intersection(sp, buff)
    p %<>% 
      group_by(Species) %>% 
      tally() %>% 
      st_drop_geometry()
    N <- sum(p$n)
    p %<>% mutate(N = N, p = n / N)
    chi <- sum(p$p^q)
    tibble(D = d, 
           Q = q,
           Chi = chi)
  }
} -> sim

sim %>% 
  mutate(A = pi * D^2) %>% 
  filter(Q != 1) %>% 
  ggplot() +
  geom_point(aes(A, Chi, col = Q)) +
  geom_smooth(aes(A, Chi, col = Q, group = Q), 
              method = "lm",
              alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_gradient(low = "steelblue", high = "tomato") +
  theme_minimal()

z <- sim %>% 
  mutate(A = pi * D^2) %>% 
  filter(Q != 1,
         Chi > 0) %>% 
  group_by(Q) %>% 
  group_modify(~ broom::tidy(lm(log10(Chi) ~ log10(A), data = .))) %>%
  filter(term == "log10(A)") %>%
  ungroup() 

z %<>% 
  transmute(Q, 
            tau = estimate,
            z = tau / (1 - Q)) %>%
  filter(Q != 1)

z %>% 
  ggplot() +
  aes(Q, z) +
  geom_point() +
  geom_smooth(alpha = 0.2, span = 1) +
  theme_minimal()

