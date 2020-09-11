library(tidyverse)
library(magrittr)
library(sf)
library(raster)

source("gen_rand_points.R")

sp <- gen_random_points(500)

delta <- list()
N <- list()
Q <- seq(-100, 100, length.out = 21)
par(mfrow = c(3, 2), mar = c(0, 0, 0, 0))
dev.off()
plot(c(-100, 100), c(-600, 600), col = NA)
for (i in 1:50) {
  grid <- st_make_grid(sp, cellsize = 1 / i)
  delta[[i]] <- 1 / i
  N[[i]] <- length(grid)
  p <- st_intersects(grid, sp) %>% sapply(function(x) {length(x)})
  chi <- list()
  for (q in Q) {
    chi[[which(q == Q)]] <- sum((p / 500)^q)
  }
  points(Q, log(unlist(chi)), pch = 20, col = rgb(0.2^(1/i), 0, 0.9^i))
  lines(Q, log(unlist(chi)), col = rgb(0.2^(1/i), 0, 0.9^i))
  if (length(grid) == nrow(sp)) {
    break
  }
}

ans <- tibble(delta = unlist(delta),
              N = unlist(N),
              P = unlist(dist))

ans %>% 
  filter(delta > 0.03) %>% 
  ggplot() +
  geom_point(aes(delta, N)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  xlab("e") +
  ylab("N(e)")

ans %>% 
  filter(delta )

# Different area extent ------
#' Here I simulate the occurrence of four species for increasing areas.
A <- seq(10, 100, length.out = 11)
Q <- seq(-10, 10, by = 1)
P <- c(0.1, 0.15, 0.15, 0.2)^2
ans <- tibble(A = NA, Q = NA, Chi = NA)
sps <- list()

for (i in 1:length(A)) {
  sp <- stack(
    raster(matrix(runif(A[i]^2) > (1 - P[1])^i, A[i], A[i])),
    raster(matrix(runif(A[i]^2) > (1 - P[2])^i, A[i], A[i])),
    raster(matrix(runif(A[i]^2) > (1 - P[3])^i, A[i], A[i])),
    raster(matrix(runif(A[i]^2) > (1 - P[4])^i, A[i], A[i]))
    # raster(matrix((runif(A[i]^2) > 1 - P[1]) * as.integer(runif(A[i]^2, 0, 10)), A[i], A[i])),
    # raster(matrix((runif(A[i]^2) > 1 - P[3]) * as.integer(runif(A[i]^2, 0, 10)), A[i], A[i])),
    # raster(matrix((runif(A[i]^2) > 1 - P[2]) * as.integer(runif(A[i]^2, 0, 10)), A[i], A[i])),
    # raster(matrix((runif(A[i]^2) > 1 - P[4]) * as.integer(runif(A[i]^2, 0, 10)), A[i], A[i]))
  )
  names(sp) <- c("sp1", "sp2", "sp3", "sp4")
  sps[[i]] <- sp
  sp_sum <- sum(sp)
  N <- sum(values(sp), na.rm = TRUE)
  for (q in Q) {
    p <- rep(NA, 4)
    for (j in 1:dim(sp)[3]) {
      p[j] <- (sum(values(sp[[j]]) > 0) / N)
    }
    chi <- sum(p^q)
    ans %<>% bind_rows(tibble(A = A[i]^2,
                              Q = q,
                              Chi = chi))
  }
}

par(mfrow = c(2, 2))
for (i in c(1, 11)) {
  plot(sum(sps[[i]]), 
       axes = FALSE, 
       box = FALSE,
       col = topo.colors(50))
  plot(sum(sps[[i]]) > 0, 
       axes = FALSE, 
       box = FALSE)
}

ans %>% 
  filter(!is.na(A)) %>% 
  ggplot() +
  geom_point(aes(A, Chi, col = Q)) +
  geom_smooth(aes(A, Chi, col = Q, group = Q), 
              method = "lm", alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_gradient(low = "steelblue", high = "tomato") +
  theme_minimal()

z <- ans %>% 
  filter(!is.na(A),
         Chi != Inf,
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
  geom_smooth() +
  theme_minimal()

