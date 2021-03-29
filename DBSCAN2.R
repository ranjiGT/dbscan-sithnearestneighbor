library(readr)
library(dplyr)
library(stringr)
library(forcats)
library(tibble)
library(tidyr)
library(ggplot2)
library(purrr)
student <- read_csv("clustering-student-mat.csv")
k <- 6
K
# Create Distance Matrix (dm)
dm <- student %>% dist() %>% as.matrix() %>% as_tibble()
# Convert distance matrix so that all distances are stored in one variable
# For each instance select the `k` nearest distance and sort data frame by distance
# `ll`: Calculate straight between smallest and largest 6-dist
dm <- dm %>%
  mutate(id = row_number()) %>%
  gather(id2, dist, -id) %>%
  group_by(id) %>%
  arrange(dist) %>%
  slice(k) %>%
  ungroup() %>%
  arrange(dist) %>%
  mutate(no = row_number()) %>%
  mutate(ll = dist[1] + (no-1)*((dist[n()] - dist[1])/n()))
#-------------------------------------------
dm_opt <- dm %>%
  mutate(lldist = ll - dist) %>%
  arrange(-lldist) %>%
  slice(1)
#----------------------------------------
ggplot(dm, aes(x = no, y = dist)) +
  geom_line() +
  geom_line(aes(y = ll), linetype = 2) +
  geom_vline(xintercept = dm_opt$no, linetype = 3) +
  geom_hline(yintercept = dm_opt$dist, linetype = 3) +
  labs(x = str_c("Points sorted by distance to ", k, "th nearest neighbor"),
       y = str_c(k, "th nearest neighbor distance"))
#--------------------------------------------------------
minPts <- k
eps <- dm_opt$dist[1]
library(dbscan)
clu <- dbscan(student, minPts = minPts, eps = eps)
student_clu <- student %>%
  bind_cols(., tibble(Cluster = clu$cluster)) %>%
  mutate(Cluster = factor(Cluster)) %>%
  mutate(Cluster = fct_recode(Cluster, "Noise" = "0"))
student_hull <- student_clu %>%
  split(.$Cluster) %>%
  purrr::map(~ slice(., chull(.$Exam1, .$Exam2))) %>%
  do.call("rbind", .)
#-----------------------------------
ggplot(student_clu, aes(Exam1, Exam2, color = Cluster, fill = Cluster))+
  geom_polygon(data = student_hull %>% filter(!Cluster == "Noise"), alpha = .5, color = "black")+geom_point(pch = 21)+
  scale_fill_discrete(drop = F)+
  scale_color_discrete(drop = F)