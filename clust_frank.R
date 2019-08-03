library(imager)
library(tidyverse)
library(purrr)

file="img/frankenstein.jpg"

# Load, convert to grayscale, filter image (to convert it to bw) and sample
load.image(file) %>% 
  grayscale() %>% 
  threshold("45%") %>% 
  as.cimg() %>% 
  as.data.frame() -> franky


clustResultx <- function(x) {
  clustCut <- tibble(cluster_id = cutree(clusters, x)) %>% bind_cols(data)
  clustCut %>% group_by(cluster_id) %>% 
    summarize(size = n()) %>% 
    filter(size > 1) %>% 
    select(cluster_id) %>% 
    inner_join(clustCut, by = "cluster_id") -> clustCut
  return(clustCut)
  }

add_segments <- function(x){
  
  df1 <- clustEvol[[x]]
  df0 <- clustEvol[[x-1]]
  
  new_points <- anti_join(df1, df0, by = "id")
  
  # Este cruce es para nuevos puntos que se unen a algun cluster existente
  new_points %>% 
    inner_join(df1, by = "cluster_id", suffix = c(".1", ".2")) %>% 
    filter(id.1 != id.2) %>% 
    mutate(d = sqrt((x.1 - x.2)^2 + (y.1 - y.2)^2)) %>% 
    group_by(id.1) %>% 
    arrange(d) %>% 
    slice(1) %>% 
    select(p1 = id.1, p2 = id.2) %>% 
    ungroup -> new_segments1
  
  # Este cruce es para nuevos cluster (de dos puntos)
  new_points %>% anti_join(bind_rows(select(new_segments1, id = p1), 
                                     select(new_segments1, id = p2)), by = "id") %>% 
    group_by(cluster_id) %>% 
    ungroup -> unpaired_points
  
  unpaired_points %>% inner_join(unpaired_points, by = "cluster_id", suffix = c(".1", ".2")) %>% 
    filter(id.1 < id.2) %>% 
    select(p1 = id.1, p2 = id.2) -> new_segments2
  
  # Este cruce es para clusters que se unen entre si
  
  new_points <- anti_join(df1, df0, by = c("id", "cluster_id"))
  
  new_points %>% 
    inner_join(df1, by = "cluster_id", suffix = c(".1", ".2")) %>% 
    filter(id.1 != id.2) %>% 
    anti_join(new_points, by = c("id.2" = "id")) %>% 
    mutate(d = sqrt((x.1 - x.2)^2 + (y.1 - y.2)^2)) %>% 
    arrange(d) %>% 
    slice(1) %>% 
    select(p1 = id.1, p2 = id.2) %>% 
    ungroup -> new_segments3

  bind_rows(new_segments1, new_segments2, new_segments3)
}

n <- 2500

franky %>% 
  sample_n(n, weight=(1-value)) %>% 
  select(x,y) %>% mutate(id = row_number()) -> data

dist_data <- dist(data %>% select(-id), method = "euclidean")

clusters <- hclust(dist_data, method = 'single')

nrow(data):1 %>% 
  map(function(x) clustResultx(x)) -> clustEvol

2:length(clustEvol) %>% 
  map(function(x) add_segments(x)) %>% 
  bind_rows() -> segments_id

segments_id %>% 
  inner_join(data, by = c("p1" = "id"), suffix = c(".1", ".2")) %>% 
  inner_join(data, by = c("p2" = "id"), suffix = c(".1", ".2")) %>% 
  select(x = x.1, y = y.1, xend = x.2, yend = y.2) -> segments

ggplot(segments) + 
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend),
             ncp = 10) +
  scale_x_continuous(expand=c(.1, .1)) +
  scale_y_continuous(expand=c(.1, .1), trans = scales::reverse_trans()) +
  coord_equal() +
  theme_void()

