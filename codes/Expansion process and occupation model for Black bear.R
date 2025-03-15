library(sf)
library(dplyr)
library(amt)
library(survival)
library(car)
library(ggplot2)

#####################################################################
##### Estimates the range expansion process of Asian black bear #####
##### and analyzes the drivers of its expansion (1978-2003)     #####
#####################################################################

species <- "blackbear"
covariables <- enviro2003


########### Step 1. Data Preparation

### Setting the path for SHP files and reading the data
df_t1 <- "./data/animal/blackbear/blackbear_1978.shp"
df_t2 <- "./data/animal/blackbear/blackbear_2003_new.shp"
grids <- st_read("./data/grid/mesh5.shp")

polygons_t1 <- st_read(df_t1)  # Occupied polygons in t1
polygons_t2 <- st_read(df_t2)  # Occupied polygons in t2

### Transforming to projected coordinate system (UTM zone 54N)
utm_crs <- 32654
polygons_t1u <- st_transform(polygons_t1, utm_crs)
polygons_t2u <- st_transform(polygons_t2, utm_crs)
grids_utm <- st_transform(grids, utm_crs)




########### Step 2. Defining Grids

### Setting non-occupied polygons
combined_polygons <- bind_rows(polygons_t1, polygons_t2)
grids_filtered <- grids %>%
  filter(!mesh_code %in% st_drop_geometry(combined_polygons)$mesh_code)
grids_filtered_utm <- st_transform(grids_filtered, utm_crs)

### Function to identify expansion
identify_expansion <- function(previous_expansion, remaining_polygons, distance = 500) {
  buffer <- st_buffer(previous_expansion, dist = distance)
  intersects <- st_intersects(remaining_polygons, buffer, sparse = FALSE)
  expansion <- remaining_polygons[rowSums(intersects) > 0, ]
  remaining <- remaining_polygons[rowSums(intersects) == 0, ]
  list(expansion = expansion, remaining = remaining)
}

### Function to identify sources
identify_source <- function(expansion, previous_stage, distance = 500) {
  buffer <- st_buffer(expansion, dist = distance)
  intersects <- st_intersects(previous_stage, buffer, sparse = FALSE)
  source <- previous_stage[rowSums(intersects) > 0, ]
  return(source)
}

### Function to identify non-expansion
identify_nonexpansion <- function(source_expansion, non_occupied, distance = 500) {
  buffer <- st_buffer(source_expansion, dist = distance)
  intersects <- st_intersects(non_occupied, buffer, sparse = FALSE)
  nonexpansion <- non_occupied[rowSums(intersects) > 0, ]
  return(nonexpansion)
}

### Initial expansion stage
buffer_t1 <- st_buffer(polygons_t1u, dist = 500)

# Initialization
expansions <- list()
remaining_polygons <- list()
remaining_polygons[[1]] <- polygons_t2u

# Performing the first expansion stage
result <- identify_expansion(buffer_t1, remaining_polygons[[1]])
expansions[[1]] <- result$expansion %>% mutate(stage = 1, population = 1)
remaining_polygons[[2]] <- result$remaining

# Repeating expansion up to 20 stages
for (i in 2:20) {
  result <- identify_expansion(expansions[[i - 1]], remaining_polygons[[i]])
  expansions[[i]] <- result$expansion %>% mutate(stage = i, population = 1)
  remaining_polygons[[i + 1]] <- result$remaining
  
  if (nrow(expansions[[i]]) == 0) {
    message("No more polygons to expand. Stage: ", i - 1)
    expansions <- expansions[1:(i - 1)]
    remaining_polygons <- remaining_polygons[1:i]
    break
  }
}

### Setting and repeating for sources
sources <- list()
sources[[1]] <- identify_source(expansions[[1]], polygons_t1u) %>% mutate(stage = 1)

for (i in 2:length(expansions)) {
  sources[[i]] <- identify_source(expansions[[i]], expansions[[i - 1]]) %>% mutate(stage = i)
  
  if (nrow(sources[[i]]) == 0) {
    message("No more sources. Stage: ", i - 1)
    sources <- sources[1:(i - 1)]
    break
  }
}

### Setting and repeating for non-expansion
nonexpansions <- list()
nonexpansions[[1]] <- identify_nonexpansion(sources[[1]], grids_filtered_utm) %>% mutate(population = 0)

i <- 2
while (i <= length(sources)) {
  nonexpansions[[i]] <- identify_nonexpansion(sources[[i]], grids_filtered_utm) %>% mutate(population = 0)
  
  if (nrow(nonexpansions[[i]]) == 0) {
    message("No more non-expansion. Stage: ", i - 1)
    nonexpansions <- nonexpansions[1:(i - 1)]
    break
  }
  
  i <- i + 1
}

# Final check
# Expansion areas for each stage are stored in the expansions list, sources in the sources list, and non-expansion in the nonexpansions list.


### Defining Grids

occupied_df <- polygons_t1u %>% mutate(species = species, type = "occupied", stage = 0, population = 1) %>%
  select(mesh_code, mesh_7, population, stage, species, type)

expansions_df <- do.call(bind_rows, expansions) %>% mutate(species = species, type = "expansion") %>%
  select(mesh_code, mesh_7, population, stage, species, type)

sources_df <- do.call(bind_rows, sources) %>% mutate(species = species, type = "sources") %>%
  select(mesh_code, mesh_7, population, stage, species, type)

nonexpansions_df <- do.call(bind_rows, nonexpansions) %>% mutate(species = species, type = "non-expansion", stage = 50) %>%
  distinct(mesh_code, .keep_all = TRUE) %>%
  select(mesh_code, mesh_7, population, stage, species, type)

# definition_type: Define type as occupied, expansion, or non-expansion for each grid
definition_type <- rbind(expansions_df, nonexpansions_df, occupied_df) %>%
  distinct(mesh_code, .keep_all = TRUE)




########### Step 3. Estimation of Range Expansion

# define_adjacent_grids function: Define grids around source grid as occupied, expansion, or non-expansion
define_adjacent_grids <- function(sources_df, definition_type, buffer_distance = 500) {
  adjacent_grids <- list()
  
  # Create mesh_stage_df after dropping geometry
  mesh_stage_df <- definition_type %>%
    st_drop_geometry() %>%
    select(mesh_code, stage) %>%
    rename(source_mesh_code = mesh_code, mesh_stage = stage)
  
  for (i in 1:nrow(sources_df)) {
    # Create a 500m buffer for each grid in sources_df and identify adjacent grids
    source_grid <- sources_df[i, ]
    source_buffer <- st_buffer(source_grid, dist = buffer_distance)
    intersecting_grids <- definition_type[st_intersects(source_buffer, definition_type, sparse = FALSE), ] %>%
      filter(mesh_code != source_grid$mesh_code[1]) %>%  # Remove rows with the same mesh_code
      mutate(source_mesh_code = source_grid$mesh_code[1], source_stage = source_grid$stage[1]) %>%
      select(source_mesh_code, source_stage, mesh_code, population, type, stage)
    
    adjacent_grids[[i]] <- intersecting_grids
  }
  
  # Combine adjacent grids and join with mesh_stage_df, then filter
  adjacent_grids_df <- do.call(bind_rows, adjacent_grids) %>%
    left_join(mesh_stage_df, by = "source_mesh_code") %>%
    filter(mesh_stage != stage, mesh_stage < stage) %>%
    select(source_mesh_code, mesh_stage, source_stage, mesh_code, stage, population, type)
  
  return(adjacent_grids_df)
}

# Find adjacent grids
adjacent_grids_df <- define_adjacent_grids(sources_df, definition_type)




########### Step 4. Define number of source grids

# Split sources_df by stage
sources_by_stage <- split(sources_df, sources_df$stage)

# Define a function to count the number of adjacent source grids for each stage
count_adjacent_sources_per_stage <- function(target_df, sources_by_stage, buffer_distance = 500) {
  result <- list()
  
  for (i in 1:nrow(target_df)) {
    grid <- target_df[i, ]
    grid_buffer <- st_buffer(grid, dist = buffer_distance)
    
    # Count adjacent source grids for each stage
    for (stage_num in seq_along(sources_by_stage)) {
      stage_sources <- sources_by_stage[[stage_num]]
      adjacent_count <- nrow(stage_sources[st_intersects(grid_buffer, stage_sources, sparse = FALSE), ])
      
      # Append result to list
      result[[length(result) + 1]] <- data.frame(
        mesh_code = grid$mesh_code,
        stage = stage_num,  # Current stage
        adjacent_counts = adjacent_count
      )
    }
  }
  
  # Return result as a dataframe
  do.call(rbind, result)
}

# Calculate the number of adjacent source grids for each stage in expansions_df
expansions_adjacent_counts <- count_adjacent_sources_per_stage(expansions_df, sources_by_stage) %>%
  filter(adjacent_counts != 0)

# Calculate the number of adjacent source grids for each stage in nonexpansions_df
nonexpansions_adjacent_counts <- count_adjacent_sources_per_stage(nonexpansions_df, sources_by_stage) %>%
  filter(adjacent_counts != 0)

# Combine expansions_adjacent_counts and nonexpansions_adjacent_counts
all_adjacent_counts <- bind_rows(expansions_adjacent_counts, nonexpansions_adjacent_counts)




########### Step 5. Final dataset for modeling

# Perform left_join on adjacent_grids_df
adjacent_grids_df_ <- adjacent_grids_df %>% mutate(stage = source_stage) %>%
  left_join(all_adjacent_counts, by = c("mesh_code", "stage")) %>%
  left_join(covariables, by = join_by(mesh_code)) %>%
  filter(!is.na(district)) %>%
  filter(ratio_clas != 'less_half') %>%
  mutate(aband = ifelse(is.na(aband), 0, aband)) %>%
  st_drop_geometry() 




########### Step 6. Occupation Modeling
adjacent_grids_df_ <- adjacent_grids_df_ %>% filter(., !district == "shikoku")

blackbear_p1p2_fit <- clogit(population ~ scale(AG) + scale(adjacent_counts) + scale(TRImean) + scale(aband) + scale(snow) +
                              scale(NL) + strata(source_mesh_code), data = adjacent_grids_df_, model = TRUE)

#vif(blackbear_p1p2_fit)
summary(blackbear_p1p2_fit)


#############################################################################

#####################################################################
##### Estimates the range expansion process of Asian black bear #####
##### and analyzes the drivers of its expansion (2003-2017)     #####
#####################################################################


species <- "blackbear"
covariables <- enviro2017


########### Step 1. Data Preparation

### Setting the path for SHP files and reading the data
df_t1 <- "./data/animal/blackbear/blackbear_2003.shp"
df_t2 <- "./data/animal/blackbear/blackbear_2017_new.shp"
grids <- st_read("./data/grid/mesh5.shp")

polygons_t1 <- st_read(df_t1)  # Occupied polygons in t1
polygons_t2 <- st_read(df_t2)  # Occupied polygons in t2

### Transforming to projected coordinate system (UTM zone 54N)
utm_crs <- 32654
polygons_t1u <- st_transform(polygons_t1, utm_crs)
polygons_t2u <- st_transform(polygons_t2, utm_crs)
grids_utm <- st_transform(grids, utm_crs)




########### Step 2. Defining Grids

### Setting non-occupied polygons
combined_polygons <- bind_rows(polygons_t1, polygons_t2)
grids_filtered <- grids %>%
  filter(!mesh_code %in% st_drop_geometry(combined_polygons)$mesh_code)
grids_filtered_utm <- st_transform(grids_filtered, utm_crs)

### Function to identify expansion
identify_expansion <- function(previous_expansion, remaining_polygons, distance = 500) {
  buffer <- st_buffer(previous_expansion, dist = distance)
  intersects <- st_intersects(remaining_polygons, buffer, sparse = FALSE)
  expansion <- remaining_polygons[rowSums(intersects) > 0, ]
  remaining <- remaining_polygons[rowSums(intersects) == 0, ]
  list(expansion = expansion, remaining = remaining)
}

### Function to identify sources
identify_source <- function(expansion, previous_stage, distance = 500) {
  buffer <- st_buffer(expansion, dist = distance)
  intersects <- st_intersects(previous_stage, buffer, sparse = FALSE)
  source <- previous_stage[rowSums(intersects) > 0, ]
  return(source)
}

### Function to identify non-expansion
identify_nonexpansion <- function(source_expansion, non_occupied, distance = 500) {
  buffer <- st_buffer(source_expansion, dist = distance)
  intersects <- st_intersects(non_occupied, buffer, sparse = FALSE)
  nonexpansion <- non_occupied[rowSums(intersects) > 0, ]
  return(nonexpansion)
}

### Initial expansion stage
buffer_t1 <- st_buffer(polygons_t1u, dist = 500)

# Initialization
expansions <- list()
remaining_polygons <- list()
remaining_polygons[[1]] <- polygons_t2u

# Performing the first expansion stage
result <- identify_expansion(buffer_t1, remaining_polygons[[1]])
expansions[[1]] <- result$expansion %>% mutate(stage = 1, population = 1)
remaining_polygons[[2]] <- result$remaining

# Repeating expansion up to 20 stages
for (i in 2:20) {
  result <- identify_expansion(expansions[[i - 1]], remaining_polygons[[i]])
  expansions[[i]] <- result$expansion %>% mutate(stage = i, population = 1)
  remaining_polygons[[i + 1]] <- result$remaining
  
  if (nrow(expansions[[i]]) == 0) {
    message("No more polygons to expand. Stage: ", i - 1)
    expansions <- expansions[1:(i - 1)]
    remaining_polygons <- remaining_polygons[1:i]
    break
  }
}

### Setting and repeating for sources
sources <- list()
sources[[1]] <- identify_source(expansions[[1]], polygons_t1u) %>% mutate(stage = 1)

for (i in 2:length(expansions)) {
  sources[[i]] <- identify_source(expansions[[i]], expansions[[i - 1]]) %>% mutate(stage = i)
  
  if (nrow(sources[[i]]) == 0) {
    message("No more sources. Stage: ", i - 1)
    sources <- sources[1:(i - 1)]
    break
  }
}

### Setting and repeating for non-expansion
nonexpansions <- list()
nonexpansions[[1]] <- identify_nonexpansion(sources[[1]], grids_filtered_utm) %>% mutate(population = 0)

i <- 2
while (i <= length(sources)) {
  nonexpansions[[i]] <- identify_nonexpansion(sources[[i]], grids_filtered_utm) %>% mutate(population = 0)
  
  if (nrow(nonexpansions[[i]]) == 0) {
    message("No more non-expansion. Stage: ", i - 1)
    nonexpansions <- nonexpansions[1:(i - 1)]
    break
  }
  
  i <- i + 1
}

# Final check
# Expansion areas for each stage are stored in the expansions list, sources in the sources list, and non-expansion in the nonexpansions list.


### Defining Grids

occupied_df <- polygons_t1u %>% mutate(species = species, type = "occupied", stage = 0, population = 1) %>%
  select(mesh_code, mesh_7, population, stage, species, type)

expansions_df <- do.call(bind_rows, expansions) %>% mutate(species = species, type = "expansion") %>%
  select(mesh_code, mesh_7, population, stage, species, type)

sources_df <- do.call(bind_rows, sources) %>% mutate(species = species, type = "sources")

nonexpansions_df <- do.call(bind_rows, nonexpansions) %>% mutate(species = species, type = "non-expansion", stage = 50) %>%
  distinct(mesh_code, .keep_all = TRUE)


# definition_type: Define type as occupied, expansion, or non-expansion for each grid
definition_type <- rbind(expansions_df, nonexpansions_df, occupied_df) %>%
  distinct(mesh_code, .keep_all = TRUE)




########### Step 3. Estimation of Range Expansion

# define_adjacent_grids function: Define grids around source grid as occupied, expansion, or non-expansion
define_adjacent_grids <- function(sources_df, definition_type, buffer_distance = 500) {
  adjacent_grids <- list()
  
  # Create mesh_stage_df after dropping geometry
  mesh_stage_df <- definition_type %>%
    st_drop_geometry() %>%
    select(mesh_code, stage) %>%
    rename(source_mesh_code = mesh_code, mesh_stage = stage)
  
  for (i in 1:nrow(sources_df)) {
    # Create a 500m buffer for each grid in sources_df and identify adjacent grids
    source_grid <- sources_df[i, ]
    source_buffer <- st_buffer(source_grid, dist = buffer_distance)
    intersecting_grids <- definition_type[st_intersects(source_buffer, definition_type, sparse = FALSE), ] %>%
      filter(mesh_code != source_grid$mesh_code[1]) %>%  # Remove rows with the same mesh_code
      mutate(source_mesh_code = source_grid$mesh_code[1], source_stage = source_grid$stage[1]) %>%
      select(source_mesh_code, source_stage, mesh_code, population, type, stage)
    
    adjacent_grids[[i]] <- intersecting_grids
  }
  
  # Combine adjacent grids and join with mesh_stage_df, then filter
  adjacent_grids_df <- do.call(bind_rows, adjacent_grids) %>%
    left_join(mesh_stage_df, by = "source_mesh_code") %>%
    filter(mesh_stage != stage, mesh_stage < stage) %>%
    select(source_mesh_code, mesh_stage, source_stage, mesh_code, stage, population, type)
  
  return(adjacent_grids_df)
}

# Find adjacent grids
adjacent_grids_df <- define_adjacent_grids(sources_df, definition_type)




########### Step 4. Define number of source grids

# Split sources_df by stage
sources_by_stage <- split(sources_df, sources_df$stage)

# Define a function to count the number of adjacent source grids for each stage
count_adjacent_sources_per_stage <- function(target_df, sources_by_stage, buffer_distance = 500) {
  result <- list()
  
  for (i in 1:nrow(target_df)) {
    grid <- target_df[i, ]
    grid_buffer <- st_buffer(grid, dist = buffer_distance)
    
    # Count adjacent source grids for each stage
    for (stage_num in seq_along(sources_by_stage)) {
      stage_sources <- sources_by_stage[[stage_num]]
      adjacent_count <- nrow(stage_sources[st_intersects(grid_buffer, stage_sources, sparse = FALSE), ])
      
      # Append result to list
      result[[length(result) + 1]] <- data.frame(
        mesh_code = grid$mesh_code,
        stage = stage_num,  # Current stage
        adjacent_counts = adjacent_count
      )
    }
  }
  
  # Return result as a dataframe
  do.call(rbind, result)
}

# Calculate the number of adjacent source grids for each stage in expansions_df
expansions_adjacent_counts <- count_adjacent_sources_per_stage(expansions_df, sources_by_stage) %>%
  filter(adjacent_counts != 0)

# Calculate the number of adjacent source grids for each stage in nonexpansions_df
nonexpansions_adjacent_counts <- count_adjacent_sources_per_stage(nonexpansions_df, sources_by_stage) %>%
  filter(adjacent_counts != 0)

# Combine expansions_adjacent_counts and nonexpansions_adjacent_counts
all_adjacent_counts <- bind_rows(expansions_adjacent_counts, nonexpansions_adjacent_counts)




########### Step 5. Final dataset for modeling

# Perform left_join on adjacent_grids_df
adjacent_grids_df_ <- adjacent_grids_df %>% mutate(stage = source_stage) %>%
  left_join(all_adjacent_counts, by = c("mesh_code", "stage")) %>%
  left_join(covariables, by = join_by(mesh_code)) %>%
  filter(!is.na(district)) %>%
  filter(ratio_clas != 'less_half') %>%
  mutate(aband = ifelse(is.na(aband), 0, aband)) %>%
  st_drop_geometry() 




########### Step 6. Occupation Modeling
adjacent_grids_df_ <- adjacent_grids_df_ %>% filter(., !district == "shikoku")

blackbear_p2p3_fit <- clogit(population ~ scale(AG) + scale(adjacent_counts) + scale(TRImean) + scale(aband) + scale(snow) +
                              scale(NL) + strata(source_mesh_code), data = adjacent_grids_df_, model = TRUE)

#vif(blackbear_p2p3_fit)
summary(blackbear_p2p3_fit)

##############################################################################

