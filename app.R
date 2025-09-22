# MFRM Simulation Data Generator

# Load packages
pacman::p_load(
  shiny, tidyverse,
  mvtnorm, DT, zip
)

# Function to generate MFRM data and residuals
generate_mfrm_data <- function(
    n_participants = 60,
    n_raters = 4,
    n_tasks = 3,
    n_criteria = 4,
    residual_type = 1, # 1: Random, 2: Rater-related, 3: Task-related
    residual_sd = 0.5, # Standard deviation of residuals (smaller = higher unidimensionality)
    within_facet_corr = 0.3, # Within-facet correlation (larger = higher multidimensionality)
    participant_sd = 1.0, # Variance in participant ability
    rater_sd = 0.2, # Variance in rater severity
    task_sd = 0.2, # Variance in task difficulty
    criteria_sd = 0.2, # Variance in criteria difficulty
    participant_mean = 0, # Mean participant ability
    rater_mean = 0, # Mean rater severity
    task_mean = 0, # Mean task difficulty
    criteria_mean = 0, # Mean criteria difficulty
    threshold_values = c(-1.2, -0.4, 0.4, 1.2), # Threshold parameters
    rating_scale_min = 1, # Minimum rating scale value
    rating_scale_max = 5, # Maximum rating scale value
    participant_distribution = "normal", # Participant ability distribution
    rater_effect_size = 1.0, # Magnitude of rater effect
    task_effect_size = 1.0, # Magnitude of task effect
    criteria_effect_size = 1.0, # Magnitude of criteria effect
    cross_design = "complete", # "complete", "balanced", or "random"
    rater_assignment_rate = 0.5, # Proportion of raters assigned to each participant (for cross_design = "random" or "balanced")
    task_assignment_rate = 1.0, # Proportion of tasks assigned to each participant
    criteria_assignment_rate = 1.0, # Proportion of criteria used in each rating
    missing_data_rate = 0.0, # Missing data occurrence rate (0-1)
    balanced_blocks = TRUE, # Create equal blocks when cross_design = "balanced"
    seed_value = 123
) {
  # Fix random seed
  set.seed(seed_value)
  
  # Rater IDs
  rater_ids <- LETTERS[1:n_raters]  # A, B, C, D
  
  # Task names
  task_names <- paste0("Task ", LETTERS[1:n_tasks])
  
  # Criteria names
  criteria_names <- paste0("Criterion ", LETTERS[1:n_criteria])
  
  # Create dataset based on cross design
  if (cross_design == "complete") {
    # Complete cross design: All combinations of participants × raters × tasks × criteria
    speaking_data <- expand.grid(
      participant_id = 1:n_participants,
      rater_id = rater_ids,
      task = task_names,
      criteria = criteria_names)
  } else if (cross_design == "balanced") {
    # Balanced design: Divide participants into groups and assign a subset of raters to each group
    n_rater_per_participant <- max(1, round(n_raters * rater_assignment_rate))
    n_task_per_participant <- max(1, round(n_tasks * task_assignment_rate))
    n_criteria_per_task <- max(1, round(n_criteria * criteria_assignment_rate))
    
    # Divide participants into groups (block design)
    if (balanced_blocks && n_raters > 1) {
      n_blocks <- ceiling(n_raters / n_rater_per_participant)
      block_size <- ceiling(n_participants / n_blocks)
      
      # Create participant and rater assignments for each block
      assignment_list <- list()
      
      for (block in 1:n_blocks) {
        # Participants in this block
        start_idx <- (block - 1) * block_size + 1
        end_idx <- min(block * block_size, n_participants)
        participants_in_block <- start_idx:end_idx
        
        # Raters in this block (cyclically assigned)
        rater_indices <- ((block - 1) * n_rater_per_participant + 1):
          (block * n_rater_per_participant)
        rater_indices <- ((rater_indices - 1) %% n_raters) + 1
        raters_in_block <- rater_ids[rater_indices]
        
        # Similarly assign tasks (as needed)
        task_indices <- sample(1:n_tasks, n_task_per_participant)
        tasks_in_block <- task_names[task_indices]
        
        # Similarly assign criteria
        criteria_indices <- sample(1:n_criteria, n_criteria_per_task)
        criteria_in_block <- criteria_names[criteria_indices]
        
        # Create combinations for this block
        block_data <- expand.grid(
          participant_id = participants_in_block,
          rater_id = raters_in_block,
          task = tasks_in_block,
          criteria = criteria_in_block
        )
        
        assignment_list[[block]] <- block_data
      }
      
      # Combine all blocks
      speaking_data <- do.call(rbind, assignment_list)
    } else {
      # Randomly assign raters and tasks to each participant
      assignment_list <- list()
      
      for (p in 1:n_participants) {
        # Randomly select raters for this participant
        selected_raters <- sample(rater_ids, n_rater_per_participant)
        
        # Randomly select tasks for this participant
        selected_tasks <- sample(task_names, n_task_per_participant)
        
        # Randomly select criteria for this participant
        selected_criteria <- sample(criteria_names, n_criteria_per_task)
        
        # Create combinations for this participant
        participant_data <- expand.grid(
          participant_id = p,
          rater_id = selected_raters,
          task = selected_tasks,
          criteria = selected_criteria
        )
        
        assignment_list[[p]] <- participant_data
      }
      
      # Combine all participant data
      speaking_data <- do.call(rbind, assignment_list)
    }
  } else if (cross_design == "random") {
    # Random design: Assign random raters and tasks to each participant
    # Calculate expected number of evaluations
    expected_evaluations <- n_participants * n_raters * n_tasks * n_criteria
    expected_raters_per_participant <- max(1, round(n_raters * rater_assignment_rate))
    expected_tasks_per_participant <- max(1, round(n_tasks * task_assignment_rate))
    expected_criteria_per_evaluation <- max(1, round(n_criteria * criteria_assignment_rate))
    
    # Number of evaluations per participant
    evaluations_per_participant <- expected_raters_per_participant * 
      expected_tasks_per_participant * 
      expected_criteria_per_evaluation
    
    # Create evaluation data for each participant
    assignment_list <- list()
    
    for (p in 1:n_participants) {
      # How many raters will evaluate this participant
      n_raters_for_this_participant <- expected_raters_per_participant
      
      # Which raters
      selected_raters <- sample(rater_ids, n_raters_for_this_participant)
      
      # How many tasks will this participant perform
      n_tasks_for_this_participant <- expected_tasks_per_participant
      
      # Which tasks
      selected_tasks <- sample(task_names, n_tasks_for_this_participant)
      
      # For each task × rater combination, which criteria to use
      participant_records <- list()
      
      for (r in selected_raters) {
        for (t in selected_tasks) {
          # Which criteria to use for this task × rater combination
          n_criteria_for_this_eval <- expected_criteria_per_evaluation
          selected_criteria <- sample(criteria_names, n_criteria_for_this_eval)
          
          # Create records for each criterion
          task_data <- data.frame(
            participant_id = p,
            rater_id = r,
            task = t,
            criteria = selected_criteria,
            stringsAsFactors = FALSE
          )
          
          participant_records[[length(participant_records) + 1]] <- task_data
        }
      }
      
      # Combine all records for this participant
      if (length(participant_records) > 0) {
        participant_all_data <- do.call(rbind, participant_records)
        assignment_list[[p]] <- participant_all_data
      }
    }
    
    # Combine all participant data
    speaking_data <- do.call(rbind, assignment_list)
  }
  
  # Generate missing data
  if (missing_data_rate > 0) {
    # Calculate number of rows to make missing
    n_total_rows <- nrow(speaking_data)
    n_missing_rows <- round(n_total_rows * missing_data_rate)
    
    if (n_missing_rows > 0) {
      # Randomly select and remove rows
      missing_indices <- sample(1:n_total_rows, n_missing_rows)
      speaking_data <- speaking_data[-missing_indices, ]
    }
  }
  
  # Organize threshold parameters
  n_categories <- length(threshold_values) + 1
  if (n_categories != (rating_scale_max - rating_scale_min + 1)) {
    # Adjust if number of rating scale and thresholds don't match
    n_thresholds_needed <- rating_scale_max - rating_scale_min
    if (length(threshold_values) > n_thresholds_needed) {
      # Truncate if too many thresholds
      threshold_values <- threshold_values[1:n_thresholds_needed]
    } else if (length(threshold_values) < n_thresholds_needed) {
      # Interpolate if too few thresholds
      old_range <- range(threshold_values)
      thresholds_to_add <- n_thresholds_needed - length(threshold_values)
      if (thresholds_to_add > 0) {
        new_thresholds <- seq(old_range[1], old_range[2], length.out = n_thresholds_needed)
        threshold_values <- new_thresholds
      }
    }
  }
  # Thresholds must be in ascending order
  threshold_values <- sort(threshold_values)
  
  # Generate participant ability (θ)
  if (participant_distribution == "normal") {
    theta <- rnorm(n_participants, mean = participant_mean, sd = participant_sd)
  } else if (participant_distribution == "uniform") {
    range <- participant_sd * sqrt(12) # Adjust to have same variance
    theta <- runif(n_participants, min = participant_mean - range/2, max = participant_mean + range/2)
  } else if (participant_distribution == "bimodal") {
    # Bimodal distribution (mixture of two normal distributions)
    group1 <- rnorm(n_participants/2, mean = participant_mean - participant_sd, sd = participant_sd/2)
    group2 <- rnorm(n_participants - n_participants/2, mean = participant_mean + participant_sd, sd = participant_sd/2)
    theta <- c(group1, group2)
  }
  
  # Rater severity (α)
  alpha <- rnorm(n_raters, mean = rater_mean, sd = rater_sd * rater_effect_size)
  
  # Task difficulty (β)
  beta <- rnorm(n_tasks, mean = task_mean, sd = task_sd * task_effect_size)
  
  # Criteria difficulty (γ)
  gamma <- rnorm(n_criteria, mean = criteria_mean, sd = criteria_sd * criteria_effect_size)
  
  # Add ability, severity, and difficulty information
  speaking_data <- speaking_data |>
    mutate(
      # Get ability value from participant ID
      ability_index = participant_id,
      ability = theta[ability_index],
      
      # Get severity from rater ID
      severity_index = match(rater_id, rater_ids),
      severity = alpha[severity_index],
      
      # Get difficulty from task name
      task_index = match(task, task_names),
      task_difficulty = beta[task_index],
      
      # Get difficulty from criteria
      criteria_index = match(criteria, criteria_names),
      criteria_difficulty = gamma[criteria_index],
      
      # Calculate theoretical logit position
      logit_position = ability - severity - task_difficulty - criteria_difficulty
    )
  
  # Function to calculate category probabilities
  calculate_category_probs <- function(logit_position, thresholds) {
    n_cats <- length(thresholds) + 1
    probs <- numeric(n_cats)
    
    # Probability for category 1
    probs[1] <- 1 / (1 + exp(logit_position - thresholds[1]))
    
    # Probabilities for categories 2 to n-1
    for (k in 2:(n_cats-1)) {
      probs[k] <- 1 / (1 + exp(logit_position - thresholds[k])) - 
        1 / (1 + exp(logit_position - thresholds[k-1]))
    }
    
    # Probability for the last category
    probs[n_cats] <- 1 - 1 / (1 + exp(logit_position - thresholds[n_cats-1]))
    
    return(probs)
  }
  
  # Calculate expected values
  speaking_data <- speaking_data |>
    rowwise() |>
    mutate(
      # Calculate probabilities for each category
      category_probs = list(calculate_category_probs(logit_position, threshold_values)),
      
      # Expected score (theoretical mean value)
      expected_score = sum(seq(from = rating_scale_min, 
                               length.out = length(category_probs)) * category_probs),
      
      # Theoretical variance
      score_variance = sum((seq(from = rating_scale_min, 
                                length.out = length(category_probs)) - expected_score)^2 * category_probs)
    ) |>
    ungroup()
  
  # Assign unique ID to each item (rater × task × criteria)
  speaking_data <- speaking_data |>
    mutate(item_id = paste(rater_id, task, criteria, sep = "_"))
  
  if (residual_type == 1) {
    # Completely random residuals - highest unidimensionality model
    speaking_data$std_residual <- rnorm(nrow(speaking_data), mean = 0, sd = residual_sd)
    
  } else if (residual_type == 2) {
    # Rater-related residual structure
    # Add correlation between items within each rater
    
    # Create correlation matrix
    n_items <- n_tasks * n_criteria
    rater_corr <- within_facet_corr  # Correlation between items within the same rater
    
    # Generate correlated residuals for each rater
    residuals_list <- list()
    
    for (r in 1:n_raters) {
      # Correlation matrix for this rater
      corr_matrix <- matrix(rater_corr, nrow = n_items, ncol = n_items)
      diag(corr_matrix) <- 1
      
      # Variance-covariance matrix
      sigma <- residual_sd^2 * corr_matrix
      
      # Sample from multivariate normal distribution
      rater_residuals <- rmvnorm(n_participants, mean = rep(0, n_items), sigma = sigma)
      
      # Convert to data frame
      rater_df <- as.data.frame(rater_residuals)
      names(rater_df) <- paste0("item_", 1:n_items)
      rater_df$participant_id <- 1:n_participants
      
      # Convert to long format
      rater_long <- pivot_longer(
        rater_df, 
        cols = starts_with("item_"),
        names_to = "item_num", 
        values_to = "residual"
      )
      
      # Create correspondence between item number and task/criteria
      item_map <- expand.grid(
        task = 1:n_tasks,
        criteria = 1:n_criteria
      ) |>
        mutate(item_num = paste0("item_", 1:n_items))
      
      # Add task and criteria information to residuals
      rater_long <- rater_long |>
        left_join(item_map, by = "item_num") |>
        mutate(
          rater_id = rater_ids[r],
          task = task_names[task],
          criteria = criteria_names[criteria]
        ) |>
        select(participant_id, rater_id, task, criteria, residual)
      
      residuals_list[[r]] <- rater_long
    }
    
    # Combine residuals for all raters
    all_residuals <- bind_rows(residuals_list)
    
    # Join residuals to original data
    speaking_data <- speaking_data |>
      left_join(
        all_residuals,
        by = c("participant_id", "rater_id", "task", "criteria")
      ) |>
      rename(std_residual = residual)
    
  } else if (residual_type == 3) {
    # Task-related residual structure
    # Add correlation between items within each task
    
    # Create correlation matrix
    n_items_per_task <- n_raters * n_criteria
    task_corr <- within_facet_corr  # Correlation between items within the same task
    
    # Generate correlated residuals for each task
    residuals_list <- list()
    
    for (t in 1:n_tasks) {
      # Correlation matrix for this task
      corr_matrix <- matrix(task_corr, nrow = n_items_per_task, ncol = n_items_per_task)
      diag(corr_matrix) <- 1
      
      # Variance-covariance matrix
      sigma <- residual_sd^2 * corr_matrix
      
      # Sample from multivariate normal distribution
      task_residuals <- rmvnorm(n_participants, mean = rep(0, n_items_per_task), sigma = sigma)
      
      # Convert to data frame
      task_df <- as.data.frame(task_residuals)
      names(task_df) <- paste0("item_", 1:n_items_per_task)
      task_df$participant_id <- 1:n_participants
      
      # Convert to long format
      task_long <- pivot_longer(
        task_df, 
        cols = starts_with("item_"),
        names_to = "item_num", 
        values_to = "residual"
      )
      
      # Create correspondence between item number and rater/criteria
      item_map <- expand.grid(
        rater = 1:n_raters,
        criteria = 1:n_criteria
      ) |>
        mutate(item_num = paste0("item_", 1:n_items_per_task))
      
      # Add rater and criteria information to residuals
      task_long <- task_long |>
        left_join(item_map, by = "item_num") |>
        mutate(
          rater_id = rater_ids[rater],
          task = task_names[t],
          criteria = criteria_names[criteria]
        ) |>
        select(participant_id, rater_id, task, criteria, residual)
      
      residuals_list[[t]] <- task_long
    }
    
    # Combine residuals for all tasks
    all_residuals <- bind_rows(residuals_list)
    
    # Join residuals to original data
    speaking_data <- speaking_data |>
      left_join(
        all_residuals,
        by = c("participant_id", "rater_id", "task", "criteria")
      ) |>
      rename(std_residual = residual)
  } else if (residual_type == 4) {
    # Criteria-related residual structure
    # Add correlation between items within each criterion
    
    # Create correlation matrix
    n_items_per_criteria <- n_raters * n_tasks
    criteria_corr <- within_facet_corr  # Correlation between items within the same criterion
    
    # Generate correlated residuals for each criterion
    residuals_list <- list()
    
    for (c in 1:n_criteria) {
      # Correlation matrix for this criterion
      corr_matrix <- matrix(criteria_corr, nrow = n_items_per_criteria, ncol = n_items_per_criteria)
      diag(corr_matrix) <- 1
      
      # Variance-covariance matrix
      sigma <- residual_sd^2 * corr_matrix
      
      # Sample from multivariate normal distribution
      criteria_residuals <- rmvnorm(n_participants, mean = rep(0, n_items_per_criteria), sigma = sigma)
      
      # Convert to data frame
      criteria_df <- as.data.frame(criteria_residuals)
      names(criteria_df) <- paste0("item_", 1:n_items_per_criteria)
      criteria_df$participant_id <- 1:n_participants
      
      # Convert to long format
      criteria_long <- pivot_longer(
        criteria_df, 
        cols = starts_with("item_"),
        names_to = "item_num", 
        values_to = "residual"
      )
      
      # Create correspondence between item number and rater/task
      item_map <- expand.grid(
        rater = 1:n_raters,
        task = 1:n_tasks
      ) |>
        mutate(item_num = paste0("item_", 1:n_items_per_criteria))
      
      # Add rater and task information to residuals
      criteria_long <- criteria_long |>
        left_join(item_map, by = "item_num") |>
        mutate(
          rater_id = rater_ids[rater],
          task = task_names[task],
          criteria = criteria_names[c]
        ) |>
        select(participant_id, rater_id, task, criteria, residual)
      
      residuals_list[[c]] <- criteria_long
    }
    
    # Combine residuals for all criteria
    all_residuals <- bind_rows(residuals_list)
    
    # Join residuals to original data
    speaking_data <- speaking_data |>
      left_join(
        all_residuals,
        by = c("participant_id", "rater_id", "task", "criteria")
      ) |>
      rename(std_residual = residual)
  }
  # Calculate observed scores from standardized residuals
  speaking_data <- speaking_data |>
    mutate(
      # First generate raw scores from standardized residuals
      raw_residual = std_residual * sqrt(score_variance),
      score_float = expected_score + raw_residual,
      score_float = pmin(pmax(score_float, rating_scale_min), rating_scale_max),
      score = round(score_float)
    ) |>
    rowwise() |>
    mutate(
      # After creating scores, calculate model residuals
      raw_residual = score - expected_score,
      std_residual = raw_residual / sqrt(score_variance),
      std_residual_sq = std_residual^2
    ) |>
    ungroup()
  
  # Return result list including metadata
  return(list(
    data = speaking_data,
    metadata = list(
      n_participants = n_participants,
      n_raters = n_raters,
      n_tasks = n_tasks,
      n_criteria = n_criteria,
      residual_type = residual_type,
      rating_scale = c(rating_scale_min, rating_scale_max),
      thresholds = threshold_values,
      participant_distribution = participant_distribution
    )
  ))
}
# Helper functions ----------------------------------------------------------

generate_thresholds <- function(rating_min, rating_max) {
  n_categories <- rating_max - rating_min + 1
  n_thresholds <- n_categories - 1
  
  if (n_thresholds <= 0) return(numeric(0))
  
  seq(-2.0, 2.0, length.out = n_thresholds)
}

create_dataset_summary <- function(dataset_list) {
  if (length(dataset_list) == 0) {
    return(tibble::tibble())
  }
  
  tibble::tibble(
    Dataset = purrr::map_chr(dataset_list, "name"),
    Seed = purrr::map_dbl(dataset_list, "seed"),
    Rows = purrr::map_int(dataset_list, ~ nrow(.x$data)),
    Participants = purrr::map_int(dataset_list, ~ dplyr::n_distinct(.x$data$participant_id)),
    Raters = purrr::map_int(dataset_list, ~ dplyr::n_distinct(.x$data$rater_id)),
    Tasks = purrr::map_int(dataset_list, ~ dplyr::n_distinct(.x$data$task)),
    Criteria = purrr::map_int(dataset_list, ~ dplyr::n_distinct(.x$data$criteria))
  )
}

metadata_to_table <- function(dataset) {
  tibble::tibble(
    Metric = c(
      "Seed",
      "Participants",
      "Raters",
      "Tasks",
      "Criteria",
      "Rating scale",
      "Thresholds"
    ),
    Value = c(
      dataset$seed,
      dataset$metadata$n_participants,
      dataset$metadata$n_raters,
      dataset$metadata$n_tasks,
      dataset$metadata$n_criteria,
      paste(dataset$metadata$rating_scale, collapse = " - "),
      paste(round(dataset$metadata$thresholds, 3), collapse = ", ")
    )
  )
}

prepare_dataset_for_export <- function(df, label = NULL) {
  export_cols <- c(
    "participant_id",
    "rater_id",
    "task",
    "criteria",
    "item_id",
    "ability",
    "severity",
    "task_difficulty",
    "criteria_difficulty",
    "expected_score",
    "score_variance",
    "std_residual",
    "std_residual_sq",
    "score"
  )
  available_cols <- intersect(export_cols, names(df))
  export_df <- df[, available_cols, drop = FALSE]
  if (!is.null(label)) {
    export_df <- dplyr::mutate(export_df, dataset_label = label, .before = 1)
  }
  export_df
}

dataset_slug <- function(name) {
  slug <- gsub("[^A-Za-z0-9]+", "-", name)
  slug <- gsub("-+", "-", slug)
  slug <- gsub("(^-|-$)", "", slug)
  slug <- tolower(slug)
  ifelse(slug == "", "dataset", slug)
}

# UI ------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Many-Facet Rasch Model (MFRM) Simulation Data Generator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      tabsetPanel(
        tabPanel(
          "Basic Settings",
          wellPanel(
            h4("Cross Design Options"),
            radioButtons(
              "cross_design", "Cross Design Type:",
              choices = list(
                "Complete Cross Design (all combinations)" = "complete",
                "Balanced Design (equal block assignment)" = "balanced",
                "Random Design (random assignment)" = "random"
              ),
              selected = "complete"
            ),
            conditionalPanel(
              condition = "input.cross_design != 'complete'",
              sliderInput(
                "rater_assignment_rate",
                "Proportion of raters assigned to each participant:",
                min = 0.1, max = 1.0, value = 0.5, step = 0.1
              ),
              sliderInput(
                "task_assignment_rate",
                "Proportion of tasks assigned to each participant:",
                min = 0.1, max = 1.0, value = 1.0, step = 0.1
              ),
              sliderInput(
                "criteria_assignment_rate",
                "Proportion of criteria used in each rating:",
                min = 0.1, max = 1.0, value = 1.0, step = 0.1
              )
            ),
            conditionalPanel(
              condition = "input.cross_design == 'balanced'",
              checkboxInput("balanced_blocks", "Create equal blocks", value = TRUE)
            ),
            sliderInput(
              "missing_data_rate", "Missing data rate:",
              min = 0.0, max = 0.5, value = 0.0, step = 0.05
            ),
            numericInput("seed_value", "Random seed value:", 123, min = 1, max = 9999)
          ),
          wellPanel(
            h4("Data Structure Settings"),
            numericInput("n_participants", "Number of participants:", 60, min = 20, max = 500),
            numericInput("n_raters", "Number of raters:", 4, min = 2, max = 20),
            numericInput("n_tasks", "Number of tasks:", 3, min = 2, max = 10),
            numericInput("n_criteria", "Number of criteria:", 4, min = 2, max = 10)
          ),
          wellPanel(
            h4("Rating Scale Settings"),
            numericInput("rating_scale_min", "Rating scale minimum:", 1, min = 0, max = 10),
            numericInput("rating_scale_max", "Rating scale maximum:", 5, min = 1, max = 20),
            checkboxInput("auto_generate_thresholds", "Auto-generate thresholds", value = TRUE),
            actionButton("update_thresholds", "Update Thresholds", class = "btn-info", width = "100%")
          ),
          wellPanel(
            h4("Residual Structure"),
            radioButtons(
              "residual_type", "Residual structure type:",
              choices = list(
                "Random residuals" = 1,
                "Rater-related residuals" = 2,
                "Task-related residuals" = 3,
                "Criteria-related residuals" = 4
              ),
              selected = 1
            ),
            sliderInput(
              "residual_sd", "Residual standard deviation:",
              min = 0.01, max = 6.0, value = 0.5, step = 0.01
            ),
            sliderInput(
              "within_facet_corr", "Within-facet correlation:",
              min = 0.0, max = 0.9, value = 0.3, step = 0.05
            )
          )
        ),
        tabPanel(
          "Ability/Difficulty",
          wellPanel(
            h4("Ability/Difficulty Parameters"),
            radioButtons(
              "participant_distribution", "Participant ability distribution:",
              choices = list(
                "Normal distribution" = "normal",
                "Uniform distribution" = "uniform"
              ),
              selected = "normal"
            ),
            sliderInput(
              "participant_mean", "Mean participant ability:",
              min = -6.0, max = 6.0, value = 0.0, step = 0.1
            ),
            sliderInput(
              "participant_sd", "Participant ability variance:",
              min = 0.05, max = 6.0, value = 1.0, step = 0.1
            ),
            sliderInput(
              "rater_mean", "Mean rater severity:",
              min = -6.0, max = 6.0, value = 0.0, step = 0.1
            ),
            sliderInput(
              "rater_sd", "Rater severity variance:",
              min = 0.05, max = 6.0, value = 0.2, step = 0.05
            ),
            sliderInput(
              "rater_effect_size", "Rater effect size:",
              min = 0.5, max = 6.0, value = 1.0, step = 0.1
            ),
            sliderInput(
              "task_mean", "Mean task difficulty:",
              min = -6.0, max = 6.0, value = 0.0, step = 0.1
            ),
            sliderInput(
              "task_sd", "Task difficulty variance:",
              min = 0.05, max = 6.0, value = 0.2, step = 0.05
            ),
            sliderInput(
              "task_effect_size", "Task effect size:",
              min = 0.5, max = 6.0, value = 1.0, step = 0.1
            ),
            sliderInput(
              "criteria_mean", "Mean criteria difficulty:",
              min = -6.0, max = 6.0, value = 0.0, step = 0.1
            ),
            sliderInput(
              "criteria_sd", "Criteria difficulty variance:",
              min = 0.05, max = 6.0, value = 0.2, step = 0.05
            ),
            sliderInput(
              "criteria_effect_size", "Criteria effect size:",
              min = 0.5, max = 6.0, value = 1.0, step = 0.1
            )
          )
        ),
        tabPanel(
          "Threshold Settings",
          wellPanel(
            h4("Threshold Parameter Settings"),
            uiOutput("dynamic_thresholds"),
            helpText("Note: Thresholds will be automatically sorted in ascending order")
          )
        ),
        tabPanel(
          "Save Parameters",
          wellPanel(
            h4("Parameter Management"),
            downloadButton("download_params", "Save Current Parameters"),
            br(), br(),
            fileInput("upload_params", "Load Parameters")
          )
        )
      ),
      
      wellPanel(
        h4("Simulation Settings"),
        numericInput(
          "n_datasets",
          "Number of datasets to generate:",
          value = 1,
          min = 1,
          max = 1000,
          step = 1
        )
      ),
      
      actionButton(
        "run_simulation",
        "Generate Datasets",
        class = "btn-lg btn-primary",
        width = "100%"
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Generated Data",
          fluidRow(
            column(
              6,
              uiOutput("dataset_selector_ui")
            ),
            column(
              6,
              tableOutput("selected_metadata")
            )
          ),
          fluidRow(
            column(
              12,
              wellPanel(
                downloadButton("download_selected_csv", "Download selected dataset (CSV)"),
                downloadButton("download_selected_rds", "Download selected dataset (RDS)"),
                downloadButton("download_all_csv", "Download all datasets (combined CSV)"),
                downloadButton("download_all_zip", "Download all datasets (ZIP of CSVs)"),
                downloadButton("download_all_rds", "Download all datasets (RDS)")
              )
            )
          ),
          h4("Dataset Summary"),
          tableOutput("dataset_summary"),
          hr(),
          h4("Data Preview"),
          DTOutput("data_table")
        ),
        tabPanel(
          "Parameters",
          h4("Parameters in Use"),
          tableOutput("param_table"),
          hr(),
          h4("Full Parameter List"),
          verbatimTextOutput("full_params")
        )
      )
    )
  )
)
# Server --------------------------------------------------------------------

server <- function(input, output, session) {
  thresholds <- reactiveVal(c(-1.2, -0.4, 0.4, 1.2))
  
  output$dynamic_thresholds <- renderUI({
    if (isTRUE(input$auto_generate_thresholds)) {
      threshold_values <- generate_thresholds(input$rating_scale_min, input$rating_scale_max)
      thresholds(threshold_values)
      tagList(
        helpText("Thresholds are auto-generated based on the rating scale"),
        tableOutput("threshold_table")
      )
    } else {
      n_thresholds <- input$rating_scale_max - input$rating_scale_min
      threshold_inputs <- lapply(1:n_thresholds, function(i) {
        threshold_value <- thresholds()[i]
        if (is.na(threshold_value)) {
          threshold_value <- -3.0 + (i - 1) * 6.0 / max(1, n_thresholds)
        }
        sliderInput(
          inputId = paste0("threshold_", i),
          label = paste0("Threshold ", i, ":"),
          min = -3.0,
          max = 3.0,
          value = threshold_value,
          step = 0.1
        )
      })
      do.call(tagList, threshold_inputs)
    }
  })
  
  output$threshold_table <- renderTable({
    threshold_values <- thresholds()
    if (length(threshold_values) == 0) {
      return(NULL)
    }
    data.frame(
      Threshold.Number = seq_along(threshold_values),
      Threshold.Value = round(threshold_values, 3)
    )
  })
  
  observeEvent(input$update_thresholds, {
    if (isTRUE(input$auto_generate_thresholds)) {
      new_thresholds <- generate_thresholds(input$rating_scale_min, input$rating_scale_max)
      thresholds(new_thresholds)
      showNotification("Thresholds auto-generated.", type = "message", duration = 3)
    } else {
      n_thresholds <- input$rating_scale_max - input$rating_scale_min
      new_thresholds <- numeric(n_thresholds)
      for (i in seq_len(n_thresholds)) {
        threshold_id <- paste0("threshold_", i)
        if (!is.null(input[[threshold_id]])) {
          new_thresholds[i] <- input[[threshold_id]]
        } else {
          new_thresholds[i] <- -3.0 + (i - 1) * 6.0 / max(1, n_thresholds)
        }
      }
      thresholds(sort(new_thresholds))
      showNotification("Thresholds manually updated.", type = "message", duration = 3)
    }
  })
  
  observeEvent(c(input$rating_scale_min, input$rating_scale_max), {
    if (isTRUE(input$auto_generate_thresholds)) {
      thresholds(generate_thresholds(input$rating_scale_min, input$rating_scale_max))
    }
  })
  
  get_current_params <- reactive({
    current_thresholds <- thresholds()
    list(
      n_participants = input$n_participants,
      n_raters = input$n_raters,
      n_tasks = input$n_tasks,
      n_criteria = input$n_criteria,
      residual_type = as.numeric(input$residual_type),
      residual_sd = input$residual_sd,
      within_facet_corr = input$within_facet_corr,
      participant_distribution = input$participant_distribution,
      participant_mean = input$participant_mean,
      participant_sd = input$participant_sd,
      rater_mean = input$rater_mean,
      rater_sd = input$rater_sd,
      task_mean = input$task_mean,
      task_sd = input$task_sd,
      criteria_mean = input$criteria_mean,
      criteria_sd = input$criteria_sd,
      rater_effect_size = input$rater_effect_size,
      task_effect_size = input$task_effect_size,
      criteria_effect_size = input$criteria_effect_size,
      threshold_values = current_thresholds,
      rating_scale_min = input$rating_scale_min,
      rating_scale_max = input$rating_scale_max,
      auto_generate_thresholds = input$auto_generate_thresholds,
      cross_design = input$cross_design,
      rater_assignment_rate = input$rater_assignment_rate,
      task_assignment_rate = input$task_assignment_rate,
      criteria_assignment_rate = input$criteria_assignment_rate,
      missing_data_rate = input$missing_data_rate,
      balanced_blocks = input$balanced_blocks,
      seed_value = input$seed_value,
      n_datasets = input$n_datasets
    )
  })
  
  output$download_params <- downloadHandler(
    filename = function() {
      paste0("mfrm-params-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rds")
    },
    content = function(file) {
      saveRDS(get_current_params(), file)
    }
  )
  
  observeEvent(input$upload_params, {
    req(input$upload_params)
    params <- readRDS(input$upload_params$datapath)
    
    updateNumericInput(session, "n_participants", value = params$n_participants)
    updateNumericInput(session, "n_raters", value = params$n_raters)
    updateNumericInput(session, "n_tasks", value = params$n_tasks)
    updateNumericInput(session, "n_criteria", value = params$n_criteria)
    updateRadioButtons(session, "residual_type", selected = params$residual_type)
    updateSliderInput(session, "residual_sd", value = params$residual_sd)
    updateSliderInput(session, "within_facet_corr", value = params$within_facet_corr)
    updateRadioButtons(session, "participant_distribution", selected = params$participant_distribution)
    updateSliderInput(session, "participant_mean", value = params$participant_mean)
    updateSliderInput(session, "participant_sd", value = params$participant_sd)
    updateSliderInput(session, "rater_mean", value = params$rater_mean)
    updateSliderInput(session, "rater_sd", value = params$rater_sd)
    updateSliderInput(session, "task_mean", value = params$task_mean)
    updateSliderInput(session, "task_sd", value = params$task_sd)
    updateSliderInput(session, "criteria_mean", value = params$criteria_mean)
    updateSliderInput(session, "criteria_sd", value = params$criteria_sd)
    updateSliderInput(session, "rater_effect_size", value = params$rater_effect_size)
    updateSliderInput(session, "task_effect_size", value = params$task_effect_size)
    updateSliderInput(session, "criteria_effect_size", value = params$criteria_effect_size)
    updateNumericInput(session, "rating_scale_min", value = params$rating_scale_min)
    updateNumericInput(session, "rating_scale_max", value = params$rating_scale_max)
    updateCheckboxInput(session, "auto_generate_thresholds", value = params$auto_generate_thresholds)
    updateRadioButtons(session, "cross_design", selected = params$cross_design)
    updateSliderInput(session, "rater_assignment_rate", value = params$rater_assignment_rate)
    updateSliderInput(session, "task_assignment_rate", value = params$task_assignment_rate)
    updateSliderInput(session, "criteria_assignment_rate", value = params$criteria_assignment_rate)
    updateSliderInput(session, "missing_data_rate", value = params$missing_data_rate)
    updateCheckboxInput(session, "balanced_blocks", value = params$balanced_blocks)
    updateNumericInput(session, "seed_value", value = params$seed_value)
    if (!is.null(params$n_datasets)) {
      updateNumericInput(session, "n_datasets", value = params$n_datasets)
    }
    if (!is.null(params$threshold_values)) {
      thresholds(params$threshold_values)
    }
  })
  
  results <- reactiveVal(NULL)
  
  observeEvent(input$run_simulation, {
    req(input$n_datasets)
    withProgress(message = "Generating datasets...", value = 0, {
      if (isTRUE(input$auto_generate_thresholds)) {
        thresholds(generate_thresholds(input$rating_scale_min, input$rating_scale_max))
      } else {
        n_thresholds_needed <- input$rating_scale_max - input$rating_scale_min
        if (length(thresholds()) != n_thresholds_needed) {
          thresholds(generate_thresholds(input$rating_scale_min, input$rating_scale_max))
          showNotification(
            "Thresholds were regenerated due to mismatch with rating scale.",
            type = "warning",
            duration = 5
          )
        }
      }
      current_thresholds <- thresholds()
      n_sets <- input$n_datasets
      base_seed <- input$seed_value
      dataset_names <- sprintf("Dataset %03d", seq_len(n_sets))
      dataset_list <- vector("list", n_sets)
      for (i in seq_len(n_sets)) {
        dataset_seed <- base_seed + i - 1
        generated <- generate_mfrm_data(
          n_participants = input$n_participants,
          n_raters = input$n_raters,
          n_tasks = input$n_tasks,
          n_criteria = input$n_criteria,
          residual_type = as.numeric(input$residual_type),
          residual_sd = input$residual_sd,
          within_facet_corr = input$within_facet_corr,
          participant_distribution = input$participant_distribution,
          participant_mean = input$participant_mean,
          participant_sd = input$participant_sd,
          rater_mean = input$rater_mean,
          rater_sd = input$rater_sd,
          task_mean = input$task_mean,
          task_sd = input$task_sd,
          criteria_mean = input$criteria_mean,
          criteria_sd = input$criteria_sd,
          rater_effect_size = input$rater_effect_size,
          task_effect_size = input$task_effect_size,
          criteria_effect_size = input$criteria_effect_size,
          threshold_values = current_thresholds,
          rating_scale_min = input$rating_scale_min,
          rating_scale_max = input$rating_scale_max,
          cross_design = input$cross_design,
          rater_assignment_rate = input$rater_assignment_rate,
          task_assignment_rate = input$task_assignment_rate,
          criteria_assignment_rate = input$criteria_assignment_rate,
          missing_data_rate = input$missing_data_rate,
          balanced_blocks = input$balanced_blocks,
          seed_value = dataset_seed
        )
        dataset_list[[i]] <- list(
          name = dataset_names[i],
          seed = dataset_seed,
          data = generated$data,
          metadata = generated$metadata
        )
        incProgress(1 / max(1, n_sets), detail = sprintf("Generated %s", dataset_names[i]))
      }
      results(list(
        datasets = dataset_list,
        dataset_names = dataset_names,
        params = get_current_params()
      ))
    })
    showNotification(
      sprintf("Generated %d dataset%s.", input$n_datasets, ifelse(input$n_datasets == 1, "", "s")),
      type = "message",
      duration = 4
    )
  })
  
  selected_dataset <- reactive({
    res <- results()
    req(res)
    selected_name <- input$dataset_selector
    if (is.null(selected_name) || !(selected_name %in% res$dataset_names)) {
      selected_name <- res$dataset_names[1]
    }
    res$datasets[[match(selected_name, res$dataset_names)]]
  })
  
  output$dataset_selector_ui <- renderUI({
    res <- results()
    if (is.null(res)) {
      helpText("Run the simulation to generate datasets.")
    } else {
      selectInput(
        "dataset_selector",
        "Select dataset to preview:",
        choices = res$dataset_names,
        selected = res$dataset_names[1]
      )
    }
  })
  
  output$selected_metadata <- renderTable({
    dataset <- selected_dataset()
    metadata_to_table(dataset)
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$dataset_summary <- renderTable({
    res <- results()
    req(res)
    create_dataset_summary(res$datasets)
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$data_table <- renderDT({
    dataset <- selected_dataset()
    dataset$data |>
      select(participant_id, rater_id, task, criteria, expected_score, std_residual, score) |>
      datatable(options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$param_table <- renderTable({
    res <- results()
    req(res)
    params <- res$params
    residual_labels <- c(
      "Random Residuals",
      "Rater-Related Residuals",
      "Task-Related Residuals",
      "Criteria-Related Residuals"
    )
    residual_type_name <- residual_labels[params$residual_type]
    param_data <- data.frame(
      Parameter = c(
        "Number of datasets",
        "Residual Structure Type",
        "Residual Standard Deviation",
        "Within-Facet Correlation",
        "Participant Ability Variance",
        "Participant Ability Distribution",
        "Rater Effect Size",
        "Task Effect Size",
        "Criteria Effect Size",
        "Rating Scale",
        "Number of Thresholds"
      ),
      Value = c(
        params$n_datasets,
        residual_type_name,
        format(params$residual_sd, nsmall = 2),
        format(params$within_facet_corr, nsmall = 2),
        format(params$participant_sd, nsmall = 2),
        params$participant_distribution,
        format(params$rater_effect_size, nsmall = 2),
        format(params$task_effect_size, nsmall = 2),
        format(params$criteria_effect_size, nsmall = 2),
        paste(params$rating_scale_min, "~", params$rating_scale_max),
        length(params$threshold_values)
      )
    )
    param_data
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$full_params <- renderPrint({
    res <- results()
    req(res)
    res$params
  })
  
  output$download_selected_csv <- downloadHandler(
    filename = function() {
      dataset_name <- input$dataset_selector
      if (is.null(dataset_name)) dataset_name <- "dataset"
      dataset_name <- dataset_slug(dataset_name)
      paste0("mfrm-", dataset_name, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      dataset <- selected_dataset()
      export_df <- prepare_dataset_for_export(dataset$data)
      write.csv(export_df, file, row.names = FALSE)
    }
  )
  
  output$download_selected_rds <- downloadHandler(
    filename = function() {
      dataset_name <- input$dataset_selector
      if (is.null(dataset_name)) dataset_name <- "dataset"
      dataset_name <- dataset_slug(dataset_name)
      paste0("mfrm-", dataset_name, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rds")
    },
    content = function(file) {
      dataset <- selected_dataset()
      saveRDS(dataset, file)
    }
  )

  output$download_all_csv <- downloadHandler(
    filename = function() {
      paste0("mfrm-datasets-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      res <- results()
      req(res)
      combined <- purrr::map2_dfr(
        res$datasets,
        res$dataset_names,
        ~ prepare_dataset_for_export(.x$data, label = .y)
      )
      write.csv(combined, file, row.names = FALSE)
    }
  )

  output$download_all_zip <- downloadHandler(
    filename = function() {
      paste0("mfrm-datasets-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".zip")
    },
    content = function(file) {
      res <- results()
      req(res)
      if (length(res$datasets) == 0) {
        stop("No datasets available for download.")
      }
      tmp_dir <- tempfile(pattern = "mfrm_csvs_")
      dir.create(tmp_dir)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
      csv_paths <- purrr::map2_chr(res$datasets, res$dataset_names, function(dataset, name) {
        export_df <- prepare_dataset_for_export(dataset$data)
        file_path <- file.path(tmp_dir, paste0(dataset_slug(name), ".csv"))
        write.csv(export_df, file_path, row.names = FALSE)
        file_path
      })
      zip::zipr(zipfile = file, files = basename(csv_paths), root = tmp_dir)
    }
  )
  
  output$download_all_rds <- downloadHandler(
    filename = function() {
      paste0("mfrm-datasets-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rds")
    },
    content = function(file) {
      res <- results()
      req(res)
      saveRDS(res, file)
    }
  )
}

shinyApp(ui = ui, server = server)
