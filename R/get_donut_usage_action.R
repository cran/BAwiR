#' Donut chart usage possessions action
#' 
#' @aliases get_donut_usage_action
#'
#' @description 
#' This function creates a donut chart that displays the percentage of 
#' possessions that each player of a team ends with a particular action
#' (turnover, field goal attempted or free throw attempted) while being 
#' on the court.
#' 
#' @usage 
#' get_donut_usage_action(data_usage_act, team_sel, type_play, 
#'                        language, min_poss, min_perc, size_orl,
#'                        size_irl = 4, vjust_title = 1)
#' 
#' @param data_usage_act Data frame with the number of possessions that each 
#' player ended and the particular action used.
#' @param team_sel String with the team's full name.
#' @param type_play Play type. Options are 'one' for free throws, 
#' 'two' for two-point field goals and 'three' for three-point 
#' field goals and 'tov' for turnovers.
#' @param language Language of the titles. Valid options are 'English' 
#' and 'Spanish' so far.
#' @param min_poss Minimum number of possessions played. 
#' @param min_perc Minimum percentage achieved.
#' @param size_orl Size of the outer ring labels.
#' @param size_irl Size of the inner ring labels.
#' @param vjust_title Adjust the title vertically when representing turnovers.
#' 
#' @return 
#' A donut chart.
#' 
#' @details
#' Donut charts are an alternative for pie charts, which have a hole 
#' in the middle, making them cleaner to read than pie charts. 
#'
#' @author 
#' Guillermo Vinue with the help of ChatGPT.
#' 
#' @seealso 
#' \code{\link{get_donut_usage}}
#' 
#' @examples 
#' \dontrun{
#' get_donut_usage_action(acb_usage_act_data_2526, "Valencia Basket", "two", "English", 1, 1, 3)
#' # For example, the interpretation here is that Sako finishes the 66.67% of his possessions
#' # scoring a two-point shot.
#'
#' get_donut_usage_action(acb_usage_act_data_2526, "Valencia Basket", "tov", "English", 1, 1, 5)
#' }
#'
#' @importFrom ggplot2 geom_rect scale_fill_identity
#' @importFrom tidyr uncount
#' @importFrom dplyr first
#'
#' @export

get_donut_usage_action <- function(data_usage_act, team_sel, type_play, 
                                   language, min_poss, min_perc, size_orl, 
                                   size_irl = 4, vjust_title = 1) {
  team <- poss_end <- poss_num <- usage_perc <- player <- NULL
  action <- percentage <- total <- fraction <- ymax <- ymin <- NULL
  ymin_action <- perc_frac <- ymax_action <- fill_col <- label <- NULL
  cumulative <- label_pos <- usage_perc_label <- NULL

  df0 <- data_usage_act %>%
    filter(team == team_sel) 
  
  df1 <- df0 %>%
    uncount(weights = poss_end) %>%
    count(player, action, name = "poss_end") %>%
    as_tibble()
  
  df2 <- df1 %>%
    group_by(player) %>%
    mutate(poss_num = sum(poss_end)) %>%
    ungroup() %>%
    mutate(percentage = round((poss_end / poss_num) *100, 2))
  
  if (type_play != "tov") {
    if (language == "English") {
      to_act <- c("Made", "Missed")
    }else{
      to_act <- c("Anotado", "Fallado")
    }
    
    if (type_play == "one") {
      df3 <- df2 %>%
        filter(grepl("Libre", action)) %>%
        mutate(action = plyr::mapvalues(action,
                                        from = c("Tiro Libre anotado", "Tiro Libre fallado"),
                                        to = to_act)) 
      
      if (language == "English") {
        hole_label <- "Free throws"
      }else{
        hole_label <- "Tiros libres" 
      }
    }else if (type_play == "two") {
      df3 <- df2 %>%
        filter(grepl("de 2", action)) %>%
        mutate(action = plyr::mapvalues(action,
                                        from = c("Tiro de 2 anotado", "Tiro de 2 fallado"),
                                        to = to_act))
      
      if (language == "English") {
        hole_label <- "Two-point shots"
      }else{
        hole_label <- "Tiros de 2" 
      }
    }else if (type_play == "three") {
      df3 <- df2 %>%
        filter(grepl("Triple", action)) %>%
        mutate(action = plyr::mapvalues(action,
                                        from = c("Triple anotado", "Triple fallado"),
                                        to = to_act))
      
      if (language == "English") {
        hole_label <- "Three-point shots"
      }else{
        hole_label <- "Triples" 
      }
    }else{
      stop("Invalid option.")
    } 
    
    df_all <- df3 %>%
      group_by(action) %>%
      mutate(total = sum(poss_end)) %>%
      ungroup() %>% 
      filter(poss_num >= min_poss) %>%
      arrange(action, desc(percentage)) %>%
      select(-contains("poss")) %>%
      mutate(player = gsub(".\\. ", "", player)) %>%
      filter(percentage >= min_perc)
    
    if (length(unique(df_all$action)) == 1) {
      if (language == "English") {
        txt_lab <- "One of the two categories does not reach \n the minimum percentage."
      }else{
        txt_lab <- "Uno de las dos tipos no alcanza \n ese porcentaje tan reducido." 
      }
      gg <- ggplot() + annotate("text", x = 4, y = 1, size = 8, label = txt_lab) + theme_void()
    }else{
      # ---- INNER RING ----
      df_inner <- df_all %>%
        distinct(action, total) %>%
        mutate(
          fraction = total / sum(total),
          ymax = cumsum(fraction),
          ymin = lag(ymax, default = 0),
          label = paste0(action, "\n", round(fraction * 100, 1), "%\n(", total, "/", sum(total), ")"),
          fill_col = c("green3", "red3")
        )
      
      # ---- OUTER RING ----
      # For each action, get ymin/ymax boundaries from the inner ring
      df_outer <- df_all %>%
        left_join(df_inner %>% select(action, ymin_action = ymin, ymax_action = ymax, total), by = "action") %>%
        group_by(action) %>%
        mutate(
          perc_frac = percentage / sum(percentage),
          ymax = ymin_action + cumsum(perc_frac * (ymax_action - ymin_action)),
          ymin = ymin_action + lag(cumsum(perc_frac * (ymax_action - ymin_action)), default = 0),
          fill_col = if (first(action) %in% c("Anotado", "Made")) {
            scales::seq_gradient_pal("palegreen1", "green4", "Lab")(scales::rescale(percentage))
          } else {
            scales::seq_gradient_pal("mistyrose", "red4", "Lab")(scales::rescale(percentage))
          }
        ) %>%
        ungroup()
      
      # ---- PLOT ----
      gg <- ggplot() +
        # Outer ring:
        geom_rect(
          data = df_outer,
          aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = fill_col),
          color = "white"
        ) +
        # Inner ring:
        geom_rect(
          data = df_inner,
          aes(ymax = ymax, ymin = ymin, xmax = 3, xmin = 2, fill = fill_col),
          color = "white"
        ) +
        scale_fill_identity() +
        coord_polar(theta = "y") +
        xlim(c(0, 4)) +
        theme_void() +
        # Inner ring labels:
        geom_text(
          data = df_inner,
          aes(x = 2.5, y = (ymin + ymax)/2, label = label),
          color = "white",
          fontface = "bold",
          size = size_irl
        ) +
        # Outer ring labels:
        geom_text(
          data = df_outer,
          aes(x = 3.5, y = (ymin + ymax)/2, label = paste0(player, "\n", percentage, "%")),
          color = "black",
          size = size_orl
        ) +
        # Center text in the donut hole
        annotate("text", x = 0, y = 0, 
                 label = hole_label, 
                 size = 8, fontface = "bold", lineheight = 1.2) +
        labs(title = NULL) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    }
  }else{
    df3 <- df2 %>%
      filter(action == "P\u00e9rdida") %>%
      select(-action) %>%
      rename(usage_perc = percentage) %>% 
      filter(poss_num >= min_poss) %>%
      filter(usage_perc >= min_perc)
    
    df4 <- df3 %>%
      arrange(desc(usage_perc)) %>%
      mutate(
        cumulative = cumsum(usage_perc),
        label_pos = cumulative - usage_perc / 2
      ) %>%
      mutate(usage_perc_label = usage_perc) %>%
      mutate(player = gsub(".*\\. ", "", player))
    
    if (language == "English") {
      hole_label <- "Turnovers"
    }else{
      hole_label <- "Balones perdidos" 
    }
    
    gg <- ggplot(df4, aes(x = 2, y = usage_perc, fill = usage_perc)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      theme_void() +
      scale_fill_gradient(low = "red", high = "green") +
      labs(title = hole_label) +
      # Category:
      geom_text(aes(y = label_pos, label = player), color = "black", size = size_orl, vjust = -2) +
      # Percentage:
      geom_text(aes(y = label_pos, label = paste0(usage_perc_label, "%")), color = "black", size = size_orl) +
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5, vjust = vjust_title, size = 16, face = "bold"))
  }
  
  return(gg)
}
