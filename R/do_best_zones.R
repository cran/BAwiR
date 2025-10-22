#' Best players by zone
#' 
#' @aliases do_best_zones
#'
#' @description 
#' Creates a visualization of the players who shoot little and score a lot 
#' in several zones at the same time.
#' 
#' @usage 
#' do_best_zones(data_best_archetypoid)
#' 
#' @param data_best_archetypoid Best players by zone computed with the
#' archetypoid algorithm.
#' 
#' @return 
#' A plot.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \link[Anthropometry]{archetypoids}
#' 
#' @examples 
#' \dontrun{
#' library(dplyr)
#' library(Anthropometry)
#' 
#' zones_court <- metrics_player_zone %>% 
#'   distinct(location) %>%
#'   pull()
#' 
#' numArch <- 10 
#' numRep <- 20
#' numArchoid <- 2 # Number of archetypoids.
#' 
#' data_arch <- data.frame()
#' 
#' # Run the algorithm for each zone one by one and save the archetypoid 
#' # with least shots and highest percentage.
#' i <- 1
#' 
#' zone <- metrics_player_zone %>% 
#'   filter(location == zones_court[i]) %>% 
#'   select(-pps_player)
#' 
#' zone_num <- zone %>% 
#'   select(total, perc_player) 
#' 
#' lass <- stepArchetypesRawData(data = zone_num, numArch = 1:numArch, 
#'                               numRep = numRep, verbose = FALSE)  
#' 
#' res_ns <- archetypoids(numArchoid, zone_num, huge = 200, step = FALSE, 
#'                        ArchObj = lass, nearest = "cand_ns",sequ = TRUE)
#' zone[res_ns$cases, ]                        
#' 
#' # Here [1, ] indicates the archetypoid of interest. Change it accordingly.
#' # Here 4 indicates the number of similar players to the archetypoid. Change it accordingly.
#' arch_targ <- zone[order(res_ns$alphas[1, ], decreasing = TRUE)[1:4], ]
#' data_arch <- rbind(data_arch, arch_targ)
#' 
#' i <- 2
#' 
#' zone <- metrics_player_zone %>% 
#'   filter(location == zones_court[i]) %>% 
#'   select(-pps_player)
#' 
#' zone_num <- zone %>% 
#'   select(total, perc_player) 
#' 
#' lass <- stepArchetypesRawData(data = zone_num, numArch = 1:numArch, 
#'                               numRep = numRep, verbose = FALSE)  
#' 
#' res_ns <- archetypoids(numArchoid, zone_num, huge = 200, step = FALSE, 
#'                        ArchObj = lass, nearest = "cand_ns",sequ = TRUE)
#' 
#' arch_targ <- zone[order(res_ns$alphas[2, ], decreasing = TRUE)[1:10], ]
#' data_arch <- rbind(data_arch, arch_targ)
#' 
#' do_best_zones(data_arch)
#' }
#'
#' @export

do_best_zones <- function(data_best_archetypoid) {
  player <- location <- id <- zone <- zones <- ids <- NULL
  
  players_rep <- data_best_archetypoid %>%
    count(player) %>%
    arrange(-n) %>%
    filter(n > 1) %>%
    pull(player)
  
  data_arch1 <- data_best_archetypoid %>% 
    select(player, location) %>% 
    rename(id = 1, zone = 2) %>%
    filter(id %in% players_rep) #%>%
    #mutate(id = gsub(".*\\. |.*\\.", "", id)) %>%
    #mutate(id = gsub(".* ", "", id))
  
  # Collapse into unique zone memberships per id:
  # df_sets ensures each ID has a list of zones.
  df_sets <- data_arch1 %>%
    distinct(id, zone) %>%
    group_by(id) %>%
    summarise(zones = list(zone), .groups = "drop") %>%
    ungroup()
  
  # Count IDs per set and concatenate their labels:
  df_counts <- df_sets %>%
    group_by(zones) %>%
    summarise(
      n = n(),
      ids = paste(id, collapse = ",\n"),
      .groups = "drop"
    ) %>%
    arrange(desc(n))
  
  # Plot with text labels:
  gg <- ggplot(df_counts, aes(x = zones, y = n)) +
    geom_col(fill = "white", colour = "black", width = 0.2) +
    geom_text(aes(label = ids), vjust = -0.7, size = 2.3, angle = 35) +
    ggupset::scale_x_upset(order_by = "degree", 
                  reverse = TRUE, 
                  sets = c("paint", "2pt_center", "2pt_left", "2pt_right", "3pt_center", "3pt_left", "3pt_left_corner", 
                           "3pt_right", "3pt_right_corner")) +
    scale_y_continuous(limits = c(0, 2.6), breaks = c(0, 1, 2)) +
    labs(x = "", y = "", title = "Players who shoot little and score a lot in more than one court region.") +
    theme_minimal() +
    theme(axis.text = element_text(size = 6)) +
    ggupset::theme_combmatrix(combmatrix.panel.point.color.fill = "black",
                     combmatrix.panel.line.size = 0,
                     combmatrix.label.text = element_text(size = 9))
  
  return(gg)
}
