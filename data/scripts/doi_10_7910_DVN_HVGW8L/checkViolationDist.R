checkViolationDist <- function(df, dist){
    count <- df %>%
        mutate(dist2closestKiln = unclass(dist2closestKiln)) %>%
        filter(dist2closestKiln <= dist) %>%
        tally()
    return(count)
}
