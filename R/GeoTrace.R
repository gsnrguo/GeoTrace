#' geotrace:
#'
#' @description Analysis potential origins of infectious diseases from spatial distance
#'
#' @usage geotrace(case,origins,pop, nsim=999, prob = NULL, agg="median",
#'       dist_type = ifelse(isTRUE(st_is_longlat(case)), "Great Circle", "Euclidean"),
#'       alternative = "two.sided", orig_names = NULL, seed = NULL)
#' \method{print}{geotrace}(x, ...)
#' \method{plot}{geotrace}(x, ...)
#'
#' @aliases geotrace print.geotrace plot.geotrace
#'
#' @param case sf object, cases data
#' @param origins sf object, potential origins of infectious diseases
#' @param pop  sf object, population of residences
#' @param nsim int, repeat times of sampling
#' @param prob vector of probability weights for obtaining the elements of the
#'             vector being sampled
#' @param agg character, distance aggregation method, either (default), or "mean".
#' @param dist_type character; for Cartesian coordinates only: one of Euclidean,
#'                  Hausdorff or Frechet; for geodetic coordinates, great circle
#'                  distances are computed; sf::st_distance()
#' @param alternative character, the alternative hypothesis: either "two.sided" (default),
#'                    "greater" or "less".
#' @param orig_names a vector of the names of potential origins
#' @param seed int, random seed
#' @param x a list of \code{geotrace} result
#' @param ... Ignore
#'
#' @importFrom sf st_crs st_distance st_transform st_is_longlat
#' @importFrom graphics abline hist par
#' @importFrom stats median
#'
#' @examples
#' ###############
#' ## JohnSnow 1854 Broad Street cholera outbreak
#' ###############
#' cases <- house_death[house_death$death_dum==1,]
#' BS_cholera <- geotrace(cases, pumps, house_death)
#' BS_cholera
#' plot(BS_cholera)
#'
#' \dontrun{
#' #############
#' ## Origins of Wuhan Covid-19
#' #############
#' orig_names <- location$Name
#' WH_cov19 <- geotrace(covid19_wh,location, wuhan_pop, prob = wuhan_pop$Wuhan_PPP,
#'            orig_names = orig_names)
#' WH_cov19
#' plot(WH_cov19)
#' }
#' @export
#'
geotrace <- function(case,origins,pop, nsim=999, prob = NULL, agg="median",
                     dist_type = ifelse(isTRUE(st_is_longlat(case)), "Great Circle", "Euclidean"),
                     alternative = "two.sided", orig_names = NULL, seed = NULL){
  crs_info <- st_crs(case)
  if (!(alternative %in% c("two.sided","greater", "less"))){
    stop("alternative must be one of two.sided, greater and less" )
  }

  if (st_crs(origins) != crs_info){
    stop("The case and origins must have same crs ")
  }

  if (st_crs(pop) != crs_info){
    stop("The pop and origins must have same crs ")
  }

  if (is.null(orig_names)){
    orig_names <- paste0("orig",seq(1,nrow(origins)))
  }

  if (!is.null(seed)){
    seeds <- c(1:nsim) + seed
  }
  agg_dis <- function(dis_mat) {
    if (agg == "mean") {
      dis <- colMeans(dis_mat)
    }
    if (agg == "median") {
      dis = apply(dis_mat, 2, median)
    }
    dis
  }

  dist_case <- st_distance(case, origins,which = dist_type)
  #
  dist_all <- agg_dis(dist_case)
  dist_cases <- dist_all
  nsample <- nrow(case)
  for (i in c(1:nsim)){
    if (!is.null(seed)){
      set.seed(seeds[i])
    }
    samp_idx <- sample(seq_len(nrow(pop)), nsample, prob=prob, replace = TRUE)
    samp <- pop[samp_idx,]
    dist_mat <- st_distance(samp,origins, which = dist_type)
    # browser()
    dist_all <- rbind(dist_all, agg_dis(dist_mat))
  }
  dist_all <- scale(dist_all, center = TRUE, scale = FALSE)
  if (alternative=="less"){
    p_value <- 1 - apply(dist_all,2, FUN = function(x) (sum(x[1] < x)))/(nsim +1)
  } else if (alternative=="greater"){
    p_value <- 1 - apply(dist_all,2, FUN = function(x) (sum(x[1] > x)))/(nsim +1)
  }else{
    p_value <- apply(dist_all,2, FUN = function(x) (sum(abs(x[1]) < abs(x))))/(nsim +1)
  }
  result <-
    list(
      p_value = p_value,
      alternative = alternative,
      dist_case = dist_cases,
      dist_type = dist_type,
      aggregation = agg,
      dist = dist_all,
      orig_names = orig_names
    )
  class(result) <- "geotrace"
  return(result)
}

print.geotrace <- function(x, ...){
  sig_data <- rbind(x[['dist_case']],x[['p_value']])
  rownames(sig_data) <- c("dist","p-value")
  col <- x[['orig_names']]
  colnames(sig_data) <- col
  print(sig_data)
  print(paste0("Distances: ", x[['dist_type']]))
  print(paste0("Aggregation of distances : ", x[['aggregation']]))
  print(paste0("Alternative: ", x[['alternative']]))

}

plot.geotrace <- function(x, ...) {
  dist <- x[['dist']]
  col <- x[['orig_names']]
  if (any(class(dist) == "numeric")) {
    hist(
      dist,
      col = "black",
      breaks = 50,
      xlab = col[i],
      main = paste0("Hist of dist-", i)
    )
    abline(v = dist[1], col = "blue", lwd = 2)
  } else{
    p <- ncol(dist)
    prow <- ceiling(p / 3)
    if (prow == 1) {
      pcol <- p
    }
    else{
      pcol <- 3
    }
    par(mfrow = c(prow, pcol))
    for (i in c(1:p)) {
      hist(
        dist[, i],
        col = "black",
        breaks = 50,
        xlab = col[i],
        main = paste0("Hist of dist-", i)
      )
      abline(v = dist[1, i],
             col = "blue",
             lwd = 2)
    }
  }

}
