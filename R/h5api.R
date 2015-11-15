# h5api Provides R interface to the Halo 5 API kindly provided by 343i
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# Required packages
require(httr)
require(jsonlite)
require(png)

# Basic URLs to access various API features
getUrl <- function(type = "profile") {
  url <- "https://www.haloapi.com/"
  if (type == "metadata") {
    url<- paste0(url_api, "metadata/h5/metadata/")
  } else if (type == "profile") {
    url <- paste0(url_api, "profile/h5/profiles/")
  } else if (type == "player_stats") {
    url <- paste0(url_api, "stats/h5/players/")
  } else if (type == "arena_stats") {
    url <- paste0(url_api, "stats/h5/arena/matches/")
  } else if (type == "campaign_stats") {
    url <- paste0(url_api, "stats/h5/campaign/matches/")
  } else if (type == "custom_stats") {
    url <- paste0(url_api, "stats/h5/custom/matches/")
  } else if (type == "warzone_stats") {
    url <- paste0(url_api, "stats/h5/warzone/matches/")
  } else if (type == "service") {
    url <- paste0(url_api, "stats/h5/servicerecords/")
  }
  return(url)
}

# Execute request to get an image (only used by two calls: emblem and spartan)
getRequestIMG <- function(url, key) {
  request <- httr::GET(url, httr::add_headers(
    "Ocp-Apim-Subscription-Key"=paste0(key))
  )
  image_data <- httr::content(request)
}
# Execute request to get data in JSON format
getRequestJSON <- function(url, key) {
  request <- httr::GET(url, httr::add_headers(
    "Ocp-Apim-Subscription-Key"=paste0(key))
  )
  json_data <- httr::content(request, as = "text")
  list_data <- jsonlite::fromJSON(json_data)
}

##
# PROFILE
##
# Basic profile calls
#' Get spartan profile emblem image
#' @export
getProfileEmblem <- function(player, size = 256, key = "") {
  # Gets the users in-game emblem in png format
  # Acceptable sizes: 95, 128, 190, 256, 512
  r_url <- paste0(getUrl("profile"),
                  tolower(player), "/emblem?size=", size)
  request <- getRequestIMG(r_url, key)
}

#' Get spartan profile spartan image
#' @export
getProfileSpartan <- function(player, size = 256, crop = "full", key = "") {
  # Gets a picture of the users spartan in png format
  # Acceptable sizes: 95, 128, 190, 256, 512
  # Acceptable crop: "full", "portrait"
  r_url <- paste0(getUrl("profile"),
                  tolower(player), "/spartan?size=", size, "&crop=", crop)
  request <- getRequestIMG(r_url, key)
}

##
# STATS
##
# Functions to access various statistics about players and matches
#' Get a player's most recent matches
#' @export
getRecentMatches <- function(url = url_player_stats, player = "", modes = "arena,campaign,custom,warzone", start = 0, count = 25, key = "") {
  # Gets statistics on the most recent matches of the provides player
  # Acceptable modes: arena, campaign, custom, warzone
  # Acceptable start: 0-...
  # Acceptable count: 1-25
  r_url <- paste0(url, tolower(player),
                  "/matches?modes=", modes,
                  "&start=", start,
                  "&count=", count)
  request <- getRequestJSON(r_url, key)
}

#' Get post-carnage report for given mode and match
#' @export
getPostCarnage <- function(mode = "", match_id = "", key = "") {
  # Abstraction of post-carnage calls into a single function
  # Acceptable mode: "arena", "campaign", "custom", "warzone"
  if (mode == "arena") {
    return(getArenaMatch(match_id = match_id, key = key))
  } else if (mode == "campaign") {
    return(getCampaignMatch(match_id = match_id, key = key))
  } else if (mode == "custom") {
    return(getCustomMatch(match_id = match_id, key = key))
  } else if (mode == "warzone") {
    return(getWarzoneMatch(match_id = match_id, key = key))
  } else {
    return("Incorrect Arguments")
  }
}

getArenaMatch <- function(url = url_arena_stats, match_id = "", key = "") {
  # Get post-carnage statistics from the given match_id
  r_url <- paste0(url, tolower(match_id))
  request <- getRequestJSON(r_url, key)
}
getCampaignMatch <- function(url = url_campaign_stats, match_id = "", key = "") {
  # Get post-carnage statistics from the given match_id
  r_url <- paste0(url, tolower(match_id))
  request <- getRequestJSON(r_url, key)
}
getCustomMatch <- function(url = url_custom_stats, match_id = "", key = "") {
  # Get post-carnage statistics from the given match_id
  r_url <- paste0(url, tolower(match_id))
  request <- getRequestJSON(r_url, key)
}
getWarzoneMatch <- function(url = url_warzone_stats, match_id = "", key = "") {
  # Get post-carnage statistics from the given match_id
  r_url <- paste0(url, tolower(match_id))
  request <- getRequestJSON(r_url, key)
}

#' Get spartan service record for a given mode
#' @export
getServiceRecord <- function(mode = "", players = "", key = "") {
  # Abstraction of service record calls into a single funciton
  # Acceptable mode: "arena", "campaign", "custom", "warzone"
  if (mode == "arena") {
    return(getArenaService(players = players, key = key))
  } else if (mode == "campaign") {
    return(getCampaignService(players = players, key = key))
  } else if (mode == "custom") {
    return(getCustomService(players = players, key = key))
  } else if (mode == "warzone") {
    return(getWarzoneService(players = players, key = key))
  } else {
    return("Incorrect Arguments")
  }
}

getArenaService <- function(url = url_service, players = "", key = "") {
  # Get the provided users service record for Arena matchmaking
  r_url <- paste0(url, "arena/?players=", tolower(players))
  request <- getRequestJSON(r_url, key)
}
getCampaignService <- function(url = url_service, players = "", key = "") {
  # Get the provided users service record for Campaign
  r_url <- paste0(url, "campaign/?players=", tolower(players))
  request <- getRequestJSON(r_url, key)
}
getCustomService <- function(url = url_service, players = "", key = "") {
  # Get the provided users service record for Custom games
  r_url <- paste0(url, "custom/?players=", tolower(players))
  request <- getRequestJSON(r_url, key)
}
getWarzoneService <- function(url = url_service, players = "", key = "") {
  # Get the provided users service record for Warzone matchmaking
  r_url <- paste0(url, "warzone/?players=", tolower(players))
  request <- getRequestJSON(r_url, key)
}

##
# METADATA
##
# Functions to access various metadata (ids etc.)
#' Get campaign missions metadata
#' @export
getCampaignMissions <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "campaign-missions")
  request <- getRequestJSON(r_url, key)
}
#' Get commendation metadata
#' @export
getCommendations <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "commendations")
  request <- getRequestJSON(r_url, key)
}
#' Get CSR designations metadata
#' @export
getCsrDesignations <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "csr-designations")
  request <- getRequestJSON(r_url, key)
}
#' Get enemies metadata
#' @export
getEnemies <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "enemies")
  request <- getRequestJSON(r_url, key)
}
#' Get flexible stats metadata
#' @export
getFlexibleStats <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "flexible-stats")
  request <- getRequestJSON(r_url, key)
}
#' Get game base variants metadata
#' @export
getGameBaseVariants <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "game-base-variants")
  request <- getRequestJSON(r_url, key)
}
#' Get game variants metadata
#' @export
getGameVariants <- function(url = url_metadata, id = "", key = "") {
  r_url <- paste0(url, "game-variants/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
#' Get impulses metadata
#' @export
getImpulses <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "impulses")
  request <- getRequestJSON(r_url, key)
}
#' Get map variants metadata
#' @export
getMapVariants <- function(url = url_metadata, id = "", key = "") {
  r_url <- paste0(url, "map-variants/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
#' Get maps metadata
#' @export
getMaps <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "maps")
  request <- getRequestJSON(r_url, key)
}
#' Get medals metadata
#' @export
getMedals <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "medals")
  request <- getRequestJSON(r_url, key)
}
#' Get playlists metadata
#' @export
getPlaylists <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "playlists")
  request <- getRequestJSON(r_url, key)
}
#' Get Requisition packs metadata
#' @export
getRequisitionPacks <- function(url = url_metadata, id = "", key = "") {
  r_url <- paste0(url, "requisition-packs/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
#' Get requisitions metadata
#' @export
getRequisitions <- function(url = url_metadata, id = "", key = "") {
  r_url <- paste0(url, "requisitions/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
#' Get skulls metadata
#' @export
getSkulls <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "skulls")
  request <- getRequestJSON(r_url, key)
}
#' Get spartan ranks metadata
#' @export
getSpartanRanks <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "spartan-ranks")
  request <- getRequestJSON(r_url, key)
}
#' Get team colours metadata
#' @export
getTeamColors <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "team-colors")
  request <- getRequestJSON(r_url, key)
}
#' Get vehicles metadata
#' @export
getVehicles <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "vehicles")
  request <- getRequestJSON(r_url, key)
}
#' Get weapons metadata
#' @export
getWeapons <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "weapons")
  request <- getRequestJSON(r_url, key)
}
