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
url_api <- "https://www.haloapi.com/"

url_metadata <- paste0(url_api, "metadata/h5/metadata/")
url_profile <- paste0(url_api, "profile/h5/profiles/")
url_player_stats <- paste0(url_api, "stats/h5/players/")

url_arena_stats <- paste0(url_api, "stats/h5/arena/matches/")
url_campaign_stats <- paste0(url_api, "stats/h5/campaign/matches/")
url_custom_stats <- paste0(url_api, "stats/h5/custom/matches/")
url_warzone_stats <- paste0(url_api, "stats/h5/warzone/matches/")

url_service <- paste0(url_api, "stats/h5/servicerecords/")

# Execute request to get an image (only used by two calls: emblem and spartan)
getRequestIMG <- function(url, key) {
  request <- GET(url, add_headers(
    "Ocp-Apim-Subscription-Key"=paste0(key))
  )
  image_data <- content(request)
}
# Execute request to get data in JSON format
getRequestJSON <- function(url, key) {
  request <- GET(url, add_headers(
    "Ocp-Apim-Subscription-Key"=paste0(key))
  )
  json_data <- content(request, as = "text")
  list_data <- fromJSON(json_data)
}

##
# PROFILE
##
# Basic profile calls
getProfileEmblem <- function(url = url_profile, player, size = 256, key = "") {
  # Gets the users in-game emblem in png format
  # Acceptable sizes: 95, 128, 190, 256, 512
  r_url <- paste0(url, tolower(player), "/emblem?size=", size)
  request <- getRequestIMG(r_url, key)
}

getProfileSpartan <- function(url = url_profile, player, size = 256, crop = "full", key = "") {
  # Gets a picture of the users spartan in png format
  # Acceptable sizes: 95, 128, 190, 256, 512
  # Acceptable crop: "full", "portrait"
  r_url <- paste0(url, tolower(player), "/spartan?size=", size, "&crop=", crop)
  request <- getRequestIMG(r_url, key)
}

##
# STATS
##
# Functions to access various statistics about players and matches
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
getWarzomeService <- function(url = url_service, players = "", key = "") {
  # Get the provided users service record for Warzone matchmaking
  r_url <- paste0(url, "warzone/?players=", tolower(players))
  request <- getRequestJSON(r_url, key)
}

##
# METADATA
##
# Functions to access various metadata (ids etc.)
getCampaignMissions <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "campaign-missions")
  request <- getRequestJSON(r_url, key)
}
getCommendations <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "commendations")
  request <- getRequestJSON(r_url, key)
}
getCsrDesignations <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "csr-designations")
  request <- getRequestJSON(r_url, key)
}
getEnemies <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "enemies")
  request <- getRequestJSON(r_url, key)
}
getFlexibleStats <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "flexible-stats")
  request <- getRequestJSON(r_url, key)
}
getGameBaseVariants <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "game-base-variants")
  request <- getRequestJSON(r_url, key)
}
getGameVariants <- function(url = url_metadata, id = "", key = "") {
  r_url <- paste0(url, "game-variants/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
getImpulses <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "impulses")
  request <- getRequestJSON(r_url, key)
}
getMapVariants <- function(url = url_metadata, id = "", key = "") {
  r_url <- paste0(url, "map-variants/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
getMaps <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "maps")
  request <- getRequestJSON(r_url, key)
}
getMedals <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "medals")
  request <- getRequestJSON(r_url, key)
}
getPlaylists <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "playlists")
  request <- getRequestJSON(r_url, key)
}
getRequisitionPacks <- function(url = url_metadata, id = "", key = "") {
  r_url <- paste0(url, "requisition-packs/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
getRequisitions <- function(url = url_metadata, id = "", key = "") {
  r_url <- paste0(url, "requisitions/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
getSkulls <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "skulls")
  request <- getRequestJSON(r_url, key)
}
getSpartanRanks <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "spartan-ranks")
  request <- getRequestJSON(r_url, key)
}
getTeamColors <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "team-colors")
  request <- getRequestJSON(r_url, key)
}
getVehicles <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "vehicles")
  request <- getRequestJSON(r_url, key)
}
getWeapons <- function(url = url_metadata, key = "") {
  r_url <- paste0(url, "weapons")
  request <- getRequestJSON(r_url, key)
}
