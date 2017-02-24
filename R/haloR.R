# haloR Provides R interface to the Halo 5 and Halo Wars 2 APIs kindly provided by 343i
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# Required packages
require(httr)
require(jsonlite)
require(png)

# Basic URLs to access various API features
# Halo 5
get_h5_api_url <- function(type = "profile") {
  url <- "https://www.haloapi.com/"
  if (type == "metadata") {
    url <- paste0(url, "metadata/h5/metadata/")
  } else if (type == "profile") {
    url <- paste0(url, "profile/h5/profiles/")
  } else if (type == "player_stats") {
    url <- paste0(url, "stats/h5/players/")
  } else if (type == "arena_stats") {
    url <- paste0(url, "stats/h5/arena/matches/")
  } else if (type == "campaign_stats") {
    url <- paste0(url, "stats/h5/campaign/matches/")
  } else if (type == "custom_stats") {
    url <- paste0(url, "stats/h5/custom/matches/")
  } else if (type == "warzone_stats") {
    url <- paste0(url, "stats/h5/warzone/matches/")
  } else if (type == "service") {
    url <- paste0(url, "stats/h5/servicerecords/")
  } else if (type == "event_stats") {
    url <- paste0(url, "stats/h5/matches/")
  } else if (type == "leaderboard") {
    url <- paste0(url, "stats/h5/player-leaderboards/csr/")
  } else if (type == "ugc") {
    url <- paste0(url, "ugc/h5/players/")
  }
  return(url)
}
# Halo Wars 2
get_hw2_api_url <- function(type = "") {
  url <- "https://www.haloapi.com/"
  if (type == "metadata") {
    url <- paste0(url, "metadata/hw2/")
  } else if (type == "stats") {
    url <- paste0(url, "stats/hw2/")
  }
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

###########################
#         HALO 5          #
###########################

##
# PROFILE
##
# Basic profile calls
#' Get spartan profile emblem image
#' @export
h5_ProfileEmblem <- function(player, size = 256, key = "") {
  # Gets the users in-game emblem in png format
  # Acceptable sizes: 95, 128, 190, 256, 512
  r_url <- paste0(get_h5_api_url("profile"),
                  tolower(player), "/emblem?size=", size)
  request <- getRequestIMG(r_url, key)
}

#' Get spartan profile spartan image
#' @export
h5_ProfileSpartan <- function(player, size = 256, crop = "full", key = "") {
  # Gets a picture of the users spartan in png format
  # Acceptable sizes: 95, 128, 190, 256, 512
  # Acceptable crop: "full", "portrait"
  r_url <- paste0(get_h5_api_url("profile"),
                  tolower(player), "/spartan?size=", size, "&crop=", crop)
  request <- getRequestIMG(r_url, key)
}

##
# STATS
##
# Functions to access various statistics about players and matches
#' Get CSR leaderboards for playlists
#' @export
h5_Leaderboard <- function(playlist = "", season = "", count = 200, key = "") {
  r_url <- paste0(get_h5_api_url("leaderboard"),
                  season, "/",
                  playlist, "/",
                  "?count=", count)
  request <- getRequestJSON(r_url, key)
}

#' Get event stats for a match
#' @export
h5_EventStats <- function(match_id = "", key = "") {
  r_url <- paste0(get_h5_api_url("event_stats"),
                  match_id, "/events")
  request <- getRequestJSON(r_url, key)
}

#' Get a player's most recent matches
#' @export
h5_RecentMatches <- function(player = "", modes = c("arena","campaign","custom","warzone"), start = 0, count = 25, key = "") {
  # Gets statistics on the most recent matches of the provides player
  # Acceptable modes: arena, campaign, custom, warzone
  # Acceptable start: 0-...
  # Acceptable count: 1-25
  r_url <- paste0(get_h5_api_url("player_stats"),
                  tolower(player),
                  "/matches?modes=", paste0(modes, collapse = ","),
                  "&start=", start,
                  "&count=", count)
  request <- getRequestJSON(r_url, key)
}

#' Get post-carnage report for given mode and match
#' @export
h5_PostCarnage <- function(mode = "", match_id = "", key = "") {
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

getArenaMatch <- function(match_id = "", key = "") {
  # Get post-carnage statistics from the given match_id
  r_url <- paste0(get_h5_api_url("arena_stats"),
                  tolower(match_id))
  request <- getRequestJSON(r_url, key)
}
getCampaignMatch <- function(match_id = "", key = "") {
  # Get post-carnage statistics from the given match_id
  r_url <- paste0(get_h5_api_url("campaign_stats"),
                  tolower(match_id))
  request <- getRequestJSON(r_url, key)
}
getCustomMatch <- function(match_id = "", key = "") {
  # Get post-carnage statistics from the given match_id
  r_url <- paste0(get_h5_api_url("custom_stats"),
                  tolower(match_id))
  request <- getRequestJSON(r_url, key)
}
getWarzoneMatch <- function(match_id = "", key = "") {
  # Get post-carnage statistics from the given match_id
  r_url <- paste0(get_h5_api_url("warzone_stats"),
                  tolower(match_id))
  request <- getRequestJSON(r_url, key)
}

#' Get spartan service record for a given mode
#' @export
h5_ServiceRecord <- function(mode = "", players = "", key = "", season = "") {
  # Abstraction of service record calls into a single funciton
  # Acceptable mode: "arena", "campaign", "custom", "warzone"
  if (mode == "arena") {
    return(getArenaService(players = players, season = season, key = key))
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

getArenaService <- function(players = "", season = "", key = "") {
  # Get the provided users service record for Arena matchmaking
  r_url <- paste0(get_h5_api_url("service"),
                  "arena/?players=", tolower(players))
  if (season != "") {
    r_url <- paste0(r_url, "&seasonId=", season)
  }
  request <- getRequestJSON(r_url, key)
}
getCampaignService <- function(players = "", key = "") {
  # Get the provided users service record for Campaign
  r_url <- paste0(get_h5_api_url("service"),
                  "campaign/?players=", tolower(players))
  request <- getRequestJSON(r_url, key)
}
getCustomService <- function(players = "", key = "") {
  # Get the provided users service record for Custom games
  r_url <- paste0(get_h5_api_url("service"),
                  "custom/?players=", tolower(players))
  request <- getRequestJSON(r_url, key)
}
getWarzoneService <- function(players = "", key = "") {
  # Get the provided users service record for Warzone matchmaking
  r_url <- paste0(get_h5_api_url("service"),
                  "warzone/?players=", tolower(players))
  request <- getRequestJSON(r_url, key)
}

##
# METADATA
##
# Functions to access various metadata (ids etc.)
#' Get campaign missions metadata
#' @export
h5_CampaignMissions <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "campaign-missions")
  request <- getRequestJSON(r_url, key)
}
#' Get commendation metadata
#' @export
h5_Commendations <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "commendations")
  request <- getRequestJSON(r_url, key)
}
#' Get CSR designations metadata
#' @export
h5_CsrDesignations <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "csr-designations")
  request <- getRequestJSON(r_url, key)
}
#' Get enemies metadata
#' @export
h5_Enemies <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "enemies")
  request <- getRequestJSON(r_url, key)
}
#' Get flexible stats metadata
#' @export
h5_FlexibleStats <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "flexible-stats")
  request <- getRequestJSON(r_url, key)
}
#' Get game base variants metadata
#' @export
h5_GameBaseVariants <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "game-base-variants")
  request <- getRequestJSON(r_url, key)
}
#' Get game variants metadata
#' @export
h5_GameVariants <- function(id = "", key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "game-variants/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
#' Get impulses metadata
#' @export
h5_Impulses <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "impulses")
  request <- getRequestJSON(r_url, key)
}
#' Get map variants metadata
#' @export
h5_MapVariants <- function(id = "", key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "map-variants/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
#' Get maps metadata
#' @export
h5_Maps <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "maps")
  request <- getRequestJSON(r_url, key)
}
#' Get medals metadata
#' @export
h5_Medals <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "medals")
  request <- getRequestJSON(r_url, key)
}
#' Get playlists metadata
#' @export
h5_Playlists <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "playlists")
  request <- getRequestJSON(r_url, key)
}
#' Get Requisition packs metadata
#' @export
h5_RequisitionPacks <- function(id = "", key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "requisition-packs/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
#' Get requisitions metadata
#' @export
h5_Requisitions <- function(id = "", key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "requisitions/", tolower(id))
  request <- getRequestJSON(r_url, key)
}
#' Get seasons metadata
#' @export
h5_Seasons <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "seasons")
  request <- getRequestJSON(r_url, key)
}
#' Get skulls metadata
#' @export
h5_Skulls <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "skulls")
  request <- getRequestJSON(r_url, key)
}
#' Get spartan ranks metadata
#' @export
h5_SpartanRanks <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "spartan-ranks")
  request <- getRequestJSON(r_url, key)
}
#' Get team colours metadata
#' @export
h5_TeamColors <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "team-colors")
  request <- getRequestJSON(r_url, key)
}
#' Get vehicles metadata
#' @export
h5_Vehicles <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "vehicles")
  request <- getRequestJSON(r_url, key)
}
#' Get weapons metadata
#' @export
h5_Weapons <- function(key = "") {
  r_url <- paste0(get_h5_api_url("metadata"),
                  "weapons")
  request <- getRequestJSON(r_url, key)
}


##
# UGC
##
# Functions to access player forge data
#' Get information about a player-created game variant
#' @export
h5_PlayerGameVariant <- function(player = "", variant = "", key = "") {
  r_url <- paste0(get_h5_api_url("ugc"),
                  player, "/",
                  "gamevariants", "/",
                  variant)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of game variants created by a player
#' @export
h5_PlayerGameVariants <- function(player = "", start = 0, count = 20, key = "") {
  r_url <- paste0(get_h5_api_url("ugc"),
                  player, "/",
                  "gamevariants",
                  "?start=", start,
                  "&count=", count)
  request <- getRequestJSON(r_url, key)
}

#' Get information about a player-created map variant
#' @export
h5_PlayerMapVariant <- function(player = "", variant = "", key = "") {
  r_url <- paste0(get_h5_api_url("ugc"),
                  player, "/",
                  "mapvariants", "/",
                  variant)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of map variants created by a player
#' @export
h5_PlayerMapVariants <- function(player = "", start = 0, count = 20, key = "") {
  r_url <- paste0(get_h5_api_url("ugc"),
                  player, "/",
                  "mapvariants",
                  "?start=", start,
                  "&count=", count)
  request <- getRequestJSON(r_url, key)
}

###########################
#      HALO WARS 2        #
###########################

##
# METADATA
##
#' Get a list of campaign levels
#' @export
hw2_CampaignLevels <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "campaign-levels",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of campaign logs
#' @export
hw2_CampaignLogs <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "campaign-logs",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of card keywords
#' @export
hw2_CardKeywords <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "card-keywords",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of cards
#' @export
hw2_Cards <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "cards",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of CSR designations
#' @export
hw2_CsrDesignations <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "csr-designations",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of difficulties
#' @export
hw2_Difficulties <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "difficulties",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of game object categories
#' @export
hw2_GameObjectCategories <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "game-object-categories",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of game objects
#' @export
hw2_GameObjects <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "game-objects",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of leader powers
#' @export
hw2_LeaderPowers <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "leader-powers",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of leaders
#' @export
hw2_Leaders <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "leaders",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of maps
#' @export
hw2_Maps <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "maps",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of packs
#' @export
hw2_Packs <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "packs",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of playlists
#' @export
hw2_Playlists <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "playlists",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of seasons
#' @export
hw2_Seasons <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "seasons",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of spartan ranks
#' @export
hw2_SpartanRanks <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "spartan-ranks",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

#' Get a list of techs
#' @export
hw2_Techs <- function(startAt = 0, key = "") {
  r_url <- paste0(get_hw2_api_url("metadata"),
                  "techs",
                  "?startAt=", startAt)
  request <- getRequestJSON(r_url, key)
}

##
# STATS
##
#' Get all events for a single match
#' @export
hw2_MatchEvents <- function(match = "", key = "") {
  r_url <- paste0(get_hw2_api_url("stats"),
                  "matches", "/",
                  match, "/events")
  request <- getRequestJSON(r_url, key)
}

#' Get match summary for a single match
#' @export
hw2_MatchResult <- function(match = "", key = "") {
  r_url <- paste0(get_hw2_api_url("stats"),
                  "matches", "/",
                  match)
  request <- getRequestJSON(r_url, key)
}

#' Get a player's campaign progress
#' @export
hw2_PlayerCampaignProgress <- function(player = "", key = "") {
  r_url <- paste0(get_hw2_api_url("stats"),
                  "players", "/",
                  player, "/campaign-progress")
  request <- getRequestJSON(r_url, key)
}

#' Get a player's match history
#' matchType can be 'custom' or 'matchmaking', or empty for both
#' @export
hw2_PlayerMatchHistory <- function(player = "", matchType = "", start = 0, count = 25, key = "") {
  r_url <- paste0(get_hw2_api_url("stats"),
                  "players", "/",
                  player, "/matches",
                  "?matchType=", matchType,
                  "&start=", start,
                  "&count=", count)
  request <- getRequestJSON(r_url, key)
}

#' Get a player's playlist ratings
#' @export
hw2_PlayerPlaylistRatings <- function(playlist = "", players = "", key = "") {
  if (length(players) > 6) {
    warning("Supplied vector of players too long..")
    return(NULL)
  }
  r_url <- paste0(get_hw2_api_url("stats"),
                  "playlist", "/",
                  playlist, "/rating?",
                  "players=", paste0(players, collapse = ","))
  request <- getRequestJSON(r_url, key)
}

#' Get a player's season stats summary
#' @export
hw2_PlayerSeasonStats <- function(player = "", season = "current", key = "") {
  r_url <- paste0(get_hw2_api_url("stats"),
                  "players", "/",
                  player, "/stats/seasons/",
                  season)
  request <- getRequestJSON(r_url, key)
}

#' Get a player's summary stats
#' @export
hw2_PlayerStats <- function(player = "", key = "") {
  r_url <- paste0(get_hw2_api_url("stats"),
                  "players", "/",
                  player, "/stats")
  request <- getRequestJSON(r_url, key)
}

#' Get players XP
#' @export
hw2_PlayersXp <- function(players = "", key = "") {
  if (length(players) > 6) {
    warning("Supplied vector of players too long..")
    return(NULL)
  }
  r_url <- paste0(get_hw2_api_url("stats"),
                  "xp", "?players=", paste0(players, collapse = ","))
  request <- getRequestJSON(r_url, key)
}
