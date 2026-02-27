library(tidyverse)

download_chr <- function(url, destfile) {
    tryCatch(
        {
            download.file(url = url, destfile = destfile)
            unzip(zipfile = destfile, exdir = dirname(destfile))
            unlink(destfile)
        },
        error = function(e) {
            warning("Failed to download: ", url)
        }
    )

    Sys.sleep(0.5)
}

iso3 <- c(
    "AUT",
    "BEL",
    "CAN",
    "CHL",
    "CZE",
    "DNK",
    "ECU",
    "EST",
    "FIN",
    "FRA",
    "DEU",
    "GRC",
    "HUN",
    "IRL",
    "ISR",
    "ITA",
    "JPN",
    "KAZ",
    "KOR",
    "LTU",
    "MEX",
    "NLD",
    "NZL",
    "NOR",
    "PER",
    "POL",
    "RUS",
    "SGP",
    "SVK",
    "SVN",
    "ESP",
    "SWE",
    "TUR",
    "GBR",
    "USA"
) %>%
    tolower()


df_urls <- expand_grid(round = 1:2, iso3 = iso3) %>%
    mutate(
        url = glue::glue(
            "https://webfs.oecd.org/piaac/cy{round}-puf-data/SPSS/prg{iso3}p{round}_sav.zip"
        )
    )

urls <- df_urls$url

walk2(.x = urls, .f = download_chr)
