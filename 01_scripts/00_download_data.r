library(tidyverse)
library(here)
library(glue)

# Download, unzip, delete zip. Creates a .done marker on success so
# re-running skips already-downloaded files.
download_chr <- function(url, destfile) {
    marker <- paste0(destfile, ".done")
    if (file.exists(marker)) {
        message("Skipping (already downloaded): ", basename(destfile))
        return(invisible(NULL))
    }

    tryCatch(
        {
            download.file(url = url, destfile = destfile, mode = "wb")
            unzip(zipfile = destfile, exdir = dirname(destfile))
            unlink(destfile)
            file.create(marker)
        },
        error = function(e) {
            warning("Failed to download: ", url)
        }
    )

    Sys.sleep(0.5)
}

#------------------------------------------------------------------------------#
#  PIAAC ----
#------------------------------------------------------------------------------#

piaac_dir <- here("00_data")
dir.create(piaac_dir, recursive = TRUE, showWarnings = FALSE)

iso3 <- c(
    "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "ECU", "EST", "FIN", "FRA",
    "DEU", "GRC", "HUN", "IRL", "ISR", "ITA", "JPN", "KAZ", "KOR", "LTU",
    "MEX", "NLD", "NZL", "NOR", "PER", "POL", "RUS", "SGP", "SVK", "SVN",
    "ESP", "SWE", "TUR", "GBR", "USA"
) %>%
    tolower()

df_piaac <- expand_grid(round = 1:2, iso3 = iso3) %>%
    mutate(
        url = glue(
            "https://webfs.oecd.org/piaac/cy{round}-puf-data/SPSS/prg{iso3}p{round}_sav.zip"
        ),
        destfile = file.path(piaac_dir, glue("prg{iso3}p{round}_sav.zip"))
    )

walk2(
    .x = df_piaac$url,
    .y = df_piaac$destfile,
    .f = download_chr
)

#------------------------------------------------------------------------------#
#  PISA ----
#------------------------------------------------------------------------------#

# 2015–2022: SPSS (.sav) — includes reading & maths plausible values
# 2006–2012: TXT format (fixed-width); note these will need different parsing
#             logic than the SPSS files when cleaning.

pisa_dir <- here("00_data", "pisa")
dir.create(pisa_dir, recursive = TRUE, showWarnings = FALSE)

pisa_urls <- tibble(
    year = c(2006, 2009, 2012, 2015, 2018, 2022),
    url = c(
        "https://www.oecd.org/content/dam/oecd/en/data/datasets/pisa/pisa-2006-datasets/data-sets-in-txt-format/INT_Stu06_Dec07.zip",
        "https://www.oecd.org/content/dam/oecd/en/data/datasets/pisa/pisa-2009-datasets/data-sets-in-txt-format/INT_STQ09_DEC11.zip",
        "https://www.oecd.org/content/dam/oecd/en/data/datasets/pisa/pisa-2012-datasets/main-survey/data-sets-in-txt-format/INT_STU12_DEC03.zip",
        "https://webfs.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_QQQ.zip",
        "https://webfs.oecd.org/pisa2018/SPSS_STU_QQQ.zip",
        "https://webfs.oecd.org/pisa2022/STU_QQQ_SPSS.zip"
    ),
    destfile = file.path(pisa_dir, paste0("pisa_stu_", year, ".zip"))
)

walk2(
    .x = pisa_urls$url,
    .y = pisa_urls$destfile,
    .f = download_chr
)
1
