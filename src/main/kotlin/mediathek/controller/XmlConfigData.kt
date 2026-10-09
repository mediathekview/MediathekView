package mediathek.controller

import mediathek.controller.starter.DownloadServices
import mediathek.daten.ListeAbo
import mediathek.daten.ListePset
import mediathek.daten.blacklist.ListeBlacklist

data class XmlConfigData(
    val programSets: ListePset,
    val downloads: DownloadServices,
    val blacklistRules: ListeBlacklist,
    val abos: ListeAbo,
)
