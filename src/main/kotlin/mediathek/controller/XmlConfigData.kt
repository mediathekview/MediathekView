package mediathek.controller

import mediathek.controller.starter.DownloadServices
import mediathek.daten.ListeAbo
import mediathek.daten.ListePset
import mediathek.daten.blacklist.ListeBlacklist
import mediathek.tool.ReplacementRules

data class XmlConfigData(
    val programSets: ListePset,
    val downloads: DownloadServices,
    val blacklistRules: ListeBlacklist,
    val abos: ListeAbo,
    val replacementRules: ReplacementRules,
)
