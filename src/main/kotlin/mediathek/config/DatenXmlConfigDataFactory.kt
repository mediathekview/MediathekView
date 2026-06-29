package mediathek.config

import mediathek.controller.XmlConfigData

object DatenXmlConfigDataFactory {
    @JvmStatic
    fun from(daten: Daten): XmlConfigData =
        XmlConfigData(
            programSets = daten.programSets.list,
            downloads = daten.downloads,
            blacklistRules = daten.blacklist.rules,
            abos = daten.abos.list,
        )
}
