package mediathek.gui.tabs.tab_online_search

import mediathek.daten.DatenFilm
import java.time.ZoneId

object OnlineSearchFilmAdapter {
    fun toDatenFilm(source: OnlineSearchResult): DatenFilm = DatenFilm().apply {
        sender = source.sender
        thema = source.topic
        title = source.title
        description = source.description
        websiteUrl = source.websiteUrl
        urlNormalQuality = source.normalQualityUrl
        lowQualityUrl = source.lowQualityUrl
        highQualityUrl = source.highQualityUrl
        subtitleUrl = source.subtitleUrl
        source.duration?.let { setFilmLengthSeconds(it.seconds.toInt()) }
        source.broadcastTime?.let { broadcast ->
            setSendeDateTime(broadcast)
            setDatumLongSeconds(broadcast.atZone(ZoneId.systemDefault()).toEpochSecond())
        }
        isSignLanguage = source.isSignLanguage
        isAudioVersion = source.isAudioDescription
        init()
    }
}
