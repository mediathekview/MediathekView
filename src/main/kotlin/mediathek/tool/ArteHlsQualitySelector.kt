package mediathek.tool

import mediathek.daten.FilmResolution
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull

object ArteHlsQualitySelector {
    fun programId(url: String, resolution: FilmResolution.Enum?): Int? {
        val httpUrl = url.toHttpUrlOrNull() ?: return null
        if (!httpUrl.host.equals("manifest-arte.akamaized.net", ignoreCase = true) ||
            !httpUrl.encodedPath.endsWith(".m3u8") ||
            !httpUrl.encodedPath.contains("/api/manifest/", ignoreCase = true)
        ) {
            return null
        }
        return when (resolution) {
            FilmResolution.Enum.HIGH_QUALITY -> 1
            FilmResolution.Enum.NORMAL -> 2
            FilmResolution.Enum.LOW -> 3
            null -> null
        }
    }
}
