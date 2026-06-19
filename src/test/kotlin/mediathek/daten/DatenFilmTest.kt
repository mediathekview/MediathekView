package mediathek.daten

import mediathek.daten.abo.DatenAbo
import mediathek.gui.bookmark.BookmarkData
import mediathek.tool.FileSize
import mediathek.tool.datum.DatumFilm
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.*
import java.util.stream.Stream
import kotlin.time.Duration.Companion.seconds

internal class DatenFilmTest {

    @ParameterizedTest
    @MethodSource("filmLengthEdgeCases")
    fun testFilmLengthCalculation(input: String?, expected: Long) {
        val film = DatenFilm()

        film.setFilmLengthSeconds(expected.toInt())
        film.init()

        assertEquals(expected.toInt(), film.filmLength)
    }

    @Test
    fun getSha256CachesAndInvalidatesOnIdentityChanges() {
        val film = DatenFilm()
        film.sender = "sender"
        film.thema = "thema"
        film.urlNormalQuality = "https://example.org/video.mp4"
        film.websiteUrl = "https://example.org/page"

        val initialHash = film.sha256
        val cachedHash = film.sha256

        assertSame(initialHash, cachedHash)

        film.thema = "updated thema"

        val updatedHash = film.sha256

        assertNotEquals(initialHash, updatedHash)
        assertSame(updatedHash, film.sha256)
    }

    @Test
    fun getSha256KeepsLegacyUtf16LeHashFormat() {
        val film = DatenFilm()
        film.sender = "ARTE"
        film.thema = "München"
        film.urlNormalQuality = "https://example.org/äöü-\uD83D\uDE80.mp4"
        film.websiteUrl = "https://example.org/seite"

        assertEquals(
            legacySha256("ARTE", "München", "https://example.org/äöü-\uD83D\uDE80.mp4", "https://example.org/seite"),
            film.sha256,
        )
    }

    @Test
    fun setDatumLongAcceptsNegativeValues() {
        val film = DatenFilm()

        film.setSendeDatumFromFilmlistValue("01.01.1966")
        film.setDatumLongSeconds(-122749200L)
        film.init()

        assertEquals((-122749200L).seconds.inWholeMilliseconds, film.datumFilmTimeMillis)
        assertNotEquals(DatumFilm.UNDEFINED_FILM_DATE, film.datumFilm)
        assertEquals((-122749200L).seconds.inWholeMilliseconds, film.datumFilm.time)
    }

    @Test
    fun changingNormalQualityUrlDoesNotBootstrapFileSizeFromPreviousUrl() {
        val oldUrl = "https://example.org/old.mp4"
        val newUrl = "https://example.org/new.mp4"
        val film = DatenFilm()
        film.setFileSize("123")
        film.urlNormalQuality = oldUrl

        assertEquals("123", film.cachedLookup(oldUrl)?.sizeText)

        val clone = DatenFilm(film)
        clone.urlNormalQuality = newUrl

        assertNull(clone.cachedLookup(newUrl))
    }

    @Test
    fun newFilmKeepsRareStateContainersUnallocated() {
        val film = DatenFilm()

        assertNull(film.privateField("cachedFileSizeLookups"))
        assertNull(film.privateField("knownBlockedCountries"))

        assertNull(film.cachedLookup("https://example.org/video.mp4"))
        assertFalse(film.isGeoBlockedForLocation(Country.DE))

        assertNull(film.privateField("cachedFileSizeLookups"))
        assertNull(film.privateField("knownBlockedCountries"))
    }

    @Test
    fun rareStateContainersAreAllocatedOnFirstStoredValue() {
        val film = DatenFilm()
        val url = "https://example.org/video.mp4"

        film.setFileSize("123")
        film.urlNormalQuality = url
        assertEquals("123", film.cachedLookup(url)?.sizeText)
        film.markGeoBlockedForLocation(Country.DE)

        assertNotNull(film.privateField("cachedFileSizeLookups"))
        assertNotNull(film.privateField("knownBlockedCountries"))
    }

    @Test
    fun fileSizeLookupCacheSeparatesSameUrlByResolution() {
        val film = DatenFilm()
        val url = "https://example.org/video/master.m3u8"

        film.applyFileSizeLookupResult(
            url,
            FileSize.LookupResult(30L * FileSize.ONE_MIB, quality = FilmResolution.Enum.HIGH_QUALITY.name),
            FilmResolution.Enum.HIGH_QUALITY.name,
        )
        film.applyFileSizeLookupResult(
            url,
            FileSize.LookupResult(10L * FileSize.ONE_MIB, quality = FilmResolution.Enum.LOW.name),
            FilmResolution.Enum.LOW.name,
        )

        assertEquals("30", film.cachedLookup(url, FilmResolution.Enum.HIGH_QUALITY.name)?.sizeText)
        assertEquals("10", film.cachedLookup(url, FilmResolution.Enum.LOW.name)?.sizeText)
        assertNull(film.cachedLookup(url, FilmResolution.Enum.NORMAL.name))
    }

    @Test
    fun normalQualityFileSizeBootstrapDoesNotApplyToOtherResolutionsForSameUrl() {
        val film = DatenFilm()
        val url = "https://example.org/video/master.m3u8"
        film.setFileSize("123")
        film.urlNormalQuality = url

        assertEquals("123", film.cachedLookup(url, FilmResolution.Enum.NORMAL.name)?.sizeText)
        assertNull(film.cachedLookup(url, FilmResolution.Enum.HIGH_QUALITY.name))
    }

    @Test
    fun publicFlagAccessorsToggleAndCopyBitState() {
        val film = DatenFilm()

        film.isAudioVersion = true
        film.isTrailerTeaser = true
        film.isSignLanguage = true
        film.isLivestream = true
        film.isNew = true
        film.setBurnedInSubtitles(true)
        film.isPlayList = true
        film.isDuplicate = true

        val copy = DatenFilm(film)

        assertTrue(copy.isAudioVersion)
        assertTrue(copy.isTrailerTeaser)
        assertTrue(copy.isSignLanguage)
        assertTrue(copy.isLivestream)
        assertTrue(copy.isNew)
        assertTrue(copy.hasBurnedInSubtitles())
        assertTrue(copy.isPlayList)
        assertTrue(copy.isDuplicate)

        copy.isAudioVersion = false
        copy.isTrailerTeaser = false
        copy.isSignLanguage = false
        copy.isLivestream = false
        copy.isNew = false
        copy.setBurnedInSubtitles(false)
        copy.isPlayList = false
        copy.isDuplicate = false

        assertFalse(copy.isAudioVersion)
        assertFalse(copy.isTrailerTeaser)
        assertFalse(copy.isSignLanguage)
        assertFalse(copy.isLivestream)
        assertFalse(copy.isNew)
        assertFalse(copy.hasBurnedInSubtitles())
        assertFalse(copy.isPlayList)
        assertFalse(copy.isDuplicate)
    }

    @Test
    fun copyPreservesTypedOptionalState() {
        val abo = DatenAbo()
        val bookmark = BookmarkData()
        val film = DatenFilm().apply {
            urlNormalQuality = "https://example.org/normal.mp4"
            lowQualityUrl = "https://example.org/low.mp4"
            highQualityUrl = "https://example.org/high.mp4"
            subtitleUrl = "https://example.org/subtitle.vtt"
            websiteUrl = "https://example.org/page"
            this.abo = abo
            this.bookmark = bookmark
            setSendeDatumFromFilmlistValue("01.01.1966")
            setDatumLongSeconds(-122749200L)
        }

        val copy = DatenFilm(film)
        copy.init()

        assertEquals(film.filmNr, copy.filmNr)
        assertEquals("https://example.org/normal.mp4", copy.urlNormalQuality)
        assertEquals("https://example.org/low.mp4", copy.lowQualityUrl)
        assertEquals("https://example.org/high.mp4", copy.highQualityUrl)
        assertEquals("https://example.org/subtitle.vtt", copy.subtitleUrl)
        assertEquals("https://example.org/page", copy.websiteUrl)
        assertSame(abo, copy.abo)
        assertSame(bookmark, copy.bookmark)
        assertTrue(copy.hasSubtitle())
        assertTrue(copy.hasLowQuality())
        assertTrue(copy.isHighQuality)
        assertTrue(copy.isBookmarked)
        assertNotEquals(DatumFilm.UNDEFINED_FILM_DATE, copy.datumFilm)
        assertEquals((-122749200L).seconds.inWholeMilliseconds, copy.datumFilm.time)
    }

    @Test
    fun sendeDatumAndSendeZeitUseCompactStorageForStandardValues() {
        val film = DatenFilm()

        film.setSendeDatumFromFilmlistValue("15.05.2026")
        film.setSendeZeitFromFilmlistValue("20:15:30")

        assertEquals("15.05.2026", film.sendeDatum)
        assertEquals("20:15:30", film.sendeZeit)
        assertEquals(20 * 3600 + 15 * 60 + 30, film.sendeZeitSecondOfDay)
    }

    @Test
    fun sendeDatumAndSendeZeitClearNonStandardValues() {
        val film = DatenFilm()

        film.setSendeDatumFromFilmlistValue("unknown")
        film.setSendeZeitFromFilmlistValue("unknown")

        assertEquals("", film.sendeDatum)
        assertEquals("", film.sendeZeit)
        assertNull(film.sendeDatumEpochDay)
        assertNull(film.sendeZeitSecondOfDay)
    }

    @Test
    fun sendeZeitAcceptsFilmListFormatWithoutSeconds() {
        val film = DatenFilm()

        film.setSendeZeitFromFilmlistValue("20:15")

        assertEquals("20:15:00", film.sendeZeit)
        assertEquals("20:15", film.sendeZeitForFilmList)
        assertEquals(20 * 3600 + 15 * 60, film.sendeZeitSecondOfDay)
    }

    @Test
    fun copyPreservesCompactSendeDatumAndSendeZeitStorage() {
        val film = DatenFilm().apply {
            setSendeDatumFromFilmlistValue("15.05.2026")
            setSendeZeitFromFilmlistValue("20:15:30")
        }

        val copy = DatenFilm(film)

        assertEquals(film.sendeDatum, copy.sendeDatum)
        assertEquals(film.sendeZeit, copy.sendeZeit)
        assertEquals(film.privateField("sendeDateTimeStorage"), copy.privateField("sendeDateTimeStorage"))
    }

    @Test
    fun optionalUrlPresenceUsesNullableState() {
        val film = DatenFilm()

        film.lowQualityUrl = ""
        film.highQualityUrl = ""
        film.subtitleUrl = ""
        film.websiteUrl = ""

        assertEquals("", film.lowQualityUrl)
        assertEquals("", film.highQualityUrl)
        assertEquals("", film.subtitleUrl)
        assertEquals("", film.websiteUrl)
        assertFalse(film.hasLowQuality())
        assertFalse(film.isHighQuality)
        assertFalse(film.hasSubtitle())
        assertNull(film.privateField("lowQualityUrlStorage"))
        assertNull(film.privateField("highQualityUrlStorage"))
        assertNull(film.privateField("subtitleUrlStorage"))
        assertNull(film.privateField("websiteUrlStorage"))

        film.lowQualityUrl = "https://example.org/low.mp4"
        film.highQualityUrl = "https://example.org/high.mp4"
        film.subtitleUrl = "https://example.org/subtitle.vtt"
        film.websiteUrl = "https://example.org/page"

        assertEquals("https://example.org/subtitle.vtt", film.subtitleUrl)
        assertEquals("https://example.org/page", film.websiteUrl)
        assertTrue(film.hasLowQuality())
        assertTrue(film.isHighQuality)
        assertTrue(film.hasSubtitle())
        assertEquals("https://example.org/low.mp4", film.privateField("lowQualityUrlStorage"))
        assertEquals("https://example.org/high.mp4", film.privateField("highQualityUrlStorage"))
        assertCompressedUrlStorage(film, "subtitleUrlStorage", "https://example.org/subtitle.vtt")
        assertCompressedUrlStorage(film, "websiteUrlStorage", "https://example.org/page")
    }

    @Test
    fun normalQualityUrlStorageUsesHostDictionary() {
        val film = DatenFilm()
        val url = "https://cdn.example.org/path/to/video.mp4"

        film.urlNormalQuality = url

        assertEquals(url, film.urlNormalQuality)
        val storedUrl = film.privateField("normalQualityUrlStorage") as String
        assertTrue(storedUrl.startsWith("~"))
        assertTrue(storedUrl.length < url.length)
    }

    @Test
    fun copyPreservesNormalQualityUrlWithHostDictionaryStorage() {
        val url = "https://cdn.example.org/path/to/video.mp4"
        val film = DatenFilm().apply {
            urlNormalQuality = url
        }

        val copy = DatenFilm(film)

        assertEquals(url, copy.urlNormalQuality)
    }

    @Test
    fun copyPreservesWebsiteAndSubtitleUrlsWithHostDictionaryStorage() {
        val film = DatenFilm().apply {
            websiteUrl = "https://www.example.org/path/to/page.html"
            subtitleUrl = "https://subtitle.example.org/path/to/subtitle.vtt"
        }

        val copy = DatenFilm(film)

        assertEquals(film.websiteUrl, copy.websiteUrl)
        assertEquals(film.subtitleUrl, copy.subtitleUrl)
    }

    @Test
    fun highQualityUrlStorageKeepsCompressedValue() {
        val film = DatenFilm()
        val sharedPrefix = "https://example.org/video/"
        val compressedHighQualityUrl = "${sharedPrefix.length}|high.mp4"

        film.urlNormalQuality = sharedPrefix + "normal.mp4"
        film.highQualityUrl = sharedPrefix + "high.mp4"

        assertEquals(sharedPrefix + "high.mp4", film.highQualityUrl)
        assertEquals(compressedHighQualityUrl, film.privateField("highQualityUrlStorage"))
        assertEquals(
            sharedPrefix + "high.mp4",
            film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY),
        )
    }

    @Test
    fun highQualityUrlStorageRebasesWhenNormalQualityUrlChanges() {
        val film = DatenFilm()
        val originalPrefix = "https://example.org/original/"
        val highQualityUrl = originalPrefix + "high.mp4"

        film.urlNormalQuality = originalPrefix + "normal.mp4"
        film.highQualityUrl = highQualityUrl
        film.urlNormalQuality = "https://example.org/updated/normal.mp4"

        assertEquals(highQualityUrl, film.highQualityUrl)
    }

    @Test
    fun datumFilmIsCreatedLazilyFromStoredTime() {
        val film = DatenFilm().apply {
            setSendeDatumFromFilmlistValue("01.01.1966")
            setDatumLongSeconds(-122749200L)
            init()
        }
        val expectedTime = (-122749200L).seconds.inWholeMilliseconds

        assertEquals(expectedTime, film.datumFilmTimeMillis)
        assertNull(film.privateField("datumFilmCache"))

        val filmDate = film.datumFilm

        assertEquals(expectedTime, filmDate.time)
        assertSame(filmDate, film.datumFilm)

        val copy = DatenFilm(film)

        assertEquals(expectedTime, copy.datumFilmTimeMillis)
        assertNull(copy.privateField("datumFilmCache"))
        assertEquals(expectedTime, copy.datumFilm.time)
    }

    @Test
    fun decompressUrlCombinesNormalQualityPrefixWithCompressedSuffix() {
        val film = DatenFilm().apply {
            urlNormalQuality = "https://example.org/video-normal.mp4"
        }

        assertEquals("https://example.org/video-high.mp4", film.decompressUrl("26|high.mp4"))
    }

    @Test
    fun countriesAsStringCachesAndInvalidatesWhenCountriesChange() {
        val film = DatenFilm()

        assertEquals("", film.countriesAsString)
        assertNull(film.privateField("countriesAsStringCache"))

        film.addCountry(Country.DE)

        assertEquals("DE", film.countriesAsString)
        val cachedSingleCountry = film.privateField("countriesAsStringCache")
        assertEquals("DE", cachedSingleCountry)
        assertSame(cachedSingleCountry, film.countriesAsString)

        film.addCountry(Country.DE)

        assertSame(cachedSingleCountry, film.privateField("countriesAsStringCache"))

        film.addCountry(Country.AT)

        assertNull(film.privateField("countriesAsStringCache"))
        assertEquals("DE-AT", film.countriesAsString)

        val copy = DatenFilm(film)

        assertEquals("DE-AT", copy.countriesAsString)
        assertEquals("DE-AT", copy.privateField("countriesAsStringCache"))

        film.clearCountries()

        assertEquals("", film.countriesAsString)
        assertNull(film.privateField("countriesAsStringCache"))
    }

    private companion object {
        @JvmStatic
        fun filmLengthEdgeCases(): Stream<Arguments> =
            Stream.of(
                Arguments.of("01:21:30", 4890L),
                Arguments.of(null, 0L),
                Arguments.of("01:91:65", 9125L),
                Arguments.of("01:31", 0L),
                Arguments.of("1:0:0", 3600L),
                Arguments.of("100:100:100", 366100L),
            )

        private fun legacySha256(vararg parts: String): String {
            val digest = MessageDigest.getInstance("SHA-256")
            for (part in parts) {
                digest.update(part.toByteArray(StandardCharsets.UTF_16LE))
            }
            return HexFormat.of().formatHex(digest.digest())
        }

        private fun DatenFilm.cachedLookup(url: String, resolution: String? = null): FileSize.LookupResult? {
            val method = DatenFilm::class.java.getDeclaredMethod(
                "getCachedFileSizeLookup",
                String::class.java,
                String::class.java,
            )
            method.isAccessible = true
            @Suppress("UNCHECKED_CAST")
            return method.invoke(this, url, resolution) as FileSize.LookupResult?
        }

        private fun DatenFilm.privateField(name: String): Any? {
            val field = DatenFilm::class.java.getDeclaredField(name)
            field.isAccessible = true
            return field.get(this)
        }

        private fun assertCompressedUrlStorage(film: DatenFilm, fieldName: String, expandedUrl: String) {
            val storedUrl = film.privateField(fieldName) as String
            assertTrue(storedUrl.startsWith("~"))
            assertTrue(storedUrl.length < expandedUrl.length)
        }
    }
}
