package mediathek.daten

import mediathek.tool.FileSize
import mediathek.tool.datum.DatumFilm
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.HexFormat
import java.util.concurrent.TimeUnit
import java.util.stream.Stream

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
        film.setNormalQualityUrl("https://example.org/video.mp4")
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
        film.setNormalQualityUrl("https://example.org/äöü-\uD83D\uDE80.mp4")
        film.websiteUrl = "https://example.org/seite"

        assertEquals(
            legacySha256("ARTE", "München", "https://example.org/äöü-\uD83D\uDE80.mp4", "https://example.org/seite"),
            film.sha256,
        )
    }

    @Test
    fun setDatumLongAcceptsNegativeValues() {
        val film = DatenFilm()

        film.sendeDatum = "01.01.1966"
        film.setDatumLongSeconds(-122749200L)
        film.init()

        assertNotEquals(DatumFilm.UNDEFINED_FILM_DATE, film.datumFilm)
        assertEquals(TimeUnit.MILLISECONDS.convert(-122749200L, TimeUnit.SECONDS), film.datumFilm.time)
    }

    @Test
    fun changingNormalQualityUrlDoesNotBootstrapFileSizeFromPreviousUrl() {
        val oldUrl = "https://example.org/old.mp4"
        val newUrl = "https://example.org/new.mp4"
        val film = DatenFilm()
        film.fileSize.setSize("123")
        film.setNormalQualityUrl(oldUrl)

        assertEquals("123", film.cachedLookup(oldUrl)?.sizeText)

        val clone = DatenFilm(film)
        clone.setNormalQualityUrl(newUrl)

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

        film.fileSize.setSize("123")
        film.setNormalQualityUrl(url)
        assertEquals("123", film.cachedLookup(url)?.sizeText)
        film.markGeoBlockedForLocation(Country.DE)

        assertNotNull(film.privateField("cachedFileSizeLookups"))
        assertNotNull(film.privateField("knownBlockedCountries"))
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

        private fun DatenFilm.cachedLookup(url: String): FileSize.LookupResult? {
            val method = DatenFilm::class.java.getDeclaredMethod("getCachedFileSizeLookup", String::class.java)
            method.isAccessible = true
            @Suppress("UNCHECKED_CAST")
            return method.invoke(this, url) as FileSize.LookupResult?
        }

        private fun DatenFilm.privateField(name: String): Any? {
            val field = DatenFilm::class.java.getDeclaredField(name)
            field.isAccessible = true
            return field.get(this)
        }
    }
}
