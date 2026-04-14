package mediathek.daten

import mediathek.tool.datum.DatumFilm
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
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
    fun setDatumLongAcceptsNegativeValues() {
        val film = DatenFilm()

        film.sendeDatum = "01.01.1966"
        film.setDatumLongSeconds(-122749200L)
        film.init()

        assertNotEquals(DatumFilm.UNDEFINED_FILM_DATE, film.datumFilm)
        assertEquals(TimeUnit.MILLISECONDS.convert(-122749200L, TimeUnit.SECONDS), film.datumFilm.time)
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
    }
}
