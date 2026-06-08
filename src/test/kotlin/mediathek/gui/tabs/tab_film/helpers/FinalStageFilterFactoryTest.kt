package mediathek.gui.tabs.tab_film.helpers

import mediathek.daten.DatenFilm
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPattern
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPatternWithDescription
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilter
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilterWithDescription
import mediathek.tool.Filter
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertInstanceOf
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.function.Predicate
import java.util.stream.Stream

internal class FinalStageFilterFactoryTest {

    @Test
    fun createFinalStageFilter_usesNoPatternFilterWithoutDescriptionForSinglePlainSearchTerm() {
        val filter = createFinalStageFilter(searchThroughDescription = false, arrIrgendwo = arrayOf("alpha"))

        assertInstanceOf(FinalStageFilterNoPattern::class.java, filter)
    }

    @Test
    fun createFinalStageFilter_usesNoPatternFilterWithDescriptionForSinglePlainSearchTerm() {
        val filter = createFinalStageFilter(searchThroughDescription = true, arrIrgendwo = arrayOf("alpha"))

        assertInstanceOf(FinalStageFilterNoPatternWithDescription::class.java, filter)
    }

    @Test
    fun createFinalStageFilter_usesPatternFilterForRegexSearchTerm() {
        val filter = createFinalStageFilter(searchThroughDescription = false, arrIrgendwo = arrayOf("#:.*alpha.*"))

        assertInstanceOf(FinalStagePatternFilter::class.java, filter)
    }

    @Test
    fun createFinalStageFilter_usesPatternFilterWithDescriptionForMultipleSearchTerms() {
        val filter = createFinalStageFilter(searchThroughDescription = true, arrIrgendwo = arrayOf("alpha", "beta"))

        assertInstanceOf(FinalStagePatternFilterWithDescription::class.java, filter)
    }

    @ParameterizedTest
    @MethodSource("legacyParityCases")
    fun `filters match legacy Java behavior`(
        filterFactory: (Array<String>) -> Predicate<DatenFilm>,
        legacyBehavior: (Array<String>, DatenFilm) -> Boolean,
        searchTerms: Array<String>,
        film: DatenFilm,
    ) {
        val filter = filterFactory(searchTerms)

        assertEquals(legacyBehavior(searchTerms, film), filter.test(film))
    }

    @ParameterizedTest
    @MethodSource("factoryLegacyParityCases")
    fun `factory-created filters match legacy Java behavior`(
        searchThroughDescription: Boolean,
        searchTerms: Array<String>,
        film: DatenFilm,
        legacyBehavior: (Array<String>, DatenFilm) -> Boolean,
    ) {
        val filter = createFinalStageFilter(searchThroughDescription, searchTerms)

        assertEquals(legacyBehavior(searchTerms, film), filter.test(film))
    }

    private companion object {
        @JvmStatic
        fun legacyParityCases(): Stream<Arguments> =
            Stream.of(
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStageFilterNoPattern(searchTerms) },
                    ::legacyNoPatternBehavior,
                    arrayOf("alpha"),
                    film(thema = "Alpha topic", title = "Ignored", description = "nothing"),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStageFilterNoPattern(searchTerms) },
                    ::legacyNoPatternBehavior,
                    arrayOf("alpha"),
                    film(thema = "Other", title = "THE ALPHA TITLE", description = "nothing"),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStageFilterNoPattern(searchTerms) },
                    ::legacyNoPatternBehavior,
                    arrayOf("alpha"),
                    film(thema = "Other", title = "Other", description = "alpha only in description"),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStageFilterNoPatternWithDescription(searchTerms) },
                    ::legacyNoPatternWithDescriptionBehavior,
                    arrayOf("alpha"),
                    film(thema = "Other", title = "Other", description = "contains alpha here"),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStageFilterNoPatternWithDescription(searchTerms) },
                    ::legacyNoPatternWithDescriptionBehavior,
                    arrayOf("alpha"),
                    film(thema = "Alpha topic", title = "Other", description = ""),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStagePatternFilter(searchTerms) },
                    ::legacyPatternBehavior,
                    arrayOf("alpha", "beta"),
                    film(thema = "Other", title = "beta title", description = "nothing"),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStagePatternFilter(searchTerms) },
                    ::legacyPatternBehavior,
                    arrayOf("#:.*alpha.*"),
                    film(thema = "Other", title = "contains alpha value", description = "nothing"),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStagePatternFilter(searchTerms) },
                    ::legacyPatternBehavior,
                    arrayOf("#:(?!.*(Trailer|Audiodeskription)).*"),
                    film(
                        thema = "Der Fernsehfilm der Woche",
                        title = "Folge 1",
                        description = "nothing",
                    ),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStagePatternFilter(searchTerms) },
                    ::legacyPatternBehavior,
                    arrayOf("#:(?!.*(Trailer|Audiodeskription)).*"),
                    film(
                        thema = "FilmMittwoch im Ersten",
                        title = "Trailer zur Sendung",
                        description = "nothing",
                    ),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStagePatternFilter(searchTerms) },
                    ::legacyPatternBehavior,
                    arrayOf("#:(?!.*(Vorschau|BRISANT|Audiodeskription)).*"),
                    film(
                        thema = "FilmMittwoch im Ersten",
                        title = "BRISANT Spezial",
                        description = "nothing",
                    ),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStagePatternFilter(searchTerms) },
                    ::legacyPatternBehavior,
                    arrayOf("#:(?!.*(Trailer|Vorschau|Interview|Morgenmagazin|Audiodeskription|klare Sprache)).*"),
                    film(
                        thema = "Tatort",
                        title = "Tatort: Der Fall",
                        description = "nothing",
                    ),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStagePatternFilter(searchTerms) },
                    ::legacyPatternBehavior,
                    arrayOf("#:(?!.*(Trailer|Vorschau|Interview|Morgenmagazin|Audiodeskription|klare Sprache)).*"),
                    film(
                        thema = "Tatort",
                        title = "Tatort Interview mit dem Team",
                        description = "nothing",
                    ),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStagePatternFilterWithDescription(searchTerms) },
                    ::legacyPatternWithDescriptionBehavior,
                    arrayOf("alpha", "beta"),
                    film(thema = "Other", title = "Other", description = "has beta in description"),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStagePatternFilterWithDescription(searchTerms) },
                    ::legacyPatternWithDescriptionBehavior,
                    arrayOf("#:.*alpha.*"),
                    film(thema = "Other", title = "Other", description = "contains alpha value"),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStagePatternFilterWithDescription(searchTerms) },
                    ::legacyPatternWithDescriptionBehavior,
                    arrayOf("#:(?!.*(Gebärdensprache)).*"),
                    film(
                        thema = "Kochen mit Martina und Moritz",
                        title = "Kochen spezial",
                        description = "Gebärdensprache",
                    ),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStageFilterNoPattern(searchTerms) },
                    ::legacyNoPatternBehavior,
                    arrayOf("axel prahl"),
                    film(
                        thema = "Krimiabend",
                        title = "Mit Axel Prahl auf Spurensuche",
                        description = "nothing",
                    ),
                ),
                Arguments.of(
                    { searchTerms: Array<String> -> FinalStageFilterNoPatternWithDescription(searchTerms) },
                    ::legacyNoPatternWithDescriptionBehavior,
                    arrayOf("axel prahl"),
                    film(
                        thema = "Krimiabend",
                        title = "Folge 1",
                        description = "Zu Gast: Axel Prahl",
                    ),
                ),
            )

        @JvmStatic
        fun factoryLegacyParityCases(): Stream<Arguments> =
            Stream.of(
                Arguments.of(
                    false,
                    arrayOf("alpha"),
                    film(thema = "Alpha topic", title = "Other", description = "description"),
                    ::legacyNoPatternBehavior,
                ),
                Arguments.of(
                    true,
                    arrayOf("alpha"),
                    film(thema = "Other", title = "Other", description = "contains alpha"),
                    ::legacyNoPatternWithDescriptionBehavior,
                ),
                Arguments.of(
                    false,
                    arrayOf("alpha", "beta"),
                    film(thema = "Other", title = "beta title", description = "description"),
                    ::legacyPatternBehavior,
                ),
                Arguments.of(
                    true,
                    arrayOf("alpha", "beta"),
                    film(thema = "Other", title = "Other", description = "contains beta"),
                    ::legacyPatternWithDescriptionBehavior,
                ),
                Arguments.of(
                    false,
                    arrayOf("#:.*alpha.*"),
                    film(thema = "Other", title = "contains alpha", description = "description"),
                    ::legacyPatternBehavior,
                ),
                Arguments.of(
                    true,
                    arrayOf("#:.*alpha.*"),
                    film(thema = "Other", title = "Other", description = "contains alpha"),
                    ::legacyPatternWithDescriptionBehavior,
                ),
                Arguments.of(
                    false,
                    arrayOf("#:(?!.*(Trailer)).*"),
                    film(thema = "Der Staatsanwalt", title = "Trailer zur Folge", description = "description"),
                    ::legacyPatternBehavior,
                ),
                Arguments.of(
                    false,
                    arrayOf("#:(?!.*(Trailer|Outtakes|Audiodeskription)).*"),
                    film(thema = "Morden im Norden", title = "Neue Folge", description = "description"),
                    ::legacyPatternBehavior,
                ),
                Arguments.of(
                    true,
                    arrayOf("#:(?!.*(Trailer|Outtakes)).*"),
                    film(thema = "neoFilm", title = "Trailer", description = "voller film"),
                    ::legacyPatternWithDescriptionBehavior,
                ),
                Arguments.of(
                    true,
                    arrayOf("axel prahl"),
                    film(thema = "Andere Sendung", title = "Folge", description = "Axel Prahl ist dabei"),
                    ::legacyNoPatternWithDescriptionBehavior,
                ),
            )

        private fun film(thema: String, title: String, description: String): DatenFilm =
            DatenFilm().apply {
                this.thema = thema
                this.title = title
                this.description = description
            }

        private fun legacyNoPatternBehavior(searchTerms: Array<String>, film: DatenFilm): Boolean {
            val searchText = searchTerms[0]
            return film.thema.lowercase().contains(searchText) || film.title.lowercase().contains(searchText)
        }

        private fun legacyNoPatternWithDescriptionBehavior(searchTerms: Array<String>, film: DatenFilm): Boolean {
            val searchText = searchTerms[0]
            var result = legacyNoPatternBehavior(searchTerms, film)

            val description = film.description
            if (description.isNotEmpty()) {
                result = description.lowercase().contains(searchText) || result
            }

            return result
        }

        private fun legacyPatternBehavior(searchTerms: Array<String>, film: DatenFilm): Boolean =
            Filter.pruefen(searchTerms, film.thema) || Filter.pruefen(searchTerms, film.title)

        private fun legacyPatternWithDescriptionBehavior(searchTerms: Array<String>, film: DatenFilm): Boolean {
            var result = legacyPatternBehavior(searchTerms, film)

            val description = film.description
            if (description.isNotEmpty()) {
                result = Filter.pruefen(searchTerms, description) || result
            }

            return result
        }
    }
}
