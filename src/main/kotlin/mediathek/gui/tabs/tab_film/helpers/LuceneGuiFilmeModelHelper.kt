/*
 * Copyright (c) 2024-2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.tabs.tab_film.helpers

import mediathek.config.Daten
import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.history.SeenHistoryController
import mediathek.daten.DatenFilm
import mediathek.daten.IndexedFilmList
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.gui.tabs.tab_film.filter.FilmFilterState
import mediathek.gui.tabs.tab_film.filter.ZeitraumSpinner
import mediathek.gui.tabs.tab_film.search.SearchFieldData
import mediathek.gui.tasks.LuceneIndexKeys
import mediathek.mainwindow.MediathekGui
import mediathek.tool.LuceneDefaultAnalyzer
import mediathek.tool.SwingErrorDialog
import org.apache.logging.log4j.LogManager
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.document.DateTools
import org.apache.lucene.index.LeafReaderContext
import org.apache.lucene.index.Term
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.queryparser.flexible.standard.StandardQueryParser
import org.apache.lucene.queryparser.flexible.standard.config.PointsConfig
import org.apache.lucene.search.*
import java.text.DecimalFormat
import java.time.LocalDateTime
import java.time.ZoneId
import java.util.*
import javax.swing.SwingUtilities
import javax.swing.table.TableModel

class LuceneGuiFilmeModelHelper(
    searchFieldData: SearchFieldData,
    filterController: FilmFilterController,
) : GuiModelHelper {
    private val support = GuiModelHelperSupport(searchFieldData, filterController)

    override val filteredTableModel: TableModel
        get() {
            val allFilms = allFilms()
            check(allFilms is IndexedFilmList) { "Lucene filtering requires an IndexedFilmList" }
            return support.getFilteredTableModel(allFilms) { filterContext ->
                filterFilms(allFilms, filterContext)
            }
        }

    private fun allFilms(): Collection<DatenFilm> = Daten.getInstance().listeFilmeNachBlackList

    private fun filterFilms(
        listeFilme: IndexedFilmList,
        filterContext: GuiModelHelperSupport.FilterExecutionContext,
    ): Collection<DatenFilm> {
        try {
            LuceneDefaultAnalyzer.buildPerFieldAnalyzer().use { analyzer ->
                val state = filterContext.state

                if (state.showUnseenOnly) {
                    SeenHistoryController.prepareSharedMemoryCache()
                }

                var stream = listeFilme.parallelStream()

                if (!filterContext.noFiltersAreSet) {
                    val parser = StandardQueryParser(analyzer)
                    parser.pointsConfigMap = PARSER_CONFIG_MAP
                    parser.allowLeadingWildcard = true
                    val initialQuery: Query = if (filterContext.searchFieldText.isEmpty()) {
                        MatchAllDocsQuery.INSTANCE
                    } else {
                        parser.parse(filterContext.searchFieldText, LuceneIndexKeys.TITEL)
                    }

                    val queryBuilder = BooleanQuery.Builder()
                    queryBuilder.add(initialQuery, BooleanClause.Occur.MUST)

                    if (!state.zeitraum.equals(ZeitraumSpinner.INFINITE_TEXT, ignoreCase = true)) {
                        try {
                            queryBuilder.add(createZeitraumQuery(analyzer, state.zeitraum), BooleanClause.Occur.FILTER)
                        } catch (ex: Exception) {
                            logger.error("Unable to add zeitraum filter", ex)
                        }
                    }
                    applyConfiguredQueries(queryBuilder, state)
                    if (filterContext.selectedSenders.isNotEmpty()) {
                        addSenderFilterQuery(queryBuilder, filterContext.selectedSenders)
                    }

                    val finalQuery = queryBuilder.build()
                    logger.info("Executing Lucene query: {}", finalQuery)

                    val searcher = IndexSearcher(listeFilme.reader)
                    val matchingDocIds = searcher.search(finalQuery, NonScoringCollectorManager())
                    val hitLength = matchingDocIds.size
                    val matchingFilms = ArrayList<DatenFilm>(hitLength)

                    logger.trace("Hit size: {}", hitLength)
                    val storedFields = searcher.storedFields()
                    for (docId in matchingDocIds) {
                        val document = storedFields.document(docId, INTEREST_SET)
                        val filmNr = document[LuceneIndexKeys.ID].toInt()
                        val matchingFilm = listeFilme.getFilmByFilmNr(filmNr)
                        if (matchingFilm != null) {
                            matchingFilms.add(matchingFilm)
                        }
                    }

                    logger.trace("Number of found Lucene index entries: {}", matchingFilms.size)
                    stream = matchingFilms.parallelStream()
                }

                if (state.showBookMarkedOnly) {
                    stream = stream.filter(DatenFilm::isBookmarked)
                }
                if (state.dontShowGeoblocked) {
                    val currentGeoLocation = ApplicationConfiguration.getInstance().geographicLocation
                    stream = stream.filter { film -> !film.isGeoBlockedForLocation(currentGeoLocation) }
                }
                if (state.dontShowAbos) {
                    stream = stream.filter { film -> film.abo == null }
                }

                val resultList = support.applyCommonFilters(stream, filterContext).toList()
                logger.trace("Resulting filmlist size after all filters applied: {}", resultList.size)

                return resultList
            }
        } catch (ex: Exception) {
            logger.error("Lucene filtering failed!", ex)
            SwingUtilities.invokeLater {
                SwingErrorDialog.showExceptionMessage(
                    MediathekGui.ui(),
                    "Die Lucene Abfrage ist inkorrekt und führt zu keinen Ergebnissen.",
                    ex,
                )
            }
            return emptyList()
        }
    }

    private fun addSenderFilterQuery(queryBuilder: BooleanQuery.Builder, selectedSenders: Collection<String>) {
        if (selectedSenders.isEmpty()) {
            return
        }

        val booleanQuery = BooleanQuery.Builder()
        for (sender in selectedSenders) {
            val term = Term(LuceneIndexKeys.SENDER, sender.lowercase(Locale.ROOT))
            booleanQuery.add(TermQuery(term), BooleanClause.Occur.SHOULD)
        }

        queryBuilder.add(booleanQuery.build(), BooleanClause.Occur.FILTER)
    }

    private fun applyConfiguredQueries(queryBuilder: BooleanQuery.Builder, state: FilmFilterState) {
        queryBuilder.addTermIf(state.showLivestreamsOnly, LuceneIndexKeys.LIVESTREAM, BooleanClause.Occur.FILTER)
        queryBuilder.addTermIf(state.showHighQualityOnly, LuceneIndexKeys.HIGH_QUALITY, BooleanClause.Occur.FILTER)
        queryBuilder.addTermIf(state.dontShowTrailers, LuceneIndexKeys.TRAILER_TEASER, BooleanClause.Occur.MUST_NOT)
        queryBuilder.addTermIf(state.dontShowAudioVersions, LuceneIndexKeys.AUDIOVERSION, BooleanClause.Occur.MUST_NOT)
        queryBuilder.addTermIf(state.dontShowSignLanguage, LuceneIndexKeys.SIGN_LANGUAGE, BooleanClause.Occur.MUST_NOT)
        queryBuilder.addTermIf(state.dontShowDuplicates, LuceneIndexKeys.DUPLICATE, BooleanClause.Occur.MUST_NOT)
        queryBuilder.addTermIf(state.showSubtitlesOnly, LuceneIndexKeys.SUBTITLE, BooleanClause.Occur.FILTER)
        queryBuilder.addTermIf(state.showNewOnly, LuceneIndexKeys.NEW, BooleanClause.Occur.FILTER)
    }

    private fun BooleanQuery.Builder.addTermIf(
        enabled: Boolean,
        field: String,
        occur: BooleanClause.Occur,
    ) {
        if (enabled) {
            add(termQuery(field), occur)
        }
    }

    private fun termQuery(field: String): Query = TermQuery(Term(field, "true"))

    private fun createZeitraumQuery(analyzer: Analyzer, zeitraumDays: String): Query {
        val numDays = zeitraumDays.toInt()
        val toDate = LocalDateTime.now()
        val fromDate = toDate.minusDays(numDays.toLong())
        val utcZone = ZoneId.of("UTC")
        val toStr = DateTools.timeToString(
            toDate.atZone(utcZone).toInstant().toEpochMilli(),
            DateTools.Resolution.DAY,
        )
        val fromStr = DateTools.timeToString(
            fromDate.atZone(utcZone).toInstant().toEpochMilli(),
            DateTools.Resolution.DAY,
        )
        val zeitraum = "[$fromStr TO $toStr]"
        return QueryParser(LuceneIndexKeys.SENDE_DATUM, analyzer).parse(zeitraum)
    }

    private class NonScoringCollector : SimpleCollector() {
        private val matchingDocIds = ArrayList<Int>()
        private var docBase = 0

        fun getMatchingDocIds(): List<Int> = matchingDocIds

        override fun doSetNextReader(context: LeafReaderContext) {
            docBase = context.docBase
        }

        override fun collect(doc: Int) {
            matchingDocIds.add(docBase + doc)
        }

        override fun scoreMode(): ScoreMode = ScoreMode.COMPLETE_NO_SCORES
    }

    private class NonScoringCollectorManager : CollectorManager<NonScoringCollector, ArrayList<Int>> {
        override fun newCollector(): NonScoringCollector = NonScoringCollector()

        override fun reduce(collectors: Collection<NonScoringCollector>): ArrayList<Int> {
            val totalSize = collectors.sumOf { it.getMatchingDocIds().size }
            val merged = ArrayList<Int>(totalSize)
            for (collector in collectors) {
                merged.addAll(collector.getMatchingDocIds())
            }
            return merged
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()
        private val PARSER_CONFIG_MAP: Map<String, PointsConfig> = mapOf(
            LuceneIndexKeys.FILM_SIZE to PointsConfig(DecimalFormat(), Int::class.javaObjectType),
            LuceneIndexKeys.FILM_LENGTH to PointsConfig(DecimalFormat(), Int::class.javaObjectType),
            LuceneIndexKeys.EPISODE to PointsConfig(DecimalFormat(), Int::class.javaObjectType),
            LuceneIndexKeys.SEASON to PointsConfig(DecimalFormat(), Int::class.javaObjectType),
        )
        private val INTEREST_SET: Set<String> = setOf(LuceneIndexKeys.ID)
    }
}
