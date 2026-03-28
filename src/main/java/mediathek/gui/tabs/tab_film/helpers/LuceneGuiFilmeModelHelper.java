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

package mediathek.gui.tabs.tab_film.helpers;

import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.IndexedFilmList;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.gui.tabs.tab_film.filter.zeitraum.ZeitraumSpinnerFormatter;
import mediathek.gui.tasks.LuceneIndexKeys;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.FilterConfiguration;
import mediathek.tool.LuceneDefaultAnalyzer;
import mediathek.tool.SwingErrorDialog;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.DateTools;
import org.apache.lucene.index.LeafReaderContext;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.queryparser.flexible.standard.StandardQueryParser;
import org.apache.lucene.queryparser.flexible.standard.config.PointsConfig;
import org.apache.lucene.search.*;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.table.TableModel;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.function.BooleanSupplier;
import java.util.stream.Stream;

public final class LuceneGuiFilmeModelHelper implements GuiModelHelper {
    private static final Logger logger = LogManager.getLogger();
    private static final Map<String, PointsConfig> PARSER_CONFIG_MAP = Map.of(
            LuceneIndexKeys.FILM_SIZE, new PointsConfig(new DecimalFormat(), Integer.class),
            LuceneIndexKeys.FILM_LENGTH, new PointsConfig(new DecimalFormat(), Integer.class),
            LuceneIndexKeys.EPISODE, new PointsConfig(new DecimalFormat(), Integer.class),
            LuceneIndexKeys.SEASON, new PointsConfig(new DecimalFormat(), Integer.class));
    private static final Set<String> INTEREST_SET = Set.of(LuceneIndexKeys.ID);

    private final GuiModelHelperSupport support;

    public LuceneGuiFilmeModelHelper(@NotNull SeenHistoryController historyController,
                                     @NotNull SearchFieldData searchFieldData,
                                     @NotNull FilterConfiguration filterConfiguration) {
        support = new GuiModelHelperSupport(historyController, searchFieldData, filterConfiguration);
    }

    @Override
    public TableModel getFilteredTableModel() {
        var allFilms = getAllFilms();
        return support.getFilteredTableModel(allFilms, this::filterFilms);
    }

    private Collection<DatenFilm> getAllFilms() {
        return Daten.getInstance().getListeFilmeNachBlackList();
    }

    private Collection<DatenFilm> filterFilms() {
        var listeFilme = (IndexedFilmList) getAllFilms();
        try (Analyzer analyzer = LuceneDefaultAnalyzer.buildPerFieldAnalyzer()) {
            var filterContext = support.createFilterExecutionContext();

            if (support.filterConfiguration().isShowUnseenOnly()) {
                support.prepareHistoryMemoryCache();
            }

            Stream<DatenFilm> stream = listeFilme.parallelStream();

            if (!support.noFiltersAreSet()) {
                var parser = new StandardQueryParser(analyzer);
                parser.setPointsConfigMap(PARSER_CONFIG_MAP);
                parser.setAllowLeadingWildcard(true);
                Query initialQuery;
                if (filterContext.searchFieldText().isEmpty())
                    initialQuery = MatchAllDocsQuery.INSTANCE;
                else
                    initialQuery = parser.parse(filterContext.searchFieldText(), LuceneIndexKeys.TITEL);

                BooleanQuery.Builder qb = new BooleanQuery.Builder();
                qb.add(initialQuery, BooleanClause.Occur.MUST);

                //Zeitraum filter on demand …
                if (!support.filterConfiguration().getZeitraum().equalsIgnoreCase(ZeitraumSpinnerFormatter.INFINITE_TEXT)) {
                    try {
                        qb.add(createZeitraumQuery(analyzer), BooleanClause.Occur.FILTER);
                    } catch (Exception ex) {
                        logger.error("Unable to add zeitraum filter", ex);
                    }
                }
                applyConfiguredQueries(qb);
                if (!filterContext.selectedSenders().isEmpty()) {
                    addSenderFilterQuery(qb, filterContext.selectedSenders());
                }

                //the complete lucene query...
                Query finalQuery = qb.build();
                logger.info("Executing Lucene query: {}", finalQuery);

                //SEARCH
                final var searcher = new IndexSearcher(listeFilme.getReader());
                final var matchingDocIds = searcher.search(finalQuery, new NonScoringCollectorManager());
                final var hitLength = matchingDocIds.size();
                var matchingFilms = new ArrayList<DatenFilm>(hitLength);

                logger.trace("Hit size: {}", hitLength);
                var storedFields = searcher.storedFields();
                for (final var docId : matchingDocIds) {
                    var d = storedFields.document(docId, INTEREST_SET);
                    var filmNr = Integer.parseInt(d.get(LuceneIndexKeys.ID));
                    var matchingFilm = listeFilme.getFilmByFilmNr(filmNr);
                    if (matchingFilm != null) {
                        matchingFilms.add(matchingFilm);
                    }
                }

                //process
                logger.trace("Number of found Lucene index entries: {}", matchingFilms.size());

                stream = matchingFilms.parallelStream();
            }

            if (support.filterConfiguration().isShowBookMarkedOnly()) {
                stream = stream.filter(DatenFilm::isBookmarked);
            }
            if (support.filterConfiguration().isDontShowGeoblocked()) {
                var currentGeoLocation = ApplicationConfiguration.getInstance().getGeographicLocation();
                stream = stream.filter(film -> !film.isGeoBlockedForLocation(currentGeoLocation));
            }
            if (support.filterConfiguration().isDontShowAbos()) {
                stream = stream.filter(film -> film.getAbo() == null);
            }

            var resultList = support.applyCommonFilters(stream, filterContext.filterThema(), filterContext.lengthFilterRange()).toList();
            logger.trace("Resulting filmlist size after all filters applied: {}", resultList.size());

            return resultList;
        } catch (Exception ex) {
            logger.error("Lucene filtering failed!", ex);
            SwingUtilities.invokeLater(() -> SwingErrorDialog.showExceptionMessage(MediathekGui.ui(),
                    "Die Lucene Abfrage ist inkorrekt und führt zu keinen Ergebnissen.", ex));
            return List.of();
        }
    }

    private void addSenderFilterQuery(@NotNull BooleanQuery.Builder qb, @NotNull Collection<String> selectedSenders) {
        if (selectedSenders.isEmpty()) {
            return; // Kein Filter hinzufügen, wenn keine Sender ausgewählt sind
        }

        BooleanQuery.Builder booleanQuery = new BooleanQuery.Builder();
        for (var sender : selectedSenders) {
            // sender must be lowercase as StandardAnalyzer converts it to lower during indexing
            TermQuery term = new TermQuery(new Term(LuceneIndexKeys.SENDER, sender.toLowerCase(Locale.ROOT)));
            booleanQuery.add(term, BooleanClause.Occur.SHOULD);
        }

        qb.add(booleanQuery.build(), BooleanClause.Occur.FILTER);
    }

    private void applyConfiguredQueries(@NotNull BooleanQuery.Builder queryBuilder) {
        for (var querySpec : createQuerySpecs()) {
            if (querySpec.enabled().getAsBoolean()) {
                queryBuilder.add(querySpec.toQuery(), querySpec.occur());
            }
        }
    }

    private List<QuerySpec> createQuerySpecs() {
        return List.of(
                termQuerySpec(support.filterConfiguration()::isShowLivestreamsOnly, LuceneIndexKeys.LIVESTREAM, BooleanClause.Occur.FILTER),
                termQuerySpec(support.filterConfiguration()::isShowHighQualityOnly, LuceneIndexKeys.HIGH_QUALITY, BooleanClause.Occur.FILTER),
                termQuerySpec(support.filterConfiguration()::isDontShowTrailers, LuceneIndexKeys.TRAILER_TEASER, BooleanClause.Occur.MUST_NOT),
                termQuerySpec(support.filterConfiguration()::isDontShowAudioVersions, LuceneIndexKeys.AUDIOVERSION, BooleanClause.Occur.MUST_NOT),
                termQuerySpec(support.filterConfiguration()::isDontShowSignLanguage, LuceneIndexKeys.SIGN_LANGUAGE, BooleanClause.Occur.MUST_NOT),
                termQuerySpec(support.filterConfiguration()::isDontShowDuplicates, LuceneIndexKeys.DUPLICATE, BooleanClause.Occur.MUST_NOT),
                termQuerySpec(support.filterConfiguration()::isShowSubtitlesOnly, LuceneIndexKeys.SUBTITLE, BooleanClause.Occur.FILTER),
                termQuerySpec(support.filterConfiguration()::isShowNewOnly, LuceneIndexKeys.NEW, BooleanClause.Occur.FILTER));
    }

    private QuerySpec termQuerySpec(@NotNull BooleanSupplier enabled, @NotNull String field, @NotNull BooleanClause.Occur occur) {
        return new QuerySpec(enabled, field, "true", occur);
    }

    private Query createZeitraumQuery(@NotNull Analyzer analyzer) throws ParseException {

        var numDays = Integer.parseInt(support.filterConfiguration().getZeitraum());
        var toDate = LocalDateTime.now();
        var fromDate = toDate.minusDays(numDays);
        var utcZone = ZoneId.of("UTC");
        //[20190101 TO 20190801]
        var toStr = DateTools.timeToString(toDate.atZone(utcZone).toInstant().toEpochMilli(),
                DateTools.Resolution.DAY);
        var fromStr = DateTools.timeToString(fromDate.atZone(utcZone).toInstant().toEpochMilli(),
                DateTools.Resolution.DAY);
        String zeitraum = String.format("[%s TO %s]", fromStr, toStr);
        return new QueryParser(LuceneIndexKeys.SENDE_DATUM, analyzer).parse(zeitraum);
    }

    private static class NonScoringCollector extends SimpleCollector {
        private final ArrayList<Integer> matchingDocIds = new ArrayList<>();
        private int docBase;

        public List<Integer> getMatchingDocIds() {
            return matchingDocIds;
        }

        @Override
        protected void doSetNextReader(LeafReaderContext context) {
            docBase = context.docBase;
        }

        @Override
        public void collect(int doc) {
            matchingDocIds.add(docBase + doc);
        }

        @Override
        public ScoreMode scoreMode() {
            return ScoreMode.COMPLETE_NO_SCORES;
        }
    }

    private static class NonScoringCollectorManager implements CollectorManager<NonScoringCollector, ArrayList<Integer>> {
        @Override
        public NonScoringCollector newCollector() {
            return new NonScoringCollector();
        }

        @Override
        public ArrayList<Integer> reduce(Collection<NonScoringCollector> collectors) {
            int totalSize = collectors.stream().mapToInt(c -> c.getMatchingDocIds().size()).sum();
            ArrayList<Integer> merged = new ArrayList<>(totalSize);
            for (var collector : collectors) {
                merged.addAll(collector.getMatchingDocIds());
            }
            return merged;
        }
    }

    private record QuerySpec(BooleanSupplier enabled, String field, String value, BooleanClause.Occur occur) {
        private Query toQuery() {
            return new TermQuery(new Term(field, value));
        }
    }
}
