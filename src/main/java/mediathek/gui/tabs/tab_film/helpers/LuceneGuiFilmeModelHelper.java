/*
 * Copyright (c) 2024-2025 derreisende77.
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

import com.google.common.base.Stopwatch;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.IndexedFilmList;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.gui.tabs.tab_film.filter.zeitraum.ZeitraumSpinnerFormatter;
import mediathek.gui.tasks.LuceneIndexKeys;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FilterConfiguration;
import mediathek.tool.LuceneDefaultAnalyzer;
import mediathek.tool.SwingErrorDialog;
import mediathek.tool.models.TModelFilm;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.DateTools;
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
import java.util.stream.Stream;

public class LuceneGuiFilmeModelHelper extends GuiModelHelper {
    private static final Logger logger = LogManager.getLogger();
    private static final Map<String, PointsConfig> PARSER_CONFIG_MAP = new HashMap<>();
    private static final HashSet<String> INTEREST_SET = new HashSet<>(List.of(LuceneIndexKeys.ID));
    static {
        PARSER_CONFIG_MAP.put(LuceneIndexKeys.FILM_SIZE, new PointsConfig(new DecimalFormat(), Integer.class));
        PARSER_CONFIG_MAP.put(LuceneIndexKeys.FILM_LENGTH, new PointsConfig(new DecimalFormat(), Integer.class));
    }

    private final Analyzer analyzer = LuceneDefaultAnalyzer.buildAnalyzer();

    public LuceneGuiFilmeModelHelper(@NotNull SeenHistoryController historyController,
                                     @NotNull SearchFieldData searchFieldData,
                                     @NotNull FilterConfiguration filterConfiguration) {
        super(historyController, searchFieldData, filterConfiguration);
    }

    private TModelFilm performTableFiltering() {
        var listeFilme = (IndexedFilmList) Daten.getInstance().getListeFilmeNachBlackList();
        try {
            calculateFilmLengthSliderValues();

            if (filterConfiguration.isShowUnseenOnly())
                historyController.prepareMemoryCache();

            String searchText = searchFieldData.searchFieldText();
            Stream<DatenFilm> stream = listeFilme.parallelStream();

            if (!noFiltersAreSet()) {
                var parser = new StandardQueryParser(analyzer);
                parser.setPointsConfigMap(PARSER_CONFIG_MAP);
                parser.setAllowLeadingWildcard(true);
                Query initialQuery;
                if (searchText.isEmpty())
                    initialQuery = new MatchAllDocsQuery();
                else
                    initialQuery = parser.parse(searchText, LuceneIndexKeys.TITEL);

                BooleanQuery.Builder qb = new BooleanQuery.Builder();
                qb.add(initialQuery, BooleanClause.Occur.MUST);

                //Zeitraum filter on demand …
                if (!filterConfiguration.getZeitraum().equalsIgnoreCase(ZeitraumSpinnerFormatter.INFINITE_TEXT)) {
                    try {
                        qb.add(createZeitraumQuery(), BooleanClause.Occur.FILTER);
                    } catch (Exception ex) {
                        logger.error("Unable to add zeitraum filter", ex);
                    }
                }
                if (filterConfiguration.isShowLivestreamsOnly()) {
                    addLivestreamQuery(qb);
                }
                if (filterConfiguration.isShowHighQualityOnly()) {
                    addHighQualityOnlyQuery(qb);
                }
                if (filterConfiguration.isDontShowTrailers()) {
                    addNoTrailerTeaserQuery(qb);
                }
                if (filterConfiguration.isDontShowAudioVersions()) {
                    addNoAudioVersionQuery(qb);
                }
                if (filterConfiguration.isDontShowSignLanguage()) {
                    addNoSignLanguageQuery(qb);
                }
                if (filterConfiguration.isDontShowDuplicates()) {
                    addNoDuplicatesQuery(qb);
                }

                if (filterConfiguration.isShowSubtitlesOnly()) {
                    addSubtitleOnlyQuery(qb);
                }
                if (filterConfiguration.isShowNewOnly()) {
                    addNewOnlyQuery(qb);
                }
                var selectedSenders = getSelectedSendersFromFilter();
                if (!selectedSenders.isEmpty()) {
                    addSenderFilterQuery(qb, selectedSenders);
                }

                //the complete lucene query...
                Query finalQuery = qb.build();
                logger.info("Executing Lucene query: {}", finalQuery);

                //SEARCH
                final var searcher = new IndexSearcher(listeFilme.getReader());
                final var docs = searcher.search(finalQuery, listeFilme.size());
                final var hit_length = docs.scoreDocs.length;

                Set<Integer> filmNrSet = HashSet.newHashSet(hit_length);

                logger.trace("Hit size: {}", hit_length);
                var watch2 = Stopwatch.createStarted();
                var storedFields = searcher.storedFields();
                for (final var hit : docs.scoreDocs) {
                    var docId = hit.doc;
                    var d = storedFields.document(docId, INTEREST_SET);
                    filmNrSet.add(Integer.parseInt(d.get(LuceneIndexKeys.ID)));
                }

                //process
                watch2.stop();
                logger.trace("Populating filmlist took: {}", watch2);
                logger.trace("Number of found Lucene index entries: {}", filmNrSet.size());

                stream = listeFilme.parallelStream()
                        .filter(film -> filmNrSet.contains(film.getFilmNr()));
            }

            if (filterConfiguration.isShowBookMarkedOnly())
                stream = stream.filter(DatenFilm::isBookmarked);
            if (filterConfiguration.isDontShowAbos())
                stream = stream.filter(film -> film.getAbo() == null);

            var resultList = applyCommonFilters(stream, filterConfiguration.getThema()).toList();
            logger.trace("Resulting filmlist size after all filters applied: {}", resultList.size());

            //adjust initial capacity
            var filmModel = new TModelFilm(resultList.size());
            filmModel.addAll(resultList);

            if (filterConfiguration.isShowUnseenOnly())
                historyController.emptyMemoryCache();

            return filmModel;
        } catch (Exception ex) {
            logger.error("Lucene filtering failed!", ex);
            SwingUtilities.invokeLater(() -> SwingErrorDialog.showExceptionMessage(MediathekGui.ui(),
                    "Die Lucene Abfrage ist inkorrekt und führt zu keinen Ergebnissen.", ex));
            return new TModelFilm();
        }
    }

    private void addSenderFilterQuery(@NotNull BooleanQuery.Builder qb, @NotNull Collection<String> selectedSenders) {
        BooleanQuery.Builder booleanQuery = new BooleanQuery.Builder();
        for (var sender : selectedSenders) {
            // sender must be lowercase as StandardAnalyzer converts it to lower during indexing
            TermQuery term = new TermQuery(new Term(LuceneIndexKeys.SENDER, sender.toLowerCase()));
            booleanQuery.add(term, BooleanClause.Occur.SHOULD);
        }

        qb.add(booleanQuery.build(), BooleanClause.Occur.FILTER);
    }

    private void addSubtitleOnlyQuery(@NotNull BooleanQuery.Builder qb) {
        var q = new TermQuery(new Term(LuceneIndexKeys.SUBTITLE, "true"));
        qb.add(q, BooleanClause.Occur.FILTER);
    }

    private void addNoDuplicatesQuery(@NotNull BooleanQuery.Builder qb) {
        var q = new TermQuery(new Term(LuceneIndexKeys.DUPLICATE, "true"));
        qb.add(q, BooleanClause.Occur.MUST_NOT);
    }

    private void addNoSignLanguageQuery(@NotNull BooleanQuery.Builder qb) {
        var q = new TermQuery(new Term(LuceneIndexKeys.SIGN_LANGUAGE, "true"));
        qb.add(q, BooleanClause.Occur.MUST_NOT);
    }

    private void addNoAudioVersionQuery(@NotNull BooleanQuery.Builder qb) {
        var q = new TermQuery(new Term(LuceneIndexKeys.AUDIOVERSION, "true"));
        qb.add(q, BooleanClause.Occur.MUST_NOT);
    }

    private void addNoTrailerTeaserQuery(@NotNull BooleanQuery.Builder qb) {
        var q = new TermQuery(new Term(LuceneIndexKeys.TRAILER_TEASER, "true"));
        qb.add(q, BooleanClause.Occur.MUST_NOT);
    }

    private void addNewOnlyQuery(@NotNull BooleanQuery.Builder qb) {
        var q = new TermQuery(new Term(LuceneIndexKeys.NEW, "true"));
        qb.add(q, BooleanClause.Occur.FILTER);
    }

    private void addLivestreamQuery(@NotNull BooleanQuery.Builder qb) {
        var q = new TermQuery(new Term(LuceneIndexKeys.LIVESTREAM, "true"));
        qb.add(q, BooleanClause.Occur.FILTER);
    }

    private void addHighQualityOnlyQuery(@NotNull BooleanQuery.Builder qb) {
        var q = new TermQuery(new Term(LuceneIndexKeys.HIGH_QUALITY, "true"));
        qb.add(q, BooleanClause.Occur.FILTER);
    }

    private Query createZeitraumQuery() throws ParseException {

        var numDays = Integer.parseInt(filterConfiguration.getZeitraum());
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

    @Override
    public TableModel getFilteredTableModel() {
        var listeFilme = (IndexedFilmList) Daten.getInstance().getListeFilmeNachBlackList();
        TModelFilm filmModel;

        if (!listeFilme.isEmpty()) {
            if (noFiltersAreSet()) {
                //adjust initial capacity
                filmModel = new TModelFilm(listeFilme.size());
                filmModel.addAll(listeFilme);
            } else {
                filmModel = performTableFiltering();
            }
        } else
            return new TModelFilm();

        return filmModel;
    }
}
