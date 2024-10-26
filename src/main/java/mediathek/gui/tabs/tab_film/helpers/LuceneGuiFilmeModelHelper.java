package mediathek.gui.tabs.tab_film.helpers;

import com.google.common.base.Stopwatch;
import javafx.collections.ObservableList;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.IndexedFilmList;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.gui.tasks.LuceneIndexKeys;
import mediathek.javafx.filterpanel.FilterActionPanel;
import mediathek.javafx.filterpanel.ZeitraumSpinner;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.SwingErrorDialog;
import mediathek.tool.models.TModelFilm;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
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

    private final StandardAnalyzer analyzer = new StandardAnalyzer();

    public LuceneGuiFilmeModelHelper(@NotNull FilterActionPanel filterActionPanel,
                                     @NotNull SeenHistoryController historyController,
                                     @NotNull SearchFieldData searchFieldData) {
        this.filterActionPanel = filterActionPanel;
        this.historyController = historyController;
        this.searchFieldData = searchFieldData;
    }

    private TModelFilm performTableFiltering() {
        var listeFilme = (IndexedFilmList) Daten.getInstance().getListeFilmeNachBlackList();
        try {
            calculateFilmLengthSliderValues();

            if (filterActionPanel.isShowUnseenOnly())
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

                //Zeitraum filter on demand...
                if (!filterActionPanel.zeitraumProperty().get().equals(ZeitraumSpinner.UNLIMITED_VALUE)) {
                    try {
                        qb.add(createZeitraumQuery(), BooleanClause.Occur.FILTER);
                    } catch (Exception ex) {
                        logger.error("Unable to add zeitraum filter", ex);
                    }
                }
                if (filterActionPanel.isShowLivestreamsOnly()) {
                    addLivestreamQuery(qb);
                }
                if (filterActionPanel.isShowOnlyHighQuality()) {
                    addHighQualityOnlyQuery(qb);
                }
                if (filterActionPanel.isDontShowTrailers()) {
                    addNoTrailerTeaserQuery(qb);
                }
                if (filterActionPanel.isDontShowAudioVersions()) {
                    addNoAudioVersionQuery(qb);
                }
                if (filterActionPanel.isDontShowSignLanguage()) {
                    addNoSignLanguageQuery(qb);
                }
                if (filterActionPanel.isDontShowDuplicates()) {
                    addNoDuplicatesQuery(qb);
                }

                if (filterActionPanel.isShowSubtitlesOnly()) {
                    addSubtitleOnlyQuery(qb);
                }
                if (filterActionPanel.isShowNewOnly()) {
                    addNewOnlyQuery(qb);
                }
                final ObservableList<String> selectedSenders = filterActionPanel.getViewSettingsPane().senderCheckList.getCheckModel().getCheckedItems();
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

            if (filterActionPanel.isShowBookMarkedOnly())
                stream = stream.filter(DatenFilm::isBookmarked);
            if (filterActionPanel.isDontShowAbos())
                stream = stream.filter(film -> film.getAbo() == null);

            var resultList = applyCommonFilters(stream, getFilterThema()).toList();
            logger.trace("Resulting filmlist size after all filters applied: {}", resultList.size());

            //adjust initial capacity
            var filmModel = new TModelFilm(resultList.size());
            filmModel.addAll(resultList);

            if (filterActionPanel.isShowUnseenOnly())
                historyController.emptyMemoryCache();

            return filmModel;
        } catch (Exception ex) {
            logger.error("Lucene filtering failed!", ex);
            SwingUtilities.invokeLater(() -> SwingErrorDialog.showExceptionMessage(MediathekGui.ui(),
                    "Die Lucene Abfrage ist inkorrekt und führt zu keinen Ergebnissen.", ex));
            return new TModelFilm();
        }
    }

    private void addSenderFilterQuery(@NotNull BooleanQuery.Builder qb, @NotNull List<String> selectedSenders) {
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
        var numDays = Integer.parseInt(filterActionPanel.zeitraumProperty().get());
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
