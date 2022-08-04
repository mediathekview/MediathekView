package mediathek.gui.tabs.tab_film.helpers;

import com.google.common.base.Stopwatch;
import javafx.collections.ObservableList;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.IndexedFilmList;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.gui.tasks.LuceneIndexKeys;
import mediathek.javafx.filterpanel.FilmActionPanel;
import mediathek.javafx.filterpanel.FilmLengthSlider;
import mediathek.javafx.filterpanel.ZeitraumSpinner;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.SwingErrorDialog;
import mediathek.tool.models.TModelFilm;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.DateTools;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.queryparser.flexible.standard.StandardQueryParser;
import org.apache.lucene.queryparser.flexible.standard.config.PointsConfig;
import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.Query;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.table.TableModel;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LuceneGuiFilmeModelHelper {
    private static final Logger logger = LogManager.getLogger();
    private static final Map<String, PointsConfig> PARSER_CONFIG_MAP = new HashMap<>();

    static {
        PARSER_CONFIG_MAP.put(LuceneIndexKeys.FILM_SIZE, new PointsConfig(new DecimalFormat(), Integer.class));
        PARSER_CONFIG_MAP.put(LuceneIndexKeys.FILM_LENGTH, new PointsConfig(new DecimalFormat(), Integer.class));
    }

    private final FilmActionPanel filmActionPanel;
    private final SeenHistoryController historyController;
    private final GuiFilme.SearchField searchField;
    private boolean showNewOnly;
    private boolean showBookmarkedOnly;
    private boolean showSubtitlesOnly;
    private boolean showHqOnly;
    private boolean dontShowSeen;
    private boolean dontShowAbos;
    private boolean showLivestreamsOnly;
    private boolean dontShowTrailers;
    private boolean dontShowGebaerdensprache;
    private boolean dontShowAudioVersions;
    private long maxLength;
    private String zeitraum;
    private SliderRange sliderRange;

    public LuceneGuiFilmeModelHelper(@NotNull FilmActionPanel filmActionPanel,
                                     @NotNull SeenHistoryController historyController,
                                     @NotNull GuiFilme.SearchField searchField) {
        this.filmActionPanel = filmActionPanel;
        this.historyController = historyController;
        this.searchField = searchField;
    }

    private String getFilterThema() {
        String filterThema = filmActionPanel.getViewSettingsPane().themaComboBox.getSelectionModel().getSelectedItem();
        if (filterThema == null) {
            filterThema = "";
        }

        return filterThema;
    }

    private boolean noFiltersAreSet() {
        return filmActionPanel.getViewSettingsPane().senderCheckList.getCheckModel().isEmpty()
                && getFilterThema().isEmpty()
                && searchField.getText().isEmpty()
                && ((int) filmActionPanel.filmLengthSlider.getLowValue() == 0)
                && ((int) filmActionPanel.filmLengthSlider.getHighValue() == FilmLengthSlider.UNLIMITED_VALUE)
                && !filmActionPanel.dontShowAbos.getValue()
                && !filmActionPanel.showUnseenOnly.getValue()
                && !filmActionPanel.showOnlyHd.getValue()
                && !filmActionPanel.showSubtitlesOnly.getValue()
                && !filmActionPanel.showLivestreamsOnly.getValue()
                && !filmActionPanel.showNewOnly.getValue()
                && !filmActionPanel.showBookMarkedOnly.getValue()
                && !filmActionPanel.dontShowTrailers.getValue()
                && !filmActionPanel.dontShowSignLanguage.getValue()
                && !filmActionPanel.dontShowAudioVersions.getValue()
                && filmActionPanel.zeitraumProperty.getValue().equalsIgnoreCase(ZeitraumSpinner.UNLIMITED_VALUE);
    }

    private void updateFilterVars() {
        showNewOnly = filmActionPanel.showNewOnly.getValue();
        showBookmarkedOnly = filmActionPanel.showBookMarkedOnly.getValue();
        showSubtitlesOnly = filmActionPanel.showSubtitlesOnly.getValue();
        showHqOnly = filmActionPanel.showOnlyHd.getValue();
        dontShowSeen = filmActionPanel.showUnseenOnly.getValue();
        dontShowAbos = filmActionPanel.dontShowAbos.getValue();
        showLivestreamsOnly = filmActionPanel.showLivestreamsOnly.getValue();
        dontShowTrailers = filmActionPanel.dontShowTrailers.getValue();
        dontShowGebaerdensprache = filmActionPanel.dontShowSignLanguage.getValue();
        dontShowAudioVersions = filmActionPanel.dontShowAudioVersions.getValue();
        zeitraum = filmActionPanel.zeitraumProperty.getValue();
    }

    private void calculateFilmLengthSliderValues() {
        final long minLength = (long) filmActionPanel.filmLengthSlider.getLowValue();
        maxLength = (long) filmActionPanel.filmLengthSlider.getHighValue();
        var minLengthInSeconds = TimeUnit.SECONDS.convert(minLength, TimeUnit.MINUTES);
        var maxLengthInSeconds = TimeUnit.SECONDS.convert(maxLength, TimeUnit.MINUTES);
        sliderRange = new SliderRange(minLengthInSeconds, maxLengthInSeconds);
    }

    private TModelFilm performTableFiltering() {
        var listeFilme = (IndexedFilmList) Daten.getInstance().getListeFilmeNachBlackList();
        try {
            updateFilterVars();
            calculateFilmLengthSliderValues();

            if (dontShowSeen)
                historyController.prepareMemoryCache();

            String searchText = searchField.getText();
            List<DatenFilm> resultList;
            Stream<DatenFilm> stream;

            if (noFiltersAreSet()) {
                resultList = new ArrayList<>(listeFilme);
                stream = resultList.parallelStream();
            } else {
                Stopwatch watch2 = Stopwatch.createStarted();
                if (searchText.isEmpty()) {
                    // search for everything...
                    searchText = "*:*";
                }

                var analyzer = listeFilme.getAnalyzer();
                var parser = new StandardQueryParser(analyzer);
                parser.setPointsConfigMap(PARSER_CONFIG_MAP);
                var initialQuery = parser.parse(searchText, LuceneIndexKeys.TITEL);

                BooleanQuery.Builder qb = new BooleanQuery.Builder();
                qb.add(initialQuery, BooleanClause.Occur.MUST);

                //Zeitraum filter on demand...
                if (!zeitraum.equals(ZeitraumSpinner.UNLIMITED_VALUE)) {
                    try {
                        qb.add(createZeitraumQuery(listeFilme), BooleanClause.Occur.FILTER);
                    } catch (Exception ex) {
                        logger.error("Unable to add zeitraum filter", ex);
                    }
                }
                if (showLivestreamsOnly) {
                    addLivestreamQuery(qb, analyzer);
                }
                if (showHqOnly) {
                    addHighQualityOnlyQuery(qb, analyzer);
                }
                if (dontShowTrailers) {
                    addNoTrailerTeaserQuery(qb, analyzer);
                }
                if (dontShowAudioVersions) {
                    addNoAudioVersionQuery(qb, analyzer);
                }
                if (dontShowGebaerdensprache) {
                    addNoSignLanguageQuery(qb, analyzer);
                }
                if (showSubtitlesOnly) {
                    addSubtitleOnlyQuery(qb, analyzer);
                }
                if (showNewOnly) {
                    addNewOnlyQuery(qb, analyzer);
                }
                final ObservableList<String> selectedSenders = filmActionPanel.getViewSettingsPane().senderCheckList.getCheckModel().getCheckedItems();
                if (!selectedSenders.isEmpty()) {
                    addSenderFilterQuery(qb, analyzer, selectedSenders);
                }

                //the complete lucene query...
                Query finalQuery = qb.build();
                logger.info("Executing Lucene query: {}", finalQuery.toString());

                //SEARCH
                var searcher = listeFilme.getIndexSearcher();
                var docs = searcher.search(finalQuery, Integer.MAX_VALUE);
                var hits = docs.scoreDocs;

                watch2.stop();
                logger.trace("Lucene index search took: {}", watch2);

                Set<Integer> filmNrSet = new HashSet<>(hits.length);
                for (var hit : hits) {
                    var d = searcher.doc(hit.doc);
                    filmNrSet.add(Integer.parseInt(d.get(LuceneIndexKeys.ID)));
                }
                logger.trace("Number of found Lucene index entries: {}", filmNrSet.size());
                stream = listeFilme.parallelStream()
                        .filter(film -> filmNrSet.contains(film.getFilmNr()));
            }

            if (showBookmarkedOnly)
                stream = stream.filter(DatenFilm::isBookmarked);
            if (dontShowAbos)
                stream = stream.filter(film -> film.getAbo() == null);

            final String filterThema = getFilterThema();
            if (!filterThema.isEmpty()) {
                stream = stream.filter(film -> film.getThema().equalsIgnoreCase(filterThema));
            }
            if (maxLength < FilmLengthSlider.UNLIMITED_VALUE) {
                stream = stream.filter(this::maxLengthCheck);
            }
            if (dontShowSeen) {
                stream = stream.filter(this::seenCheck);
            }
            //perform min length filtering after all others may have reduced the available entries...
            stream = stream.filter(this::minLengthCheck);

            resultList = stream.collect(Collectors.toList());
            //logger.trace("Resulting filmlist size after all filters applied: {}", resultList.size());
            stream.close();

            //adjust initial capacity
            var filmModel = new TModelFilm(resultList.size());
            filmModel.addAll(resultList);

            resultList.clear();

            if (dontShowSeen)
                historyController.emptyMemoryCache();

            return filmModel;
        } catch (Exception ex) {
            logger.error("Lucene filtering failed!", ex);
            SwingUtilities.invokeLater(() -> SwingErrorDialog.showExceptionMessage(MediathekGui.ui(),
                    "Die Lucene Abfrage ist inkorrekt und führt zu keinen Ergebnissen.", ex));
            return new TModelFilm();
        }
    }

    private void addSenderFilterQuery(@NotNull BooleanQuery.Builder qb, @NotNull StandardAnalyzer analyzer, @NotNull List<String> selectedSenders) throws ParseException {
        StringBuilder sb = new StringBuilder();
        sb.append("+(");
        for (var sender : selectedSenders) {
            sb.append("sender:");
            sb.append(sender);
            sb.append(" ");
        }
        sb.append(")");
        var q = new QueryParser(LuceneIndexKeys.SENDER, analyzer).parse(sb.toString());
        qb.add(q, BooleanClause.Occur.FILTER);
    }
    private void addSubtitleOnlyQuery(@NotNull BooleanQuery.Builder qb, @NotNull StandardAnalyzer analyzer) throws ParseException {
        var q = new QueryParser(LuceneIndexKeys.SUBTITLE, analyzer)
                .parse("\"true\"");
        qb.add(q, BooleanClause.Occur.FILTER);
    }

    private void addNoSignLanguageQuery(@NotNull BooleanQuery.Builder qb, @NotNull StandardAnalyzer analyzer) throws ParseException {
        var q = new QueryParser(LuceneIndexKeys.SIGN_LANGUAGE, analyzer).parse("\"true\"");
        qb.add(q, BooleanClause.Occur.MUST_NOT);
    }

    private void addNoAudioVersionQuery(@NotNull BooleanQuery.Builder qb, @NotNull StandardAnalyzer analyzer) throws ParseException {
        var q = new QueryParser(LuceneIndexKeys.AUDIOVERSION, analyzer)
                .parse("\"true\"");
        qb.add(q, BooleanClause.Occur.MUST_NOT);
    }

    private void addNoTrailerTeaserQuery(@NotNull BooleanQuery.Builder qb, @NotNull StandardAnalyzer analyzer) throws ParseException {
        var q = new QueryParser(LuceneIndexKeys.TRAILER_TEASER, analyzer).parse("\"true\"");
        qb.add(q, BooleanClause.Occur.MUST_NOT);
    }

    private void addNewOnlyQuery(@NotNull BooleanQuery.Builder qb, @NotNull StandardAnalyzer analyzer) throws ParseException {
        var q = new QueryParser(LuceneIndexKeys.NEW, analyzer).parse("\"true\"");
        qb.add(q, BooleanClause.Occur.FILTER);
    }

    private void addLivestreamQuery(@NotNull BooleanQuery.Builder qb, @NotNull StandardAnalyzer analyzer) throws ParseException {
        var q = new QueryParser(LuceneIndexKeys.LIVESTREAM, analyzer).parse("\"true\"");
        qb.add(q, BooleanClause.Occur.FILTER);
    }

    private void addHighQualityOnlyQuery(@NotNull BooleanQuery.Builder qb, @NotNull StandardAnalyzer analyzer) throws ParseException {
        var q = new QueryParser(LuceneIndexKeys.HIGH_QUALITY, analyzer).parse("\"true\"");
        qb.add(q, BooleanClause.Occur.FILTER);
    }

    private Query createZeitraumQuery(@NotNull IndexedFilmList listeFilme) throws ParseException {
        var numDays = Integer.parseInt(zeitraum);
        var to_Date = LocalDateTime.now();
        var from_Date = to_Date.minusDays(numDays);
        var utcZone = ZoneId.of("UTC");
        //[20190101 TO 20190801]
        var toStr = DateTools.timeToString(to_Date.atZone(utcZone).toInstant().toEpochMilli(),
                DateTools.Resolution.DAY);
        var fromStr = DateTools.timeToString(from_Date.atZone(utcZone).toInstant().toEpochMilli(),
                DateTools.Resolution.DAY);
        String zeitraum = String.format("[%s TO %s]", fromStr, toStr);
        return new QueryParser(LuceneIndexKeys.SENDE_DATUM, listeFilme.getAnalyzer()).parse(zeitraum);
    }

    private boolean maxLengthCheck(DatenFilm film) {
        return film.getFilmLength() < sliderRange.getMaxLengthInSeconds();
    }

    private boolean seenCheck(DatenFilm film) {
        return !historyController.hasBeenSeenFromCache(film);
    }

    private boolean minLengthCheck(DatenFilm film) {
        var filmLength = film.getFilmLength();
        if (filmLength == 0)
            return true; // always show entries with length 0, which are internally "no length"
        else
            return filmLength >= sliderRange.getMinLengthInSeconds();
    }

    /**
     * Filter the filmlist.
     *
     * @return the filtered table model.
     */
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
