package mediathek.gui.tabs.tab_film.helpers;

import com.google.common.base.Stopwatch;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.IndexedFilmList;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.gui.tasks.LuceneIndexKeys;
import mediathek.gui.filterpanel.FilmActionPanelSwing;
import mediathek.gui.filterpanel.FilmLengthSliderSwing;
import mediathek.gui.filterpanel.ZeitraumSpinnerSwing;
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
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LuceneGuiFilmeModelHelper extends GuiModelHelper{
    private static final Logger logger = LogManager.getLogger();
    private static final Map<String, PointsConfig> PARSER_CONFIG_MAP = new HashMap<>();

    static {
        PARSER_CONFIG_MAP.put(LuceneIndexKeys.FILM_SIZE, new PointsConfig(new DecimalFormat(), Integer.class));
        PARSER_CONFIG_MAP.put(LuceneIndexKeys.FILM_LENGTH, new PointsConfig(new DecimalFormat(), Integer.class));
    }


    public LuceneGuiFilmeModelHelper(@NotNull FilmActionPanelSwing filmActionPanel,
                                     @NotNull SeenHistoryController historyController,
                                     @NotNull SearchFieldData searchFieldData) {
        this.filmActionPanel = filmActionPanel;
        this.historyController = historyController;
        this.searchFieldData = searchFieldData;
    }

    private String getFilterThema() {
        String filterThema = filmActionPanel.getViewSettingsPane().themaComboBox.getSelectedThema();
        if (filterThema == null) {
            filterThema = "";
        }

        return filterThema;
    }

    @Override
    protected boolean noFiltersAreSet() {
        var filmLengthSlider = filmActionPanel.getFilmLengthSlider();

        return filmActionPanel.getViewSettingsPane().senderCheckList.getCheckBoxListSelectedValues().length == 0
                && getFilterThema().isEmpty()
                && searchFieldData.isEmpty()
                && ((int) filmLengthSlider.getLowValue() == 0)
                && ((int) filmLengthSlider.getHighValue() == FilmLengthSliderSwing.UNLIMITED_VALUE)
                && !filmActionPanel.isDontShowAbos()
                && !filmActionPanel.isShowUnseenOnly()
                && !filmActionPanel.isShowOnlyHighQuality()
                && !filmActionPanel.isShowSubtitlesOnly()
                && !filmActionPanel.isShowLivestreamsOnly()
                && !filmActionPanel.isShowNewOnly()
                && !filmActionPanel.isShowBookMarkedOnly()
                && !filmActionPanel.isDontShowTrailers()
                && !filmActionPanel.isDontShowSignLanguage()
                && !filmActionPanel.isDontShowAudioVersions()
                && filmActionPanel.zeitraumProperty().getValue().equals(ZeitraumSpinnerSwing.UNLIMITED_VALUE);
    }


    private TModelFilm performTableFiltering() {
        var listeFilme = (IndexedFilmList) Daten.getInstance().getListeFilmeNachBlackList();
        try {
            calculateFilmLengthSliderValues();

            if (filmActionPanel.isShowUnseenOnly())
                historyController.prepareMemoryCache();

            String searchText = searchFieldData.searchFieldText();
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
                if (!filmActionPanel.zeitraumProperty().getValue().equals(ZeitraumSpinnerSwing.UNLIMITED_VALUE)) {
                    try {
                        qb.add(createZeitraumQuery(listeFilme), BooleanClause.Occur.FILTER);
                    } catch (Exception ex) {
                        logger.error("Unable to add zeitraum filter", ex);
                    }
                }
                if (filmActionPanel.isShowLivestreamsOnly()) {
                    addLivestreamQuery(qb, analyzer);
                }
                if (filmActionPanel.isShowOnlyHighQuality()) {
                    addHighQualityOnlyQuery(qb, analyzer);
                }
                if (filmActionPanel.isDontShowTrailers()) {
                    addNoTrailerTeaserQuery(qb, analyzer);
                }
                if (filmActionPanel.isDontShowAudioVersions()) {
                    addNoAudioVersionQuery(qb, analyzer);
                }
                if (filmActionPanel.isDontShowSignLanguage()) {
                    addNoSignLanguageQuery(qb, analyzer);
                }
                if (filmActionPanel.isShowSubtitlesOnly()) {
                    addSubtitleOnlyQuery(qb, analyzer);
                }
                if (filmActionPanel.isShowNewOnly()) {
                    addNewOnlyQuery(qb, analyzer);
                }
                final Object[] selectedSenders = filmActionPanel.getViewSettingsPane().senderCheckList.getCheckBoxListSelectedValues();

                List<String> senderList = Arrays.stream(selectedSenders)
                        .map(Object::toString)  // Falls es sich nicht um Strings handelt
                        .collect(Collectors.toList());

                if (!senderList.isEmpty()) {
                    addSenderFilterQuery(qb, analyzer, senderList);
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
                    final var id = searcher.storedFields().document(hit.doc).get(LuceneIndexKeys.ID);
                    filmNrSet.add(Integer.parseInt(id));
                }
                logger.trace("Number of found Lucene index entries: {}", filmNrSet.size());
                stream = listeFilme.parallelStream()
                        .filter(film -> filmNrSet.contains(film.getFilmNr()));
            }

            if (filmActionPanel.isShowBookMarkedOnly())
                stream = stream.filter(DatenFilm::isBookmarked);
            if (filmActionPanel.isDontShowAbos())
                stream = stream.filter(film -> film.getAbo() == null);

            final String filterThema = getFilterThema();
            if (!filterThema.isEmpty()) {
                stream = stream.filter(film -> film.getThema().equalsIgnoreCase(filterThema));
            }
            if (maxLength < FilmLengthSliderSwing.UNLIMITED_VALUE) {
                stream = stream.filter(this::maxLengthCheck);
            }
            if (filmActionPanel.isShowUnseenOnly()) {
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

            if (filmActionPanel.isShowUnseenOnly())
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
        var numDays = Integer.parseInt((String) filmActionPanel.zeitraumProperty().getValue());
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
