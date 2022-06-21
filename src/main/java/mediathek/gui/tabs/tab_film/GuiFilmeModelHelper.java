package mediathek.gui.tabs.tab_film;

import javafx.collections.ObservableList;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPattern;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPatternWithDescription;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilter;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilterWithDescription;
import mediathek.javafx.filterpanel.FXSearchControlFieldMode;
import mediathek.javafx.filterpanel.FilmActionPanel;
import mediathek.javafx.filterpanel.FilmLengthSlider;
import mediathek.tool.Filter;
import mediathek.tool.models.TModelFilm;
import org.jetbrains.annotations.NotNull;

import javax.swing.table.TableModel;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class GuiFilmeModelHelper {
    private final FilmActionPanel filmActionPanel;
    private TModelFilm filmModel;
    private final ListeFilme listeFilme;
    private final SeenHistoryController historyController;
    private boolean searchThroughDescriptions;
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
    private String[] arrIrgendwo;
    private long minLengthInSeconds;
    private long maxLengthInSeconds;
    private final GuiFilme.SearchField newSearchField;

    public GuiFilmeModelHelper(@NotNull FilmActionPanel filmActionPanel,
                               @NotNull SeenHistoryController historyController,
                               @NotNull GuiFilme.SearchField newSearchField) {
        this.filmActionPanel = filmActionPanel;
        this.historyController = historyController;
        this.newSearchField = newSearchField;

        listeFilme = Daten.getInstance().getListeFilmeNachBlackList();
    }

    private String getFilterThema() {
        String filterThema = filmActionPanel.themaBox.getSelectionModel().getSelectedItem();
        if (filterThema == null) {
            filterThema = "";
        }

        return filterThema;
    }

    private String[] evaluateThemaTitel() {
        String[] arrThemaTitel;

        final String filterThemaTitel = newSearchField.getText();
        if (Filter.isPattern(filterThemaTitel)) {
            arrThemaTitel = new String[]{filterThemaTitel};
        } else {
            arrThemaTitel = filterThemaTitel.split(",");
            for (int i = 0; i < arrThemaTitel.length; ++i) {
                arrThemaTitel[i] = arrThemaTitel[i].trim().toLowerCase();
            }
        }

        return arrThemaTitel;
    }

    private boolean noFiltersAreSet() {
        return filmActionPanel.getViewSettingsPane().senderCheckList.getCheckModel().isEmpty()
                && getFilterThema().isEmpty()
                && newSearchField.getText().isEmpty()
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
                && !filmActionPanel.dontShowAudioVersions.getValue();
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
        searchThroughDescriptions = newSearchField.getSearchMode() == FXSearchControlFieldMode.IRGENDWO;

        arrIrgendwo = evaluateThemaTitel();
    }

    private void calculateFilmLengthSliderValues() {
        final long minLength = (long) filmActionPanel.filmLengthSlider.getLowValue();
        maxLength = (long) filmActionPanel.filmLengthSlider.getHighValue();
        minLengthInSeconds = TimeUnit.SECONDS.convert(minLength, TimeUnit.MINUTES);
        maxLengthInSeconds = TimeUnit.SECONDS.convert(maxLength, TimeUnit.MINUTES);
    }

    private void performTableFiltering() {
        updateFilterVars();
        calculateFilmLengthSliderValues();

        final String filterThema = getFilterThema();
        final ObservableList<String> selectedSenders = filmActionPanel.getViewSettingsPane().senderCheckList.getCheckModel().getCheckedItems();

        if (dontShowSeen)
            historyController.prepareMemoryCache();

        var stream = listeFilme.parallelStream();
        if (!selectedSenders.isEmpty()) {
            //ObservableList.contains() is insanely slow...this speeds up to factor 250!
            Set<String> senderSet = new HashSet<>(selectedSenders.size());
            senderSet.addAll(selectedSenders);
            stream = stream.filter(f -> senderSet.contains(f.getSender()));
        }
        if (showNewOnly)
            stream = stream.filter(DatenFilm::isNew);
        if (showBookmarkedOnly)
            stream = stream.filter(DatenFilm::isBookmarked);
        if (showLivestreamsOnly)
            stream = stream.filter(DatenFilm::isLivestream);
        if (showHqOnly)
            stream = stream.filter(DatenFilm::isHighQuality);
        if (dontShowTrailers)
            stream = stream.filter(film -> !film.isTrailerTeaser());
        if (dontShowGebaerdensprache)
            stream = stream.filter(film -> !film.isSignLanguage());
        if (dontShowAudioVersions)
            stream = stream.filter(film -> !film.isAudioVersion());
        if (dontShowAbos)
            stream = stream.filter(film -> film.getAbo() == null);
        if (showSubtitlesOnly) {
            stream = stream.filter(this::subtitleCheck);
        }
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

        //final stage filtering...
        final boolean searchFieldEmpty = arrIrgendwo.length == 0;
        if (!searchFieldEmpty) {
            stream = stream.filter(createFinalStageFilter());
        }

        var list = stream.collect(Collectors.toList());
        stream.close();

        //adjust initial capacity
        filmModel = new TModelFilm(list.size());
        filmModel.addAll(list);

        list.clear();

        if (dontShowSeen)
            historyController.emptyMemoryCache();
    }

    private Predicate<DatenFilm> createFinalStageFilter() {
        //if arrIrgendwo contains more than one search fields fall back to "old" pattern search
        //otherwise use more optimized search
        boolean isPattern = Filter.isPattern(arrIrgendwo[0]) || arrIrgendwo.length > 1;
        Predicate<DatenFilm> filter;
        if (searchThroughDescriptions) {
            if (isPattern)
                filter = new FinalStagePatternFilterWithDescription(arrIrgendwo);
            else
                filter = new FinalStageFilterNoPatternWithDescription(arrIrgendwo);
        }
        else {
            if (isPattern)
                filter = new FinalStagePatternFilter(arrIrgendwo);
            else
                filter = new FinalStageFilterNoPattern(arrIrgendwo);
        }
        return filter;
    }

    private boolean subtitleCheck(DatenFilm film) {
        return film.hasSubtitle() || film.hasBurnedInSubtitles();
    }
    private boolean maxLengthCheck(DatenFilm film) {
        return film.getFilmLength() < maxLengthInSeconds;
    }

    private boolean seenCheck(DatenFilm film) {
        return !historyController.hasBeenSeenFromCache(film);
    }

    private boolean minLengthCheck(DatenFilm film) {
        final long filmLength = film.getFilmLength();
        if (filmLength == 0)
            return true; // always show entries with length 0, which are internally "no length"
        else
            return filmLength >= minLengthInSeconds;
    }

    /**
     * Filter the filmlist.
     *
     * @return the filtered table model.
     */
    public TableModel getFilteredTableModel() {
        if (!listeFilme.isEmpty()) {
            if (noFiltersAreSet()) {
                //adjust initial capacity
                filmModel = new TModelFilm(listeFilme.size());
                filmModel.addAll(listeFilme);
            } else {
                performTableFiltering();
            }
        } else
            return new TModelFilm();

        return filmModel;
    }
}
