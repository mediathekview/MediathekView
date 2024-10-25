package mediathek.gui.tabs.tab_film.helpers;

import javafx.collections.ObservableList;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPattern;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPatternWithDescription;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilter;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilterWithDescription;
import mediathek.javafx.filterpanel.FilmLengthSlider;
import mediathek.javafx.filterpanel.FilterActionPanel;
import mediathek.tool.Filter;
import mediathek.tool.models.TModelFilm;
import org.jetbrains.annotations.NotNull;

import javax.swing.table.TableModel;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class GuiFilmeModelHelper extends GuiModelHelper {
    private TModelFilm filmModel;
    private String[] arrIrgendwo;

    public GuiFilmeModelHelper(@NotNull FilterActionPanel filterActionPanel,
                               @NotNull SeenHistoryController historyController,
                               @NotNull SearchFieldData searchFieldData) {
        this.filterActionPanel = filterActionPanel;
        this.historyController = historyController;
        this.searchFieldData = searchFieldData;
    }

    private void performTableFiltering() {
        arrIrgendwo = searchFieldData.evaluateThemaTitel();

        calculateFilmLengthSliderValues();

        final String filterThema = getFilterThema();
        final ObservableList<String> selectedSenders = filterActionPanel.getViewSettingsPane().senderCheckList.getCheckModel().getCheckedItems();

        if (filterActionPanel.isShowUnseenOnly())
            historyController.prepareMemoryCache();

        var stream = Daten.getInstance().getListeFilmeNachBlackList().parallelStream();
        if (!selectedSenders.isEmpty()) {
            //ObservableList.contains() is insanely slow...this speeds up to factor 250!
            Set<String> senderSet = new HashSet<>(selectedSenders.size());
            senderSet.addAll(selectedSenders);
            stream = stream.filter(f -> senderSet.contains(f.getSender()));
        }
        if (filterActionPanel.isShowNewOnly())
            stream = stream.filter(DatenFilm::isNew);
        if (filterActionPanel.isShowBookMarkedOnly())
            stream = stream.filter(DatenFilm::isBookmarked);
        if (filterActionPanel.isShowLivestreamsOnly())
            stream = stream.filter(DatenFilm::isLivestream);
        if (filterActionPanel.isShowOnlyHighQuality())
            stream = stream.filter(DatenFilm::isHighQuality);
        if (filterActionPanel.isDontShowTrailers())
            stream = stream.filter(film -> !film.isTrailerTeaser());
        if (filterActionPanel.isDontShowSignLanguage())
            stream = stream.filter(film -> !film.isSignLanguage());
        if (filterActionPanel.isDontShowAudioVersions())
            stream = stream.filter(film -> !film.isAudioVersion());
        if (filterActionPanel.isDontShowAbos())
            stream = stream.filter(film -> film.getAbo() == null);
        if (filterActionPanel.isDontShowDuplicates()) {
            stream = stream.filter(film -> !film.isDuplicate());
        }
        if (filterActionPanel.isShowSubtitlesOnly()) {
            stream = stream.filter(this::subtitleCheck);
        }
        if (!filterThema.isEmpty()) {
            stream = stream.filter(film -> film.getThema().equalsIgnoreCase(filterThema));
        }
        if (maxLength < FilmLengthSlider.UNLIMITED_VALUE) {
            stream = stream.filter(this::maxLengthCheck);
        }
        if (filterActionPanel.isShowUnseenOnly()) {
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

        if (filterActionPanel.isShowUnseenOnly())
            historyController.emptyMemoryCache();
    }

    private Predicate<DatenFilm> createFinalStageFilter() {
        //if arrIrgendwo contains more than one search fields fall back to "old" pattern search
        //otherwise use more optimized search
        boolean isPattern = Filter.isPattern(arrIrgendwo[0]) || arrIrgendwo.length > 1;
        Predicate<DatenFilm> filter;
        if (searchFieldData.searchThroughDescriptions()) {
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

    @Override
    public TableModel getFilteredTableModel() {
        final var listeFilme = Daten.getInstance().getListeFilmeNachBlackList();

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
