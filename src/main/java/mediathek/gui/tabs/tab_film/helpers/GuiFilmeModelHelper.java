package mediathek.gui.tabs.tab_film.helpers;

import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPattern;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPatternWithDescription;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilter;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilterWithDescription;
import mediathek.gui.filterpanel.filterpanel.FilmActionPanelSwing;
import mediathek.gui.filterpanel.filterpanel.FilmLengthSliderSwing;
import mediathek.tool.Filter;
import mediathek.tool.models.TModelFilm;
import org.jetbrains.annotations.NotNull;

import javax.swing.table.TableModel;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class GuiFilmeModelHelper extends GuiModelHelper {
    private TModelFilm filmModel;
    private String[] arrIrgendwo;

    public GuiFilmeModelHelper(@NotNull FilmActionPanelSwing filmActionPanel,
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

        return filmActionPanel.getViewSettingsPane().senderCheckList.getSelectionModel().getSelectedItemsCount() == 0
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
                && !filmActionPanel.isDontShowAudioVersions();
    }


    private void performTableFiltering() {
        arrIrgendwo = searchFieldData.evaluateThemaTitel();

        calculateFilmLengthSliderValues();

        final String filterThema = getFilterThema();
        final Object[] selectedSenders = filmActionPanel.getViewSettingsPane().senderCheckList.getCheckBoxListSelectedValues();

        List<String> senderList = Arrays.stream(selectedSenders)
                .map(Object::toString)  // Falls es sich nicht um Strings handelt
                .collect(Collectors.toList());
        if (filmActionPanel.isShowUnseenOnly())
            historyController.prepareMemoryCache();

        var stream = Daten.getInstance().getListeFilmeNachBlackList().parallelStream();
        if (!senderList.isEmpty()) {
            //ObservableList.contains() is insanely slow...this speeds up to factor 250!
            Set<String> senderSet = new HashSet<>(senderList.size());
            senderSet.addAll(senderList);
            stream = stream.filter(f -> senderSet.contains(f.getSender()));
        }
        if (filmActionPanel.isShowNewOnly())
            stream = stream.filter(DatenFilm::isNew);
        if (filmActionPanel.isShowBookMarkedOnly())
            stream = stream.filter(DatenFilm::isBookmarked);
        if (filmActionPanel.isShowLivestreamsOnly())
            stream = stream.filter(DatenFilm::isLivestream);
        if (filmActionPanel.isShowOnlyHighQuality())
            stream = stream.filter(DatenFilm::isHighQuality);
        if (filmActionPanel.isDontShowTrailers())
            stream = stream.filter(film -> !film.isTrailerTeaser());
        if (filmActionPanel.isDontShowSignLanguage())
            stream = stream.filter(film -> !film.isSignLanguage());
        if (filmActionPanel.isDontShowAudioVersions())
            stream = stream.filter(film -> !film.isAudioVersion());
        if (filmActionPanel.isDontShowAbos())
            stream = stream.filter(film -> film.getAbo() == null);
        if (filmActionPanel.isShowSubtitlesOnly()) {
            stream = stream.filter(this::subtitleCheck);
        }
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

        if (filmActionPanel.isShowUnseenOnly())
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
