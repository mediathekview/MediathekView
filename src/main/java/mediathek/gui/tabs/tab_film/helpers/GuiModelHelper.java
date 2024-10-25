package mediathek.gui.tabs.tab_film.helpers;

import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.javafx.filterpanel.FilmLengthSlider;
import mediathek.javafx.filterpanel.FilterActionPanel;
import mediathek.javafx.filterpanel.ZeitraumSpinner;

import javax.swing.table.TableModel;
import java.util.concurrent.TimeUnit;

public abstract class GuiModelHelper {
    protected SliderRange sliderRange;
    protected long maxLength;
    protected FilterActionPanel filterActionPanel;
    protected SeenHistoryController historyController;
    protected SearchFieldData searchFieldData;

    /**
     * Filter the filmlist.
     *
     * @return the filtered table model.
     */
    public abstract TableModel getFilteredTableModel();

    protected boolean maxLengthCheck(DatenFilm film) {
        return film.getFilmLength() < sliderRange.maxLengthInSeconds();
    }

    protected boolean minLengthCheck(DatenFilm film) {
        var filmLength = film.getFilmLength();
        if (filmLength == 0)
            return true; // always show entries with length 0, which are internally "no length"
        else
            return filmLength >= sliderRange.minLengthInSeconds();
    }

    protected boolean noFiltersAreSet() {
        var filmLengthSlider = filterActionPanel.getFilmLengthSlider();

        return filterActionPanel.getViewSettingsPane().senderCheckList.getCheckModel().isEmpty()
                && getFilterThema().isEmpty()
                && searchFieldData.isEmpty()
                && ((int) filmLengthSlider.getLowValue() == 0)
                && ((int) filmLengthSlider.getHighValue() == FilmLengthSlider.UNLIMITED_VALUE)
                && !filterActionPanel.isDontShowAbos()
                && !filterActionPanel.isShowUnseenOnly()
                && !filterActionPanel.isShowOnlyHighQuality()
                && !filterActionPanel.isShowSubtitlesOnly()
                && !filterActionPanel.isShowLivestreamsOnly()
                && !filterActionPanel.isShowNewOnly()
                && !filterActionPanel.isShowBookMarkedOnly()
                && !filterActionPanel.isDontShowTrailers()
                && !filterActionPanel.isDontShowSignLanguage()
                && !filterActionPanel.isDontShowAudioVersions()
                && !filterActionPanel.isDontShowDuplicates()
                && filterActionPanel.zeitraumProperty().get().equalsIgnoreCase(ZeitraumSpinner.UNLIMITED_VALUE);
    }

    protected boolean seenCheck(DatenFilm film) {
        return !historyController.hasBeenSeenFromCache(film);
    }

    protected void calculateFilmLengthSliderValues() {
        final long minLength = (long) filterActionPanel.getFilmLengthSlider().getLowValue();
        maxLength = (long) filterActionPanel.getFilmLengthSlider().getHighValue();
        var minLengthInSeconds = TimeUnit.SECONDS.convert(minLength, TimeUnit.MINUTES);
        var maxLengthInSeconds = TimeUnit.SECONDS.convert(maxLength, TimeUnit.MINUTES);
        sliderRange = new SliderRange(minLengthInSeconds, maxLengthInSeconds);
    }

    protected String getFilterThema() {
        String filterThema = filterActionPanel.getViewSettingsPane().themaComboBox.getSelectionModel().getSelectedItem();

        return filterThema == null ? "" : filterThema;
    }
}
