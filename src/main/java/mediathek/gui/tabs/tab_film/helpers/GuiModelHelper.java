package mediathek.gui.tabs.tab_film.helpers;

import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.javafx.filterpanel.FilmActionPanel;

import javax.swing.table.TableModel;
import java.util.concurrent.TimeUnit;

public abstract class GuiModelHelper {
    protected SliderRange sliderRange;
    protected long maxLength;
    protected FilmActionPanel filmActionPanel;
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

    protected abstract boolean noFiltersAreSet();

    protected boolean seenCheck(DatenFilm film) {
        return !historyController.hasBeenSeenFromCache(film);
    }

    protected void calculateFilmLengthSliderValues() {
        final long minLength = (long) filmActionPanel.getFilmLengthSlider().getLowValue();
        maxLength = (long) filmActionPanel.getFilmLengthSlider().getHighValue();
        var minLengthInSeconds = TimeUnit.SECONDS.convert(minLength, TimeUnit.MINUTES);
        var maxLengthInSeconds = TimeUnit.SECONDS.convert(maxLength, TimeUnit.MINUTES);
        sliderRange = new SliderRange(minLengthInSeconds, maxLengthInSeconds);
    }
}
