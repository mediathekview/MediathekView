package mediathek.gui.tabs.tab_film.searchfilters;

import mediathek.daten.DatenFilm;
import org.jetbrains.annotations.NotNull;

import java.util.function.Predicate;

public class FinalStageFilterNoPattern implements Predicate<DatenFilm> {
    private final String searchText;

    public FinalStageFilterNoPattern(@NotNull String[] searchStr) {
        this.searchText = searchStr[0];
    }

    @Override
    public boolean test(DatenFilm datenFilm) {
        // searchText cannot be empty as it is checked before
        return datenFilm.getThema().toLowerCase().contains(searchText)
                || datenFilm.getTitle().toLowerCase().contains(searchText);
    }
}
