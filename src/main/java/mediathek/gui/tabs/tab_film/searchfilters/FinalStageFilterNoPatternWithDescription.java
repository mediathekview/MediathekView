package mediathek.gui.tabs.tab_film.searchfilters;

import mediathek.daten.DatenFilm;
import org.jetbrains.annotations.NotNull;

import java.util.function.Predicate;

public class FinalStageFilterNoPatternWithDescription implements Predicate<DatenFilm> {
    private final String searchText;

    public FinalStageFilterNoPatternWithDescription(@NotNull String[] searchStr) {
        this.searchText = searchStr[0];
    }

    @Override
    public boolean test(DatenFilm datenFilm) {
        var result = searchDefault(datenFilm);

        // search description if available
        var description = datenFilm.getDescription();
        if (!description.isEmpty()) {
            result = description.toLowerCase().contains(searchText) || result;
        }

        return result;
    }

    private boolean searchDefault(@NotNull DatenFilm film) {
        return film.getThema().toLowerCase().contains(searchText)
                || film.getTitle().toLowerCase().contains(searchText);
    }
}
