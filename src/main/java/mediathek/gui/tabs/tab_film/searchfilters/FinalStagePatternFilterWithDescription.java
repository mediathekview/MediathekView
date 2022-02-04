package mediathek.gui.tabs.tab_film.searchfilters;

import mediathek.daten.DatenFilm;
import mediathek.tool.Filter;
import org.jetbrains.annotations.NotNull;

import java.util.function.Predicate;

public record FinalStagePatternFilterWithDescription(@NotNull String[] searchStr) implements Predicate<DatenFilm> {
    @Override
    public boolean test(@NotNull DatenFilm film) {
        var result = searchDefault(film);

        // search description if available
        var description = film.getDescription();
        if (!description.isEmpty())
            result = Filter.pruefen(searchStr, description) || result;

        return result;
    }

    private boolean searchDefault(@NotNull DatenFilm film) {
        return Filter.pruefen(searchStr, film.getThema())
                || Filter.pruefen(searchStr, film.getTitle());
    }
}
