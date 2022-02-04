package mediathek.gui.tabs.tab_film.searchfilters;

import mediathek.daten.DatenFilm;
import mediathek.tool.Filter;
import org.jetbrains.annotations.NotNull;

import java.util.function.Predicate;

public record FinalStagePatternFilter(@NotNull String[] searchStr) implements Predicate<DatenFilm> {
    @Override
    public boolean test(@NotNull DatenFilm film) {
        return Filter.pruefen(searchStr, film.getThema())
                || Filter.pruefen(searchStr, film.getTitle());
    }
}
