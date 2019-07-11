package mediathek.tool;

import mediathek.daten.DatenFilm;
import org.jetbrains.annotations.NotNull;

public class MSLong implements Comparable<MSLong> {

    private long l = 0;

    public MSLong(final long l) {
        this.l = l;
    }

    public MSLong(DatenFilm film) {
        if (film.getSize().equals("<1")) {
            film.setSize("1");
        }

        try {
            if (!film.getSize().isEmpty()) {
                l = Long.valueOf(film.getSize());
            }
        } catch (NumberFormatException ex) {
            Log.errorLog(649891025, ex, "String: " + film.getSize());
            l = 0;
        }
    }

    @Override
    public String toString() {
        return (l == 0) ? "" : Long.toString(l);
    }

    @Override
    public int compareTo(@NotNull MSLong other) {
        return (Long.compare(l, other.l));
    }
}
