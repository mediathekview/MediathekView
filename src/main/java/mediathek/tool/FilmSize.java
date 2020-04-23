package mediathek.tool;

import mediathek.daten.DatenFilm;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

/**
 * Store film size in bytes.
 */
public class FilmSize implements Comparable<FilmSize> {
    private int size;
    private static final Logger logger = LogManager.getLogger();

    public FilmSize(final int size) {
        this.size = size;
    }

    public FilmSize(DatenFilm film) {
        if (film.getSize().equalsIgnoreCase("<1")) {
            film.setSize("1");
        }

        try {
            if (!film.getSize().isEmpty()) {
                size = Integer.parseInt(film.getSize());
            }
        } catch (NumberFormatException ex) {
            logger.error("String: {}", film.getSize(),ex);
            size = 0;
        }
    }

    @Override
    public String toString() {
        return (size == 0) ? "" : Long.toString(size);
    }

    @Override
    public int compareTo(@NotNull FilmSize other) {
        return (Integer.compare(size, other.size));
    }
}
