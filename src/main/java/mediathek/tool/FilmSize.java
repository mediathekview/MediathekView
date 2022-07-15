package mediathek.tool;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

/**
 * Store film size in Megabytes.
 */
public class FilmSize implements Comparable<FilmSize> {
    private int size;
    private static final Logger logger = LogManager.getLogger();

    public FilmSize() {
        this.size = 0;
    }

    public void setSize(String strSize) {
        if (strSize.equalsIgnoreCase("<1")) {
            size = 1;
        }

        try {
            if (!strSize.isEmpty()) {
                size = Integer.parseInt(strSize);
            }
        } catch (NumberFormatException ex) {
            logger.error("String: {}", strSize,ex);
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
