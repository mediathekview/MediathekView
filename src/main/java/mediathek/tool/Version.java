package mediathek.tool;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

public record Version(int major, int minor, int patch) implements Comparable<Version>{
    public static final Version INVALID_VERSION = new Version(0,0,0);
    private static final Logger logger = LogManager.getLogger();

    public static Version fromString(String versionsstring) {
        Version result;

        final String[] versions = versionsstring.replaceAll("-SNAPSHOT", "").split("\\.");
        if (versions.length == 3) {
            try {
                result = new Version(Integer.parseInt(versions[0]), Integer.parseInt(versions[1]), Integer.parseInt(versions[2]));
            } catch (NumberFormatException ex) {
                logger.error("Fehler beim Parsen der Version: {}", versionsstring, ex);
                result = INVALID_VERSION;
            }
        } else
            result = INVALID_VERSION;

        return result;
    }

    /**
     * Gibt die Version als gewichtete Zahl zurück.
     *
     * @return gewichtete Zahl als Integer
     */
    private int toNumber() {
        return major * 100 + minor * 10 + patch;
    }

    /**
     * Gibt die Version als String zurück
     *
     * @return String mit der Version
     */
    @Override
    public String toString() {
        return String.format("%d.%d.%d", major, minor, patch);
    }

    @Override
    public int compareTo(@NotNull Version other) {
        return Integer.compare(other.toNumber(), this.toNumber());
    }
}
