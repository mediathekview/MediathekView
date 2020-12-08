package mediathek.tool;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record Version(int major, int minor, int patch) {
    private static final Version INVALID_VERSION = new Version(0,0,0);
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
     * Check if this version is invalid.
     * @return true if invalid, false otherwise.
     */
    public boolean isInvalid() {
        return equals(INVALID_VERSION);
    }

    /**
     * Check if other version is newer than we are.
     * @param other the other version to check.
     * @return true if other is newer, otherwise false.
     */
    public boolean isOlderThan(Version other) {
        return other.toNumber() > this.toNumber();
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
}
