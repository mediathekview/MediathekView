package mSearch.tool;

public class Version {

    private int major;
    private int minor;
    private int patch;

    public Version(int major, int minor, int patch) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
    }

    public Version(String versionsstring) {
        String[] versions = versionsstring.replaceAll("-SNAPSHOT", "").split("\\.");
        if (versions.length == 3) {
            try {
                major = Integer.parseInt(versions[0]);
                minor = Integer.parseInt(versions[1]);
                patch = Integer.parseInt(versions[2]);
            } catch (NumberFormatException ex) {
                Log.errorLog(12344564, ex, "Fehler beim Parsen der Version '" + versionsstring + "'.");
                major = 0;
                minor = 0;
                patch = 0;
            }
        }
    }

    public Version() {
        major = 0;
        minor = 0;
        patch = 0;
    }

    public int getMajor() {
        return major;
    }

    public void setMajor(int major) {
        this.major = major;
    }

    public int getMinor() {
        return minor;
    }

    public void setMinor(int minor) {
        this.minor = minor;
    }

    public int getPatch() {
        return patch;
    }

    public void setPatch(int patch) {
        this.patch = patch;
    }

    /**
     * Gibt die Version als gewichtete Zahl zurück.
     *
     * @return gewichtete Zahl als Integer
     */
    public int toNumber() {
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

    /**
     * Nimmt ein Objekt vom Typ Version an und vergleicht ihn mit sich selbst
     *
     * @param a Versionsobjekt welches zum vergleich rangezogen werden soll
     * @return 1 Version a ist größer, 0 Versionen sind gleich oder -1 Version a ist kleiner
     */
    public int compare(Version versionzwei) {
        return Integer.compare(versionzwei.toNumber(), this.toNumber());
    }

}
