package mediathek.update;

import mSearch.tool.Version;

/**
 * Encapsulates the retrieved update information.
 */
class ServerProgramInformation {
    private Version version;
    private String releaseNotes;
    private String updateUrl;

    public Version getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = new Version(version);
    }

    public String getReleaseNotes() {
        return releaseNotes;
    }

    public void setReleaseNotes(String release) {
        this.releaseNotes = release;
    }

    public String getUpdateUrl() {
        return updateUrl;
    }

    public void setUpdateUrl(String updateUrl) {
        this.updateUrl = updateUrl;
    }

    /**
     * Tag definition for server response file
     */
    class ParserTags {
        final static String VERSION = "Program_Version";
        final static String RELEASE_NOTES = "Program_Release_Info";
        final static String UPDATE_URL = "Download_Programm";
        final static String INFO = "Info";
        final static String INFO_NO = "number";
    }
}
