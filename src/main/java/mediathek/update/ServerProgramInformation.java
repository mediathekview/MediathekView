package mediathek.update;

import mediathek.tool.Version;

public record ServerProgramInformation(Version version) {
    /**
     * Tag definition for server response file
     */
    static class ParserTags {
        final static String VERSION = "Program_Version";
        final static String INFO = "Info";
        final static String INFO_NO = "number";
    }
}
