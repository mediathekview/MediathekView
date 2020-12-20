package mediathek.daten;

public enum DatenFilmFlags {
    /**
     * Is this film an audio version? (aka Hörfassung)
     */
    AUDIO_VERSION,
    /**
     * Flag indicating a trailer, teaser or german Vorschau.
     */
    TRAILER_TEASER,
    /**
     * Flag that this entry is in sign language (aka Gebärdensprache).
     */
    SIGN_LANGUAGE,
    /**
     * Indicates that entry is a livestream, not a regular movie.
     */
    LIVESTREAM,
    /**
     * Indicates that the film is "new" and has not been in the local filmlist before.
     */
    NEW_ENTRY,
    /**
     * Indicates that a film has "burned in" subtitles.
     */
    BURNED_IN_SUBTITLES,
    /**
     * Indicates that film contains a .m3u8 URL.
     */
    PLAYLIST
}
