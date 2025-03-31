package mediathek.gui.tasks;

public class LuceneIndexKeys {
    /**
     * In Abfragen nicht zu verwenden!
     */
    public static final String ID = "id";
    /**
     * String-Value
     */
    public static final String SENDER = "sender";
    /**
     * String-Value
     */
    public static final String TITEL = "titel";
    /**
     * String-Value
     */
    public static final String THEMA = "thema";
    /**
     * String-Value
     */
    public static final String BESCHREIBUNG = "beschreibung";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String LIVESTREAM = "livestream";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String HIGH_QUALITY = "highquality";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String SUBTITLE = "untertitel";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String TRAILER_TEASER = "trailerteaser";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String AUDIOVERSION = "audioversion";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String SIGN_LANGUAGE = "signlanguage";
    /**
     * Datum im Format "YYYYMMDD". String.
     * Nicht existente Werte sind "19000101".
     */
    public static final String SENDE_DATUM = "sendedatum";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String NEW = "neu";
    /**
     * Filmlänge in Sekunde. Integer-Value. 0 wenn nicht vorhanden.
     */
    public static final String FILM_LENGTH = "länge";
    /**
     * Filmgröße in Megabytes. Integer-Value. 0 wenn nicht vorhanden.
     */
    public static final String FILM_SIZE = "größe";
    /**
     * Boolean Key ist nur vorhanden, wenn Film als Duplikat klassifiziert wurde. Dann "true".
     */
    public static final String DUPLICATE = "duplicate";
}
