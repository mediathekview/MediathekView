package mediathek.daten;

public class DatenFilmCaptions {
    public static String getTitleByFieldIndex(int fieldindex) {
        return switch (fieldindex) {
            case DatenFilm.FILM_NR -> "Nr";
            case DatenFilm.FILM_SENDER -> "Sender";
            case DatenFilm.FILM_THEMA -> "Thema";
            case DatenFilm.FILM_TITEL -> "Titel";
            case DatenFilm.FILM_DATUM -> "Datum";
            case DatenFilm.FILM_ZEIT -> "Zeit";
            case DatenFilm.FILM_DAUER -> "Dauer";
            case DatenFilm.FILM_GROESSE -> "Größe [MB]";
            case DatenFilm.FILM_HD -> "HQ";
            case DatenFilm.FILM_UT -> "UT";
            case DatenFilm.FILM_GEO -> "Geo";
            case DatenFilm.FILM_URL -> "URL";
            default -> throw new IndexOutOfBoundsException("UNKNOWN COLUMN NAME: " + fieldindex);
        };
    }
}
