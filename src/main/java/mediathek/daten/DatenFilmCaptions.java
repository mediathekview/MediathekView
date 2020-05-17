package mediathek.daten;

public class DatenFilmCaptions 
{
	public static String getTitleByFieldIndex(int fieldindex)
	{
        switch (fieldindex) {
            case DatenFilm.FILM_NR: return getNrTitle();
            case DatenFilm.FILM_SENDER: return getSenderTitle();
            case DatenFilm.FILM_THEMA: return getThemaTitle();
            case DatenFilm.FILM_TITEL: return getTitleTitle();
            case DatenFilm.FILM_ABSPIELEN: return getPlaybackTitle();
            case DatenFilm.FILM_AUFZEICHNEN: return getRecordTitle();
            case DatenFilm.FILM_DATUM: return getSendeDatumTitle();
            case DatenFilm.FILM_ZEIT: return getSendeZeitTitle();
            case DatenFilm.FILM_DAUER: return getDauerTitle();
            case DatenFilm.FILM_GROESSE: return getSizeTitle();
            case DatenFilm.FILM_HD: return getHdTitle();
            case DatenFilm.FILM_UT: return getSubtitleTitle();
            case DatenFilm.FILM_GEO: return getGeoTitle();
            case DatenFilm.FILM_URL: return getUrlTitle();
            case DatenFilm.FILM_ABO_NAME: return getAboNameTitle();
            case DatenFilm.FILM_DATUM_LONG: return getDatumLongTitle();
            case DatenFilm.FILM_URL_HISTORY: return getUrlHistoryTitle();
            case DatenFilm.FILM_REF: return getRefTitle();
            case DatenFilm.FILM_URL_HD: return getUrlHdTitle();
            case DatenFilm.FILM_URL_SUBTITLE: return getUrlSubtitleTitle();
            case DatenFilm.FILM_URL_KLEIN: return getUrlKleinTitle();
            case DatenFilm.FILM_MERKEN: return getBookmarkTitle();

            default:
                throw new IndexOutOfBoundsException("UNKNOWN COLUMN NAME: " + fieldindex);
        }

	}
	
	// - Each field gets an "get<FieldName>Title" to get the German Title of the Field
	public static String getNrTitle()
	{
		return "Nr";
	}
	
	public static String getSenderTitle()
	{
		return "Sender";
	}

	public static String getThemaTitle()
	{
		return "Thema";
	}

	public static String getTitleTitle()
	{
		return "Titel";
	}

	public static String getPlaybackTitle()
	{
		return "?Abspielen?";
	}
	
	public static String getRecordTitle()
	{
		return "?Aufnehmen?";
	}

	public static String getBookmarkTitle()
	{
		return "?Merken?";
	}

	public static String getSendeDatumTitle()
	{
		return "Datum";
	}
	
	public static String getSendeZeitTitle()
	{
		return "Zeit";
	}
	
	public static String getDauerTitle()
	{
		return "Dauer";
	}

	public static String getSizeTitle()
	{
		return "Größe [MB]";
	}

    public static String getHdTitle()
	{
		return "HQ";
	}
	public static String getSubtitleTitle()
	{
		return "UT";
	}
	public static String getGeoTitle()
	{
		return "Geo";
	}
	public static String getUrlTitle()
	{
		return "URL";
	}
	public static String getAboNameTitle()
	{
		return "Abo";
	}

	public static String getDatumLongTitle()
	{
		return "?DatumLong?";
	}
	public static String getUrlHistoryTitle()
	{
		return "?URL Historie?";
	}
	public static String getRefTitle()
	{
		return "?Ref?";
	}
	public static String getUrlHdTitle()
	{
		return "URL HQ";
	}
	public static String getUrlSubtitleTitle()
	{
		return "URL UT";
	}
	public static String getUrlKleinTitle()
	{
		return "?URL Klein?";
	}
}
