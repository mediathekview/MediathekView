package mSearch.filmlisten.reader;

import com.fasterxml.jackson.core.JsonParser;
import mSearch.daten.DatenFilm;

import java.io.IOException;

public class FastAutoFilmListReader extends FilmListReader {
    public FastAutoFilmListReader() {
        super();
        //limit memory usage to 24 MBytes
        DECOMPRESSOR_MEMORY_LIMIT = 24 * 1024 * 1024;
    }

    @Override
    protected void parseWebsiteLink(JsonParser jp, DatenFilm datenFilm) throws IOException {
        //we do not process these in FASTAUTO mode
        jp.nextToken();
    }

    @Override
    protected void parseGeo(JsonParser jp, DatenFilm datenFilm) throws IOException {
        //we do not process these in FASTAUTO mode
        jp.nextToken();
    }
}
