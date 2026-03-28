package mediathek.daten;

import mediathek.config.StandardLocations;
import mediathek.tool.ApplicationConfiguration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.store.*;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public class IndexedFilmList extends ListeFilme {
    private static final Logger logger = LogManager.getLogger();
    private Directory luceneDirectory;
    private DirectoryReader reader;
    private Map<Integer, DatenFilm> filmNrIndex;

    public IndexedFilmList() {
        try  {
            var indexPath = StandardLocations.getFilmIndexPath();
            luceneDirectory = createLuceneDirectory(indexPath);
        } catch (Exception ex) {
            logger.error("Creation of Lucene index directory failed!", ex);
        }
    }

    private Directory createLuceneDirectory(Path indexPath) throws Exception {
        var mode = ApplicationConfiguration.getConfiguration()
                .getString(ApplicationConfiguration.LUCENE_DIRECTORY_MODE, "auto")
                .trim()
                .toLowerCase(Locale.ROOT);

        logger.info("Using Lucene directory mode '{}' for index path {}", mode, indexPath);
        return switch (mode) {
            case "mmap" -> new MMapDirectory(indexPath);
            case "niofs" -> new NIOFSDirectory(indexPath);
            case "auto" -> FSDirectory.open(indexPath);
            case "in-memory" -> new ByteBuffersDirectory();
            default -> {
                logger.warn("Unknown Lucene directory mode '{}', falling back to 'auto'", mode);
                yield FSDirectory.open(indexPath);
            }
        };
    }

    public DirectoryReader getReader() {
        return reader;
    }

    public void setReader(DirectoryReader reader) {
        this.reader = reader;
    }

    public Directory getLuceneDirectory() {
        return luceneDirectory;
    }

    public synchronized DatenFilm getFilmByFilmNr(int filmNr) {
        if (filmNrIndex == null || filmNrIndex.size() != size()) {
            rebuildFilmNrIndex();
        }
        return filmNrIndex.get(filmNr);
    }

    private void rebuildFilmNrIndex() {
        var rebuiltIndex = new HashMap<Integer, DatenFilm>(Math.max(16, size()));
        for (var film : this) {
            rebuiltIndex.put(film.getFilmNr(), film);
        }
        filmNrIndex = rebuiltIndex;
    }

    @Override
    public synchronized void clear() {
        super.clear();
        filmNrIndex = null;
    }

    @Override
    public synchronized boolean add(DatenFilm film) {
        filmNrIndex = null;
        return super.add(film);
    }

    @Override
    public synchronized void add(int index, DatenFilm element) {
        filmNrIndex = null;
        super.add(index, element);
    }

    @Override
    public synchronized boolean addAll(java.util.Collection<? extends DatenFilm> c) {
        filmNrIndex = null;
        return super.addAll(c);
    }

    @Override
    public synchronized boolean addAll(int index, java.util.Collection<? extends DatenFilm> c) {
        filmNrIndex = null;
        return super.addAll(index, c);
    }

    @Override
    public synchronized DatenFilm remove(int index) {
        filmNrIndex = null;
        return super.remove(index);
    }

    @Override
    public synchronized boolean remove(Object o) {
        filmNrIndex = null;
        return super.remove(o);
    }

    @Override
    public synchronized DatenFilm set(int index, DatenFilm element) {
        filmNrIndex = null;
        return super.set(index, element);
    }
}
