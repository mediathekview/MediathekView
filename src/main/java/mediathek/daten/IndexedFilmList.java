package mediathek.daten;

import mediathek.config.StandardLocations;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.MMapDirectory;

public class IndexedFilmList extends ListeFilme {
    private static final Logger logger = LogManager.getLogger();
    private Directory luceneDirectory;
    private DirectoryReader reader;

    public IndexedFilmList() {
        try  {
            luceneDirectory = new MMapDirectory(StandardLocations.getFilmIndexPath());

        } catch (Exception ex) {
            logger.error("Creation of Lucene index directory failed!", ex);
        }
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
}
