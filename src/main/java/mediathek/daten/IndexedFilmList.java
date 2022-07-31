package mediathek.daten;

import mediathek.config.StandardLocations;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.store.NRTCachingDirectory;

import java.beans.PropertyChangeListener;

public class IndexedFilmList extends ListeFilme {
    private static final String PCS_INDEX = "lucene_index";
    private static final Logger logger = LogManager.getLogger();
    private final StandardAnalyzer analyzer = new StandardAnalyzer();
    private Directory luceneDirectory;
    private IndexWriter writer;
    private boolean validIndex;

    public IndexedFilmList() {
        try {
            var fsDir = FSDirectory.open(StandardLocations.getFilmIndexPath());
            luceneDirectory = new NRTCachingDirectory(fsDir, 20.0, 100.0);

            IndexWriterConfig indexWriterConfig = new IndexWriterConfig(analyzer);
            indexWriterConfig.setRAMBufferSizeMB(256d);
            writer = new IndexWriter(luceneDirectory, indexWriterConfig);
        } catch (Exception ex) {
            logger.error("Creation of Lucene index failed!", ex);
            setValidIndex(false);
        }
    }

    /**
     * Return whether the filmlist has a valid Lucene index.
     * @return true if a index was already created.
     */
    public boolean hasValidIndex() {
        return validIndex;
    }

    public void setValidIndex(boolean index) {
        var oldValue = validIndex;
        this.validIndex = index;
        this.pcs.firePropertyChange(PCS_INDEX, oldValue, validIndex);
    }

    public void addValidIndexChangeListener(PropertyChangeListener listener) {
        this.pcs.addPropertyChangeListener(PCS_INDEX, listener);
    }

    public StandardAnalyzer getAnalyzer() {
        return analyzer;
    }

    public IndexWriter getWriter() {
        return writer;
    }

    public Directory getLuceneDirectory() {
        return luceneDirectory;
    }
}
