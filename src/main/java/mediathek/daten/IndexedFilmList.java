package mediathek.daten;

import mediathek.config.StandardLocations;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.store.NRTCachingDirectory;

public class IndexedFilmList extends ListeFilme {
    private static final Logger logger = LogManager.getLogger();
    private final StandardAnalyzer analyzer = new StandardAnalyzer();
    private Directory luceneDirectory;
    private IndexWriter writer;
    private DirectoryReader reader;
    private IndexSearcher indexSearcher;

    public IndexedFilmList() {
        try {
            var fsDir = FSDirectory.open(StandardLocations.getFilmIndexPath());
            luceneDirectory = new NRTCachingDirectory(fsDir, 20.0, 100.0);

            IndexWriterConfig indexWriterConfig = new IndexWriterConfig(analyzer);
            indexWriterConfig.setRAMBufferSizeMB(256d);
            writer = new IndexWriter(luceneDirectory, indexWriterConfig);
        } catch (Exception ex) {
            logger.error("Creation of Lucene index failed!", ex);
        }
    }

    public IndexSearcher getIndexSearcher() {
        return indexSearcher;
    }

    public void setIndexSearcher(IndexSearcher indexSearcher) {
        this.indexSearcher = indexSearcher;
    }

    public StandardAnalyzer getAnalyzer() {
        return analyzer;
    }

    public IndexWriter getWriter() {
        return writer;
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
