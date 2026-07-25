package mediathek.gui.tabs.tab_film.helpers

import mediathek.gui.tasks.LuceneIndexKeys
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.NumericDocValuesField
import org.apache.lucene.document.StringField
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.search.BooleanClause
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.MatchAllDocsQuery
import org.apache.lucene.store.ByteBuffersDirectory
import org.apache.lucene.store.Directory
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.*

internal class LuceneGuiFilmeModelHelperTest {

    @Test
    fun `sender filter returns matching film numbers from doc values`() {
        ByteBuffersDirectory().use { directory ->
            IndexWriter(directory, IndexWriterConfig()).use { writer ->
                writer.addDocument(filmDocument(filmNr = 1, sender = "ARD"))
                writer.addDocument(filmDocument(filmNr = 2, sender = "ZDF"))
                writer.addDocument(filmDocument(filmNr = 3, sender = "ARTE"))
                writer.commit()
            }

            DirectoryReader.open(directory).use { reader ->
                val query = BooleanQuery.Builder()
                    .add(MatchAllDocsQuery.INSTANCE, BooleanClause.Occur.MUST)
                    .add(createSenderFilterQuery(listOf("ARD", "Arte")), BooleanClause.Occur.FILTER)
                    .build()

                val matchingFilmNrs = IndexSearcher(reader)
                    .search(query, FilmNumberCollectorManager())
                    .sorted()

                assertEquals(listOf(1, 3), matchingFilmNrs)
            }
        }
    }

    @Test
    fun `searcher cache refreshes when filmlist reader changes`() {
        val cache = CurrentReaderIndexSearcherCache()
        ByteBuffersDirectory().use { firstDirectory ->
            ByteBuffersDirectory().use { secondDirectory ->
                writeFilms(firstDirectory, filmDocument(filmNr = 1, sender = "ARD"))
                writeFilms(secondDirectory, filmDocument(filmNr = 2, sender = "ARD"))

                DirectoryReader.open(firstDirectory).use { firstReader ->
                    val firstSearcher = cache.searcherFor(firstReader)
                    assertSame(firstSearcher, cache.searcherFor(firstReader))
                    assertEquals(listOf(1), matchingFilmNrs(firstSearcher))

                    DirectoryReader.open(secondDirectory).use { secondReader ->
                        val secondSearcher = cache.searcherFor(secondReader)

                        assertNotSame(firstSearcher, secondSearcher)
                        assertSame(secondSearcher, cache.searcherFor(secondReader))
                        assertEquals(listOf(2), matchingFilmNrs(secondSearcher))
                    }
                }
            }
        }
    }

    @Test
    fun `near real time reader remains searchable after writer closes without commit`() {
        ByteBuffersDirectory().use { directory ->
            val reader = IndexWriter(
                directory,
                IndexWriterConfig().apply {
                    openMode = IndexWriterConfig.OpenMode.CREATE
                    setCommitOnClose(false)
                },
            ).use { writer ->
                writer.addDocument(filmDocument(filmNr = 1, sender = "ARD"))
                DirectoryReader.open(writer)
            }

            reader.use {
                assertEquals(listOf(1), matchingFilmNrs(IndexSearcher(it)))
            }
        }
    }

    private fun writeFilms(directory: Directory, vararg documents: Document) {
        IndexWriter(directory, IndexWriterConfig()).use { writer ->
            documents.forEach(writer::addDocument)
            writer.commit()
        }
    }

    private fun matchingFilmNrs(searcher: IndexSearcher): List<Int> {
        val query = BooleanQuery.Builder()
            .add(MatchAllDocsQuery.INSTANCE, BooleanClause.Occur.MUST)
            .add(createSenderFilterQuery(listOf("ARD")), BooleanClause.Occur.FILTER)
            .build()
        return searcher.search(query, FilmNumberCollectorManager()).sorted()
    }

    private fun filmDocument(filmNr: Int, sender: String): Document =
        Document().apply {
            add(StringField(LuceneIndexKeys.SENDER, sender.lowercase(Locale.ROOT), Field.Store.NO))
            add(NumericDocValuesField(LuceneIndexKeys.ID_DOC_VALUE, filmNr.toLong()))
        }
}
