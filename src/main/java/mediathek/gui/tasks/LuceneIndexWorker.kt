/*
 * Copyright (c) 2024-2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui.tasks

import kotlinx.coroutines.asCoroutineDispatcher
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.joinAll
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import mediathek.config.Daten
import mediathek.config.StandardLocations.getFilmIndexPath
import mediathek.daten.DatenFilm
import mediathek.daten.IndexedFilmList
import mediathek.mainwindow.MediathekGui
import mediathek.tool.FileUtils.deletePathRecursively
import mediathek.tool.LuceneDefaultAnalyzer
import mediathek.tool.SwingErrorDialog
import mediathek.tool.datum.DateUtil
import mediathek.tool.datum.DatumFilm
import mediathek.tool.time.Stopwatch
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import org.apache.lucene.document.*
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import java.io.IOException
import java.nio.file.Files
import java.time.format.DateTimeFormatter
import java.util.*
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.JLabel
import javax.swing.JProgressBar
import javax.swing.SwingUtilities
import javax.swing.SwingWorker

class LuceneIndexWorker(private val progLabel: JLabel, private val progressBar: JProgressBar) :
    SwingWorker<Void?, Void?>() {

    @Throws(IOException::class)
    private fun createIndexDocument(film: DatenFilm): Document {
        val doc = Document()
        // store fields for debugging, otherwise they should stay disabled
        doc.add(StringField(LuceneIndexKeys.ID, film.filmNr.toString(), Field.Store.YES))
        doc.add(StringField(LuceneIndexKeys.NEW, film.isNew.toString(), Field.Store.NO))
        doc.add(StringField(LuceneIndexKeys.SENDER, film.sender.lowercase(Locale.ROOT), Field.Store.NO))
        doc.add(TextField(LuceneIndexKeys.TITEL, film.title, Field.Store.NO))
        doc.add(TextField(LuceneIndexKeys.THEMA, film.thema, Field.Store.NO))
        doc.add(IntPoint(LuceneIndexKeys.FILM_LENGTH, film.filmLength))
        doc.add(IntPoint(LuceneIndexKeys.FILM_SIZE, film.fileSize.toInteger()))

        doc.add(TextField(LuceneIndexKeys.BESCHREIBUNG, film.description, Field.Store.NO))
        doc.add(StringField(LuceneIndexKeys.LIVESTREAM, film.isLivestream.toString(), Field.Store.NO))
        doc.add(StringField(LuceneIndexKeys.HIGH_QUALITY, film.isHighQuality.toString(), Field.Store.NO))
        doc.add(
            StringField(
                LuceneIndexKeys.SUBTITLE,
                (film.hasSubtitle() || film.hasBurnedInSubtitles()).toString(),
                Field.Store.NO
            )
        )
        doc.add(StringField(LuceneIndexKeys.TRAILER_TEASER, film.isTrailerTeaser.toString(), Field.Store.NO))
        doc.add(StringField(LuceneIndexKeys.AUDIOVERSION, film.isAudioVersion.toString(), Field.Store.NO))
        doc.add(StringField(LuceneIndexKeys.SIGN_LANGUAGE, film.isSignLanguage.toString(), Field.Store.NO))
        doc.add(StringField(LuceneIndexKeys.DUPLICATE, film.isDuplicate.toString(), Field.Store.NO))
        doc.add(IntPoint(LuceneIndexKeys.SEASON, film.season))
        doc.add(IntPoint(LuceneIndexKeys.EPISODE, film.episode))

        addSendeDatum(doc, film)
        addSendeZeit(doc, film)
        addWochentag(doc, film)

        return doc
    }

    private fun addSendeZeit(doc: Document, film: DatenFilm) {
        val startzeit = film.sendeZeit
        if (!startzeit.isEmpty()) {
            doc.add(StringField(LuceneIndexKeys.START_TIME, startzeit, Field.Store.NO))
        }
    }

    private fun addWochentag(doc: Document, film: DatenFilm) {
        val date = film.datumFilm
        if (date !== DatumFilm.UNDEFINED_FILM_DATE) {
            val strDate = FORMATTER.format(date.zonedDateTime)
            doc.add(TextField(LuceneIndexKeys.SENDE_WOCHENTAG, strDate, Field.Store.NO))
        }
    }

    private fun addSendeDatum(doc: Document, film: DatenFilm) {
        val sendeDatumStr = DateTools.timeToString(
            DateUtil.convertFilmDateToLuceneDate(film),
            DateTools.Resolution.DAY
        )
        doc.add(StringField(LuceneIndexKeys.SENDE_DATUM, sendeDatumStr, Field.Store.NO))
    }

    private fun createIndexWriter(liste: IndexedFilmList): IndexWriter {
        val indexWriterConfig = IndexWriterConfig(LuceneDefaultAnalyzer.buildPerFieldAnalyzer())
        indexWriterConfig.openMode = OpenMode.CREATE
        val ramBufferSizeMb = calculateRamBufferSizeMb()
        indexWriterConfig.ramBufferSizeMB = ramBufferSizeMb
        LOG.trace("Using Lucene RAM buffer size: {} MB", ramBufferSizeMb)
        return IndexWriter(liste.luceneDirectory, indexWriterConfig)
    }

    private fun calculateRamBufferSizeMb(): Double {
        val heapMb = Runtime.getRuntime().maxMemory().toDouble() / (1024.0 * 1024.0)
        val suggested = heapMb * 0.05
        return suggested.coerceIn(128.0, 2048.0)
    }

    private fun updateProgress(processedCount: Int, totalCount: Int, oldProgress: AtomicInteger) {
        val progress = if (totalCount == 0) 100 else (processedCount * 100) / totalCount
        var previous = oldProgress.get()
        while (progress > previous) {
            if (oldProgress.compareAndSet(previous, progress)) {
                SwingUtilities.invokeLater { progressBar.value = progress }
                break
            }
            previous = oldProgress.get()
        }
    }

    private fun flushBatch(
        writer: IndexWriter,
        batch: MutableList<Document>,
        counter: AtomicInteger,
        totalCount: Int,
        oldProgress: AtomicInteger
    ) {
        if (batch.isEmpty()) {
            return
        }

        writer.addDocuments(batch)
        val processedCount = counter.addAndGet(batch.size)
        updateProgress(processedCount, totalCount, oldProgress)
        batch.clear()
    }

    override fun doInBackground(): Void? {
        try {
            SwingUtilities.invokeLater {
                val ui = MediathekGui.ui()
                ui.toggleBlacklistAction.isEnabled = false
                ui.editBlacklistAction.isEnabled = false
                ui.loadFilmListAction.isEnabled = false

                progLabel.text = "Indiziere Filme"
                progressBar.isIndeterminate = false
                progressBar.minimum = 0
                progressBar.maximum = 100
                progressBar.value = 0
            }

            //index filmlist after blacklist only
            val filmListe = Daten.getInstance().listeFilmeNachBlackList as IndexedFilmList
            createIndexWriter(filmListe).use { writer ->
                val watch = Stopwatch.createStarted()
                val counter = AtomicInteger(0)
                val totalCount = filmListe.size
                val oldProgress = AtomicInteger(0)

                val indexingThreads = (Runtime.getRuntime().availableProcessors() - 1).coerceAtLeast(1)
                val queueCapacity = (indexingThreads * 256).coerceAtLeast(512)
                val writeBatchSize = 256
                val indexingDispatcher = Executors.newFixedThreadPool(indexingThreads).asCoroutineDispatcher()
                indexingDispatcher.use { indexingDispatcher ->
                    runBlocking {
                        val filmChannel = Channel<DatenFilm>(queueCapacity)

                        val producer = launch(indexingDispatcher) {
                            try {
                                for (film in filmListe) {
                                    filmChannel.send(film)
                                }
                            } finally {
                                filmChannel.close()
                            }
                        }

                        val consumers = List(indexingThreads) {
                            launch(indexingDispatcher) {
                                val batch = ArrayList<Document>(writeBatchSize)
                                for (film in filmChannel) {
                                    try {
                                        batch.add(createIndexDocument(film))
                                        if (batch.size >= writeBatchSize) {
                                            flushBatch(writer, batch, counter, totalCount, oldProgress)
                                        }
                                    } catch (ex: IOException) {
                                        LOG.error("Lucene indexing failed for a film entry", ex)
                                    }
                                }

                                try {
                                    flushBatch(writer, batch, counter, totalCount, oldProgress)
                                } catch (ex: IOException) {
                                    LOG.error("Lucene indexing failed while flushing a document batch", ex)
                                }
                            }
                        }

                        producer.join()
                        consumers.joinAll()
                    }
                }
                SwingUtilities.invokeLater { progressBar.value = 100 }
                SwingUtilities.invokeLater {
                    progLabel.text = "Schreibe Index"
                    progressBar.isIndeterminate = true
                }
                writer.commit()
                watch.stop()
                LOG.trace("Lucene index creation took {}", watch)

                filmListe.reader?.close()
                filmListe.reader = DirectoryReader.open(filmListe.luceneDirectory)
            }
        } catch (ex: Exception) {
            LOG.error("Lucene film index most probably damaged, deleting it.")
            try {
                val indexPath = getFilmIndexPath()
                if (Files.exists(indexPath)) {
                    deletePathRecursively(indexPath)
                }
            } catch (e: IOException) {
                LOG.error("Unable to delete lucene index path", e)
            }
            SwingUtilities.invokeLater {
                SwingErrorDialog.showExceptionMessage(
                    MediathekGui.ui(),
                    "Der Filmindex ist beschädigt und wurde gelöscht.\nDas Programm wird beendet, bitte starten Sie es erneut.",
                    ex
                )
                MediathekGui.ui().quitApplication()
            }
        }

        return null
    }

    override fun done() {
        val ui = MediathekGui.ui()
        ui.toggleBlacklistAction.setEnabled(true)
        ui.editBlacklistAction.setEnabled(true)
        ui.loadFilmListAction.setEnabled(true)
    }

    companion object {
        private val LOG: Logger = LogManager.getLogger()
        private val FORMATTER = DateTimeFormatter.ofPattern("EEEE", Locale.GERMAN)
    }
}
