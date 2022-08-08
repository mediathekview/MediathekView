package mediathek.gui.tasks;

import com.google.common.base.Stopwatch;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.daten.IndexedFilmList;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.SwingErrorDialog;
import mediathek.tool.datum.DateUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.document.*;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.search.IndexSearcher;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.io.IOException;

public class LuceneIndexWorker extends SwingWorker<Void, Void> {
    private static final Logger logger = LogManager.getLogger();
    private final JProgressBar progressBar;
    private final JLabel progLabel;
    private int oldProgress = 0;

    public LuceneIndexWorker(@NotNull JLabel progLabel, @NotNull JProgressBar progressBar) {
        this.progressBar = progressBar;
        this.progLabel = progLabel;

        SwingUtilities.invokeLater(() -> {
            progLabel.setText("Blacklist anwenden");
            progressBar.setIndeterminate(true);
        });
    }

    private void indexFilm(@NotNull IndexWriter writer, @NotNull DatenFilm film) throws IOException {
        var doc = new Document();
        // store fields for debugging, otherwise they should stay disabled
        doc.add(new StringField(LuceneIndexKeys.ID, Integer.toString(film.getFilmNr()), Field.Store.YES));
        doc.add(new StringField(LuceneIndexKeys.NEW, Boolean.toString(film.isNew()), Field.Store.NO));
        doc.add(new TextField(LuceneIndexKeys.SENDER, film.getSender(), Field.Store.NO));
        doc.add(new TextField(LuceneIndexKeys.TITEL, film.getTitle(), Field.Store.NO));
        doc.add(new TextField(LuceneIndexKeys.THEMA, film.getThema(), Field.Store.NO));
        doc.add(new IntPoint(LuceneIndexKeys.FILM_LENGTH, film.getFilmLength()));
        doc.add(new IntPoint(LuceneIndexKeys.FILM_SIZE, film.getFileSize().toInteger()));

        doc.add(new TextField(LuceneIndexKeys.BESCHREIBUNG, film.getDescription(), Field.Store.NO));
        doc.add(new StringField(LuceneIndexKeys.LIVESTREAM, Boolean.toString(film.isLivestream()), Field.Store.NO));
        doc.add(new StringField(LuceneIndexKeys.HIGH_QUALITY, Boolean.toString(film.isHighQuality()), Field.Store.NO));
        doc.add(new StringField(LuceneIndexKeys.SUBTITLE, Boolean.toString(film.hasSubtitle() || film.hasBurnedInSubtitles()), Field.Store.NO));
        doc.add(new StringField(LuceneIndexKeys.TRAILER_TEASER, Boolean.toString(film.isTrailerTeaser()), Field.Store.NO));
        doc.add(new StringField(LuceneIndexKeys.AUDIOVERSION, Boolean.toString(film.isAudioVersion()), Field.Store.NO));
        doc.add(new StringField(LuceneIndexKeys.SIGN_LANGUAGE, Boolean.toString(film.isSignLanguage()), Field.Store.NO));

        addSendeDatum(doc, film);

        writer.addDocument(doc);
    }

    private void addSendeDatum(@NotNull Document doc, @NotNull DatenFilm film) {
        String sendeDatumStr = DateTools.timeToString(DateUtil.convertFilmDateToLuceneDate(film),
                DateTools.Resolution.DAY);
        doc.add(new StringField(LuceneIndexKeys.SENDE_DATUM, sendeDatumStr, Field.Store.NO));
    }

    @Override
    protected Void doInBackground() {
        var filmListe = (IndexedFilmList) Daten.getInstance().getListeFilmeNachBlackList();
        SwingUtilities.invokeLater(() -> {
            progLabel.setText("Indiziere Filme");
            progressBar.setMinimum(0);
            progressBar.setMaximum(100);
            progressBar.setValue(0);
            progressBar.setIndeterminate(false);
        });

        //index filmlist after blacklist only
        var writer = filmListe.getWriter();
        var totalSize = (float) filmListe.size();

        try {
            int counter = 0;
            Stopwatch watch = Stopwatch.createStarted();
            //for safety delete all entries
            writer.deleteAll();

            for (var film : filmListe) {
                counter++;
                indexFilm(writer, film);

                final var progress = (int) (100.0f * (counter / totalSize));
                if (progress != oldProgress) {
                    oldProgress = progress;
                    SwingUtilities.invokeLater(() -> progressBar.setValue(progress));
                }
            }
            writer.commit();
            watch.stop();
            logger.trace("Lucene index creation took {}", watch);

            var reader = filmListe.getReader();
            if (reader != null) {
                reader.close();
            }
            reader = DirectoryReader.open(filmListe.getLuceneDirectory());
            filmListe.setReader(reader);

            filmListe.setIndexSearcher(new IndexSearcher(reader));
        } catch (Exception ex) {
            SwingUtilities.invokeLater(() -> {
                SwingErrorDialog.showExceptionMessage(MediathekGui.ui(),
                        "Fehler bei der Erstellung des Filmindex.\nDas Programm wird beendet da es nicht lauffähig ist.", ex);
                MediathekGui.ui().quitApplication();
            });
        }

        return null;
    }

}
