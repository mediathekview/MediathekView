package mediathek.gui.tasks;

import com.google.common.base.Stopwatch;
import mediathek.config.Daten;
import mediathek.daten.IndexedFilmList;
import mediathek.tool.datum.DateUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.document.*;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.search.IndexSearcher;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class LuceneIndexWorker extends SwingWorker<Void, Void> {
    private static final Logger logger = LogManager.getLogger();
    private final JProgressBar progressBar;
    private final JLabel progLabel;

    public LuceneIndexWorker(@NotNull JLabel progLabel, @NotNull JProgressBar progressBar) {
        this.progressBar = progressBar;
        this.progLabel = progLabel;

        SwingUtilities.invokeLater(() -> {
            progLabel.setText("Blacklist anwenden");
            progressBar.setIndeterminate(true);
        });
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
        try {
            int counter = 0;
            Stopwatch watch = Stopwatch.createStarted();
            //for safety delete all entries
            filmListe.getWriter().deleteAll();
            var totalSize = (float) filmListe.size();
            int oldProgress = 0;
            for (var film : filmListe) {
                counter++;

                var doc = new Document();
                // store fields for debugging, otherwise they should stay disabled
                doc.add(new StringField(LuceneIndexKeys.ID, Integer.toString(film.getFilmNr()), Field.Store.YES));
                doc.add(new TextField(LuceneIndexKeys.SENDER, film.getSender(), Field.Store.NO));
                doc.add(new TextField(LuceneIndexKeys.TITEL, film.getTitle(), Field.Store.NO));
                doc.add(new TextField(LuceneIndexKeys.THEMA, film.getThema(), Field.Store.NO));
                doc.add(new TextField(LuceneIndexKeys.BESCHREIBUNG, film.getDescription(), Field.Store.NO));
                if (film.isLivestream())
                    doc.add(new StringField(LuceneIndexKeys.LIVESTREAM, Boolean.toString(true), Field.Store.NO));
                if (film.isHighQuality())
                    doc.add(new StringField(LuceneIndexKeys.HIGH_QUALITY, Boolean.toString(true), Field.Store.NO));
                if (film.hasSubtitle() || film.hasBurnedInSubtitles()) {
                    doc.add(new StringField(LuceneIndexKeys.SUBTITLE, Boolean.toString(true), Field.Store.NO));
                }

                doc.add(new StringField(LuceneIndexKeys.TRAILER_TEASER, Boolean.toString(film.isTrailerTeaser()), Field.Store.NO));
                doc.add(new StringField(LuceneIndexKeys.AUDIOVERSION, Boolean.toString(film.isAudioVersion()), Field.Store.NO));
                doc.add(new StringField(LuceneIndexKeys.SIGN_LANGUAGE, Boolean.toString(film.isSignLanguage()), Field.Store.NO));
                try {
                    String sendeDatumStr = DateTools.timeToString(DateUtil.convertFilmDateToLuceneDate(film),
                            DateTools.Resolution.DAY);
                    doc.add(new StringField(LuceneIndexKeys.SENDE_DATUM, sendeDatumStr, Field.Store.NO));
                } catch (Exception ex) {
                    logger.error("Error indexing sendedatum", ex);
                }
                filmListe.getWriter().addDocument(doc);
                final var progress = (int) (100.0f * (counter / totalSize));
                if (progress != oldProgress) {
                    oldProgress = progress;
                    SwingUtilities.invokeLater(() -> progressBar.setValue(progress));
                }
            }
            filmListe.getWriter().commit();
            watch.stop();
            logger.trace("Lucene index creation took {}", watch);

            var reader = filmListe.getReader();
            if (reader != null) {
                reader.close();
            }

            reader = DirectoryReader.open(filmListe.getLuceneDirectory());
            filmListe.setReader(reader);

            filmListe.setIndexSearcher(new IndexSearcher(reader));

            filmListe.setValidIndex(true);
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(98);
        }

        return null;
    }

}
