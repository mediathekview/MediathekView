package mediathek.gui.tasks;

import com.google.common.base.Stopwatch;
import mediathek.config.Daten;
import mediathek.daten.IndexedFilmList;
import mediathek.tool.datum.DatumFilm;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.document.*;
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
                doc.add(new StringField("id", Integer.toString(film.getFilmNr()), Field.Store.YES));
                doc.add(new TextField("sender", film.getSender(), Field.Store.NO));
                doc.add(new TextField("titel", film.getTitle(), Field.Store.NO));
                doc.add(new TextField("thema", film.getThema(), Field.Store.NO));
                doc.add(new TextField("beschreibung", film.getDescription(), Field.Store.NO));
                try {
                    var filmDate = film.getDatumFilm();
                    if (!filmDate.equals(DatumFilm.UNDEFINED_FILM_DATE)) {
                        var sendeDatumStr = DateTools.timeToString(filmDate.toInstant().toEpochMilli(), DateTools.Resolution.MINUTE);
                        doc.add(new StringField("sendedatum", sendeDatumStr, Field.Store.NO));
                    }
                } catch (Exception ignored) {
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
            filmListe.setValidIndex(true);
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(98);
        }

        return null;
    }

}
