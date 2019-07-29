package mediathek.daten;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.javafx.EventObservableList;
import javafx.application.Platform;
import javafx.collections.ObservableList;
import mediathek.config.Konstanten;
import mediathek.tool.GermanStringSorter;
import org.apache.commons.lang3.time.FastDateFormat;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@SuppressWarnings("serial")
public class ListeFilme extends ArrayList<DatenFilm> {
    public static final String FILMLISTE = "Filmliste";
    private final static String DATUM_ZEIT_FORMAT = "dd.MM.yyyy, HH:mm";
    private static final FastDateFormat sdf_ = FastDateFormat.getInstance(DATUM_ZEIT_FORMAT,new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"));
    private static final FastDateFormat formatter = FastDateFormat.getInstance(DATUM_ZEIT_FORMAT);
    private static final Logger logger = LogManager.getLogger(ListeFilme.class);
    private final FilmListMetaData metaData = new FilmListMetaData();
    /**
     * List of available senders which notifies its users.
     */
    private final EventList<String> m_senderList = new BasicEventList<>();
    /**
     * javafx proxy class to the sender list.
     */
    private final ObservableList<String> obs_senderList = new EventObservableList<>(m_senderList);
    public boolean neueFilme = false;

    /**
     * Get the basic sender channel list, useful e.g. for swing models
     *
     * @return the plain list of channels
     */
    public EventList<String> getBaseSenderList() {
        return m_senderList;
    }

    public FilmListMetaData metaData() {
        return metaData;
    }

    public ObservableList<String> getSenders() {
        return obs_senderList;
    }

    public synchronized void importFilmliste(DatenFilm film) {
        addInit(film);
    }

    /**
     * Search all themas within list based on sender.
     * If sender is empty, return full list of themas.
     *
     * @param sender sender name as String
     * @return List of themas as String.
     */
    public List<String> getThemen(String sender) {
        Stream<DatenFilm> mystream = parallelStream();
        //if sender is empty return all themas...
        if (!sender.isEmpty())
            mystream = mystream.filter(f -> f.getSender().equals(sender));

        return mystream.map(DatenFilm::getThema)
                .distinct()
                .sorted(GermanStringSorter.getInstance())
                .collect(Collectors.toList());
    }

    private void addHash(DatenFilm f, HashSet<String> hash, boolean index) {
        if (index) {
            hash.add(f.getIndex());
        } else {
            hash.add(f.getUrl());
        }
    }

    public synchronized void updateListe(ListeFilme listeEinsortieren, boolean index /* Vergleich über Index, sonst nur URL */, boolean ersetzen) {
        // in eine vorhandene Liste soll eine andere Filmliste einsortiert werden
        // es werden nur Filme die noch nicht vorhanden sind, einsortiert
        // "ersetzen": true: dann werden gleiche (index/URL) in der Liste durch neue ersetzt
        final HashSet<String> hash = new HashSet<>(listeEinsortieren.size() + 1, 1);

        if (ersetzen) {
            listeEinsortieren.forEach((DatenFilm f) -> addHash(f, hash, index));

            Iterator<DatenFilm> it = this.iterator();
            while (it.hasNext()) {
                DatenFilm f = it.next();
                if (index) {
                    if (hash.contains(f.getIndex())) {
                        it.remove();
                    }
                } else if (hash.contains(f.getUrl())) {
                    it.remove();
                }
            }

            listeEinsortieren.forEach(this::addInit);
        } else {
            // ==============================================
            this.forEach(f -> addHash(f, hash, index));

            for (DatenFilm f : listeEinsortieren) {
                if (index) {
                    if (!hash.contains(f.getIndex())) {
                        addInit(f);
                    }
                } else if (!hash.contains(f.getUrl())) {
                    addInit(f);
                }
            }
        }
        hash.clear();
    }

    private void addInit(DatenFilm film) {
        film.init();
        add(film);
    }

    @Override
    public boolean add(DatenFilm aFilm) {
        return super.add(aFilm);
    }

    @Override
    public synchronized void clear() {
        neueFilme = false;

        super.clear();
    }

    public synchronized void setMetaData(FilmListMetaData meta) {
        metaData.setDatum(meta.getDatum());
        metaData.setId(meta.getId());
    }

    public synchronized DatenFilm getFilmByUrl(final String url) {
        return parallelStream().filter(f -> f.getUrl().equalsIgnoreCase(url)).findAny().orElse(null);
    }

    public synchronized DatenFilm getFilmByUrl_klein_hoch_hd(String url) {
        // Problem wegen gleicher URLs
        // wird versucht, einen Film mit einer kleinen/Hoher/HD-URL zu finden
        DatenFilm ret = null;
        for (DatenFilm f : this) {
            if (f.getUrl().equals(url)) {
                ret = f;
                break;
            } else if (f.getUrlFuerAufloesung(FilmResolution.AUFLOESUNG_HD).equals(url)) {
                ret = f;
                break;
            } else if (f.getUrlFuerAufloesung(FilmResolution.AUFLOESUNG_KLEIN).equals(url)) {
                ret = f;
                break;
            }
        }

        return ret;
    }

    public synchronized String genDate() {
        // Tag, Zeit in lokaler Zeit wann die Filmliste erstellt wurde
        // in der Form "dd.MM.yyyy, HH:mm"
        final String date = metaData.getDatum();

        String ret;
        try {
            final Date filmDate = sdf_.parse(date);
            ret = formatter.format(filmDate);
        } catch (ParseException ignored) {
            ret = date;
        }

        return ret;
    }

    public synchronized String getId() {
        // liefert die ID einer Filmliste
        return metaData.getId();
    }

    /**
     * Get the age of the film list.
     *
     * @return Age in seconds.
     */
    public long getAge() {
        long ret = 0;

        Date filmDate = getAgeAsDate();
        if (filmDate != null) {
            ret = (System.currentTimeMillis() - filmDate.getTime()) / 1000;
            if (ret < 0) {
                ret = 0;
            }
        }
        return ret;
    }

    /**
     * Get the age of the film list.
     *
     * @return Age as a {@link java.util.Date} object.
     */
    private Date getAgeAsDate() {
        String date = metaData.getDatum();

        Date filmDate = null;
        try {
            filmDate = sdf_.parse(date);
        } catch (ParseException ignored) {
        }

        return filmDate;
    }

    /**
     * Check if available Filmlist is older than a specified value.
     *
     * @return true if too old or if the list is empty.
     */
    public synchronized boolean isTooOld() {
        return (isEmpty()) || (isOlderThan(Konstanten.ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE));
    }

    /**
     * Check if Filmlist is too old for using a diff list.
     *
     * @return true if empty or too old.
     */
    public synchronized boolean isTooOldForDiff() {
        if (isEmpty()) {
            return true;
        }
        try {
            final String dateMaxDiff_str = new SimpleDateFormat("yyyy.MM.dd__").format(new Date()) + Konstanten.TIME_MAX_AGE_FOR_DIFF + ":00:00";
            final Date dateMaxDiff = new SimpleDateFormat("yyyy.MM.dd__HH:mm:ss").parse(dateMaxDiff_str);
            final Date dateFilmliste = getAgeAsDate();
            if (dateFilmliste != null) {
                return dateFilmliste.getTime() < dateMaxDiff.getTime();
            }
        } catch (Exception ignored) {
        }
        return true;
    }

    /**
     * Check if list is older than specified parameter.
     *
     * @param sekunden The age in seconds.
     * @return true if older.
     */
    public boolean isOlderThan(long sekunden) {
        final long ret = getAge();
        if (ret != 0) {
            logger.info("Die Filmliste ist {} Minuten alt", ret / 60);
        }
        return ret > sekunden;
    }

    public synchronized long countNewFilms() {
        return stream().filter(DatenFilm::isNew).count();
    }

    public void fillSenderList() {
        var writeLock = m_senderList.getReadWriteLock().writeLock();

        try {
            writeLock.lock();
            var list = stream().map(DatenFilm::getSender).distinct().collect(Collectors.toList());
            Platform.runLater(() -> {
                m_senderList.clear();
                m_senderList.addAll(list);
            });
        }
        catch (Exception e) {
            logger.error("error fillSenderList", e);
        }
        finally {
            writeLock.unlock();
        }
    }
}
