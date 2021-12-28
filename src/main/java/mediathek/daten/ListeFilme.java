package mediathek.daten;

import mediathek.config.Konstanten;
import mediathek.tool.GermanStringSorter;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ListeFilme extends ArrayList<DatenFilm> {
    public static final String FILMLISTE = "Filmliste";
    private final FilmListMetaData metaData = new FilmListMetaData();
    public boolean neueFilme;

    public FilmListMetaData metaData() {
        return metaData;
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

    public synchronized void updateFromFilmList(@NotNull ListeFilme newFilmsList) {
        // In die vorhandene Liste soll eine andere Filmliste einsortiert werden
        // es werden nur Filme die noch nicht vorhanden sind, einsortiert
        final HashSet<String> hashNewFilms = new HashSet<>(newFilmsList.size() + 1, 1);

        newFilmsList.forEach(newFilm -> hashNewFilms.add(newFilm.getUniqueHash()));
        this.removeIf(currentFilm -> hashNewFilms.contains(currentFilm.getUniqueHash()));

        hashNewFilms.clear();

        newFilmsList.forEach(film -> {
            film.init();
            add(film);
        });
    }

    @Override
    public synchronized void clear() {
        super.clear();
        neueFilme = false;
    }

    public synchronized void setMetaData(FilmListMetaData meta) {
        metaData.setDatum(meta.getDatum());
        metaData.setId(meta.getId());
    }

    /**
     * Find movie with given url and sendername
     * @param url    String wiht URL
     * @param sender String with sender name
     * @return DatenFilm object if found or null
     */
    public synchronized DatenFilm getFilmByUrlAndSender(final String url, final String sender) {
      return parallelStream().filter(f -> f.getUrlNormalQuality().equalsIgnoreCase(url) && f.getSender().equalsIgnoreCase(sender)).findAny().orElse(null);
    }

    public synchronized DatenFilm getFilmByUrl_klein_hoch_hd(String url) {
        // Problem wegen gleicher URLs
        // wird versucht, einen Film mit einer kleinen/Hoher/HD-URL zu finden
        DatenFilm ret = null;
        for (DatenFilm f : this) {
            if (f.getUrlNormalQuality().equals(url)) {
                ret = f;
                break;
            } else if (f.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY).equals(url)) {
                ret = f;
                break;
            } else if (f.getUrlFuerAufloesung(FilmResolution.Enum.LOW).equals(url)) {
                ret = f;
                break;
            }
        }

        return ret;
    }

    /**
     * List needs update when it is either empty or too old.
     * @return true if we need an update.
     */
    public boolean needsUpdate() {
        return (isEmpty()) || (metaData().isOlderThan(Konstanten.ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE));
    }

    public synchronized long countNewFilms() {
        return stream().filter(DatenFilm::isNew).count();
    }
}
