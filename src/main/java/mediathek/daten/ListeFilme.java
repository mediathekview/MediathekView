/*
 * Copyright (c) 2026 derreisende77.
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

package mediathek.daten;

import mediathek.config.Konstanten;
import mediathek.tool.GermanStringSorter;
import org.jspecify.annotations.NonNull;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class ListeFilme extends ArrayList<DatenFilm> {
    private static final String PCS_METADATA = "metaData";
    protected final PropertyChangeSupport pcs = new PropertyChangeSupport(this);
    private FilmListMetaData metaData = new FilmListMetaData();

    public FilmListMetaData getMetaData() {
        return metaData;
    }

    public void setMetaData(FilmListMetaData meta) {
        var oldValue = metaData;
        metaData = meta;
        this.pcs.firePropertyChange(PCS_METADATA, oldValue, metaData);
    }

    public void addMetaDataChangeListener(PropertyChangeListener listener) {
        this.pcs.addPropertyChangeListener(PCS_METADATA, listener);
    }

    /**
     * case-insensitive .distinct() implementation.
     * @param keyExtractor the function to be applied to the key
     * @param <T> template param
     */
    private static <T> Predicate<T> distinctByKey(Function<? super T, ?> keyExtractor) {
        Set<Object> seen = ConcurrentHashMap.newKeySet();
        return t -> seen.add(keyExtractor.apply(t));
    }

    private static String normalizeKey(String value) {
        return value == null ? "" : value.toLowerCase(Locale.ROOT);
    }

    /**
     * Search all themas within list based on sender.
     * If sender is empty, return full list of themas.
     *
     * @param sender sender name as String
     * @return IMMUTABLE List of themas as String.
     */
    public List<String> getThemen(String sender) {
        List<DatenFilm> snapshot;
        synchronized (this) {
            snapshot = List.copyOf(this);
        }

        Stream<DatenFilm> mystream = snapshot.parallelStream();
        //if sender is empty return all themas...
        if (!sender.isEmpty())
            mystream = mystream.filter(f -> f.getSender().equals(sender));

        return mystream.map(DatenFilm::getThema)
                .filter(distinctByKey(ListeFilme::normalizeKey))
                .sorted(GermanStringSorter.INSTANCE).toList();
    }

    /**
     * Search all distinct themas within list based on senders.
     * If senders is empty, return the full distinct thema list.
     *
     * @param senders sender names
     * @return immutable sorted list of distinct themas
     */
    public List<String> getThemen(@NonNull Collection<String> senders) {
        List<DatenFilm> snapshot;
        synchronized (this) {
            snapshot = List.copyOf(this);
        }

        var normalizedSenders = new HashSet<String>();
        senders.forEach(sender -> normalizedSenders.add(normalizeKey(sender)));

        var seenThemen = new HashSet<String>();
        var result = new ArrayList<String>();
        boolean includeAllSenders = normalizedSenders.isEmpty();

        for (DatenFilm film : snapshot) {
            if (!includeAllSenders && !normalizedSenders.contains(normalizeKey(film.getSender()))) {
                continue;
            }

            var thema = film.getThema();
            if (seenThemen.add(normalizeKey(thema))) {
                result.add(thema);
            }
        }

        result.sort(GermanStringSorter.INSTANCE);
        return List.copyOf(result);
    }

    public synchronized void updateFromFilmList(@NonNull ListeFilme newFilmsList) {
        // In die vorhandene Liste soll eine andere Filmliste einsortiert werden
        // es werden nur Filme, die noch nicht vorhanden sind, einsortiert
        var hashNewFilms = new HashSet<DatenFilm.FilmIdentity>(newFilmsList.size() + 1, 1);
        newFilmsList.forEach(newFilm -> hashNewFilms.add(newFilm.getFilmIdentity()));

        this.removeIf(currentFilm -> hashNewFilms.contains(currentFilm.getFilmIdentity()));

        hashNewFilms.clear();

        newFilmsList.forEach(film -> {
            film.init();
            add(film);
        });
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
        return (isEmpty()) || (getMetaData().isOlderThan(Konstanten.ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE));
    }

    public synchronized long countNewFilms() {
        return stream().filter(DatenFilm::isNew).count();
    }
}
