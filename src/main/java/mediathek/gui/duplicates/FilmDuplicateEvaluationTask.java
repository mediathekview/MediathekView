package mediathek.gui.duplicates;

import ca.odell.glazedlists.TransactionList;
import com.google.common.base.Stopwatch;
import com.google.common.collect.Sets;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class FilmDuplicateEvaluationTask implements Runnable {
    private static final Logger logger = LogManager.getLogger();
    private final ListeFilme listeFilme;

    public FilmDuplicateEvaluationTask() {
        this.listeFilme = Daten.getInstance().getListeFilme();
    }

    private void printDuplicateStatistics() {
        Stopwatch watch = Stopwatch.createStarted();
        final var duplicates = listeFilme.parallelStream()
                .filter(DatenFilm::isDuplicate)
                .toList();

        var statisticsEventList = Daten.getInstance().getFilmListDuplicateStatisticsList();

        Map<String, Long> statisticsMap = duplicates.parallelStream().collect(Collectors.groupingBy(DatenFilm::getSender, Collectors.counting()));
        TransactionList<DuplicateStatistics> tList = new TransactionList<>(statisticsEventList);
        tList.getReadWriteLock().writeLock().lock();
        tList.clear();
        for (var sender : statisticsMap.keySet()) {
            tList.add(new DuplicateStatistics(sender, statisticsMap.get(sender)));
        }
        tList.getReadWriteLock().writeLock().unlock();
        watch.stop();

        /*long dupes = 0;
        statisticsEventList.getReadWriteLock().readLock().lock();
        for (var item : statisticsEventList) {
            dupes += item.count();
        }
        statisticsEventList.getReadWriteLock().readLock().unlock();*/
        logger.trace("Duplicate stream filter took: {}", watch);
        logger.trace("Number of duplicates: {}", duplicates.size());
        //logger.trace("Calculated dupes: {}", dupes);
    }

    private void checkDuplicates() {
        logger.trace("Start Duplicate URL search");
        final Set<String> urlCache = Sets.newConcurrentHashSet();

        Stopwatch watch = Stopwatch.createStarted();
        listeFilme.stream()
                .filter(f -> !f.isLivestream())
                .forEach(film -> {
            final var url = film.getUrlNormalQuality();
            final var duplicate = urlCache.contains(url);
            film.setDuplicate(duplicate);
            urlCache.add(url);
        });
        watch.stop();
        logger.trace("Duplicate URL search took: {}", watch);
        urlCache.clear();
    }

    @Override
    public void run() {
        checkDuplicates();
        printDuplicateStatistics();
    }
}
