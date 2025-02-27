package mediathek.gui.duplicates;

import ca.odell.glazedlists.TransactionList;
import com.google.common.base.Stopwatch;
import com.google.common.collect.Sets;
import com.google.common.hash.Hashing;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.nio.charset.StandardCharsets;
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

        var statisticsEventList = Daten.getInstance().getDuplicateStatistics();

        Map<String, Long> statisticsMap = duplicates.parallelStream().collect(Collectors.groupingBy(DatenFilm::getSender, Collectors.counting()));
        TransactionList<FilmStatistics> tList = new TransactionList<>(statisticsEventList);
        tList.getReadWriteLock().writeLock().lock();
        tList.beginEvent(true);
        tList.clear();
        for (var sender : statisticsMap.keySet()) {
            tList.add(new FilmStatistics(sender, statisticsMap.get(sender)));
        }
        tList.commitEvent();
        tList.getReadWriteLock().writeLock().unlock();
        watch.stop();

        logger.trace("Duplicate stream filter took: {}", watch);
        logger.trace("Number of duplicates: {}", duplicates.size());
    }

    private void checkDuplicates() {
        logger.trace("Start Duplicate URL search");
        final Set<Long> urlCache = Sets.newConcurrentHashSet();

        var hf = Hashing.murmur3_128();
        Stopwatch watch = Stopwatch.createStarted();
        listeFilme.stream()
                .filter(f -> !f.isLivestream())
                .sorted(new BigSenderPenaltyComparator())
                .forEach(film -> {
                    var hasher = hf.newHasher()
                            .putString(film.getUrlNormalQuality(), StandardCharsets.UTF_8);
                    if (film.isHighQuality())
                        hasher = hasher.putString(film.getHighQualityUrl(), StandardCharsets.UTF_8);
                    if (film.hasLowQuality())
                        hasher = hasher.putString(film.getLowQualityUrl(), StandardCharsets.UTF_8);
                    final var hc = hasher.hash();
                    final var hash = hc.padToLong();

                    film.setDuplicate(urlCache.contains(hash));
                    urlCache.add(hash);
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
