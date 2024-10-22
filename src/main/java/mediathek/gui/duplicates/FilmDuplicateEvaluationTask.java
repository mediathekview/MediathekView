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
import java.util.Comparator;
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
        tList.clear();
        for (var sender : statisticsMap.keySet()) {
            tList.add(new FilmStatistics(sender, statisticsMap.get(sender)));
        }
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
                    final var hc = hf.newHasher()
                            .putString(film.getUrlNormalQuality(), StandardCharsets.UTF_8)
                            .putString(film.getHighQualityUrl(), StandardCharsets.UTF_8)
                            .hash();
                    final var hash = hc.padToLong();

                    film.setDuplicate(urlCache.contains(hash));
                    urlCache.add(hash);
                });
        watch.stop();
        logger.trace("Duplicate URL search took: {}", watch);
        urlCache.clear();
    }

    private void calculateCommonStats() {
        Stopwatch watch = Stopwatch.createStarted();
        final var films = listeFilme.parallelStream()
                .filter(f -> !f.isLivestream())
                .toList();

        var statisticsList = Daten.getInstance().getCommonStatistics();

        Map<String, Long> statisticsMap = films.parallelStream()
                .collect(Collectors.groupingBy(DatenFilm::getSender, Collectors.counting()));
        TransactionList<FilmStatistics> tList = new TransactionList<>(statisticsList);
        tList.getReadWriteLock().writeLock().lock();
        tList.clear();
        for (var sender : statisticsMap.keySet()) {
            tList.add(new FilmStatistics(sender, statisticsMap.get(sender)));
        }
        tList.getReadWriteLock().writeLock().unlock();
        watch.stop();

        logger.trace("common stats calculation took: {}", watch);
        logger.trace("Number of films: {}", films.size());
    }

    @Override
    public void run() {
        calculateCommonStats();
        checkDuplicates();
        printDuplicateStatistics();
    }

    private static class BigSenderPenaltyComparator implements Comparator<DatenFilm> {
        @Override
        public int compare(DatenFilm s1, DatenFilm s2) {
            // "ARD" und "ZDF" immer am Ende um die kleineren Mediatheken nicht zu benachteiligen
            final var s1_sender = s1.getSender();
            final var s2_sender = s2.getSender();
            if (s1_sender.equals("ARD") || s1_sender.equals("ZDF")) {
                return 1;
            }
            if (s2_sender.equals("ARD") || s2_sender.equals("ZDF")) {
                return -1;
            }
            // Alphabetisch sortieren für alle anderen
            return s1.compareTo(s2);
        }
    }
}
