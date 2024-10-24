package mediathek.gui.duplicates;

import ca.odell.glazedlists.TransactionList;
import com.google.common.base.Stopwatch;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Map;
import java.util.stream.Collectors;

public class CommonStatsEvaluationTask implements Runnable {
    private static final Logger logger = LogManager.getLogger();

    @Override
    public void run() {
        Stopwatch watch = Stopwatch.createStarted();
        final var films = Daten.getInstance().getListeFilme().parallelStream()
                .filter(f -> !f.isLivestream())
                .toList();

        var statisticsList = Daten.getInstance().getCommonStatistics();

        Map<String, Long> statisticsMap = films.parallelStream()
                .collect(Collectors.groupingBy(DatenFilm::getSender, Collectors.counting()));
        TransactionList<FilmStatistics> tList = new TransactionList<>(statisticsList);
        statisticsList.getReadWriteLock().writeLock().lock();
        tList.beginEvent(true);
        tList.clear();
        for (var sender : statisticsMap.keySet()) {
            tList.add(new FilmStatistics(sender, statisticsMap.get(sender)));
        }
        tList.commitEvent();
        statisticsList.getReadWriteLock().writeLock().unlock();
        watch.stop();

        logger.trace("common stats calculation took: {}", watch);
        logger.trace("Number of films: {}", films.size());
    }
}
