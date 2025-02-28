package mediathek.gui.duplicates;

import ca.odell.glazedlists.TransactionList;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;

import java.util.Map;
import java.util.stream.Collectors;

public class CommonStatsEvaluationTask implements Runnable {
    @Override
    public void run() {
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
    }
}
