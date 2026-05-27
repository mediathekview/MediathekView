package mediathek.gui.duplicates

import mediathek.config.Daten

class CommonStatsEvaluationTask : Runnable {
    override fun run() {
        val statisticsMap = Daten.getInstance().listeFilme.parallelStream()
            .filter { film -> !film.isLivestream }
            .countFilmsBySender()

        replaceFilmStatistics(Daten.getInstance().commonStatistics, statisticsMap)
    }
}
