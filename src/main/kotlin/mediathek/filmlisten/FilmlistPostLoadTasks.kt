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

package mediathek.filmlisten

import mediathek.config.Daten
import mediathek.daten.IndexedFilmList
import mediathek.gui.duplicates.CommonStatsEvaluationTask
import mediathek.gui.duplicates.FilmDuplicateEvaluationTask
import mediathek.gui.tasks.BlacklistFilterWorker
import mediathek.gui.tasks.FilmlistWriterWorker
import mediathek.gui.tasks.LuceneIndexWorker
import mediathek.gui.tasks.RefreshAboWorker
import mediathek.tool.ApplicationConfiguration
import javax.swing.JLabel
import javax.swing.JProgressBar

class FilmlistPostLoadTasks(
    private val daten: Daten,
    private val label: JLabel,
    private val progressBar: JProgressBar,
) {
    suspend fun run(writeFilmList: Boolean) {
        RefreshAboWorker(label, progressBar).execute()
        BlacklistFilterWorker(label, progressBar).execute()

        if (ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FILM_EVALUATE_DUPLICATES, true)) {
            FilmDuplicateEvaluationTask().run()
        }

        CommonStatsEvaluationTask().run()

        if (writeFilmList) {
            FilmlistWriterWorker(label, progressBar).run()
        }
        if (daten.listeFilmeNachBlackList is IndexedFilmList) {
            LuceneIndexWorker(label, progressBar).execute()
        }
    }
}
