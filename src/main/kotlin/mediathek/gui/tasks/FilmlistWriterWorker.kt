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

package mediathek.gui.tasks

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.config.StandardLocations.getFilmlistFilePathString
import mediathek.filmlisten.writer.FilmListWriter
import javax.swing.JLabel
import javax.swing.JProgressBar
import kotlin.math.roundToInt

class FilmlistWriterWorker(
    private val progLabel: JLabel,
    private val progressBar: JProgressBar,
) {
    suspend fun run() = coroutineScope {
        val progressUpdates = Channel<Int>(Channel.CONFLATED)
        val progressJob = launch(Dispatchers.Swing) {
            for (progress in progressUpdates) {
                applyProgress(progress)
            }
        }

        try {
            withContext(Dispatchers.Swing) {
                progLabel.text = "Schreibe Filmliste"
                applyProgress(0)
            }

            withContext(Dispatchers.IO) {
                var lastProgress = 0
                FilmListWriter(false).writeFilmList(getFilmlistFilePathString(), Daten.getInstance().listeFilme) { prog ->
                    val progress = (100.0 * prog).roundToInt().coerceIn(0, 100)
                    if (progress >= lastProgress + 1) {
                        lastProgress = progress
                        progressUpdates.trySend(progress)
                    }
                }
            }
            progressUpdates.trySend(100)
        } finally {
            progressUpdates.close()
            progressJob.join()
        }
    }

    private fun applyProgress(progress: Int) {
        progressBar.isIndeterminate = false
        progressBar.minimum = 0
        progressBar.maximum = 100
        progressBar.value = progress
    }
}
