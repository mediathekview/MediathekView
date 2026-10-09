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

package mediathek.sqlite

import kotlinx.coroutines.*
import mediathek.controller.history.SeenHistoryStore
import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.sql.SqlDatabaseConfig
import org.apache.commons.lang3.SystemUtils
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.absolutePathString

class SqliteRecoverService {
    suspend fun recover(
        sourceDatabase: Path,
        targetDatabase: Path,
        onProgress: suspend (RecoveryProgress) -> Unit = {}
    ): RecoverySummary = withContext(Dispatchers.IO) {
        val normalizedSource = sourceDatabase.toAbsolutePath().normalize()
        val normalizedTarget = targetDatabase.toAbsolutePath().normalize()

        require(Files.exists(normalizedSource)) {
            "Die Quelldatenbank existiert nicht: $normalizedSource"
        }
        require(normalizedSource != normalizedTarget) {
            "Quelldatenbank und Zieldatenbank müssen unterschiedlich sein."
        }

        onProgress(RecoveryProgress("Suche sqlite3 im PATH...", true))
        val sqlite3Path = try {
            GuiFunktionenProgramme.findExecutableOnPath("sqlite3")
        } catch (ex: RuntimeException) {
            if (SystemUtils.IS_OS_WINDOWS) {
                val msg = "<html>" +
                    "Das Programm <i>sqlite3.exe</i> wurde nicht gefunden.<br/>" +
                    "Laden Sie es unter <a href=\"https://www.sqlite.org/download.html\">https://www.sqlite.org/download.html</a> herunter.<br/><br/>" +
                    "Kopieren Sie das Programm in den <i>bin</i>-Ordner." +
                    "</html>"
                throw IllegalStateException(msg, ex)
            }
            else
                throw IllegalStateException("Das Programm sqlite3 wurde im PATH nicht gefunden.", ex)
        }

        if (Files.exists(normalizedTarget)) {
            try {
                Files.delete(normalizedTarget)
            } catch (ex: IOException) {
                throw IllegalStateException("Zieldatenbank konnte nicht überschrieben werden: $normalizedTarget", ex)
            }
        }

        onProgress(RecoveryProgress("Erzeuge Recovery-SQL aus ${normalizedSource.fileName}...", true))

        coroutineScope {
            val recoverProcess = ProcessBuilder(
                sqlite3Path.toAbsolutePath().toString(),
                "-batch",
                normalizedSource.absolutePathString(),
                ".recover"
            ).start()
            val importProcess = ProcessBuilder(
                sqlite3Path.toAbsolutePath().toString(),
                "-batch",
                normalizedTarget.absolutePathString()
            ).start()

            currentCoroutineContext().job.invokeOnCompletion { cause: Throwable? ->
                if (cause is CancellationException) {
                    recoverProcess.destroyForcibly()
                    importProcess.destroyForcibly()
                }
            }

            val recoverErr = async { recoverProcess.errorStream.bufferedReader().use { it.readText().trim() } }
            val importErr = async { importProcess.errorStream.bufferedReader().use { it.readText().trim() } }
            val pipeJob = async {
                recoverProcess.inputStream.use { input ->
                    importProcess.outputStream.use { output ->
                        input.copyTo(output)
                    }
                }
            }

            onProgress(RecoveryProgress("Importiere wiederhergestellte Daten in ${normalizedTarget.fileName}...", true))

            try {
                pipeJob.await()
                ensureActive()

                val recoverExit = recoverProcess.waitFor()
                val importExit = importProcess.waitFor()
                val (recoverError, importError) = awaitAll(recoverErr, importErr)

                if (recoverExit != 0 || importExit != 0) {
                    throw IllegalStateException(buildFailureMessage(recoverError, importError))
                }
            } catch (ex: CancellationException) {
                recoverProcess.destroyForcibly()
                importProcess.destroyForcibly()
                throw ex
            } finally {
                recoverProcess.inputStream.close()
                recoverProcess.errorStream.close()
                importProcess.inputStream.close()
                importProcess.errorStream.close()
            }
        }

        onProgress(RecoveryProgress("Bereinige Duplikate und aktualisiere den URL-Index...", true))
        val duplicateCount = SeenHistoryStore(
            dataSource = SqlDatabaseConfig.createDataSource(normalizedTarget),
            dbPath = normalizedTarget
        ).use { store ->
            store.normalizeRecoveredDatabase()
        }

        RecoverySummary(
            sourceDatabase = normalizedSource,
            targetDatabase = normalizedTarget,
            executablePath = sqlite3Path.toAbsolutePath(),
            duplicateCount = duplicateCount
        )
    }

    private fun buildFailureMessage(recoverError: String, importError: String): String {
        val message = StringBuilder("SQLite-Recovery per sqlite3-Kommando fehlgeschlagen.")
        if (recoverError.isNotBlank()) {
            message.append("\n\nRecover:\n").append(recoverError)
        }
        if (importError.isNotBlank()) {
            message.append("\n\nImport:\n").append(importError)
        }
        return message.toString()
    }

    data class RecoverySummary(
        val sourceDatabase: Path,
        val targetDatabase: Path,
        val executablePath: Path,
        val duplicateCount: Int
    )

    data class RecoveryProgress(
        val message: String,
        val indeterminate: Boolean
    )
}
