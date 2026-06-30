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

import mediathek.SplashScreenLifecycle
import mediathek.config.Konstanten
import mediathek.controller.history.SeenHistoryStore
import mediathek.swing.SwingDispatch
import org.apache.logging.log4j.LogManager
import org.sqlite.SQLiteErrorCode
import org.sqlite.SQLiteException
import java.awt.Dialog
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.JDialog
import javax.swing.JOptionPane

internal object SeenHistoryCorruptionHandler {
    private val logger = LogManager.getLogger()
    private val warningShown = AtomicBoolean(false)

    fun openStoreOrThrowCorrupt(
        dbPath: Path,
        openStore: (Path) -> SeenHistoryStore,
    ): SeenHistoryStore {
        try {
            return openStore(dbPath)
        } catch (ex: Exception) {
            if (!isCorruption(ex)) {
                throw ex
            }
            logger.warn("Seen history database is corrupt: {}", dbPath, ex)
            throw CorruptSeenHistoryDatabaseException(dbPath, ex)
        }
    }

    fun resolveCorruption(
        dbPath: Path,
        openStore: (Path) -> SeenHistoryStore,
    ): SeenHistoryStore {
        hideSplashScreen()
        showCorruptionWarning(dbPath)
        return openTemporaryStore(openStore)
    }

    private fun showCorruptionWarning(dbPath: Path) {
        if (!warningShown.compareAndSet(false, true)) {
            return
        }
        showMessage(
            "<html>Die History-Datenbank ist beschädigt:<br/>$dbPath<br/><br/>" +
                "Für diese Sitzung wird eine leere temporäre History-Datenbank verwendet.<br/>" +
                "Reparieren Sie die Datenbank mittels dem Menüpunkt:<br/>" +
                "<i>Hilfe/Hilfsmittel/History-Datenbank wiederherstellen...</i><br/><br/>" +
                    "<b>Alle gesehenen Filme gehen ab jetzt verloren!</b></html>"
        )
    }

    private fun openTemporaryStore(openStore: (Path) -> SeenHistoryStore): SeenHistoryStore {
        val tempDb = Files.createTempFile("mediathek-history-session-", ".db")
        tempDb.toFile().deleteOnExit()
        return openStore(tempDb)
    }

    private fun isCorruption(ex: Throwable): Boolean {
        return generateSequence(ex) { it.cause }
            .filterIsInstance<SQLiteException>()
            .any { sqliteEx ->
                sqliteEx.resultCode == SQLiteErrorCode.SQLITE_CORRUPT ||
                    sqliteEx.resultCode == SQLiteErrorCode.SQLITE_CORRUPT_INDEX ||
                    sqliteEx.resultCode == SQLiteErrorCode.SQLITE_CORRUPT_SEQUENCE ||
                    sqliteEx.resultCode == SQLiteErrorCode.SQLITE_CORRUPT_VTAB ||
                    sqliteEx.resultCode == SQLiteErrorCode.SQLITE_NOTADB
            }
    }

    private fun showMessage(message: String) {
        SwingDispatch.callAndWait("Show seen history corruption warning") {
            val optionPane = JOptionPane(message, JOptionPane.ERROR_MESSAGE)
            val dialog = optionPane.createDialog(null, Konstanten.PROGRAMMNAME).apply {
                modalityType = Dialog.ModalityType.APPLICATION_MODAL
                isAlwaysOnTop = true
                defaultCloseOperation = JDialog.DISPOSE_ON_CLOSE
                toFront()
                requestFocusInWindow()
            }
            dialog.isVisible = true
        }
    }

    private fun hideSplashScreen() {
        SwingDispatch.callAndWait("Hide splash screen") {
            SplashScreenLifecycle.close()
        }
    }

    class CorruptSeenHistoryDatabaseException(
        val dbPath: Path,
        cause: Throwable
    ) : IllegalStateException("Seen history database is corrupt: $dbPath", cause)
}
