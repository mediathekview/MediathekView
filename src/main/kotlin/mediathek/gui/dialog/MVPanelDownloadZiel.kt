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

package mediathek.gui.dialog

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancel
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import kotlinx.coroutines.withContext
import mediathek.config.MVColor
import mediathek.config.StandardLocations
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenDownload
import mediathek.tool.FileDialogs
import mediathek.tool.FileSpecifier
import mediathek.tool.FilenameUtils
import mediathek.tool.GuiFunktionen
import mediathek.tool.SVGIconUtilities
import org.apache.logging.log4j.LogManager
import java.awt.Color
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import javax.swing.DefaultComboBoxModel
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.swing.UIManager
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.JTextComponent
import kotlin.time.Duration.Companion.milliseconds

class MVPanelDownloadZiel(
    private val parent: JFrame?,
    private val datenDownload: DatenDownload,
    private val letztenPfadAnzeigen: Boolean,
) : MVPanelDownloadZielBase() {
    private val panelScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var pathNameCheckJob: Job? = null

    init {
        jButtonPath.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
        jButtonDelPath.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg")
        jLabelExists.text = ""
        jButtonPath.addActionListener(ZielBeobachter())
        jButtonDelPath.addActionListener {
            ApplicationConfiguration.getInstance().savedDownloadTargetPaths = ""
            jComboBoxPath.model = DefaultComboBoxModel(arrayOf(datenDownload.targetPath))
        }
        jTextFieldName.text = datenDownload.targetFileName
        jTextFieldName.document.addDocumentListener(object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent) = update()

            override fun removeUpdate(e: DocumentEvent) = update()

            override fun changedUpdate(e: DocumentEvent) = update()

            private fun update() {
                checkPfadName()
                val name = jTextFieldName.text
                val checkedName = FilenameUtils.checkFilenameForIllegalCharacters(name, false /* pfad */)
                if (name != checkedName) {
                    jTextFieldName.background = MVColor.DOWNLOAD_FEHLER.color
                } else {
                    jTextFieldName.background = UIManager.getDefaults().getColor("TextField.background")
                }
            }
        })
        setModelPfad(datenDownload.targetPath)
        pathEditor.isOpaque = true
        pathEditor.document.addDocumentListener(object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent) = update()

            override fun removeUpdate(e: DocumentEvent) = update()

            override fun changedUpdate(e: DocumentEvent) = update()

            private fun update() {
                checkPfadName()
                val path = pathEditor.text
                val editor = jComboBoxPath.editor.editorComponent
                if (path != FilenameUtils.checkFilenameForIllegalCharacters(path, true)) {
                    editor.background = MVColor.DOWNLOAD_FEHLER.color
                } else {
                    editor.background = UIManager.getDefaults().getColor("TextField.background")
                }
            }
        })
        checkPfadName()
    }

    override fun removeNotify() {
        pathNameCheckJob?.cancel()
        panelScope.cancel()
        super.removeNotify()
    }

    private val pathEditor: JTextComponent
        get() = jComboBoxPath.editor.editorComponent as JTextComponent

    private fun setModelPfad(pfad: String) {
        val paths = ArrayList<String>()
        // wenn gewünscht, den letzten verwendeten Pfad an den Anfang setzen
        if (!letztenPfadAnzeigen && pfad.isNotEmpty()) {
            // dann kommt der Pfad des Sets an den Anfang
            paths.add(pfad)
        }
        val savedPaths = ApplicationConfiguration.getInstance().savedDownloadTargetPaths
        if (savedPaths.isNotEmpty()) {
            val storedPaths = savedPaths.split("<>")
            for (path in storedPaths) {
                if (!paths.contains(path)) {
                    paths.add(path)
                }
            }
        }

        if (letztenPfadAnzeigen && pfad.isNotEmpty()) {
            // dann kommt der Pfad des Sets an den Schluss
            if (!paths.contains(pfad)) {
                paths.add(pfad)
            }
        }
        jComboBoxPath.model = DefaultComboBoxModel(paths.toTypedArray())
    }

    private fun checkPfadName() {
        val pfad = pathEditor.text
        val name = jTextFieldName.text
        val path = if (pfad.endsWith(File.separator)) {
            pfad.substring(0, pfad.length - 1)
        } else {
            pfad
        }
        val pfadName = GuiFunktionen.concatPaths(path, name)
        val targetChanged = name != datenDownload.targetFileName || pfad != datenDownload.targetPath

        pathNameCheckJob?.cancel()
        updatePathNameStatus(fileExists = false, targetChanged = targetChanged)
        pathNameCheckJob = panelScope.launch {
            delay(150.milliseconds)
            val fileExists = withContext(Dispatchers.IO) {
                runCatching { File(pfadName).exists() }
                    .onFailure { logger.debug("Path existence check failed for {}", pfadName, it) }
                    .getOrDefault(false)
            }
            if (pathEditor.text == pfad && jTextFieldName.text == name) {
                updatePathNameStatus(fileExists = fileExists, targetChanged = targetChanged)
            }
        }
    }

    private fun updatePathNameStatus(fileExists: Boolean, targetChanged: Boolean) {
        if (fileExists) {
            jLabelExists.foreground = Color.RED
            jLabelExists.text = "Datei existiert schon!"
        } else if (targetChanged) {
            jLabelExists.foreground = MVColor.DOWNLOAD_DATEINAME_NEU.color
            jLabelExists.text = "Neuer Name"
        } else {
            jLabelExists.foreground = MVColor.DOWNLOAD_DATEINAME_ALT.color
            jLabelExists.text = ""
        }
    }

    fun applyTargetSelection(): Boolean {
        // setzt den neuen Namen und liefert, ob er sich geändert hat
        var pfad = jComboBoxPath.selectedItem!!.toString()
        var name = jTextFieldName.text
        if (pfad.endsWith(File.separator)) {
            pfad = pfad.substring(0, pfad.length - 1)
        }

        // zur Sicherheit bei Unsinn im Set
        if (pfad.isEmpty()) {
            pfad = StandardLocations.getStandardDownloadPath()
        }
        if (name.isEmpty()) {
            name = SimpleDateFormat("yyyyMMdd").format(Date()) + '_' + datenDownload.topic + '-' + datenDownload.title + ".mp4"
        }

        val fileSpecifier = FileSpecifier(pfad, name)
        fileSpecifier.checkLength()

        if (fileSpecifier.path != pfad || fileSpecifier.fileName != name) {
            JOptionPane.showMessageDialog(
                parent,
                "Dateiname war zu lang und wurde gekürzt!",
                "Pfad zu lang!",
                JOptionPane.ERROR_MESSAGE,
            )
        }

        val originalPath = datenDownload.targetPathFileName

        datenDownload.setTarget(fileSpecifier)

        return originalPath != datenDownload.targetPathFileName
    }

    val currentPath: String
        get() = pathEditor.text

    fun addPathDocumentListener(listener: DocumentListener) {
        pathEditor.document.addDocumentListener(listener)
    }

    private inner class ZielBeobachter : ActionListener {
        override fun actionPerformed(e: ActionEvent) {
            val selectedItem = jComboBoxPath.selectedItem?.toString().orEmpty()
            val selectedDirectory = FileDialogs.chooseDirectoryLocation(this@MVPanelDownloadZiel, "Film speichern", selectedItem)
            if (selectedDirectory != null) {
                jComboBoxPath.addItem(selectedDirectory.absolutePath)
                jComboBoxPath.selectedItem = selectedDirectory.absolutePath
            }
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}
