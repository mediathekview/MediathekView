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

package mediathek.tool

import org.apache.commons.lang3.SystemUtils
import java.awt.Component
import java.awt.Dialog
import java.awt.FileDialog
import java.awt.Frame
import java.io.File
import javax.swing.JDialog
import javax.swing.JFileChooser

object FileDialogs {
    private const val MAC_DIRECTORY_DIALOG_PROPERTY = "apple.awt.fileDialogForDirectories"

    private val prefersNativeFileDialogs: Boolean
        get() = SystemUtils.IS_OS_MAC_OSX || SystemUtils.IS_OS_WINDOWS

    private val FileDialog.selectedFileOrNull: File?
        get() = files.firstOrNull()

    @JvmStatic
    fun chooseDirectoryLocation(parent: Frame, title: String, initialFile: String): File? =
        if (SystemUtils.IS_OS_MAC_OSX) {
            withTemporarySystemProperty(MAC_DIRECTORY_DIALOG_PROPERTY, "true") {
                showNativeDialog(parent, title, mode = FileDialog.LOAD, initialDirectory = initialFile)
            }
        } else {
            showSwingDialog(
                parent = parent,
                title = title,
                selectionMode = JFileChooser.DIRECTORIES_ONLY,
                initialDirectory = initialFile,
                showDialog = JFileChooser::showOpenDialog
            )
        }

    fun chooseLoadFileLocation(parent: JDialog, title: String, initialFile: String): File? =
        if (prefersNativeFileDialogs) {
            showNativeDialog(parent, title, mode = FileDialog.LOAD, initialDirectory = initialFile)
        } else {
            showSwingDialog(
                parent = parent,
                title = title,
                selectionMode = JFileChooser.FILES_ONLY,
                initialDirectory = initialFile,
                showDialog = JFileChooser::showOpenDialog
            )
        }

    @JvmStatic
    fun chooseLoadFileLocation(parent: Frame, title: String, initialFile: String): File? =
        if (prefersNativeFileDialogs) {
            showNativeDialog(parent, title, mode = FileDialog.LOAD, initialDirectory = initialFile)
        } else {
            showSwingDialog(
                parent = parent,
                title = title,
                selectionMode = JFileChooser.FILES_ONLY,
                initialDirectory = initialFile,
                showDialog = JFileChooser::showOpenDialog
            )
        }

    /**
     * Show a native file dialog where possible, otherwise use the crappy swing file dialog.
     * @param parent the parent for the dialog. Used only for native dialogs
     * @param title Title of the shown dialog
     * @param initialFile path info for initial directory/file display.
     * @return the selected file or null if action was cancelled.
     */
    @JvmStatic
    fun chooseSaveFileLocation(parent: Frame, title: String, initialFile: String): File? =
        if (prefersNativeFileDialogs) {
            showNativeDialog(parent, title, mode = FileDialog.SAVE, initialFile = initialFile)
        } else {
            // Linux HiDPI does not work with either AWT FileDialog or JavaFX FileChooser as of JFX 14.0.1.
            showSwingDialog(
                parent = parent,
                title = title,
                selectionMode = JFileChooser.FILES_ONLY,
                initialFile = initialFile,
                showDialog = JFileChooser::showSaveDialog
            )
        }

    fun chooseSaveFileLocation(parent: JDialog, title: String, initialFile: String): File? =
        if (prefersNativeFileDialogs) {
            showNativeDialog(parent, title, mode = FileDialog.SAVE, initialFile = initialFile)
        } else {
            // Linux HiDPI does not work with either AWT FileDialog or JavaFX FileChooser as of JFX 14.0.1.
            showSwingDialog(
                parent = parent,
                title = title,
                selectionMode = JFileChooser.FILES_ONLY,
                initialFile = initialFile,
                showDialog = JFileChooser::showSaveDialog
            )
        }

    private fun showNativeDialog(
        parent: Frame,
        title: String,
        mode: Int,
        initialDirectory: String = "",
        initialFile: String = ""
    ): File? = showNativeDialog(FileDialog(parent, title), mode, initialDirectory, initialFile)

    private fun showNativeDialog(
        parent: Dialog,
        title: String,
        mode: Int,
        initialDirectory: String = "",
        initialFile: String = ""
    ): File? = showNativeDialog(FileDialog(parent, title), mode, initialDirectory, initialFile)

    private fun showNativeDialog(
        chooser: FileDialog,
        mode: Int,
        initialDirectory: String,
        initialFile: String
    ): File? = chooser.apply {
        this.mode = mode
        isMultipleMode = false
        initialDirectory.takeIf(String::isNotEmpty)?.let { directory = it }
        initialFile.takeIf(String::isNotEmpty)?.let { file = it }
        isVisible = true
    }.selectedFileOrNull

    private fun showSwingDialog(
        parent: Component,
        title: String,
        selectionMode: Int,
        initialDirectory: String = "",
        initialFile: String = "",
        showDialog: JFileChooser.(Component?) -> Int
    ): File? = JFileChooser().apply {
        initialDirectory.takeIf(String::isNotEmpty)?.let { currentDirectory = File(it) }
        initialFile.takeIf(String::isNotEmpty)?.let { selectedFile = File(it) }
        fileSelectionMode = selectionMode
        dialogTitle = title
        isFileHidingEnabled = true
    }.takeIf { it.showDialog(parent) == JFileChooser.APPROVE_OPTION }?.selectedFile

    private inline fun <T> withTemporarySystemProperty(name: String, value: String, block: () -> T): T {
        val previousValue = System.getProperty(name)
        System.setProperty(name, value)
        return try {
            block()
        } finally {
            if (previousValue == null) {
                System.clearProperty(name)
            } else {
                System.setProperty(name, previousValue)
            }
        }
    }
}
