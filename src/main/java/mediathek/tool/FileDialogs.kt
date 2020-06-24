package mediathek.tool

import org.apache.commons.lang3.SystemUtils
import java.awt.FileDialog
import java.awt.Frame
import java.io.File
import javax.swing.JFileChooser

class FileDialogs {
    companion object {
        /**
         * Show a native file dialog where possible, otherwise use the crappy swing file dialog.
         * @param parent the parent for the dialog. Used only for native dialogs
         * @param title Title of the shown dialog
         * @param initialFile path info for initial directory/file display.
         * @return the selected file or null if action was cancelled.
         */
        @JvmStatic
        fun chooseSaveFileLocation(parent: Frame, title: String, initialFile: String): File? {
            var resultFile: File? = null
            if (SystemUtils.IS_OS_MAC_OSX || SystemUtils.IS_OS_WINDOWS) {
                val chooser = FileDialog(parent, title)
                chooser.mode = FileDialog.SAVE
                chooser.isMultipleMode = false
                if (initialFile.isNotEmpty()) {
                    chooser.directory = initialFile
                }
                chooser.isVisible = true
                if (chooser.file != null) {
                    val files = chooser.files
                    if (files.isNotEmpty()) {
                        resultFile = files[0]
                    }
                }
            } else {
                //Linux HiDPI does not work with either AWT FileDialog or JavaFX FileChooser as of JFX 14.0.1
                val chooser = JFileChooser()
                if (initialFile.isNotEmpty()) {
                    chooser.currentDirectory = File(initialFile)
                }
                chooser.fileSelectionMode = JFileChooser.FILES_ONLY
                chooser.dialogTitle = title
                chooser.isFileHidingEnabled = true
                val returnVal = chooser.showSaveDialog(null)
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    resultFile = File(chooser.selectedFile.absolutePath)
                }
            }
            return resultFile
        }
    }
}