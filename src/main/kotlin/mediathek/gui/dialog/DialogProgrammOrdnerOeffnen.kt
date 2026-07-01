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

import mediathek.tool.EscapeKeyHandler
import mediathek.tool.SVGIconUtilities
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.awt.FileDialog
import java.awt.Frame
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.io.File
import java.lang.reflect.InvocationTargetException
import java.util.Optional
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JFileChooser
import javax.swing.JOptionPane
import javax.swing.SwingUtilities

class DialogProgrammOrdnerOeffnen private constructor(
    private val parentComponent: Frame?,
    modal: Boolean,
    initialProgram: String,
    dialogTitle: String,
    text: String,
) : DialogProgrammOrdnerOeffnenBase(parentComponent, modal) {
    private var selectedProgramPath: String? = null

    init {
        jButtonZiel.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
        title = dialogTitle
        jTextArea1.lineWrap = true
        jTextArea1.wrapStyleWord = true
        jTextArea1.text = text
        jTextArea1.caretPosition = 0
        jButtonOk.addActionListener {
            if (checkProgram()) {
                closeDialog()
            }
        }
        jButtonAbbrechen.addActionListener { closeDialog() }
        jButtonZiel.addActionListener(ZielBeobachter())
        jTextFieldProgramm.text = initialProgram
        pack()
        setLocationRelativeTo(parentComponent)

        EscapeKeyHandler.installHandler(this) {
            dispose()
        }
    }

    private fun selectedProgram(): Optional<String> = Optional.ofNullable(selectedProgramPath)

    private fun checkProgram(): Boolean {
        val program = jTextFieldProgramm.text.trim()
        if (program.isEmpty()) {
            return false
        }

        try {
            val programFile = File(program)
            when {
                !programFile.exists() -> {
                    JOptionPane.showMessageDialog(
                        parentComponent,
                        "Das Programm:  \"$program\"  existiert nicht!",
                        "Fehler",
                        JOptionPane.ERROR_MESSAGE,
                    )
                }

                !programFile.canExecute() -> {
                    JOptionPane.showMessageDialog(
                        parentComponent,
                        "Das Programm:  \"$program\"  kann nicht ausgeführt werden!",
                        "Fehler",
                        JOptionPane.ERROR_MESSAGE,
                    )
                }

                else -> {
                    selectedProgramPath = program
                    return true
                }
            }
        } catch (ex: Exception) {
            logger.warn("Failed to validate program path: {}", program, ex)
        }
        return false
    }

    private fun closeDialog() {
        dispose()
    }

    private inner class ZielBeobachter : ActionListener {
        override fun actionPerformed(e: ActionEvent) {
            // we can use native chooser on Mac...
            if (SystemUtils.IS_OS_MAC_OSX) {
                val chooser = FileDialog(parentComponent, "Dateimanager suchen")
                chooser.mode = FileDialog.LOAD
                chooser.isVisible = true
                if (chooser.file != null) {
                    try {
                        val destination = File(chooser.directory + chooser.file)
                        jTextFieldProgramm.text = destination.absolutePath
                    } catch (ex: Exception) {
                        logger.error(ex)
                    }
                }
            } else {
                val chooser = JFileChooser()
                val directory = jTextFieldProgramm.text
                if (directory.isNotEmpty()) {
                    chooser.currentDirectory = File(directory)
                } else {
                    chooser.currentDirectory = File(SystemUtils.USER_HOME)
                }
                chooser.fileSelectionMode = JFileChooser.FILES_ONLY
                val returnValue = chooser.showOpenDialog(this@DialogProgrammOrdnerOeffnen)
                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldProgramm.text = chooser.selectedFile.absolutePath
                    } catch (ex: Exception) {
                        logger.error(ex)
                    }
                }
            }
        }
    }

    companion object {
        private val logger = LogManager.getLogger()

        @JvmStatic
        fun showDialog(
            parent: Frame?,
            initialProgram: String,
            title: String,
            text: String,
        ): Optional<String> {
            if (SwingUtilities.isEventDispatchThread()) {
                return showDialogOnEventDispatchThread(parent, initialProgram, title, text)
            }

            val selectedProgram = AtomicReference(Optional.empty<String>())
            try {
                SwingUtilities.invokeAndWait {
                    selectedProgram.set(showDialogOnEventDispatchThread(parent, initialProgram, title, text))
                }
            } catch (ex: InterruptedException) {
                Thread.currentThread().interrupt()
                logger.warn("Interrupted while showing program chooser dialog", ex)
            } catch (ex: InvocationTargetException) {
                logger.warn("Failed to show program chooser dialog", ex.cause)
            }
            return selectedProgram.get()
        }

        private fun showDialogOnEventDispatchThread(
            parent: Frame?,
            initialProgram: String,
            title: String,
            text: String,
        ): Optional<String> {
            val dialog = DialogProgrammOrdnerOeffnen(parent, true, initialProgram, title, text)
            dialog.isVisible = true
            return dialog.selectedProgram()
        }
    }
}
