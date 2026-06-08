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
import org.apache.logging.log4j.LogManager
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import java.awt.event.InputEvent
import java.io.File
import javax.swing.JComponent

object GuiFunktionen {
    private val logger = LogManager.getLogger()
    private const val EXTERNAL_UPDATE_PROPERTY = "externalUpdateCheck"

    @JvmStatic
    fun isNotUsingExternalUpdater(): Boolean {
        val externalUpdateCheck = System.getProperty(EXTERNAL_UPDATE_PROPERTY)
        val usesExternalUpdater = externalUpdateCheck?.let {
            it.equals("true", ignoreCase = true) || it.isEmpty()
        } ?: false

        return !usesExternalUpdater
    }

    @JvmStatic
    fun showErrorIndication(component: JComponent, hasError: Boolean) {
        component.putClientProperty("JComponent.outline", if (hasError) "error" else "")
    }

    @JvmStatic
    fun isUsingExternalUpdater(): Boolean = !isNotUsingExternalUpdater()

    fun copyToClipboard(s: String) {
        Toolkit.getDefaultToolkit().systemClipboard.setContents(StringSelection(s), null)
    }

    fun addsPfad(pfad1: String?, pfad2: String?): String {
        val result = concatPaths(pfad1, pfad2)
        if (result.isEmpty()) {
            logger.error("addsPfad({},{}):", pfad1, pfad2)
        }
        return result
    }

    @JvmStatic
    fun concatPaths(pfad1: String?, pfad2: String?): String {
        if (pfad1 == null || pfad2 == null) {
            return ""
        }
        if (pfad1.isEmpty() || pfad2.isEmpty()) {
            return pfad1 + pfad2
        }

        var firstPath: String = pfad1
        while (firstPath.endsWith(File.separator)) {
            firstPath = firstPath.substring(0, firstPath.length - 1)
        }
        return if (pfad2.startsWith(File.separator)) {
            firstPath + pfad2
        } else {
            firstPath + File.separator + pfad2
        }
    }

    fun cutName(name: String, length: Int): String =
        if (name.length > length) {
            name.substring(0, length - 4) + name.substring(name.length - 4)
        } else {
            name
        }

    fun getDateiName(pfad: String?): String {
        var result = if (!pfad.isNullOrEmpty()) {
            pfad.substring(pfad.lastIndexOf('/') + 1)
        } else {
            ""
        }
        if (result.contains("?")) {
            result = result.substring(0, result.indexOf('?'))
        }
        if (result.contains("&")) {
            result = result.substring(0, result.indexOf('&'))
        }
        if (result.isEmpty()) {
            logger.error("getDateiName({})", pfad)
        }
        return result
    }

    @JvmStatic
    fun getPlatformControlKey(): Int =
        if (SystemUtils.IS_OS_MAC_OSX) {
            InputEvent.META_DOWN_MASK
        } else {
            InputEvent.CTRL_DOWN_MASK
        }
}
