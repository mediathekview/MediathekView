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
package ca.odell.glazedlists.impl

import javax.swing.Icon
import javax.swing.ImageIcon
import javax.swing.UIManager
import javax.swing.plaf.metal.MetalLookAndFeel

/** Provides table-header sort icons matching the active look and feel. */
internal object SortIconFactory {
    private object Loader {
        private const val RESOURCE_ROOT = "resources"
        private const val DEFAULT_RESOURCE_PATH = "aqua"

        private val lookAndFeelResourcePathMap: Map<String, String> = HashMap<String, String>().apply {
            put("Mac OS X Aqua", "aqua")
            put("Metal/Steel", "metal")
            put("Metal/Ocean", "ocean")
            put("Windows", "windows")
            put("WinLAF", "windows")
        }

        private var defaultIcons: Array<Icon?>? = null

        private val iconFileNames = arrayOf(
            "unsorted.png",
            "primary_sorted.png",
            "primary_sorted_reverse.png",
            "primary_sorted_alternate.png",
            "primary_sorted_alternate_reverse.png",
            "secondary_sorted.png",
            "secondary_sorted_reverse.png",
            "secondary_sorted_alternate.png",
            "secondary_sorted_alternate_reverse.png",
        )

        /** Loads and caches icons matching the active Swing look and feel. */
        fun loadIcons(): Array<Icon?> {
            defaultIcons?.let { return it }

            var lookAndFeelName = UIManager.getLookAndFeel().name
            if (lookAndFeelName == "Metal") {
                lookAndFeelName = "Metal/${MetalLookAndFeel.getCurrentTheme().name}"
            }
            val resourcePath = lookAndFeelResourcePathMap[lookAndFeelName] ?: DEFAULT_RESOURCE_PATH
            return loadIcons("$RESOURCE_ROOT/$resourcePath").also { defaultIcons = it }
        }

        /** Loads a fresh icon array from [path]. */
        fun loadIcons(path: String): Array<Icon?> {
            val jarLoader = SortIconFactory::class.java.classLoader
            return arrayOfNulls<Icon>(iconFileNames.size).also { pathIcons ->
                for (index in 1 until pathIcons.size) {
                    jarLoader.getResource("$path/${iconFileNames[index]}")?.let { iconLocation ->
                        pathIcons[index] = ImageIcon(iconLocation)
                    }
                }
            }
        }
    }

    /** Loads and caches icons matching the active Swing look and feel. */
    fun loadIcons(): Array<Icon?> = Loader.loadIcons()

    /** Loads a fresh icon array from [path]. */
    fun loadIcons(path: String): Array<Icon?> = Loader.loadIcons(path)
}
