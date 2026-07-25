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

object JvmSettingsValidator {
    private val requiredOptions = listOf(
        JvmOptionRequirement { arguments -> hasJvmOption(arguments, "-XX:+UseShenandoahGC") },
        JvmOptionRequirement { arguments -> hasJvmOption(arguments, "-XX:ShenandoahGCHeuristics=compact") },
        JvmOptionRequirement { arguments -> hasJvmOption(arguments, "-XX:+UseStringDeduplication") },
        JvmOptionRequirement { arguments -> hasJvmOptionStartingWith(arguments, "-XX:MaxRAMPercentage=") },
        JvmOptionRequirement { arguments -> !hasJvmOptionStartingWith(arguments, "-Xmx") },
    )

    fun hasRequiredJvmSettings(paramList: List<String>): Boolean {
        val meetsCommonRequirements = requiredOptions.all { requirement -> requirement.matches(paramList) }
        if (!meetsCommonRequirements) {
            return false
        }

        return !SystemUtils.IS_OS_LINUX ||
            hasLinuxAddOpensOption(paramList)
    }

    fun getErrorMessageString(): String {
        val linuxAddOpensMessage = if (SystemUtils.IS_OS_LINUX) {
            "<li><b>--add-opens=java.desktop/sun.awt.X11=ALL-UNNAMED</b></li>"
        } else {
            ""
        }

        return buildString {
            append("<html>")
            append("<b>Inkorrekte/fehlende JVM Parameter erkannt</b><br/><br/>")
            append("Bitte stellen Sie sicher, dass die folgenden Parameter an die JVM übergeben werden:<br/>")
            append("<ul>")
            append("<li>-XX:+UseShenandoahGC</li>")
            append("<li>-XX:ShenandoahGCHeuristics=compact</li>")
            append("<li>-XX:+UseStringDeduplication</li>")
            append("<li>-XX:MaxRAMPercentage=<b>XX.X</b></li>")
            append(linuxAddOpensMessage)
            append("</ul><br/>")
            append("<b>-Xmx</b> sollte nicht mehr genutzt werden!")
            append("</html>")
        }
    }

    private fun hasJvmOption(paramList: List<String>, option: String): Boolean =
        paramList.any { argument -> argument.equals(option, ignoreCase = true) }

    private fun hasJvmOptionStartingWith(paramList: List<String>, optionPrefix: String): Boolean =
        paramList.any { argument -> argument.startsWith(optionPrefix) }

    private fun hasLinuxAddOpensOption(paramList: List<String>): Boolean {
        val optionName = "--add-opens"
        val optionValue = "java.desktop/sun.awt.X11=ALL-UNNAMED"
        val inlineOption = "$optionName=$optionValue"

        for (index in paramList.indices) {
            val argument = paramList[index]
            if (argument.equals(inlineOption, ignoreCase = true)) {
                return true
            }
            if (argument.equals(optionName, ignoreCase = true)
                && index + 1 < paramList.size
                && paramList[index + 1].equals(optionValue, ignoreCase = true)
            ) {
                return true
            }
        }

        return false
    }

    private fun interface JvmOptionRequirement {
        fun matches(arguments: List<String>): Boolean
    }
}
