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

package mediathek.daten

import mediathek.controller.starter.RuntimeExec
import mediathek.tool.GuiFunktionenProgramme

class DatenProg() {
    private var arr: Array<String> = defaultValues()

    constructor(
        name: String,
        programmpfad: String,
        schalter: String,
        restart: String,
        downloadmanager: String,
    ) : this() {
        this.name = name
        programPath = programmpfad
        switches = schalter
        arr[PROGRAMM_RESTART] = restart.ifEmpty { false.toString() }
        arr[PROGRAMM_DOWNLOADMANAGER] = downloadmanager.ifEmpty { false.toString() }
    }

    fun copy(): DatenProg {
        val ret = DatenProg()
        arr.copyInto(ret.arr)
        return ret
    }

    fun toArray(): Array<String> =
        arr.clone()

    fun copyFrom(values: Array<String>) {
        arr = defaultValues()
        values.copyInto(arr, endIndex = minOf(values.size, arr.size))
    }

    operator fun get(index: Int): String =
        arr[index]

    operator fun set(index: Int, value: String) {
        arr[index] = value
    }

    var name: String
        get() = arr[PROGRAMM_NAME]
        set(value) {
            arr[PROGRAMM_NAME] = value
        }

    var targetFileName: String
        get() = arr[PROGRAMM_ZIEL_DATEINAME]
        set(value) {
            arr[PROGRAMM_ZIEL_DATEINAME] = value
        }

    var programPath: String
        get() = arr[PROGRAMM_PROGRAMMPFAD]
        set(value) {
            arr[PROGRAMM_PROGRAMMPFAD] = value
        }

    var switches: String
        get() = arr[PROGRAMM_SCHALTER]
        set(value) {
            arr[PROGRAMM_SCHALTER] = value
        }

    var prefix: String
        get() = arr[PROGRAMM_PRAEFIX]
        set(value) {
            arr[PROGRAMM_PRAEFIX] = value
        }

    var suffix: String
        get() = arr[PROGRAMM_SUFFIX]
        set(value) {
            arr[PROGRAMM_SUFFIX] = value
        }

    var isRestart: Boolean
        get() = arr[PROGRAMM_RESTART].isNotEmpty() && arr[PROGRAMM_RESTART].toBoolean()
        set(value) {
            arr[PROGRAMM_RESTART] = value.toString()
        }

    var isDownloadManager: Boolean
        get() = arr[PROGRAMM_DOWNLOADMANAGER].isNotEmpty() && arr[PROGRAMM_DOWNLOADMANAGER].toBoolean()
        set(value) {
            arr[PROGRAMM_DOWNLOADMANAGER] = value.toString()
        }

    fun urlTesten(url: String?): Boolean =
        url != null &&
            GuiFunktionenProgramme.checkPrefix(prefix, url) &&
            GuiFunktionenProgramme.checkSuffix(suffix, url)

    val programmAufruf: String
        get() = "$programPath $switches"

    val programmAufrufArray: String
        get() = buildString {
            append(programPath)
            for (switch in switches.split(" ")) {
                append(RuntimeExec.TRENNER_PROG_ARRAY)
                append(switch)
            }
        }

    override fun toString(): String =
        buildString {
            for (i in 0 until MAX_ELEM) {
                if (i == 0) {
                    append("| ***|")
                } else {
                    append("|    |")
                }
                append(COLUMN_NAMES[i])
                append(": ")
                append(arr[i])
                append(System.lineSeparator())
            }
        }

    companion object {
        const val PROGRAMM_NAME = 0
        const val PROGRAMM_ZIEL_DATEINAME = 1
        const val PROGRAMM_PROGRAMMPFAD = 2
        const val PROGRAMM_SCHALTER = 3
        const val PROGRAMM_PRAEFIX = 4
        const val PROGRAMM_SUFFIX = 5
        const val PROGRAMM_RESTART = 6
        const val PROGRAMM_DOWNLOADMANAGER = 7

        const val MAX_ELEM = 8
        const val TAG = "Programm"

        val COLUMN_NAMES: Array<String> = arrayOf(
            "Beschreibung",
            "Zieldateiname",
            "Programm",
            "Schalter",
            "Präfix",
            "Suffix",
            "Restart",
            "Downloadmanager",
        )

        val XML_NAMES: Array<String> = arrayOf(
            "Programmname",
            "Zieldateiname",
            "Programmpfad",
            "Programmschalter",
            "Praefix",
            "Suffix",
            "Restart",
            "Downloadmanager",
        )

        fun makeProgAufrufArray(pArray: String): String =
            pArray.split(RuntimeExec.TRENNER_PROG_ARRAY)
                .joinToString(separator = " ")
                .trim()

        private fun defaultValues(): Array<String> =
            Array(MAX_ELEM) { "" }.apply {
                this[PROGRAMM_RESTART] = false.toString()
                this[PROGRAMM_DOWNLOADMANAGER] = false.toString()
            }
    }
}
