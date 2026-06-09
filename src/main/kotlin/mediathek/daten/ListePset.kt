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

import mediathek.config.StandardLocations
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.dialog.DialogOk
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade
import mediathek.gui.messages.ProgramSetChangedEvent
import mediathek.tool.MessageBus
import javax.swing.JFrame
import javax.swing.table.DefaultTableModel
import javax.swing.table.TableModel

class ListePset : ArrayList<DatenPset>() {
    // Liste aller Programmsets
    var version: String = ""

    /**
     * Make the specified pset active as the current player. Deactivates all other psets.
     * @param pset the current pset to activate
     */
    fun activateAsPlayer(pset: DatenPset) {
        forEach { set -> set.setAbspielen(false) }
        pset.setAbspielen(true)
    }

    private fun normalizePlaybackSelection(datenPset: DatenPset?) {
        if (datenPset != null && datenPset.istAbspielen()) {
            forEach { set -> set.setAbspielen(false) }
        }
    }

    private fun normalizePlaybackSelection(collection: Collection<DatenPset>) {
        var lastActive: DatenPset? = null
        for (datenPset in collection) {
            if (datenPset.istAbspielen()) {
                lastActive?.setAbspielen(false)
                lastActive = datenPset
            }
        }
        normalizePlaybackSelection(lastActive)
    }

    override fun add(element: DatenPset): Boolean {
        normalizePlaybackSelection(element)
        return super.add(element)
    }

    override fun add(index: Int, element: DatenPset) {
        normalizePlaybackSelection(element)
        super.add(index, element)
    }

    override fun addAll(elements: Collection<DatenPset>): Boolean {
        normalizePlaybackSelection(elements)
        return super.addAll(elements)
    }

    override fun addAll(index: Int, elements: Collection<DatenPset>): Boolean {
        normalizePlaybackSelection(elements)
        return super.addAll(index, elements)
    }

    override fun set(index: Int, element: DatenPset): DatenPset {
        normalizePlaybackSelection(element)
        return super.set(index, element)
    }

    val psetAbspielen: DatenPset?
        get() = firstOrNull { datenPset -> datenPset.istAbspielen() }

    fun getPsetAbo(name: String): DatenPset? {
        // liefert mit dem Namen eines Abos die passende Programmgruppe zurück
        // wird nichts gefunden, wird die erste Programmgruppe (der Abos) genommen
        return when {
            isEmpty() -> null
            size == 1 -> first()
            else -> firstOrNull { pset -> pset.istAbo() && pset.name == name }
                ?: listeAbo.firstOrNull()
                ?: firstOrNull()
        }
    }

    val listeSpeichern: ListePset
        get() = filterTo(ListePset()) { pset -> pset.istSpeichern() }

    fun hasDownloadProgramSet(): Boolean = any { pset -> pset.istSpeichern() }

    val listeButton: ListePset
        get() = filterTo(ListePset()) { pset -> pset.istButton() }

    val listeAbo: ListePset
        get() = filterTo(ListePset()) { pset -> pset.istAbo() }

    fun hasAboProgramSet(): Boolean = any { pset -> pset.istAbo() }

    val objectDataCombo: Array<String>
        get() = Array(size) { index -> this[index].name }

    fun auf(idx: Int, auf: Boolean): Int {
        val prog = removeAt(idx)
        var neu = idx
        if (auf) {
            if (neu > 0) {
                --neu
            }
        } else if (neu < size) {
            ++neu
        }
        add(neu, prog)

        MessageBus.messageBus.publishAsync(ProgramSetChangedEvent())

        return neu
    }

    fun addPset(datenPset: DatenPset) {
        add(datenPset)

        MessageBus.messageBus.publishAsync(ProgramSetChangedEvent())
    }

    fun addPset(liste: ListePset): Boolean {
        var ret = true
        for (entry in liste) {
            if (!add(entry)) {
                ret = false
            }
        }

        if (ret) {
            MessageBus.messageBus.publishAsync(ProgramSetChangedEvent())
        }

        return ret
    }

    fun createModel(): TableModel {
        val data = Array(size) { index -> createModelRow(this[index]) }
        return PsetTableModel(data)
    }

    private class PsetTableModel(data: Array<Array<Any?>>) : DefaultTableModel(data, DatenPset.COLUMN_NAMES) {
        override fun isCellEditable(row: Int, column: Int): Boolean = false

        override fun getColumnClass(columnIndex: Int): Class<*> =
            when (columnIndex) {
                DatenPset.PROGRAMMSET_IST_ABSPIELEN,
                DatenPset.PROGRAMMSET_IST_SPEICHERN,
                -> Boolean::class.javaObjectType
                else -> String::class.java
            }
    }

    companion object {
        const val MUSTER_PFAD_ZIEL = "ZIELPFAD"
        const val MUSTER_PFAD_VLC = "PFAD_VLC"
        const val MUSTER_PFAD_FFMPEG = "PFAD_FFMPEG"

        @JvmStatic
        fun progMusterErsetzen(parent: JFrame?, liste: ListePset) {
            for (pSet in liste) {
                progMusterErsetzen(parent, pSet)
            }

            MessageBus.messageBus.publishAsync(ProgramSetChangedEvent())
        }

        private fun progMusterErsetzen(parent: JFrame?, pSet: DatenPset) {
            pSet.zielPfad = pSet.zielPfad.replace(MUSTER_PFAD_ZIEL, StandardLocations.getStandardDownloadPath())
            var vlc = ""
            var ffmpeg = ""

            // damit nur die Variablen abgefragt werden, die auch verwendet werden
            for (prog in pSet.listeProg) {
                if (prog.programPath.contains(MUSTER_PFAD_VLC) || prog.switches.contains(MUSTER_PFAD_VLC)) {
                    vlc = getPfadVlc(parent)
                    break
                }
            }

            for (prog in pSet.listeProg) {
                if (prog.programPath.contains(MUSTER_PFAD_FFMPEG) || prog.switches.contains(MUSTER_PFAD_FFMPEG)) {
                    ffmpeg = getPfadFFmpeg(parent)
                    break
                }
            }

            for (prog in pSet.listeProg) {
                prog.programPath = prog.programPath.replace(MUSTER_PFAD_VLC, vlc)
                prog.switches = prog.switches.replace(MUSTER_PFAD_VLC, vlc)
                prog.programPath = prog.programPath.replace(MUSTER_PFAD_FFMPEG, ffmpeg)
                prog.switches = prog.switches.replace(MUSTER_PFAD_FFMPEG, ffmpeg)
            }
        }

        private fun getPfadVlc(parent: JFrame?): String {
            // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
            if (ApplicationConfiguration.getInstance().standardVlcPath.isEmpty()) {
                DialogOk(null, true, PanelProgrammPfade(parent, true, false), "Pfade Standardprogramme").isVisible = true
            }
            return ApplicationConfiguration.getInstance().standardVlcPath
        }

        private fun getPfadFFmpeg(parent: JFrame?): String {
            // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
            if (ApplicationConfiguration.getInstance().standardFFmpegPath.isEmpty()) {
                DialogOk(null, true, PanelProgrammPfade(parent, false, true), "Pfade Standardprogramme").isVisible = true
            }
            return ApplicationConfiguration.getInstance().standardFFmpegPath
        }

        private fun createModelRow(datenPset: DatenPset): Array<Any?> {
            val values = datenPset.toArray()
            return Array<Any?>(DatenPset.MAX_ELEM) { index -> values[index] }.apply {
                this[DatenPset.PROGRAMMSET_IST_ABSPIELEN] = datenPset.istAbspielen()
                this[DatenPset.PROGRAMMSET_IST_SPEICHERN] = datenPset.istSpeichern()
            }
        }
    }
}
