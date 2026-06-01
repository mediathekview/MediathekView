/*
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten

import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.MVMessageDialog
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.awt.Color
import java.util.Optional
import javax.swing.JOptionPane

class DatenPset() : Comparable<DatenPset> {
    val listeProg = ListeProg()
    private var praefixDirekt = ""
    private var suffixDirekt = ""
    private var themaAnlegen: Boolean? = null
    private var istAbspielen: Boolean? = null
    private var istSpeichern: Boolean? = null
    private var istButton: Boolean? = null
    private var istAbo: Boolean? = null
    private var laengeBeschraenken: Boolean? = null
    private var laengeFieldBeschraenken: Boolean? = null
    private var aufloesungValue: FilmResolution.Enum? = null
    private var beschreibung = ""
    private var infoUrl = ""
    private var infodatei: Boolean? = null
    private var spotlight: Boolean? = null
    private var subtitle: Boolean? = null

    @set:JvmName("setNameValue")
    var name: String = ""

    var farbe: Color? = null

    @set:JvmName("setZielPfadValue")
    var zielPfad: String = ""

    @set:JvmName("setZielDateinameValue")
    var zielDateiname: String = ""

    var maxLaenge: Int? = null

    var maxLaengeField: Int? = null

    var addOn: String = ""
        private set

    var isThemaAnlegen: Boolean
        get() = themaAnlegen == true
        set(value) {
            themaAnlegen = value
        }

    var isLaengeBeschraenken: Boolean
        get() = laengeBeschraenken == true
        set(value) {
            laengeBeschraenken = value
        }

    var isLaengeFieldBeschraenken: Boolean
        get() = laengeFieldBeschraenken == true
        set(value) {
            laengeFieldBeschraenken = value
        }

    var isSpotlight: Boolean
        get() = spotlight == true
        set(value) {
            spotlight = value
        }

    var aufloesung: FilmResolution.Enum?
        get() = aufloesungValue
        set(value) {
            aufloesungValue = value ?: FilmResolution.Enum.NORMAL
        }

    val isLabel: Boolean
        get() = listeProg.isEmpty() && name.isNotEmpty()

    val isFreeLine: Boolean
        get() = name.isEmpty()

    /**
     * Return the specified foreground color if present.
     * @return the requested foreground color for the PSet.
     */
    val foregroundColor: Optional<Color>
        get() = Optional.ofNullable(farbe)

    init {
        initialize()
    }

    constructor(name: String?) : this() {
        // neue Pset sind immer gleich Button
        setName(name)
        this[PROGRAMMSET_IST_BUTTON] = true.toString()
    }

    /**
     * Indicate whether a subtitle file should be downloaded.
     * @return true if download, otherwise false.
     */
    fun shouldDownloadSubtitle(): Boolean = subtitle == true

    /**
     * Indicate wheter an Infofile should be created.
     * @return true for creation, false otherwise
     */
    fun shouldCreateInfofile(): Boolean = infodatei == true

    fun getPraefixDirekt(): String = praefixDirekt

    fun getSuffixDirekt(): String = suffixDirekt

    fun getBeschreibung(): String = beschreibung

    fun setInfodatei(infodatei: Boolean) {
        this.infodatei = infodatei
    }

    fun setSubtitle(subtitle: Boolean) {
        this.subtitle = subtitle
    }

    fun setName(name: String?) {
        this.name = name.orEmpty()
    }

    fun clearFarbe() {
        farbe = null
    }

    operator fun get(index: Int): String =
        when (index) {
            PROGRAMMSET_NAME -> name
            PROGRAMMSET_PRAEFIX_DIREKT -> praefixDirekt
            PROGRAMMSET_SUFFIX_DIREKT -> suffixDirekt
            PROGRAMMSET_FARBE -> colorToString(farbe)
            PROGRAMMSET_ZIEL_PFAD -> zielPfad
            PROGRAMMSET_ZIEL_DATEINAME -> zielDateiname
            PROGRAMMSET_THEMA_ANLEGEN -> boolToString(themaAnlegen)
            PROGRAMMSET_IST_ABSPIELEN -> boolToString(istAbspielen)
            PROGRAMMSET_IST_SPEICHERN -> boolToString(istSpeichern)
            PROGRAMMSET_IST_BUTTON -> boolToString(istButton)
            PROGRAMMSET_IST_ABO -> boolToString(istAbo)
            PROGRAMMSET_LAENGE_BESCHRAENKEN -> boolToString(laengeBeschraenken)
            PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN -> boolToString(laengeFieldBeschraenken)
            PROGRAMMSET_MAX_LAENGE -> intToString(maxLaenge)
            PROGRAMMSET_MAX_LAENGE_FIELD -> intToString(maxLaengeField)
            PROGRAMMSET_AUFLOESUNG -> aufloesungValue?.toString().orEmpty()
            PROGRAMMSET_ADD_ON -> addOn
            PROGRAMMSET_BESCHREIBUNG -> beschreibung
            PROGRAMMSET_INFO_URL -> infoUrl
            PROGRAMMSET_INFODATEI -> boolToString(infodatei)
            PROGRAMMSET_SPOTLIGHT -> boolToString(spotlight)
            PROGRAMMSET_SUBTITLE -> boolToString(subtitle)
            else -> throw ArrayIndexOutOfBoundsException(index)
        }

    operator fun set(index: Int, value: String?) {
        val normalizedValue = value.orEmpty()

        when (index) {
            PROGRAMMSET_NAME -> name = normalizedValue
            PROGRAMMSET_PRAEFIX_DIREKT -> praefixDirekt = normalizedValue
            PROGRAMMSET_SUFFIX_DIREKT -> suffixDirekt = normalizedValue
            PROGRAMMSET_FARBE -> farbe = parseColor(normalizedValue)
            PROGRAMMSET_ZIEL_PFAD -> zielPfad = normalizedValue
            PROGRAMMSET_ZIEL_DATEINAME -> zielDateiname = normalizedValue
            PROGRAMMSET_THEMA_ANLEGEN -> themaAnlegen = parseBoolean(normalizedValue)
            PROGRAMMSET_IST_ABSPIELEN -> istAbspielen = parseBoolean(normalizedValue)
            PROGRAMMSET_IST_SPEICHERN -> istSpeichern = parseBoolean(normalizedValue)
            PROGRAMMSET_IST_BUTTON -> istButton = parseBoolean(normalizedValue)
            PROGRAMMSET_IST_ABO -> istAbo = parseBoolean(normalizedValue)
            PROGRAMMSET_LAENGE_BESCHRAENKEN -> laengeBeschraenken = parseBoolean(normalizedValue)
            PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN -> laengeFieldBeschraenken = parseBoolean(normalizedValue)
            PROGRAMMSET_MAX_LAENGE -> maxLaenge = parseInteger(normalizedValue)
            PROGRAMMSET_MAX_LAENGE_FIELD -> maxLaengeField = parseInteger(normalizedValue)
            PROGRAMMSET_AUFLOESUNG -> aufloesungValue = normalizedValue.ifEmpty { null }?.let(FilmResolution.Enum::fromLegacyString)
            PROGRAMMSET_ADD_ON -> addOn = normalizedValue
            PROGRAMMSET_BESCHREIBUNG -> beschreibung = normalizedValue
            PROGRAMMSET_INFO_URL -> infoUrl = normalizedValue
            PROGRAMMSET_INFODATEI -> infodatei = parseBoolean(normalizedValue)
            PROGRAMMSET_SPOTLIGHT -> spotlight = parseBoolean(normalizedValue)
            PROGRAMMSET_SUBTITLE -> subtitle = parseBoolean(normalizedValue)
            else -> throw ArrayIndexOutOfBoundsException(index)
        }
    }

    fun toArray(): Array<String> =
        arrayOf(
            name,
            praefixDirekt,
            suffixDirekt,
            colorToString(farbe),
            zielPfad,
            zielDateiname,
            boolToString(themaAnlegen),
            boolToString(istAbspielen),
            boolToString(istSpeichern),
            boolToString(istButton),
            boolToString(istAbo),
            boolToString(laengeBeschraenken),
            boolToString(laengeFieldBeschraenken),
            intToString(maxLaenge),
            intToString(maxLaengeField),
            aufloesungValue?.toString().orEmpty(),
            addOn,
            beschreibung,
            infoUrl,
            boolToString(infodatei),
            boolToString(spotlight),
            boolToString(subtitle),
        )

    fun copyFrom(values: Array<out String?>?) {
        clearFields()
        if (values == null) {
            initializeDefaults()
            return
        }

        for (index in 0 until minOf(values.size, MAX_ELEM)) {
            this[index] = values[index]
        }
        initializeDefaults()
    }

    fun addProg(prog: DatenProg) {
        listeProg.addEntry(prog)
    }

    fun getProg(i: Int): DatenProg = listeProg[i]

    fun progsContainPath(): Boolean =
        listeProg.any { prog ->
            prog.switches.contains("**") ||
                prog.switches.contains("%a") ||
                prog.switches.contains("%b")
        }

    fun isEmpty(): Boolean =
        toArray().all { it.isEmpty() } && listeProg.isEmpty()

    fun istAbspielen(): Boolean = istAbspielen == true

    fun setAbspielen(value: Boolean) {
        istAbspielen = value
    }

    fun istSpeichern(): Boolean = istSpeichern == true

    fun istButton(): Boolean = istButton == true

    fun istAbo(): Boolean = istAbo == true

    fun setSpeichern(set: Boolean) {
        this[PROGRAMMSET_IST_SPEICHERN] = set.toString()
    }

    fun setButton(set: Boolean) {
        this[PROGRAMMSET_IST_BUTTON] = set.toString()
    }

    fun setAbo(set: Boolean) {
        this[PROGRAMMSET_IST_ABO] = set.toString()
    }

    fun getProgUrl(url: String): DatenProg? {
        if (listeProg.isEmpty()) {
            MVMessageDialog.showMessageDialog(null, "Programme einrichten!", "Kein Programm", JOptionPane.INFORMATION_MESSAGE)
            return null
        }

        return if (listeProg.size == 1) {
            listeProg[0]
        } else {
            listeProg.firstOrNull { prog -> prog.urlTesten(url) } ?: listeProg[listeProg.size - 1]
        }
    }

    fun getZielDateiname(url: String): String {
        val prog = getProgUrl(url)
        var ret = this[PROGRAMMSET_ZIEL_DATEINAME]
        if (!checkDownloadDirekt(url) && prog != null && prog.targetFileName.isNotEmpty()) {
            ret = prog.targetFileName
        }
        return ret
    }

    fun setZielPfad(zielPfad: String?) {
        this.zielPfad = zielPfad.orEmpty()
    }

    fun copy(): DatenPset {
        val ret = DatenPset()
        ret.copyFrom(toArray())
        // es darf nur einen geben!
        ret.setName("Kopie-$name")
        ret[PROGRAMMSET_IST_ABSPIELEN] = false.toString()
        for (prog in listeProg) {
            ret.addProg(prog.copy())
        }
        return ret
    }

    fun checkDownloadDirekt(url: String): Boolean =
        (praefixDirekt.isNotEmpty() || suffixDirekt.isNotEmpty()) &&
            GuiFunktionenProgramme.checkPrefix(praefixDirekt, url) &&
            GuiFunktionenProgramme.checkSuffix(suffixDirekt, url)

    override fun toString(): String =
        buildString {
            append("================================================")
            appendLine()
            append("| Programmset")
            appendLine()
            for (i in 0 until MAX_ELEM) {
                append("| ")
                append(COLUMN_NAMES[i])
                append(": ")
                append(this@DatenPset[i])
                appendLine()
            }
            for (prog in listeProg) {
                append("|")
                appendLine()
                append(prog)
            }
            append("|_______________________________________________")
            appendLine()
        }

    private fun initialize() {
        clearFields()
        initializeDefaults()
    }

    private fun clearFields() {
        name = ""
        praefixDirekt = ""
        suffixDirekt = ""
        farbe = null
        zielPfad = ""
        zielDateiname = ""
        themaAnlegen = null
        istAbspielen = null
        istSpeichern = null
        istButton = null
        istAbo = null
        laengeBeschraenken = null
        laengeFieldBeschraenken = null
        maxLaenge = null
        maxLaengeField = null
        aufloesungValue = null
        addOn = ""
        beschreibung = ""
        infoUrl = ""
        infodatei = null
        spotlight = null
        subtitle = null
    }

    private fun initializeDefaults() {
        if (this[PROGRAMMSET_THEMA_ANLEGEN].isEmpty()) {
            themaAnlegen = true
        }
        if (this[PROGRAMMSET_IST_ABSPIELEN].isEmpty()) {
            istAbspielen = false
        }
        if (this[PROGRAMMSET_IST_SPEICHERN].isEmpty()) {
            istSpeichern = false
        }
        if (this[PROGRAMMSET_IST_BUTTON].isEmpty()) {
            istButton = false
        }
        if (this[PROGRAMMSET_IST_ABO].isEmpty()) {
            istAbo = false
        }
        if (this[PROGRAMMSET_LAENGE_BESCHRAENKEN].isEmpty()) {
            laengeBeschraenken = false
        }
        if (this[PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN].isEmpty()) {
            laengeFieldBeschraenken = false
        }
        if (this[PROGRAMMSET_INFODATEI].isEmpty()) {
            infodatei = false
        }
        if (this[PROGRAMMSET_SPOTLIGHT].isEmpty()) {
            spotlight = SystemUtils.IS_OS_MAC_OSX
        }
        if (this[PROGRAMMSET_SUBTITLE].isEmpty()) {
            subtitle = false
        }
        if (this[PROGRAMMSET_AUFLOESUNG].isEmpty()) {
            aufloesungValue = FilmResolution.Enum.NORMAL
        }
    }

    private fun parseColor(value: String): Color? {
        if (value.isEmpty()) {
            return null
        }
        return try {
            val rgb = value.split(",", limit = 3)
            require(rgb.size == 3) { "Invalid RGB color: $value" }
            Color(rgb[0].toInt(), rgb[1].toInt(), rgb[2].toInt())
        } catch (ex: Exception) {
            logger.error("getFarbe()", ex)
            null
        }
    }

    override fun compareTo(other: DatenPset): Int = 0

    companion object {
        const val PROGRAMMSET_NAME = 0
        const val PROGRAMMSET_PRAEFIX_DIREKT = 1
        const val PROGRAMMSET_SUFFIX_DIREKT = 2
        const val PROGRAMMSET_FARBE = 3
        const val PROGRAMMSET_ZIEL_PFAD = 4
        const val PROGRAMMSET_ZIEL_DATEINAME = 5
        const val PROGRAMMSET_THEMA_ANLEGEN = 6
        const val PROGRAMMSET_IST_ABSPIELEN = 7
        const val PROGRAMMSET_IST_SPEICHERN = 8
        const val PROGRAMMSET_IST_BUTTON = 9
        const val PROGRAMMSET_IST_ABO = 10
        const val PROGRAMMSET_LAENGE_BESCHRAENKEN = 11
        const val PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN = 12
        const val PROGRAMMSET_MAX_LAENGE = 13
        const val PROGRAMMSET_MAX_LAENGE_FIELD = 14
        const val PROGRAMMSET_AUFLOESUNG = 15
        const val PROGRAMMSET_ADD_ON = 16
        const val PROGRAMMSET_BESCHREIBUNG = 17
        const val PROGRAMMSET_INFO_URL = 18
        const val PROGRAMMSET_INFODATEI = 19
        const val PROGRAMMSET_SPOTLIGHT = 20
        const val PROGRAMMSET_SUBTITLE = 21

        const val TAG = "Programmset"
        const val MAX_ELEM = 22

        val COLUMN_NAMES: Array<String> = arrayOf(
            "Setname",
            "Präfix",
            "Suffix",
            "Farbe",
            "Zielpfad",
            "Zieldateiname",
            "Thema anlegen",
            "Abspielen",
            "Speichern",
            "Button",
            "Abo",
            "Länge",
            "Länge Feld",
            "max Länge",
            "max Länge Feld",
            "Auflösung",
            "AddOn",
            "Beschreibung",
            "Url Info",
            "Infodatei",
            "Spotlight",
            "Untertitel",
        )

        val XML_NAMES: Array<String> = arrayOf(
            "Name",
            "Praefix",
            "Suffix",
            "Farbe",
            "Zielpfad",
            "Zieldateiname",
            "Thema-anlegen",
            "Abspielen",
            "Speichern",
            "Button",
            "Abo",
            "Laenge",
            "Laenge-Feld",
            "max-Laenge",
            "max-Laenge-Feld",
            "Aufloesung",
            "AddOn",
            "Beschreibung",
            "Info-URL",
            "Infodatei",
            "Spotlight",
            "Untertitel",
        )

        private val logger = LogManager.getLogger(DatenPset::class.java)

        private fun boolToString(value: Boolean?): String = value?.toString().orEmpty()

        private fun parseBoolean(value: String): Boolean? = value.ifEmpty { null }?.toBoolean()

        private fun intToString(value: Int?): String = value?.toString().orEmpty()

        private fun parseInteger(value: String): Int? = value.ifEmpty { null }?.toInt()

        private fun colorToString(value: Color?): String =
            value?.let { "${it.red},${it.green},${it.blue}" }.orEmpty()
    }
}
