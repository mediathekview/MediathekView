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

package mediathek.config

import com.formdev.flatlaf.FlatLaf
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.tool.MVC
import org.apache.logging.log4j.LogManager
import java.awt.Color
import java.util.regex.Pattern
import javax.swing.UIManager
import kotlin.io.path.createDirectories
import kotlin.io.path.exists
import kotlin.io.path.readText
import kotlin.io.path.writeText
import kotlin.math.min

object MVColor {
    const val MVC_TEXT = 0
    const val MVC_COLOR = 1

    private val logger = LogManager.getLogger(MVColor::class.java)
    private val darkBlue = rgb(137, 192, 255)
    private const val STORAGE_FILENAME = "app-colors.json"
    private const val FILE_VERSION = 1
    private const val LEGACY_SEPARATOR = "#=#"
    private val json = Json {
        prettyPrint = true
        prettyPrintIndent = "  "
        ignoreUnknownKeys = true
    }

        val FILM_HISTORY = color("film_history", rgb(225, 225, 225), rgb(70, 70, 70), "Filme, gesehen")

        val FILM_BOOKMARKED = color("film_bookmarked", rgb(204, 238, 255), rgb(33, 66, 84), "Filme, gemerkt")

        val FILM_DUPLICATE = color("film_duplicate", rgb(255, 209, 220), rgb(92, 52, 67), "Film, Duplikat")

        val NEW_COLOR = color("film_new", Color.BLUE, darkBlue, "Film, neu")

        val REGEX_PATTERN_COLOR = color("filter_regex", Color.BLUE, darkBlue, "Filter, Regex")

        val SELECTED_COLOR = color("selected_icon", Color.BLUE, darkBlue, "Auswahl, Icon")

        val ALTERNATE_ROW_COLOR = color("table_alternate_row", rgb(247, 247, 247), rgb(51, 51, 51), "Tabelle, Alternierende Zeilen")

        val DOWNLOAD_IST_ABO = color("download_is_abo", rgb(138, 67, 0), rgb(255, 191, 117), "Download ist ein Abo")

        val DOWNLOAD_IST_DIREKTER_DOWNLOAD = color("download_is_direct", rgb(0, 72, 138), darkBlue, "Download ist ein direkter Download")

        val DOWNLOAD_WAIT = color("download_wait", rgb(239, 244, 255), rgb(49, 56, 72), "Download, noch nicht gestartet")

        val DOWNLOAD_WAIT_SEL = color("download_wait_selected", rgb(199, 206, 222), rgb(72, 82, 103), "Download, noch nicht gestartet, selektiert")

        val DOWNLOAD_RUN = color("download_run", rgb(241, 228, 188), rgb(88, 74, 39), "Download, läuft")

        val DOWNLOAD_RUN_SEL = color("download_run_selected", rgb(206, 178, 92), rgb(122, 102, 54), "Download, läuft, selektiert")

        val DOWNLOAD_FERTIG = color("download_finished", rgb(188, 241, 195), rgb(43, 88, 50), "Download, fertig")

        val DOWNLOAD_FERTIG_SEL = color("download_finished_selected", rgb(115, 206, 92), rgb(60, 123, 67), "Download, fertig, selektiert")

        val DOWNLOAD_FEHLER = color("download_error", rgb(241, 188, 221), rgb(102, 53, 82), "Download, fehlerhaft")

        val DOWNLOAD_FEHLER_SEL = color("download_error_selected", rgb(206, 92, 128), rgb(143, 67, 95), "Download, fehlerhaft, selektiert")

        val DOWNLOAD_DATEINAME_NEU = color("download_filename_new", rgb(0, 140, 0), rgb(114, 212, 117), "Download, Dateiname ist neu")

        val DOWNLOAD_DATEINAME_ALT = color("download_filename_old", rgb(0, 0, 200), darkBlue, "Download, Dateiname ist der alte")

        private val colors = listOf(
            FILM_HISTORY,
            FILM_BOOKMARKED,
            FILM_DUPLICATE,
            NEW_COLOR,
            REGEX_PATTERN_COLOR,
            SELECTED_COLOR,
            ALTERNATE_ROW_COLOR,
            DOWNLOAD_IST_ABO,
            DOWNLOAD_IST_DIREKTER_DOWNLOAD,
            DOWNLOAD_WAIT,
            DOWNLOAD_WAIT_SEL,
            DOWNLOAD_RUN,
            DOWNLOAD_RUN_SEL,
            DOWNLOAD_FERTIG,
            DOWNLOAD_FERTIG_SEL,
            DOWNLOAD_FEHLER,
            DOWNLOAD_FEHLER_SEL,
            DOWNLOAD_DATEINAME_NEU,
            DOWNLOAD_DATEINAME_ALT
        )

        private val colorsByKey = colors.associateBy(MVC::key)

        private val legacyKeysByColorKey = mapOf(
            FILM_HISTORY.key to "FARBE_FILM_HISTORY",
            FILM_BOOKMARKED.key to "FARBE_FILM_BOOKMARKED",
            FILM_DUPLICATE.key to "FARBE_FILM_DUPLICATE",
            NEW_COLOR.key to "FARBE_FILM_NEU",
            REGEX_PATTERN_COLOR.key to "FARBE_FILTER_REGEX",
            SELECTED_COLOR.key to "FARBE_SELECTED_ICON",
            ALTERNATE_ROW_COLOR.key to "FARBE_TABLE_ALTERNATE_ROW",
            DOWNLOAD_IST_ABO.key to "FARBE_DOWNLOAD_IST_ABO",
            DOWNLOAD_IST_DIREKTER_DOWNLOAD.key to "FARBE_DOWNLOAD_IST_DIREKTER_DOWNLOAD",
            DOWNLOAD_WAIT.key to "FARBE_DOWNLOAD_WAIT",
            DOWNLOAD_WAIT_SEL.key to "FARBE_DOWNLOAD_WAIT_SEL",
            DOWNLOAD_RUN.key to "FARBE_DOWNLOAD_RUN",
            DOWNLOAD_RUN_SEL.key to "FARBE_DOWNLOAD_RUN_SEL",
            DOWNLOAD_FERTIG.key to "FARBE_DOWNLOAD_FERTIG",
            DOWNLOAD_FERTIG_SEL.key to "FARBE_DOWNLOAD_FERTIG_SEL",
            DOWNLOAD_FEHLER.key to "FARBE_DOWNLOAD_FEHLER",
            DOWNLOAD_FEHLER_SEL.key to "FARBE_DOWNLOAD_FEHLER_SEL",
            DOWNLOAD_DATEINAME_NEU.key to "FARBE_DOWNLOAD_DATEINAME_NEU",
            DOWNLOAD_DATEINAME_ALT.key to "FARBE_DOWNLOAD_DATEINAME_ALT"
        )

        private val colorKeysByLegacyKey = legacyKeysByColorKey.entries.associate { (colorKey, legacyKey) ->
            legacyKey to colorKey
        }

        private fun color(key: String, lightDefault: Color, darkDefault: Color, label: String): MVC =
            MVC(key, lightDefault, darkDefault, label)

        private fun rgb(red: Int, green: Int, blue: Int) = Color(red, green, blue)

        fun getRegExPatternColor(): Color = REGEX_PATTERN_COLOR.color

        fun getBlueColor(): Color = getRegExPatternColor()

        fun getAlternatingRowColor(): Color {
            if (!FlatLaf.isLafDark()) {
                return ALTERNATE_ROW_COLOR.color
            }

            if (ALTERNATE_ROW_COLOR.hasOverride()) {
                return ALTERNATE_ROW_COLOR.color
            }

            return brightenColor(UIManager.getColor("Table.background"), 0.25f)
        }

        private fun brightenColor(originalColor: Color, factor: Float): Color {
            val hsb = Color.RGBtoHSB(originalColor.red, originalColor.green, originalColor.blue, null)
            return Color.getHSBColor(hsb[0], hsb[1], min(1f, hsb[2] + factor))
        }

    private fun storagePath() = StandardLocations.getSettingsDirectory().resolve(STORAGE_FILENAME)

    fun load() {
        reset()

        val storagePath = storagePath()
        if (storagePath.exists()) {
            loadFromJson(storagePath)
        }
    }

    @JvmStatic
    fun save() {
        val storagePath = storagePath()
        try {
            storagePath.parent?.createDirectories()
            storagePath.writeText(json.encodeToString(serializeOverrides()))
        } catch (ex: Exception) {
            logger.error("save colors", ex)
        }
    }

    fun get(index: Int): MVC = colors[index]

    @JvmStatic
    fun getColors(): List<MVC> = colors

    fun reset() {
        colors.forEach(MVC::reset)
    }

    private fun serializeOverrides(): AppColorsFile =
        AppColorsFile(
            version = FILE_VERSION,
            colors = colors
                .filter(MVC::hasOverride)
                .associate { mvc ->
                    mvc.key to ColorOverride(
                        light = mvc.takeIf { it.hasOverride(false) }?.getOverrideColor(false)?.toColorComponents(),
                        dark = mvc.takeIf { it.hasOverride(true) }?.getOverrideColor(true)?.toColorComponents()
                    )
                }
        )

    private fun loadFromJson(storagePath: java.nio.file.Path): Boolean =
        try {
            val parsed = json.decodeFromString<AppColorsFile>(storagePath.readText())
            parsed.colors.forEach { (colorKey, override) ->
                val mvc = colorsByKey[colorKey] ?: return@forEach
                override.light?.let { mvc.setColor(false, it.toColor()) }
                override.dark?.let { mvc.setColor(true, it.toColor()) }
            }
            true
        } catch (ex: Exception) {
            logger.error("load colors from {}", storagePath, ex)
            false
        }

    internal fun isLegacyColorKey(legacyKey: String): Boolean =
        legacyKey in colorKeysByLegacyKey

    internal fun migrateLegacyColors(legacyValues: Map<String, String>): Boolean {
        if (legacyValues.isEmpty()) {
            return false
        }

        val storagePath = storagePath()
        if (storagePath.exists() && loadFromJson(storagePath)) {
            reset()
            return false
        }

        reset()
        var migrated = false
        legacyValues.forEach { (legacyKey, legacyValue) ->
            val colorKey = colorKeysByLegacyKey[legacyKey] ?: return@forEach
            val mvc = colorsByKey[colorKey] ?: return@forEach
            val hadOverride = mvc.hasOverride()
            applyLegacyValue(mvc, legacyValue)
            migrated = migrated || (!hadOverride && mvc.hasOverride())
        }

        if (migrated) {
            save()
        }
        reset()

        return migrated
    }

    private fun applyLegacyValue(mvc: MVC, legacyValue: String?) {
        if (legacyValue.isNullOrBlank()) {
            return
        }

        val parts = Pattern.compile(Pattern.quote(LEGACY_SEPARATOR)).split(legacyValue, -1).toList()
        try {
            if (parts.size == 1) {
                mvc.set(Color(parts[0].toInt()))
                return
            }

            applyLegacyColorPart(parts, 0) { mvc.setColor(false, it) }
            applyLegacyColorPart(parts, 1) { mvc.setColor(true, it) }
        } catch (ex: RuntimeException) {
            logger.warn("Ignoring invalid legacy color value '{}' for {}", legacyValue, mvc.key, ex)
        }
    }

    private fun applyLegacyColorPart(parts: List<String>, index: Int, apply: (Color) -> Unit) {
        if (index >= parts.size || parts[index].isBlank()) {
            return
        }
        apply(Color(parts[index].toInt()))
    }

    private fun Color.toColorComponents() = ColorComponents(
        r = red,
        g = green,
        b = blue,
        a = alpha
    )

    private fun ColorComponents.toColor(): Color = Color(r, g, b, a)

    @Serializable
    private data class AppColorsFile(
        val version: Int = FILE_VERSION,
        val colors: Map<String, ColorOverride> = emptyMap()
    )

    @Serializable
    private data class ColorOverride(
        val light: ColorComponents? = null,
        val dark: ColorComponents? = null
    )

    @Serializable
    private data class ColorComponents(
        val r: Int,
        val g: Int,
        val b: Int,
        val a: Int
    )
}
