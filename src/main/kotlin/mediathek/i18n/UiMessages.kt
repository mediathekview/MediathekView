/*
 * Copyright (c) 2026 Tony Chemit.
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

package mediathek.i18n

import java.text.MessageFormat
import java.util.ResourceBundle

/**
 * Loads interface text for the language selected at startup without changing the JVM default locale.
 * The base bundle contains German; missing translations inherit it without using the host OS language.
 * Created on 17/10/2026.
 * @author Tony Chemit
 * @since 15.0.0
 */
object UiMessages {
    private const val BASE_NAME = "mediathek.i18n.Messages"
    private val control = ResourceBundle.Control.getNoFallbackControl(ResourceBundle.Control.FORMAT_PROPERTIES)

    @Volatile
    var language: UiLanguage = UiLanguage.GERMAN
        private set

    /** Selects the session language before creating Swing components; preference changes require a restart. */
    fun initialize(language: UiLanguage) {
        this.language = language
    }

    /** Supplies the same explicit locale to Kotlin code and JFormDesigner-generated Java forms. */
    @JvmStatic
    fun getBundle(baseName: String): ResourceBundle = ResourceBundle.getBundle(baseName, language.locale, control)

    /** Looks up a label, including inherited German text when its English translation is absent. */
    @JvmStatic
    fun text(key: String): String = getBundle(BASE_NAME).getString(key)

    /** Formats a complete translated message; MessageFormat also supports choice patterns for counts. */
    @JvmStatic
    fun format(key: String, vararg arguments: Any?): String =
        MessageFormat(text(key), language.locale).format(arguments)
}
