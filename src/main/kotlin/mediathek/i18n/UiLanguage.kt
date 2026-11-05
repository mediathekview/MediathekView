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

import java.util.Locale

/**
 * Supported interface languages with stable configuration codes and native display names.
 * Created on 17/10/2026.
 * @author Tony Chemit
 * @since 15.0.0
 */
enum class UiLanguage(val code: String, val locale: Locale, private val displayName: String) {
    GERMAN("de", Locale.GERMAN, "Deutsch"),
    ENGLISH("en", Locale.ENGLISH, "English");

    /** Returns the language name independently of the currently selected interface language. */
    override fun toString(): String = displayName

    companion object {
        /** Restores a saved code, retaining German for missing or unsupported preferences. */
        fun fromCode(code: String?): UiLanguage = entries.firstOrNull { it.code == code } ?: GERMAN
    }
}
