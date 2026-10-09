/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.gui.tasks

import mediathek.gui.tasks.LuceneIndexKeys.ID


object LuceneIndexKeys {
    /**
     * In Abfragen nicht zu verwenden!
     */
    const val ID = "id"

    /**
     * Numeric doc-value mirror of [ID] for efficient result collection.
     * In Abfragen nicht zu verwenden!
     */
    const val ID_DOC_VALUE = "id_doc_value"

    /**
     * String-Value
     */
    const val SENDER = "sender"

    /**
     * String-Value
     */
    const val TITEL = "titel"

    /**
     * String-Value
     */
    const val THEMA = "thema"

    /**
     * String-Value
     */
    const val BESCHREIBUNG = "beschreibung"

    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    const val LIVESTREAM = "livestream"

    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    const val HIGH_QUALITY = "highquality"

    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    const val SUBTITLE = "untertitel"

    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    const val TRAILER_TEASER = "trailerteaser"

    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    const val AUDIOVERSION = "audioversion"

    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    const val SIGN_LANGUAGE = "signlanguage"

    /**
     * Datum im Format "YYYYMMDD". String.
     * Nicht existente Werte sind "19000101".
     */
    const val SENDE_DATUM = "sendedatum"

    /**
     * Wochentag (Montag, Dienstag,...) des Sendedatums. String.
     */
    const val SENDE_WOCHENTAG = "wochentag"

    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    const val NEW = "neu"

    /**
     * Filmlänge in Sekunde. Integer-Value. 0 wenn nicht vorhanden.
     */
    const val FILM_LENGTH = "länge"

    /**
     * Filmgröße in Megabytes. Integer-Value. 0 wenn nicht vorhanden.
     */
    const val FILM_SIZE = "größe"

    /**
     * Boolean Key ist nur vorhanden, wenn Film als Duplikat klassifiziert wurde. Dann "true".
     */
    const val DUPLICATE = "duplicate"

    /**
     * Startzeit des Films HH:mm:ss. String.
     */
    const val START_TIME = "startzeit"

    /**
     * Season. Integer. 0 wenn nicht vorhanden.
     */
    const val SEASON = "season"

    /**
     * Episode. Integer. 0 wenn nicht vorhanden.
     */
    const val EPISODE = "episode"
}
