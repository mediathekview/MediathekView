/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.gui.tasks;

public class LuceneIndexKeys {
    /**
     * In Abfragen nicht zu verwenden!
     */
    public static final String ID = "id";
    /**
     * String-Value
     */
    public static final String SENDER = "sender";
    /**
     * String-Value
     */
    public static final String TITEL = "titel";
    /**
     * String-Value
     */
    public static final String THEMA = "thema";
    /**
     * String-Value
     */
    public static final String BESCHREIBUNG = "beschreibung";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String LIVESTREAM = "livestream";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String HIGH_QUALITY = "highquality";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String SUBTITLE = "untertitel";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String TRAILER_TEASER = "trailerteaser";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String AUDIOVERSION = "audioversion";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String SIGN_LANGUAGE = "signlanguage";
    /**
     * Datum im Format "YYYYMMDD". String.
     * Nicht existente Werte sind "19000101".
     */
    public static final String SENDE_DATUM = "sendedatum";
    /**
     * Wochentag (Montag, Dienstag,...) des Sendedatums. String.
     */
    public static final String SENDE_WOCHENTAG = "wochentag";
    /**
     * Boolean Key ist nur vorhanden, wenn Bedingung erfüllt. Dann "true".
     */
    public static final String NEW = "neu";
    /**
     * Filmlänge in Sekunde. Integer-Value. 0 wenn nicht vorhanden.
     */
    public static final String FILM_LENGTH = "länge";
    /**
     * Filmgröße in Megabytes. Integer-Value. 0 wenn nicht vorhanden.
     */
    public static final String FILM_SIZE = "größe";
    /**
     * Boolean Key ist nur vorhanden, wenn Film als Duplikat klassifiziert wurde. Dann "true".
     */
    public static final String DUPLICATE = "duplicate";
    /**
     * Startzeit des Films HH:mm:ss. String.
     */
    public static final String START_TIME = "startzeit";
    /**
     * Season. Integer. 0 wenn nicht vorhanden.
     */
    public static final String SEASON = "season";
    /**
     * Episode. Integer. 0 wenn nicht vorhanden.
     */
    public static final String EPISODE = "episode";
}
