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

package mediathek.gui.tabs.tab_film.filter

import org.apache.logging.log4j.LogManager
import java.text.ParseException
import javax.swing.JFormattedTextField
import javax.swing.JSpinner
import javax.swing.SpinnerNumberModel

class ZeitraumSpinner : JSpinner(SpinnerNumberModel(INFINITE_VALUE, INFINITE_VALUE, MAX_VALUE, STEP_SIZE)) {
    companion object {
        const val INFINITE_TEXT = "∞"
        const val INFINITE_VALUE = 0
        private const val MAX_VALUE = 365
        private const val STEP_SIZE = 1
        private val logger = LogManager.getLogger(ZeitraumSpinner::class.java)
    }

    init {
        configureFormatter()
    }

    fun restoreValue(zeitraumVal: String, fallbackWriter: (String) -> Unit) {
        runCatching {
            value = parseToSpinnerValue(zeitraumVal)
        }.onFailure { exception ->
            if (exception is NumberFormatException) {
                logger.error("Failed to parse zeitraum value: {}", zeitraumVal, exception)
                logger.error("Using default value: {}", INFINITE_VALUE)
                value = INFINITE_VALUE
                fallbackWriter(INFINITE_TEXT)
            } else {
                throw exception
            }
        }
    }

    fun installValueChangeListener(valueConsumer: (String) -> Unit) {
        addChangeListener {
            valueConsumer(currentValueAsText())
        }
    }

    private fun configureFormatter() {
        (editor as DefaultEditor).textField.formatterFactory = object : JFormattedTextField.AbstractFormatterFactory() {
            override fun getFormatter(textField: JFormattedTextField): JFormattedTextField.AbstractFormatter {
                val existingFormatter = textField.formatter
                return existingFormatter as? ZeitraumSpinnerFormatter ?: ZeitraumSpinnerFormatter
            }
        }
    }

    private fun currentValueAsText(): String = toDisplayText(value as Int)

    private fun parseToSpinnerValue(rawValue: String): Int {
        return if (rawValue == INFINITE_TEXT) {
            INFINITE_VALUE
        } else {
            rawValue.toInt()
        }
    }

    private fun toDisplayText(spinnerValue: Int): String {
        return if (spinnerValue == INFINITE_VALUE) {
            INFINITE_TEXT
        } else {
            spinnerValue.toString()
        }
    }

    private object ZeitraumSpinnerFormatter : JFormattedTextField.AbstractFormatter() {
        private fun readResolve(): Any = ZeitraumSpinnerFormatter
        override fun stringToValue(text: String): Any {
            return try {
                if (text == INFINITE_TEXT) {
                    INFINITE_VALUE
                } else {
                    text.toInt()
                }
            } catch (_: NumberFormatException) {
                throw parseExceptionFor(text)
            }
        }

        override fun valueToString(value: Any?): String {
            return when (value) {
                INFINITE_VALUE -> INFINITE_TEXT
                else -> value.toString()
            }
        }

        private fun parseExceptionFor(text: String): ParseException {
            text.forEachIndexed { index, char ->
                if (!char.isDigit()) {
                    return ParseException("Not a digit.", index)
                }
            }
            return ParseException("Failed to parse input \"$text\".", 0)
        }
    }
}
