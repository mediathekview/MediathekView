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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see http://www.gnu.org/licenses/.
 */
package ca.odell.glazedlists

import java.time.LocalTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.util.*

/**
 * A factory for creating Sequencers.
 *
 * @author James Lemieux
 */
object Sequencers {
    fun monthSequencer(): SequenceList.Sequencer<Date> = MonthSequencer()

    /**
     * This Sequencer produces a sequence of [Date] objects normalized
     * to the first millisecond of each month.
     */
    private class MonthSequencer : SequenceList.Sequencer<Date> {
        private val zoneId = ZoneId.systemDefault()

        override fun previous(value: Date): Date {

            var dateTime = value.toInstant().atZone(zoneId)
            if (dateTime.dayOfMonth == 1 && dateTime.toLocalTime() == LocalTime.MIDNIGHT) {
                dateTime = dateTime.minusMonths(1)
            }

            return monthStart(dateTime)
        }

        override fun next(value: Date): Date {

            return monthStart(value.toInstant().atZone(zoneId).plusMonths(1))
        }

        private fun monthStart(dateTime: ZonedDateTime): Date {
            val monthStart = dateTime.toLocalDate().withDayOfMonth(1).atStartOfDay()
            val preferredOffset = zoneId.rules.getValidOffsets(monthStart).lastOrNull()
            return Date.from(ZonedDateTime.ofLocal(monthStart, zoneId, preferredOffset).toInstant())
        }
    }
}
