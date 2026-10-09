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
package ca.odell.glazedlists.impl.swing

import ca.odell.glazedlists.ThresholdList
import javax.swing.BoundedRangeModel
import javax.swing.DefaultBoundedRangeModel

/** Binds a Swing range model's value and maximum to a threshold list's lower and upper bounds. */
internal open class LowerThresholdRangeModel(
    private val target: ThresholdList<*>?,
) : DefaultBoundedRangeModel(), BoundedRangeModel {
    override fun getMaximum(): Int {
        val currentTarget = target!!
        currentTarget.readWriteLock.readLock().lock()
        return try {
            currentTarget.upperThreshold
        } finally {
            currentTarget.readWriteLock.readLock().unlock()
        }
    }

    override fun getValue(): Int {
        val currentTarget = target!!
        currentTarget.readWriteLock.readLock().lock()
        return try {
            currentTarget.lowerThreshold
        } finally {
            currentTarget.readWriteLock.readLock().unlock()
        }
    }

    override fun setRangeProperties(
        newValue: Int,
        newExtent: Int,
        newMin: Int,
        newMax: Int,
        adjusting: Boolean,
    ) {
        val currentTarget = target!!
        currentTarget.readWriteLock.writeLock().lock()
        try {
            var normalizedMin = newMin
            var normalizedMax = newMax
            if (normalizedMin > normalizedMax) normalizedMin = normalizedMax
            if (newValue > normalizedMax) normalizedMax = newValue
            if (newValue < normalizedMin) normalizedMin = newValue

            var changed =
                newExtent != extent ||
                    normalizedMin != minimum ||
                    adjusting != valueIsAdjusting

            if (newValue != value) {
                currentTarget.lowerThreshold = newValue
                changed = true
            }
            if (normalizedMax != maximum) {
                currentTarget.upperThreshold = normalizedMax
                changed = true
            }

            if (changed) {
                super.setRangeProperties(newValue, newExtent, normalizedMin, normalizedMax, adjusting)
            }
        } finally {
            currentTarget.readWriteLock.writeLock().unlock()
        }
    }
}
