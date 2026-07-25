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

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.ThresholdList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class ThresholdRangeModelsTest {
    @Test
    fun lowerModelMapsValueAndMaximumToThresholds() {
        val target = thresholdList()
        val model = LowerThresholdRangeModel(target)
        var changes = 0
        model.addChangeListener { changes++ }

        model.setRangeProperties(15, 3, 0, 25, true)

        assertEquals(15, target.lowerThreshold)
        assertEquals(25, target.upperThreshold)
        assertEquals(15, model.value)
        assertEquals(25, model.maximum)
        assertEquals(0, model.minimum)
        assertEquals(3, model.extent)
        assertTrue(model.valueIsAdjusting)
        assertEquals(1, changes)
        target.dispose()
    }

    @Test
    fun upperModelMapsValueAndMinimumToThresholds() {
        val target = thresholdList()
        val model = UpperThresholdRangeModel(target)
        var changes = 0
        model.addChangeListener { changes++ }

        model.setRangeProperties(25, 4, 5, 30, true)

        assertEquals(5, target.lowerThreshold)
        assertEquals(25, target.upperThreshold)
        assertEquals(25, model.value)
        assertEquals(5, model.minimum)
        assertEquals(30, model.maximum)
        assertEquals(4, model.extent)
        assertTrue(model.valueIsAdjusting)
        assertEquals(1, changes)
        target.dispose()
    }

    @Test
    fun invalidRangesAreNormalizedBeforeUpdatingThresholds() {
        val lowerTarget = thresholdList()
        val lowerModel = LowerThresholdRangeModel(lowerTarget)
        lowerModel.setRangeProperties(30, 0, 40, 20, false)

        assertEquals(30, lowerTarget.lowerThreshold)
        assertEquals(30, lowerTarget.upperThreshold)
        assertEquals(20, lowerModel.minimum)
        assertFalse(lowerModel.valueIsAdjusting)

        val upperTarget = thresholdList()
        val upperModel = UpperThresholdRangeModel(upperTarget)
        upperModel.setRangeProperties(-10, 0, 0, 20, false)

        assertEquals(-10, upperTarget.lowerThreshold)
        assertEquals(-10, upperTarget.upperThreshold)
        assertEquals(20, upperModel.maximum)
        assertFalse(upperModel.valueIsAdjusting)

        lowerTarget.dispose()
        upperTarget.dispose()
    }

    @Test
    fun unchangedRangePropertiesDoNotFireAChangeEvent() {
        val target = thresholdList()
        val lowerModel = LowerThresholdRangeModel(target)
        lowerModel.setRangeProperties(10, 0, 0, 20, false)
        var changes = 0
        lowerModel.addChangeListener { changes++ }

        lowerModel.setRangeProperties(10, 0, 0, 20, false)

        assertEquals(0, changes)
        target.dispose()
    }

    private fun thresholdList(): ThresholdList<Int> =
        ThresholdList(BasicEventList<Int>()) { it }.apply {
            lowerThreshold = 10
            upperThreshold = 20
        }
}
