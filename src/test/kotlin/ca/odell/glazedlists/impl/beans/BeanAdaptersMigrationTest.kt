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
package ca.odell.glazedlists.impl.beans

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test

internal class BeanAdaptersMigrationTest {
    @Test
    fun beanFunctionsPreserveRawAndStringValuesIncludingNull() {
        val bean = SampleBean(7, null, Child("nested"))
        val rawFunction = BeanFunction<SampleBean, Int>(SampleBean::class.java, "count")
        val stringFunction = StringBeanFunction(SampleBean::class.java, "count")
        val nullableStringFunction = StringBeanFunction(SampleBean::class.java, "label")

        assertEquals(7, rawFunction(bean))
        assertEquals("7", stringFunction(bean))
        assertNull(nullableStringFunction(bean))
    }

    @Test
    fun thresholdEvaluatorLoadsItsPropertyLazilyAndReusesIt() {
        val evaluator = BeanThresholdEvaluator<SampleBean>("count")

        assertEquals(3, evaluator.evaluate(SampleBean(3, "first", null)))
        assertEquals(9, evaluator.evaluate(SampleBean(9, "second", null)))
    }

    @Test
    fun beanTextFilteratorAppendsNonNullNestedValuesAndIgnoresNullElements() {
        val filterator = BeanTextFilterator<Any, SampleBean?>("count", "label", "child.name")
        val strings = mutableListOf("existing")
        val values = mutableListOf<Any>()
        val bean = SampleBean(4, null, Child("nested"))

        filterator.getFilterStrings(strings, bean)
        filterator.getFilterStrings(strings, null)
        filterator.getFilterValues(values, bean)
        filterator.getFilterValues(values, null)

        assertEquals(listOf("existing", "4", "nested"), strings)
        assertEquals(listOf(4, "nested"), values)
    }

    @Test
    fun eagerBeanTextFilteratorConstructorUsesTheSuppliedBeanClass() {
        val filterator = BeanTextFilterator<Any, SampleBean>(
            SampleBean::class.java,
            "count",
            "label",
        )
        val values = mutableListOf<Any>()

        filterator.getFilterValues(values, SampleBean(5, "value", null))

        assertEquals(listOf(5, "value"), values)
    }

    @Suppress("unused")
    private class SampleBean(
        var count: Int,
        var label: String?,
        var child: Child?,
    )

    @Suppress("unused")
    private class Child(var name: String?)
}
