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
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test

@Suppress("unused") // fixture accessors are invoked reflectively by BeanProperty
internal class BeanPropertyTypeResolutionTest {
    @Test
    fun `inherited generic getter resolves its concrete return type`() {
        val property = BeanProperty(StringValueBean::class.java, "value", true, false)

        assertSame(String::class.java, property.valueClass)
    }

    @Test
    fun `inherited generic setter resolves its concrete parameter type`() {
        val property = BeanProperty(StringSetterBean::class.java, "value", false, true)

        assertSame(String::class.java, property.valueClass)
    }

    @Test
    fun `dotted property resolves an inherited generic intermediate getter`() {
        val property = BeanProperty(StringContainer::class.java, "nested.label", true, false)

        assertSame(String::class.java, property.valueClass)
        assertEquals("nested value", property.get(StringContainer()))
    }

    @Test
    fun `parameterized getter resolves to its raw return class`() {
        val property = BeanProperty(StringListBean::class.java, "values", true, false)

        assertSame(List::class.java, property.valueClass)
    }

    @Test
    fun `unresolved generic getter falls back to Object`() {
        val property = BeanProperty(GenericValueBean::class.java, "value", true, false)

        assertSame(Any::class.java, property.valueClass)
    }

    @Test
    fun `inherited generic array getter resolves its concrete array type`() {
        val property = BeanProperty(StringArrayBean::class.java, "values", true, false)

        assertSame(emptyArray<String>().javaClass, property.valueClass)
    }

    private open class GenericValueBean<T>(private val storedValue: T) {
        fun getValue(): T = storedValue
    }

    private class StringValueBean : GenericValueBean<String>("")

    private open class GenericSetterBean<T> {
        fun setValue(@Suppress("UNUSED_PARAMETER") value: T) = Unit
    }

    private class StringSetterBean : GenericSetterBean<String>()

    private open class GenericContainer<T>(private val storedNested: T) {
        fun getNested(): T = storedNested
    }

    private class StringContainer : GenericContainer<NestedBean>(NestedBean())

    private class NestedBean {
        fun getLabel(): String = "nested value"
    }

    private open class GenericListBean<T>(private val storedValues: List<T>) {
        fun getValues(): List<T> = storedValues
    }

    private class StringListBean : GenericListBean<String>(emptyList())

    private open class GenericArrayBean<T>(private val storedValues: Array<T>) {
        fun getValues(): Array<T> = storedValues
    }

    private class StringArrayBean : GenericArrayBean<String>(emptyArray())
}
