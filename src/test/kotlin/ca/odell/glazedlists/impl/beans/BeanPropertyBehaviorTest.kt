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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.impl.beans

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.lang.reflect.UndeclaredThrowableException

internal class BeanPropertyBehaviorTest {
    @Test
    fun identityPropertyReturnsTheOriginalInstanceAndCannotBeWritable() {
        val bean = MutableBean("value")
        val property = BeanProperty(MutableBean::class.java, "this", readable = true, writable = false)

        assertSame(bean, property[bean])
        assertSame(MutableBean::class.java, property.valueClass)
        assertTrue(property.isReadable)
        assertFalse(property.isWritable)

        val failure = assertThrows(IllegalArgumentException::class.java) {
            BeanProperty(MutableBean::class.java, "this", readable = true, writable = true)
        }
        assertEquals("The identity property name (this) cannot be writable", failure.message)
    }

    @Test
    fun nestedGetterAndSetterStopAtANullIntermediateValue() {
        val bean = ParentBean(null)
        val readable = BeanProperty(ParentBean::class.java, "child.value", readable = true, writable = false)
        val writable = BeanProperty(ParentBean::class.java, "child.value", readable = false, writable = true)

        assertNull(readable[bean])
        assertNull(writable.set(bean, "replacement"))
        assertNull(bean.child)
    }

    @Test
    fun setterMutatesTheBeanAndPreservesTheHelpfulTypeMismatchMessage() {
        val bean = MutableBean("before")
        val property = BeanProperty(MutableBean::class.java, "value", readable = true, writable = true)

        assertNull(property.set(bean, "after"))
        assertEquals("after", bean.value)

        val failure = assertThrows(IllegalArgumentException::class.java) {
            property.set(bean, 42)
        }
        assertEquals(
            "MutableBean.setValue(String) cannot be called with an instance of Integer",
            failure.message,
        )
    }

    @Test
    fun getterAndSetterFailuresExposeTheOriginalCause() {
        val getter = BeanProperty(ThrowingBean::class.java, "value", readable = true, writable = false)
        val setter = BeanProperty(ThrowingBean::class.java, "value", readable = false, writable = true)

        val getFailure = assertThrows(UndeclaredThrowableException::class.java) {
            getter[ThrowingBean()]
        }
        assertEquals("get failed", getFailure.cause?.message)

        val setFailure = assertThrows(UndeclaredThrowableException::class.java) {
            setter.set(ThrowingBean(), "ignored")
        }
        assertEquals("set failed", setFailure.cause?.message)
    }

    @Test
    fun equalityUsesTheConcreteClassBeanClassAndPropertyName() {
        val first = BeanProperty(MutableBean::class.java, "value", readable = true, writable = false)
        val equal = BeanProperty(MutableBean::class.java, "value", readable = false, writable = true)
        val different = BeanProperty(MutableBean::class.java, "this", readable = true, writable = false)

        assertEquals(first, equal)
        assertEquals(first.hashCode(), equal.hashCode())
        assertNotEquals(first, different)
    }

    @Test
    fun trailingPropertySeparatorsRetainJavaSplitSemantics() {
        val bean = MutableBean("value")
        val property = BeanProperty(MutableBean::class.java, "value.", readable = true, writable = false)

        assertEquals("value", property[bean])
        assertEquals(String::class.java, property.valueClass)
    }

    @Suppress("unused")
    private class MutableBean(var value: String)

    @Suppress("unused")
    private class ParentBean(var child: MutableBean?)

    @Suppress("unused")
    private class ThrowingBean {
        fun getValue(): String = error("get failed")

        fun setValue(@Suppress("UNUSED_PARAMETER") value: String) {
            error("set failed")
        }
    }
}
