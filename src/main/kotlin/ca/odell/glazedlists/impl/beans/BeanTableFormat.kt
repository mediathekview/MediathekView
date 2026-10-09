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

import ca.odell.glazedlists.GlazedLists
import ca.odell.glazedlists.gui.AdvancedTableFormat
import ca.odell.glazedlists.gui.WritableTableFormat

/** Reflective table format for JavaBean-style objects. */
internal open class BeanTableFormat<E : Any>(
    beanClass: Class<E>?,
    @JvmField protected var propertyNames: Array<String>,
    @JvmField protected var columnLabels: Array<String>,
    private val editable: BooleanArray,
) : WritableTableFormat<E>, AdvancedTableFormat<E> {
    @JvmField
    protected var beanProperties: Array<BeanProperty<E>>? = null

    @JvmField
    protected var comparators: Array<Comparator<*>?> = arrayOfNulls(propertyNames.size)

    @JvmField
    protected var classes: Array<Class<*>> = Array(propertyNames.size) { Any::class.java }

    init {
        if (beanClass == null) {
            for (column in classes.indices) {
                classes[column] = Any::class.java
                comparators[column] = GlazedLists.comparableComparator<Comparable<Any?>>()
            }
        } else {
            loadPropertyDescriptors(beanClass)
            val properties = checkNotNull(beanProperties)
            for (column in classes.indices) {
                val rawClass = checkNotNull(properties[column].valueClass)
                classes[column] = primitiveToObjectMap.getOrDefault(rawClass, rawClass)
                comparators[column] =
                    if (Comparable::class.java.isAssignableFrom(classes[column])) {
                        GlazedLists.comparableComparator<Comparable<Any?>>()
                    } else {
                        null
                    }
            }
        }
    }

    constructor(beanClass: Class<E>?, propertyNames: Array<String>, columnLabels: Array<String>) :
        this(beanClass, propertyNames, columnLabels, BooleanArray(propertyNames.size))

    @Suppress("UNCHECKED_CAST")
    protected open fun loadPropertyDescriptors(beanClass: Class<E>) {
        beanProperties = java.lang.reflect.Array.newInstance(
            BeanProperty::class.java,
            propertyNames.size,
        ) as Array<BeanProperty<E>>
        val properties = checkNotNull(beanProperties)
        for (index in propertyNames.indices) {
            properties[index] = BeanProperty(beanClass, propertyNames[index], true, editable[index])
        }
    }

    override fun getColumnCount(): Int = columnLabels.size

    override fun getColumnName(column: Int): String = columnLabels[column]

    override fun getColumnValue(baseObject: E, column: Int): Any? {
        if (beanProperties == null) loadPropertyDescriptors(baseObject.javaClass)
        return checkNotNull(beanProperties)[column][baseObject]
    }

    override fun isEditable(baseObject: E, column: Int): Boolean = editable[column]

    override fun setColumnValue(baseObject: E, editedValue: Any?, column: Int): E {
        if (beanProperties == null) loadPropertyDescriptors(baseObject.javaClass)
        checkNotNull(beanProperties)[column].set(baseObject, editedValue)
        return baseObject
    }

    override fun getColumnClass(column: Int): Class<*> = classes[column]

    override fun getColumnComparator(column: Int): Comparator<*>? = comparators[column]

    private companion object {
        @JvmField
        protected val primitiveToObjectMap: Map<Class<*>, Class<*>> = java.util.Map.of(
            Boolean::class.javaPrimitiveType!!, Boolean::class.javaObjectType,
            Char::class.javaPrimitiveType!!, Char::class.javaObjectType,
            Byte::class.javaPrimitiveType!!, Byte::class.javaObjectType,
            Short::class.javaPrimitiveType!!, Short::class.javaObjectType,
            Int::class.javaPrimitiveType!!, Int::class.javaObjectType,
            Long::class.javaPrimitiveType!!, Long::class.javaObjectType,
            Float::class.javaPrimitiveType!!, Float::class.javaObjectType,
            Double::class.javaPrimitiveType!!, Double::class.javaObjectType,
        )
    }
}
