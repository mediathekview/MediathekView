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

import ca.odell.glazedlists.Filterator
import ca.odell.glazedlists.TextFilterator

/** Extracts configured JavaBean properties for generic and text filtering. */
internal open class BeanTextFilterator<D, E> : TextFilterator<E>, Filterator<D, E> {
    private val propertyNames: Array<out String>
    private var beanProperties: Array<BeanProperty<E>>? = null

    constructor(vararg propertyNames: String) {
        this.propertyNames = propertyNames
    }

    constructor(beanClass: Class<E>, vararg propertyNames: String) {
        this.propertyNames = propertyNames
        loadPropertyDescriptors(beanClass)
    }

    override fun getFilterStrings(baseList: MutableList<String>, element: E) {
        if (element == null) return
        ensurePropertiesLoaded(element)
        beanProperties!!.forEach { property ->
            property[element]?.toString()?.let(baseList::add)
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun getFilterValues(baseList: MutableList<D>, element: E) {
        if (element == null) return
        ensurePropertiesLoaded(element)
        beanProperties!!.forEach { property ->
            property[element]?.let { baseList.add(it as D) }
        }
    }

    @Suppress("UNCHECKED_CAST")
    private fun ensurePropertiesLoaded(element: E) {
        if (beanProperties == null) {
            loadPropertyDescriptors((element as Any).javaClass as Class<E>)
        }
    }

    private fun loadPropertyDescriptors(beanClass: Class<E>) {
        beanProperties = Array(propertyNames.size) { index ->
            BeanProperty(beanClass, propertyNames[index], readable = true, writable = false)
        }
    }
}
