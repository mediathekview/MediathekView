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

import ca.odell.glazedlists.ThresholdList

/** Evaluates a numeric JavaBean property for a [ThresholdList]. */
internal class BeanThresholdEvaluator<E : Any>(
    private val propertyName: String,
) : ThresholdList.Evaluator<E> {
    private var beanProperty: BeanProperty<E>? = null

    override fun evaluate(element: E): Int {
        if (beanProperty == null) loadPropertyDescriptors(element.javaClass)
        return beanProperty!![element] as Int
    }

    @Suppress("UNCHECKED_CAST")
    private fun loadPropertyDescriptors(beanClass: Class<out E>) {
        beanProperty = BeanProperty(
            beanClass as Class<E>,
            propertyName,
            readable = true,
            writable = false,
        )
    }
}
