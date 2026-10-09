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
package ca.odell.glazedlists

/** A writable, sorted range view selected by integer-valued thresholds. */
@Suppress(
    "INAPPLICABLE_JVM_NAME",
    "UNCHECKED_CAST",
)
class ThresholdList<E> private constructor(
    sortedSource: SortedList<E>,
    evaluator: Evaluator<E>,
) : RangeList<E>(sortedSource) {
    private var lowerThresholdValue = Int.MIN_VALUE
    private var upperThresholdValue = Int.MAX_VALUE

    var lowerThreshold: Int
        get() = lowerThresholdValue
        set(value) {
            lowerThresholdValue = value
            adjustRange()
        }

    var upperThreshold: Int
        get() = upperThresholdValue
        set(value) {
            upperThresholdValue = value
            adjustRange()
        }

    val evaluator: Evaluator<E> = evaluator
    private val sortedSource: SortedList<E> = sortedSource

    /** Creates a threshold list based on an integer JavaBean property. */
    constructor(source: EventList<E>, propertyName: String) :
            this(source, GlazedLists.thresholdEvaluator<E>(propertyName) as Evaluator<E>)

    /** Creates a threshold list based on [evaluator]. */
    constructor(source: EventList<E>, evaluator: Evaluator<E>) :
            this(SortedList(source, ThresholdComparator(evaluator)), evaluator)

    fun setLowerThreshold(element: E) {
        lowerThreshold = evaluator.evaluate(element)
    }

    fun setUpperThreshold(element: E) {
        upperThreshold = evaluator.evaluate(element)
    }

    override fun contains(element: E): Boolean {
        return withinRange(element) && source!!.contains(element)
    }

    override fun indexOf(element: E): Int {
        if (!withinRange(element)) return -1
        return source!!.indexOf(element)
    }

    override fun lastIndexOf(element: E): Int {
        if (!withinRange(element)) return -1
        return source!!.lastIndexOf(element)
    }

    private fun withinRange(element: E): Boolean {
        val evaluation = evaluator.evaluate(element)
        return evaluation in lowerThreshold..upperThreshold
    }

    override fun setHeadRange(startIndex: Int, endIndex: Int) {
        lowerThresholdValue = sourceIndexToThreshold(startIndex)
        upperThresholdValue = sourceIndexToThreshold(endIndex)
        adjustRange()
    }

    override fun setTailRange(startIndex: Int, endIndex: Int) {
        lowerThresholdValue = sourceIndexToThreshold(source!!.size - startIndex)
        upperThresholdValue = sourceIndexToThreshold(source!!.size - endIndex)
        adjustRange()
    }

    private fun sourceIndexToThreshold(sourceIndex: Int): Int =
        when {
            sourceIndex < 0 -> Int.MIN_VALUE
            sourceIndex < source!!.size -> evaluator.evaluate(source!![sourceIndex])
            else -> Int.MIN_VALUE
        }

    override val startIndex: Int
        get() = sortedSource.sortIndex(lowerThreshold as E)

    override val endIndex: Int
        get() {
            var index = sortedSource.lastSortIndex(upperThreshold as E)
            if (index < sortedSource.size && evaluator.evaluate(sortedSource[index]) == upperThreshold) {
                index++
            }
            return index
        }

    override fun dispose() {
        sortedSource.dispose()
        super.dispose()
    }

    /** Maps values to the integer used for threshold comparisons. */
    fun interface Evaluator<E> {
        fun evaluate(element: E): Int
    }

    /** Adapts an [Evaluator] to the comparator used by the internal sorted list. */
    internal class ThresholdComparator<E>(
        private val evaluator: Evaluator<E>,
    ) : Comparator<E> {
        override fun compare(alpha: E, beta: E): Int {
            val alphaValue = if (alpha is Int) alpha else evaluator.evaluate(alpha)
            val betaValue = if (beta is Int) beta else evaluator.evaluate(beta)
            return alphaValue.compareTo(betaValue)
        }

        override fun equals(other: Any?): Boolean =
            this === other || (other is ThresholdComparator<*> && evaluator == other.evaluator)

        override fun hashCode(): Int = evaluator.hashCode()
    }
}
