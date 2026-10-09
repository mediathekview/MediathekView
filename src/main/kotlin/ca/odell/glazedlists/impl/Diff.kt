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
package ca.odell.glazedlists.impl

import ca.odell.glazedlists.EventList

/**
 * Eugene W. Myers' O(ND) difference algorithm, used to update an [EventList]
 * with the minimum number of insertions and deletions.
 */
internal object Diff {
    fun <E> replaceAll(
        target: EventList<E>,
        source: List<E>,
        updates: Boolean,
    ) {
        replaceAll(target, source, updates, GlazedListsImpl.equalsComparator())
    }

    fun <E> replaceAll(
        target: EventList<E>,
        source: List<E>,
        updates: Boolean,
        comparator: Comparator<E>,
    ) {
        val editScript = shortestEditScript(ListDiffMatcher(target, source, comparator))
        var targetIndex = 0
        var sourceIndex = 0

        for (index in 1 until editScript.size) {
            val previousPoint = editScript[index - 1]
            val currentPoint = editScript[index]
            val deltaX = currentPoint.x - previousPoint.x
            val deltaY = currentPoint.y - previousPoint.y

            when (deltaX) {
                deltaY -> {
                    if (updates) {
                        for (offset in 0 until deltaX) {
                            target[targetIndex + offset] = source[sourceIndex + offset]
                        }
                    }
                    targetIndex += deltaX
                    sourceIndex += deltaY
                }

                1 -> {
                    if (deltaY != 0) throw IllegalStateException()
                    target.removeAt(targetIndex)
                }

                0 -> {
                    if (deltaY != 1) throw IllegalStateException()
                    target.add(targetIndex, source[sourceIndex])
                    sourceIndex++
                    targetIndex++
                }

                else -> throw IllegalStateException()
            }
        }
    }

    private fun shortestEditScript(input: DiffMatcher): List<Point> {
        val maxPoint = Point(input.alphaLength, input.betaLength)
        val maxSteps = input.alphaLength + input.betaLength
        var previousFrontier = emptyArray<Point?>()
        var currentFrontier = arrayOfNulls<Point>(1)

        for (distance in 0..maxSteps) {
            if (currentFrontier.size <= distance) {
                currentFrontier = currentFrontier.copyOf(maxOf(distance + 1, currentFrontier.size * 2))
            }

            for (frontierIndex in 0..distance) {
                val belowLeft = if (frontierIndex > 0) previousFrontier[frontierIndex - 1] else null
                val aboveRight = if (frontierIndex < distance) previousFrontier[frontierIndex] else null
                val point =
                    when {
                        distance == 0 -> Point(0, 0)
                        frontierIndex == 0 ||
                            (frontierIndex != distance &&
                                requireNotNull(belowLeft).x < requireNotNull(aboveRight).x) ->
                            requireNotNull(aboveRight).createDeltaPoint(0, 1)

                        else -> requireNotNull(belowLeft).createDeltaPoint(1, 0)
                    }

                val furthestPoint = extendDiagonal(point, maxPoint, input)
                currentFrontier[frontierIndex] = furthestPoint

                if (furthestPoint.isEqualToOrGreaterThan(maxPoint)) return furthestPoint.trail()
            }

            val reusableFrontier = previousFrontier
            previousFrontier = currentFrontier
            currentFrontier = reusableFrontier
        }
        throw IllegalStateException()
    }

    private fun extendDiagonal(
        point: Point,
        maxPoint: Point,
        input: DiffMatcher,
    ): Point {
        var x = point.x
        var y = point.y
        while (x < maxPoint.x && y < maxPoint.y && input.matchPair(x, y)) {
            x++
            y++
        }
        return if (x == point.x) point else Point(x, y, point)
    }

    private class Point(
        val x: Int,
        val y: Int,
        private val predecessor: Point? = null,
    ) {
        fun createDeltaPoint(deltaX: Int, deltaY: Int): Point =
            Point(x + deltaX, y + deltaY, this)

        fun isEqualToOrGreaterThan(other: Point): Boolean = x >= other.x && y >= other.y

        fun trail(): List<Point> {
            val reverse = ArrayList<Point>()
            var current: Point? = this
            while (current != null) {
                reverse += current
                current = current.predecessor
            }
            reverse.reverse()
            return reverse
        }
    }

    private interface DiffMatcher {
        val alphaLength: Int
        val betaLength: Int

        fun matchPair(alphaIndex: Int, betaIndex: Int): Boolean
    }

    private class ListDiffMatcher<E>(
        private val alpha: List<E>,
        private val beta: List<E>,
        private val comparator: Comparator<E>,
    ) : DiffMatcher {
        override val alphaLength: Int
            get() = alpha.size

        override val betaLength: Int
            get() = beta.size

        override fun matchPair(alphaIndex: Int, betaIndex: Int): Boolean =
            comparator.compare(alpha[alphaIndex], beta[betaIndex]) == 0
    }
}
