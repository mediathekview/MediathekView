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
package ca.odell.glazedlists.impl.adt

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.lang.reflect.Field
import java.util.Random
import kotlin.collections.ArrayDeque
import kotlin.math.abs

internal class BarcodeBehaviorTest {
    @Test
    fun insertionRemovalRankSelectAndDiagnosticStringsStayCharacterized() {
        val barcode = Barcode()
        val model = mutableListOf<Boolean>()

        barcode.addWhite(0, 3)
        repeat(3) { model.add(false) }
        barcode.addBlack(1, 2)
        repeat(2) { offset -> model.add(1 + offset, true) }
        barcode.add(0, Barcode.BLACK, 1)
        model.add(0, true)
        barcode.add(4, Barcode.WHITE, 2)
        repeat(2) { offset -> model.add(4 + offset, false) }
        barcode.add(6, Barcode.BLACK, 3)
        repeat(3) { offset -> model.add(6 + offset, true) }

        assertBarcodeState(barcode, model)
        assertEquals("[ [ null (0, 0) <0> 1 <1> (0, 0) null ] (1, 1) <1> 2 <2> (3, 5) [ null (0, 0) <2> 3 <1> (0, 0) null ] ]", barcodeRoot(barcode).toString())

        barcode.setWhite(2, 2)
        repeat(2) { offset -> model[2 + offset] = false }
        assertBarcodeState(barcode, model)

        barcode.setBlack(1, 4)
        repeat(4) { offset -> model[1 + offset] = true }
        assertBarcodeState(barcode, model)

        barcode.remove(2, 4)
        repeat(4) { model.removeAt(2) }
        assertBarcodeState(barcode, model)
        assertEquals(5, barcode.findSequenceOfMinimumSize(1, Barcode.WHITE))
        assertEquals(5, barcode.findSequenceOfMinimumSize(2, Barcode.WHITE))
        assertEquals(0, barcode.findSequenceOfMinimumSize(2, Barcode.BLACK))
    }

    @Test
    fun iteratorTraversalAndMutationStateStayCharacterized() {
        val barcode = barcodeOf("_XX__X_")
        val model = colors("_XX__X_").toMutableList()
        assertBarcodeState(barcode, model)

        val blackIterator = barcode.iterator()
        assertEquals(-1, blackIterator.index)
        assertEquals(-1, blackIterator.getBlackIndex())
        assertEquals(-1, blackIterator.getWhiteIndex())
        assertTrue(blackIterator.hasNextBlack())
        assertSame(Barcode.BLACK, blackIterator.nextBlack())
        assertEquals(1, blackIterator.index)
        assertEquals(0, blackIterator.getBlackIndex())
        assertEquals(-1, blackIterator.getWhiteIndex())
        assertEquals(0, blackIterator.getColourIndex(Barcode.BLACK))
        assertEquals(-1, blackIterator.getColourIndex(Barcode.WHITE))

        assertSame(Barcode.BLACK, blackIterator.nextBlack())
        assertEquals(2, blackIterator.index)
        assertEquals(1, blackIterator.setWhite())
        model[2] = false
        assertBarcodeState(barcode, model)
        assertEquals(2, blackIterator.index)
        assertEquals(-1, blackIterator.getBlackIndex())
        assertEquals(1, blackIterator.getWhiteIndex())

        val whiteIterator = barcode.iterator()
        assertSame(Barcode.WHITE, whiteIterator.nextWhite())
        assertEquals(0, whiteIterator.index)
        assertEquals(0, whiteIterator.setBlack())
        model[0] = true
        assertBarcodeState(barcode, model)
        assertEquals(0, whiteIterator.index)
        assertEquals(0, whiteIterator.getBlackIndex())
        assertEquals(-1, whiteIterator.getWhiteIndex())

        val traversal = barcode.iterator()
        assertSame(Barcode.BLACK, traversal.next())
        assertSame(Barcode.BLACK, traversal.nextColour(Barcode.BLACK))
        assertEquals(1, traversal.index)
        traversal.remove()
        model.removeAt(1)
        assertBarcodeState(barcode, model)
        assertEquals(0, traversal.index)
        assertEquals(0, traversal.getBlackIndex())

        while (traversal.hasNextWhite()) {
            traversal.nextWhite()
            if (traversal.index == 4) {
                assertEquals(4, traversal.set(Barcode.BLACK))
                model[4] = true
                break
            }
        }
        assertBarcodeState(barcode, model)
        assertEquals(5, traversal.index)
        assertEquals(-1, traversal.getBlackIndex())
        assertEquals(3, traversal.getWhiteIndex())
    }

    @Test
    fun nullColourFallbacksMatchLegacyJavaIdentityChecks() {
        val added = Barcode()
        added.addWhite(0, 4)
        added.add(1, null, 2)
        assertBarcodeState(added, colors("_XX___"))

        val set = barcodeOf("__X__")
        set.set(1, null, 1)
        assertBarcodeState(set, colors("_XX__"))

        val colourIndices = barcodeOf("_XX_")
        assertEquals(colourIndices.blackSize(), colourIndices.colourSize(null))
        assertEquals(colourIndices.getIndex(0, Barcode.BLACK), colourIndices.getIndex(0, null))
        assertEquals(colourIndices.getColourIndex(1, Barcode.BLACK), colourIndices.getColourIndex(1, null))

        val sequence = barcodeOf("X__X")
        assertEquals(sequence.findSequenceOfMinimumSize(2, Barcode.BLACK), sequence.findSequenceOfMinimumSize(2, null))
        assertNotEquals(sequence.findSequenceOfMinimumSize(2, Barcode.WHITE), sequence.findSequenceOfMinimumSize(2, null))

        val trailingWhiteOnly = Barcode().apply { addWhite(0, 3) }
        assertEquals(0, trailingWhiteOnly.findSequenceOfMinimumSize(2, null))
        assertThrows(NullPointerException::class.java) { trailingWhiteOnly.getIndex(0, null) }

        val blackOnlyIterator = barcodeOf("XX").iterator()
        assertFalse(blackOnlyIterator.hasNextColour(null))
        assertTrue(blackOnlyIterator.hasNextColour(Barcode.BLACK))

        val whiteIterator = barcodeOf("__X").iterator()
        assertSame(Barcode.WHITE, whiteIterator.nextColour(null))

        val setIteratorBarcode = barcodeOf("X_")
        val setIterator = setIteratorBarcode.iterator()
        assertSame(Barcode.BLACK, setIterator.nextBlack())
        assertEquals(0, setIterator.set(null))
        assertBarcodeState(setIteratorBarcode, colors("__"))

        val colourIndexIterator = barcodeOf("_X_").iterator()
        assertSame(Barcode.BLACK, colourIndexIterator.nextBlack())
        assertEquals(0, colourIndexIterator.getColourIndex(null))
        assertEquals(-1, colourIndexIterator.getColourIndex(Barcode.WHITE))
    }

    @Test
    fun allRotationsPreserveIdentityParentLinksAndBalance() {
        assertRotation(listOf(6, 4, 2))
        assertRotation(listOf(2, 4, 6))
        assertRotation(listOf(6, 2, 4))
        assertRotation(listOf(2, 6, 4))
    }

    @Test
    fun deletionTopologiesDetachRemovedNodesAndPreserveBalancedStructure() {
        run {
            val barcode = isolatedBarcode(4, 2, 6)
            val model = listOf(4, 2, 6).toModel(15).toMutableList()
            val originalRoot = rootNode(barcode)
            val removedLeaf = nodeAt(barcode, 2)
            val survivingRight = nodeAt(barcode, 6)

            barcode.remove(2, 1)
            model.removeAt(2)

            assertDetached(removedLeaf)
            assertBarcodeState(barcode, model)
            val root = rootNode(barcode)
            assertSame(originalRoot, root)
            assertSame(survivingRight, rightChild(root))
            assertNull(leftChild(root))
        }

        run {
            val barcode = isolatedBarcode(4, 2, 6, 0)
            val model = listOf(4, 2, 6, 0).toModel(15).toMutableList()
            val originalRoot = rootNode(barcode)
            val removedNode = nodeAt(barcode, 2)
            val promotedChild = nodeAt(barcode, 0)
            val survivingRight = nodeAt(barcode, 6)

            barcode.remove(2, 1)
            model.removeAt(2)

            assertDetached(removedNode)
            assertBarcodeState(barcode, model)
            val root = rootNode(barcode)
            assertSame(originalRoot, root)
            assertSame(promotedChild, leftChild(root))
            assertSame(root, parentOf(promotedChild))
            assertSame(survivingRight, rightChild(root))
        }

        run {
            val barcode = isolatedBarcode(4, 2, 6)
            val model = listOf(4, 2, 6).toModel(15).toMutableList()
            val originalRoot = rootNode(barcode)
            val successor = nodeAt(barcode, 6)
            val left = nodeAt(barcode, 2)

            barcode.remove(4, 1)
            model.removeAt(4)

            assertDetached(successor)
            assertBarcodeState(barcode, model)
            val root = rootNode(barcode)
            assertSame(originalRoot, root)
            assertSame(left, leftChild(root))
            assertNull(rightChild(root))
        }

        run {
            val barcode = isolatedBarcode(4, 2, 8, 6)
            val model = listOf(4, 2, 8, 6).toModel(15).toMutableList()
            val originalRoot = rootNode(barcode)
            val successor = nodeAt(barcode, 6)
            val left = nodeAt(barcode, 2)
            val right = nodeAt(barcode, 8)

            barcode.remove(4, 1)
            model.removeAt(4)

            assertDetached(successor)
            assertBarcodeState(barcode, model)
            val root = rootNode(barcode)
            assertSame(originalRoot, root)
            assertSame(left, leftChild(root))
            assertSame(right, rightChild(root))
            assertNull(leftChild(right))
            assertSame(root, parentOf(right))
        }
    }

    @Test
    fun deletionTriggeredAvlRotationsPreserveIdentityAndCachedState() {
        run {
            val barcode = isolatedBarcode(4, 2, 6, 8)
            val model = listOf(4, 2, 6, 8).toModel(15).toMutableList()
            val removedLeaf = nodeAt(barcode, 2)
            val expectedRoot = nodeAt(barcode, 6)
            val expectedLeft = nodeAt(barcode, 4)
            val expectedRight = nodeAt(barcode, 8)

            barcode.remove(2, 1)
            model.removeAt(2)

            assertDetached(removedLeaf)
            assertBarcodeState(barcode, model)
            val root = rootNode(barcode)
            assertSame(expectedRoot, root)
            assertSame(expectedLeft, leftChild(root))
            assertSame(expectedRight, rightChild(root))
            assertSame(root, parentOf(expectedLeft))
            assertSame(root, parentOf(expectedRight))
        }

        run {
            val barcode = isolatedBarcode(4, 2, 8, 6)
            val model = listOf(4, 2, 8, 6).toModel(15).toMutableList()
            val removedLeaf = nodeAt(barcode, 2)
            val expectedRoot = nodeAt(barcode, 6)
            val expectedLeft = nodeAt(barcode, 4)
            val expectedRight = nodeAt(barcode, 8)

            barcode.remove(2, 1)
            model.removeAt(2)

            assertDetached(removedLeaf)
            assertBarcodeState(barcode, model)
            val root = rootNode(barcode)
            assertSame(expectedRoot, root)
            assertSame(expectedLeft, leftChild(root))
            assertSame(expectedRight, rightChild(root))
            assertSame(root, parentOf(expectedLeft))
            assertSame(root, parentOf(expectedRight))
        }
    }

    @Test
    fun iteratorMutationsCoverWhiteRemovalTrailingWhiteRemovalCurrentNodeUnlinkAndGenericSetBlack() {
        run {
            val barcode = barcodeOf("_X__")
            val iterator = barcode.iterator()

            assertSame(Barcode.WHITE, iterator.nextWhite())
            iterator.remove()
            barcode.validate()
            assertEquals(4, barcode.size())
            assertEquals(2, barcode.whiteSize())
            assertEquals(1, barcode.blackSize())
            assertEquals(-1, iterator.index)
            assertEquals(-1, iterator.getBlackIndex())
        }

        run {
            val barcode = barcodeOf("X__")
            val model = colors("X__").toMutableList()
            val iterator = barcode.iterator()

            assertSame(Barcode.BLACK, iterator.nextBlack())
            assertSame(Barcode.WHITE, iterator.nextWhite())
            iterator.remove()
            model.removeAt(1)

            assertBarcodeState(barcode, model)
            assertEquals(0, iterator.index)
            assertEquals(0, iterator.getBlackIndex())
        }

        run {
            val barcode = barcodeOf("X_X")
            val model = colors("X_X").toMutableList()
            val iterator = barcode.iterator()

            assertSame(Barcode.BLACK, iterator.nextBlack())
            assertSame(Barcode.BLACK, iterator.nextBlack())
            val currentNode = requireNotNull(field(iterator.javaClass, "currentNode").get(iterator))
            assertEquals(1, iterator.setWhite())
            model[2] = false

            assertDetached(currentNode)
            assertBarcodeState(barcode, model)
            assertEquals(2, iterator.index)
            assertEquals(-1, iterator.getBlackIndex())
            assertEquals(1, iterator.getWhiteIndex())
        }

        run {
            val barcode = barcodeOf("___X")
            val model = colors("___X").toMutableList()
            val iterator = barcode.iterator()

            assertSame(Barcode.WHITE, iterator.nextWhite())
            assertSame(Barcode.WHITE, iterator.nextWhite())
            assertEquals(0, iterator.set(Barcode.BLACK))
            model[1] = true

            assertBarcodeState(barcode, model)
            assertEquals(1, iterator.index)
            assertEquals(0, iterator.getBlackIndex())
        }
    }

    @Test
    fun deletionTopologiesClearReuseAndBoundaryFailuresStayCharacterized() {
        val barcode = barcodeOf("_X_X_X_X_X_")
        val model = colors("_X_X_X_X_X_").toMutableList()
        val originalRoot = barcodeRoot(barcode)
        val rootIndex = nodeStartIndex(requireNotNull(originalRoot))

        barcode.remove(rootIndex, 1)
        model.removeAt(rootIndex)
        assertBarcodeState(barcode, model)
        assertNotNull(barcodeRoot(barcode))
        barcode.clear()
        assertBarcodeState(barcode, emptyList())
        assertTrue(barcode.isEmpty)

        barcode.addWhite(0, 4)
        barcode.setBlack(2, 1)
        assertBarcodeState(barcode, colors("__X_"))

        val empty = Barcode()
        assertEquals(0, empty.size())
        assertEquals(0, empty.whiteSize())
        assertEquals(0, empty.blackSize())
        assertSame(Barcode.WHITE, empty[0])
        assertEquals(0, empty.getWhiteIndex(0))
        assertEquals(-1, empty.getBlackIndex(0))
        assertEquals(0, empty.getColourIndex(0, false, Barcode.WHITE))
        assertEquals(-1, empty.getColourIndex(0, true, Barcode.BLACK))
        assertThrows(NullPointerException::class.java) { empty.getIndex(0, Barcode.BLACK) }
        assertEquals(0, empty.getIndex(0, Barcode.WHITE))

        val iterator = empty.iterator()
        assertFalse(iterator.hasNext())
        assertFalse(iterator.hasNextBlack())
        assertFalse(iterator.hasNextWhite())
        assertFalse(iterator.hasNextColour(Barcode.BLACK))
        assertFalse(iterator.hasNextColour(Barcode.WHITE))
        assertThrows(NoSuchElementException::class.java) { empty.iterator().next() }
        assertThrows(NoSuchElementException::class.java) { empty.iterator().nextBlack() }
        assertThrows(NoSuchElementException::class.java) { empty.iterator().nextWhite() }
        assertThrows(NoSuchElementException::class.java) { empty.iterator().nextColour(Barcode.BLACK) }
        assertThrows(NoSuchElementException::class.java) { empty.iterator().remove() }
        assertThrows(NoSuchElementException::class.java) { empty.iterator().setBlack() }
        assertThrows(NoSuchElementException::class.java) { empty.iterator().setWhite() }
        assertThrows(NoSuchElementException::class.java) { empty.iterator().set(Barcode.BLACK) }
    }

    @Test
    fun largeRunsStayCompressedAndPreserveRankSelectSurface() {
        val barcode = Barcode()
        barcode.addWhite(0, 20_000)
        barcode.setBlack(2_000, 8_000)
        barcode.setBlack(14_000, 3_000)
        barcode.setBlack(19_000, 500)

        val model = MutableList(20_000) { false }.apply {
            repeat(8_000) { this[2_000 + it] = true }
            repeat(3_000) { this[14_000 + it] = true }
            repeat(500) { this[19_000 + it] = true }
        }

        assertBarcodeStructure(barcode, model, verifyMappings = false)
        assertEquals(3, countNodes(barcodeRoot(barcode)))
        assertEquals(2_000, barcode.findSequenceOfMinimumSize(4_000, Barcode.BLACK))
        assertEquals(0, barcode.findSequenceOfMinimumSize(2_000, Barcode.WHITE))
        assertEquals(20_000, barcode.size())
        assertEquals(model.count(Boolean::not), barcode.whiteSize())
        assertEquals(model.count { it }, barcode.blackSize())
        assertEquals(14_000, barcode.getIndex(8_000, Barcode.BLACK))
        assertEquals(11_500, barcode.getIndex(3_500, Barcode.WHITE))
    }

    @Test
    fun fixedSeedOracleKeepsSequenceMappingsIteratorIndicesAndStructureAfterEveryMutation() {
        val barcode = Barcode()
        val model = mutableListOf<Boolean>()
        val random = Random(0x0BAD_C0DEL)
        val operations = ArrayDeque<String>()

        repeat(400) {
            when (random.nextInt(5)) {
                0 -> {
                    val index = random.nextInt(model.size + 1)
                    val color = random.nextBoolean()
                    val length = random.nextInt(4) + 1
                    repeat(length) { model.add(index + it, color) }
                    barcode.add(index, color(color), length)
                    operations += "add($index, ${if (color) "BLACK" else "WHITE"}, $length)"
                }

                1 -> {
                    if (model.isEmpty()) {
                        val length = random.nextInt(4) + 1
                        repeat(length) { model += false }
                        barcode.addWhite(0, length)
                        operations += "addWhite(0, $length)"
                    } else {
                        val index = random.nextInt(model.size)
                        val length = 1
                        val color = random.nextBoolean()
                        repeat(length) { offset -> model[index + offset] = color }
                        barcode.set(index, color(color), length)
                        operations += "set($index, ${if (color) "BLACK" else "WHITE"}, $length)"
                    }
                }

                2 -> {
                    if (model.isEmpty()) {
                        val length = random.nextInt(4) + 1
                        repeat(length) { model += true }
                        barcode.addBlack(0, length)
                        operations += "addBlack(0, $length)"
                    } else {
                        val index = random.nextInt(model.size)
                        val length = 1
                        repeat(length) { model.removeAt(index) }
                        barcode.remove(index, length)
                        operations += "remove($index, $length)"
                    }
                }

                3 -> {
                    barcode.clear()
                    model.clear()
                    operations += "clear()"
                }

                else -> {
                    val index = random.nextInt(model.size + 1)
                    val length = random.nextInt(4) + 1
                    repeat(length) { model.add(index + it, false) }
                    barcode.addWhite(index, length)
                    operations += "addWhite($index, $length)"
                }
            }

            while (operations.size > 12) operations.removeFirst()
            assertBarcodeStructure(barcode, model, context = operations.joinToString(" -> "))
        }
    }

    private fun assertRotation(indices: List<Int>) {
        val barcode = Barcode()
        barcode.addWhite(0, 9)
        val nodesByIndex = mutableMapOf<Int, Any>()

        indices.forEach { index ->
            barcode.setBlack(index, 1)
            nodesByIndex[index] = requireNotNull(findNodeCoveringIndex(barcodeRoot(barcode), index))
        }

        val root = requireNotNull(barcodeRoot(barcode))
        assertSame(nodesByIndex.getValue(4), root)
        assertBarcodeState(barcode, indices.toModel(9))
    }

    private fun assertBarcodeState(barcode: Barcode, model: List<Boolean>) {
        assertBarcodeStructure(barcode, model)
    }

    private fun assertBarcodeStructure(
        barcode: Barcode,
        model: List<Boolean>,
        verifyMappings: Boolean = true,
        context: String = "",
    ) {
        try {
            barcode.validate()
        } catch (exception: RuntimeException) {
            throw AssertionError("Validation failed after: $context", exception)
        }
        assertEquals(model.size, barcode.size(), context)
        assertEquals(model.isEmpty(), barcode.isEmpty)
        assertEquals(model.count(Boolean::not), barcode.whiteSize())
        assertEquals(model.count { it }, barcode.blackSize())
        assertEquals(model.count(Boolean::not), barcode.colourSize(Barcode.WHITE))
        assertEquals(model.count { it }, barcode.colourSize(Barcode.BLACK))
        assertEquals(model.joinToString("") { if (it) "X" else "_" }, barcode.toString())

        val expectedRuns = expectedBlackRuns(model)
        val expectedTreeSize = expectedRuns.lastOrNull()?.let { it.start + it.size } ?: 0
        val expectedTrailingWhitespace = model.size - expectedTreeSize
        assertEquals(expectedTreeSize, field(barcode.javaClass, "treeSize").getInt(barcode))
        assertEquals(expectedTrailingWhitespace, field(barcode.javaClass, "whiteSpace").getInt(barcode))

        val root = barcodeRoot(barcode)
        if (expectedRuns.isEmpty()) {
            assertNull(root)
        } else {
            assertNotNull(root)
        }

        val structure = assertStructure(root, barcode, null)
        assertEquals(expectedRuns, structure.runs)
        assertEquals(model.count { it }, structure.blackSize)
        assertEquals(expectedTreeSize, structure.totalSize)
        assertEquals(expectedRuns.size, countNodes(root))

        if (verifyMappings) {
            assertMappings(barcode, model)
            assertIterators(barcode, model)
        }
    }

    private fun assertMappings(barcode: Barcode, model: List<Boolean>) {
        if (model.isEmpty()) {
            assertEquals(-1, barcode.findSequenceOfMinimumSize(1, Barcode.BLACK))
            assertEquals(-1, barcode.findSequenceOfMinimumSize(1, Barcode.WHITE))
            return
        }

        model.indices.forEach { index ->
            val black = model[index]
            assertSame(color(black), barcode[index])
            assertEquals(colorRankAt(model, index, true).takeIf { black } ?: -1, barcode.getBlackIndex(index))
            assertEquals(colorRankAt(model, index, false).takeIf { !black } ?: -1, barcode.getWhiteIndex(index))
            assertEquals(colorRankAt(model, index, true).takeIf { black } ?: -1, barcode.getColourIndex(index, Barcode.BLACK))
            assertEquals(colorRankAt(model, index, false).takeIf { !black } ?: -1, barcode.getColourIndex(index, Barcode.WHITE))
            assertEquals(expectedColourIndex(model, index, left = true, color = true), barcode.getBlackIndex(index, true))
            assertEquals(expectedColourIndex(model, index, left = false, color = true), barcode.getBlackIndex(index, false))
            assertEquals(expectedColourIndex(model, index, left = true, color = false), barcode.getWhiteIndex(index, true))
            assertEquals(expectedColourIndex(model, index, left = false, color = false), barcode.getWhiteIndex(index, false))
            assertEquals(expectedColourIndex(model, index, left = true, color = true), barcode.getColourIndex(index, true, Barcode.BLACK))
            assertEquals(expectedColourIndex(model, index, left = false, color = true), barcode.getColourIndex(index, false, Barcode.BLACK))
            assertEquals(expectedColourIndex(model, index, left = true, color = false), barcode.getColourIndex(index, true, Barcode.WHITE))
            assertEquals(expectedColourIndex(model, index, left = false, color = false), barcode.getColourIndex(index, false, Barcode.WHITE))
        }

        repeat(model.count { it }) { blackIndex ->
            assertEquals(indexOfRank(model, blackIndex, true), barcode.getIndex(blackIndex, Barcode.BLACK))
        }
        repeat(model.count(Boolean::not)) { whiteIndex ->
            val realIndex = indexOfRank(model, whiteIndex, false)
            assertEquals(realIndex, barcode.getIndex(whiteIndex, Barcode.WHITE))
            assertEquals(whiteSequenceIndex(model, realIndex), barcode.getWhiteSequenceIndex(whiteIndex))
            assertEquals(expectedBlackBeforeWhite(model, whiteIndex), barcode.getBlackBeforeWhite(whiteIndex))
        }

        for (minimumSize in 1..minOf(model.size + 1, 6)) {
            assertEquals(findSequenceStart(model, minimumSize, true), barcode.findSequenceOfMinimumSize(minimumSize, Barcode.BLACK))
            assertEquals(findSequenceStart(model, minimumSize, false), barcode.findSequenceOfMinimumSize(minimumSize, Barcode.WHITE))
        }
    }

    private fun assertIterators(barcode: Barcode, model: List<Boolean>) {
        val iterator = barcode.iterator()
        assertEquals(-1, iterator.index)
        assertEquals(-1, iterator.getBlackIndex())
        assertEquals(-1, iterator.getWhiteIndex())

        model.forEachIndexed { index, black ->
            assertTrue(iterator.hasNext())
            assertSame(color(black), iterator.next())
            assertEquals(index, iterator.index)
            assertEquals(colorRankAt(model, index, true).takeIf { black } ?: -1, iterator.getBlackIndex())
            assertEquals(colorRankAt(model, index, false).takeIf { !black } ?: -1, iterator.getWhiteIndex())
            assertEquals(colorRankAt(model, index, true).takeIf { black } ?: -1, iterator.getColourIndex(Barcode.BLACK))
            assertEquals(colorRankAt(model, index, false).takeIf { !black } ?: -1, iterator.getColourIndex(Barcode.WHITE))
        }
        assertFalse(iterator.hasNext())

        val blackIterator = barcode.iterator()
        val blackIndices = model.indices.filter(model::get)
        blackIndices.forEachIndexed { ordinal, index ->
            assertTrue(blackIterator.hasNextBlack())
            assertTrue(blackIterator.hasNextColour(Barcode.BLACK))
            assertSame(Barcode.BLACK, blackIterator.nextBlack())
            assertEquals(index, blackIterator.index)
            assertEquals(ordinal, blackIterator.getBlackIndex())
            assertEquals(-1, blackIterator.getWhiteIndex())
        }
        assertFalse(blackIterator.hasNextBlack())
        assertFalse(blackIterator.hasNextColour(Barcode.BLACK))

        val whiteIterator = barcode.iterator()
        val whiteIndices = model.indices.filter { !model[it] }
        whiteIndices.forEachIndexed { ordinal, index ->
            assertTrue(whiteIterator.hasNextWhite())
            assertTrue(whiteIterator.hasNextColour(Barcode.WHITE))
            assertSame(Barcode.WHITE, whiteIterator.nextColour(Barcode.WHITE))
            assertEquals(index, whiteIterator.index)
            assertEquals(-1, whiteIterator.getBlackIndex())
            assertEquals(ordinal, whiteIterator.getWhiteIndex())
        }
        assertFalse(whiteIterator.hasNextWhite())
        assertFalse(whiteIterator.hasNextColour(Barcode.WHITE))
    }

    private fun assertStructure(node: Any?, host: Barcode, parent: Any?): StructureState {
        if (node == null) return StructureState(emptyList(), 0, 0, 0)

        assertSame(host, hostOf(node))
        assertSame(parent, parentOf(node))

        val leftNode = leftChild(node)
        val rightNode = rightChild(node)
        val left = assertStructure(leftNode, host, node)
        val right = assertStructure(rightNode, host, node)
        val whiteSpace = field(node.javaClass, "whiteSpace").getInt(node)
        val rootSize = field(node.javaClass, "rootSize").getInt(node)
        val blackLeftSize = field(node.javaClass, "blackLeftSize").getInt(node)
        val blackRightSize = field(node.javaClass, "blackRightSize").getInt(node)
        val treeLeftSize = field(node.javaClass, "treeLeftSize").getInt(node)
        val treeRightSize = field(node.javaClass, "treeRightSize").getInt(node)
        val height = field(node.javaClass, "height").getInt(node)

        assertEquals(left.blackSize, blackLeftSize)
        assertEquals(right.blackSize, blackRightSize)
        assertEquals(left.totalSize, treeLeftSize)
        assertEquals(right.totalSize, treeRightSize)
        assertTrue(rootSize > 0)
        assertTrue(abs(left.height - right.height) <= 1)
        assertEquals(maxOf(left.height, right.height) + 1, height)

        return StructureState(
            left.runs + BlackRun(left.totalSize + whiteSpace, whiteSpace, rootSize) + right.runs.map { it.shifted(left.totalSize + whiteSpace + rootSize) },
            left.blackSize + rootSize + right.blackSize,
            left.totalSize + whiteSpace + rootSize + right.totalSize,
            height,
        )
    }

    private fun findNodeCoveringIndex(node: Any?, index: Int): Any? {
        if (node == null) return null

        val treeLeftSize = field(node.javaClass, "treeLeftSize").getInt(node)
        val whiteSpace = field(node.javaClass, "whiteSpace").getInt(node)
        val rootSize = field(node.javaClass, "rootSize").getInt(node)
        val localIndex = index - treeLeftSize

        return when {
            localIndex < 0 -> findNodeCoveringIndex(leftChild(node), index)
            localIndex >= whiteSpace + rootSize -> findNodeCoveringIndex(rightChild(node), localIndex - whiteSpace - rootSize)
            localIndex < whiteSpace -> null
            else -> node
        }
    }

    private fun nodeStartIndex(node: Any): Int =
        field(node.javaClass, "treeLeftSize").getInt(node) + field(node.javaClass, "whiteSpace").getInt(node)

    private fun barcodeRoot(barcode: Barcode): Any? = field(barcode.javaClass, "root").get(barcode)

    private fun countNodes(node: Any?): Int =
        if (node == null) {
            0
        } else {
            1 + countNodes(leftChild(node)) + countNodes(rightChild(node))
        }

    private fun expectedBlackRuns(model: List<Boolean>): List<BlackRun> {
        val runs = mutableListOf<BlackRun>()
        var index = 0
        var previousBlackEnd = 0
        while (index < model.size) {
            if (!model[index]) {
                index++
                continue
            }

            val start = index
            while (index < model.size && model[index]) index++
            runs += BlackRun(start, start - previousBlackEnd, index - start)
            previousBlackEnd = index
        }
        return runs
    }

    private fun colors(pattern: String): List<Boolean> = pattern.map { it == 'X' }

    private fun barcodeOf(pattern: String): Barcode {
        val barcode = Barcode()
        if (pattern.isEmpty()) return barcode

        barcode.addWhite(0, pattern.length)
        var index = 0
        while (index < pattern.length) {
            if (pattern[index] == '_') {
                index++
                continue
            }

            val start = index
            while (index < pattern.length && pattern[index] == 'X') index++
            barcode.setBlack(start, index - start)
        }
        return barcode
    }

    private fun isolatedBarcode(vararg blackIndices: Int): Barcode =
        Barcode().apply {
            addWhite(0, 15)
            blackIndices.forEach { setBlack(it, 1) }
        }

    private fun rootNode(barcode: Barcode): Any = requireNotNull(barcodeRoot(barcode))

    private fun nodeAt(barcode: Barcode, index: Int): Any = requireNotNull(findNodeCoveringIndex(barcodeRoot(barcode), index))

    private fun hostOf(node: Any): Any? = field(node.javaClass, "host").get(node)

    private fun parentOf(node: Any): Any? = field(node.javaClass, "parent").get(node)

    private fun leftChild(node: Any): Any? = field(node.javaClass, "left").get(node)

    private fun rightChild(node: Any): Any? = field(node.javaClass, "right").get(node)

    private fun assertDetached(node: Any) {
        assertNull(hostOf(node))
        assertNull(parentOf(node))
    }

    private fun List<Int>.toModel(size: Int): List<Boolean> = List(size) { it in this }

    private fun color(black: Boolean): Any = if (black) Barcode.BLACK else Barcode.WHITE

    private fun colorRankAt(model: List<Boolean>, index: Int, black: Boolean): Int =
        model.subList(0, index + 1).count { it == black } - 1

    private fun indexOfRank(model: List<Boolean>, rank: Int, black: Boolean): Int {
        var seen = 0
        model.forEachIndexed { index, value ->
            if (value == black) {
                if (seen == rank) return index
                seen++
            }
        }
        throw IndexOutOfBoundsException("No rank $rank for color $black")
    }

    private fun expectedColourIndex(model: List<Boolean>, index: Int, left: Boolean, color: Boolean): Int {
        if (model[index] == color) return colorRankAt(model, index, color)

        val searchRange =
            if (left) {
                index - 1 downTo 0
            } else {
                index + 1 until model.size
            }
        for (candidate in searchRange) {
            if (model[candidate] == color) return colorRankAt(model, candidate, color)
        }
        return if (left) -1 else model.count { it == color }
    }

    private fun whiteSequenceIndex(model: List<Boolean>, realIndex: Int): Int {
        var sequenceIndex = 0
        var cursor = realIndex - 1
        while (cursor >= 0 && !model[cursor]) {
            sequenceIndex++
            cursor--
        }
        return sequenceIndex
    }

    private fun expectedBlackBeforeWhite(model: List<Boolean>, whiteIndex: Int): Int {
        val realIndex = indexOfRank(model, whiteIndex, false)
        for (cursor in realIndex - 1 downTo 0) {
            if (model[cursor]) return colorRankAt(model, cursor, true)
        }
        return -1
    }

    private fun findSequenceStart(model: List<Boolean>, minimumSize: Int, black: Boolean): Int {
        var index = 0
        while (index < model.size) {
            if (model[index] != black) {
                index++
                continue
            }
            val start = index
            while (index < model.size && model[index] == black) index++
            if (index - start >= minimumSize) return start
        }
        return -1
    }

    private fun field(type: Class<*>, name: String): Field =
        runCatching {
            type.getDeclaredField(name).also { it.isAccessible = true }
        }.getOrElse {
            val parent = type.superclass ?: throw it
            field(parent, name)
        }

    private data class BlackRun(val start: Int, val whiteSpace: Int, val size: Int) {
        fun shifted(offset: Int): BlackRun = copy(start = start + offset)
    }

    private data class StructureState(
        val runs: List<BlackRun>,
        val blackSize: Int,
        val totalSize: Int,
        val height: Int,
    )
}
