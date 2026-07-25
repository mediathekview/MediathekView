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
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.impl.adt

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.lang.reflect.Field
import java.util.*
import kotlin.math.abs

internal class SparseListBehaviorTest {
    @Test
    fun sparseInsertionRemovalAndNodeIdentityStayCharacterized() {
        val sparseList = SparseList()
        val alpha = "alpha"
        val beta = "beta"
        val gamma = "gamma"

        sparseList.addNulls(0, 2)
        sparseList.add(1, alpha)
        sparseList.add(3, beta)
        sparseList.addNulls(4, 1)
        sparseList.add(4, gamma)
        assertSparseListState(sparseList, listOf(null, alpha, null, beta, gamma, null))

        val alphaNode = sparseList.getNode(1)
        val betaNode = sparseList.getNode(3)
        val gammaNode = sparseList.getNode(4)
        assertNotNull(alphaNode)
        assertNotNull(betaNode)
        assertNotNull(gammaNode)

        assertEquals(alpha, sparseList.set(1, "alpha2"))
        assertSame(alphaNode, sparseList.getNode(1))
        assertSparseListState(sparseList, listOf(null, "alpha2", null, beta, gamma, null))

        assertEquals(null, sparseList.set(2, "delta"))
        assertSparseListState(sparseList, listOf(null, "alpha2", "delta", beta, gamma, null))
        val deltaNode = sparseList.getNode(2)
        assertNotNull(deltaNode)

        assertEquals(beta, sparseList.removeAt(3))
        assertSparseListState(sparseList, listOf(null, "alpha2", "delta", gamma, null))
        assertDetached(betaNode)
        assertSame(alphaNode, sparseList.getNode(1))
        assertSame(deltaNode, sparseList.getNode(2))
        assertSame(gammaNode, sparseList.getNode(3))
    }

    @Test
    fun iteratorAndAbstractListSurfaceStayCharacterized() {
        val traversalList = sparseListOf(null, "alpha", null, "beta", null)
        assertSparseListState(traversalList, listOf(null, "alpha", null, "beta", null))

        val uninitialized = traversalList.iterator()
        assertThrows(IllegalStateException::class.java) { uninitialized.remove() }

        val traversal = traversalList.iterator()
        assertNull(traversal.next())
        assertEquals("alpha", traversal.next())
        assertNull(traversal.next())
        assertEquals("beta", traversal.next())
        assertNull(traversal.next())
        assertFalse(traversal.hasNext())
        assertThrows(NoSuchElementException::class.java) { traversal.next() }

        val surfaceList = sparseListOf(null, "beta")
        val listIterator = surfaceList.listIterator()
        assertNull(listIterator.next())
        assertEquals("beta", listIterator.next())
        listIterator.set("beta2")
        assertSparseListState(surfaceList, listOf(null, "beta2"))
        listIterator.add("omega")
        assertSparseListState(surfaceList, listOf(null, "beta2", "omega"))
        assertEquals("omega", surfaceList.last())
        assertEquals(1, surfaceList.indexOf("beta2"))
        assertEquals(2, surfaceList.lastIndexOf("omega"))
        assertTrue(surfaceList.contains("beta2"))
        assertFalse(surfaceList.contains("missing"))
        assertEquals(listOf(null, "beta2", "omega"), surfaceList.subList(0, surfaceList.size))
        assertEquals(listOf(null, "beta2", "omega"), surfaceList.toList())
    }

    @Test
    fun clearReuseAndBoundaryFailureModesStayCharacterized() {
        val sparseList = SparseList()
        assertEquals(0, sparseList.size)
        assertNull(sparseList[0])
        assertNull(sparseList.getNode(0))
        assertNull(sparseList.set(0, null))
        assertEquals(0, sparseList.size)
        assertFalse(sparseList.iterator().hasNext())
        assertThrows(NoSuchElementException::class.java) { sparseList.iterator().next() }

        sparseList.addNulls(5, 2)
        assertSparseListState(sparseList, listOf(null, null))
        assertNull(sparseList[99])
        assertNull(sparseList.getNode(99))

        sparseList.add(1, "middle")
        assertSparseListState(sparseList, listOf(null, "middle", null))
        sparseList.clear()
        assertSparseListState(sparseList, emptyList())

        sparseList.addNulls(0, 3)
        sparseList[2] = "tail"
        assertSparseListState(sparseList, listOf(null, null, "tail"))
        sparseList.clear()
        sparseList.add(0, "reused")
        sparseList.add(1, "again")
        assertSparseListState(sparseList, listOf("reused", "again"))

        val invalidRemove = SparseList()
        assertNull(invalidRemove.removeAt(0))
        assertEquals(-1, invalidRemove.size)
    }

    @Test
    fun iteratorRemoveLegacyEdgeCasesStayCharacterized() {
        val leadingNullList = sparseListOf(null, "a", null, "b", null)
        val leadingNullIterator = leadingNullList.iterator()
        assertNull(leadingNullIterator.next())
        leadingNullIterator.remove()
        assertEquals(5, leadingNullList.size)
        assertEquals("a", leadingNullList[0])
        assertNull(leadingNullList[1])
        assertEquals("b", leadingNullList[2])
        assertThrows(NullPointerException::class.java) { leadingNullList[3] }
        assertNull(leadingNullList[4])
        assertNull(leadingNullIterator.next())
        assertEquals("b", leadingNullIterator.next())

        val firstValueList = sparseListOf(null, "a", null, "b", null)
        val firstValueIterator = firstValueList.iterator()
        assertNull(firstValueIterator.next())
        assertEquals("a", firstValueIterator.next())
        firstValueIterator.remove()
        assertEquals(5, firstValueList.size)
        assertNull(firstValueList[0])
        assertNull(firstValueList[1])
        assertEquals("b", firstValueList[2])
        assertThrows(NullPointerException::class.java) { firstValueList[3] }
        assertNull(firstValueList[4])
        assertNull(firstValueIterator.next())
        assertNull(firstValueIterator.next())

        val interiorNullList = sparseListOf(null, "a", null, "b", null)
        val interiorNullIterator = interiorNullList.iterator()
        assertNull(interiorNullIterator.next())
        assertEquals("a", interiorNullIterator.next())
        assertNull(interiorNullIterator.next())
        interiorNullIterator.remove()
        assertEquals(5, interiorNullList.size)
        assertNull(interiorNullList[0])
        assertEquals("a", interiorNullList[1])
        assertEquals("b", interiorNullList[2])
        assertThrows(NullPointerException::class.java) { interiorNullList[3] }
        assertNull(interiorNullList[4])
        assertThrows(NullPointerException::class.java) { interiorNullIterator.next() }
        assertNull(interiorNullIterator.next())

        val interiorValueList = sparseListOf(null, "a", null, "b", null)
        val interiorValueIterator = interiorValueList.iterator()
        assertNull(interiorValueIterator.next())
        assertEquals("a", interiorValueIterator.next())
        assertNull(interiorValueIterator.next())
        assertEquals("b", interiorValueIterator.next())
        assertThrows(NullPointerException::class.java) { interiorValueIterator.remove() }
        assertEquals(5, interiorValueList.size)
        assertNull(interiorValueList[0])
        assertEquals("a", interiorValueList[1])
        assertNull(interiorValueList[2])
        assertEquals("b", interiorValueList[3])
        assertNull(interiorValueList[4])
        assertNull(interiorValueIterator.next())
        assertThrows(NoSuchElementException::class.java) { interiorValueIterator.next() }

        val trailingNullList = sparseListOf(null, "a", null, "b", null)
        val trailingNullIterator = trailingNullList.iterator()
        assertNull(trailingNullIterator.next())
        assertEquals("a", trailingNullIterator.next())
        assertNull(trailingNullIterator.next())
        assertEquals("b", trailingNullIterator.next())
        assertNull(trailingNullIterator.next())
        trailingNullIterator.remove()
        assertSparseListState(trailingNullList, listOf(null, "a", null, "b"))
        assertThrows(NoSuchElementException::class.java) { trailingNullIterator.next() }

        val repeatedRemoveList = sparseListOf(null, "a", null, "b", null)
        val repeatedRemoveIterator = repeatedRemoveList.iterator()
        assertNull(repeatedRemoveIterator.next())
        assertEquals("a", repeatedRemoveIterator.next())
        assertNull(repeatedRemoveIterator.next())
        assertEquals("b", repeatedRemoveIterator.next())
        assertNull(repeatedRemoveIterator.next())
        repeatedRemoveIterator.remove()
        repeatedRemoveIterator.remove()
        assertEquals(3, repeatedRemoveList.size)
        assertEquals(4, treeSize(repeatedRemoveList))
        assertNull(repeatedRemoveList[0])
        assertEquals("a", repeatedRemoveList[1])
        assertNull(repeatedRemoveList[2])
        assertThrows(NoSuchElementException::class.java) { repeatedRemoveIterator.next() }
    }

    @Test
    fun detachedNodesClearTheirPrivateHostAcrossDeletionPaths() {
        val directRemovalLeafList = sparseListOf(null, "a", null, "b", null)
        val directRemovalLeafNode = directRemovalLeafList.getNode(3)
        assertNoChildren(directRemovalLeafNode)
        assertEquals("b", directRemovalLeafList.removeAt(3))
        assertDetached(directRemovalLeafNode)

        val setToNullLeafList = sparseListOf(null, "a", null, "b", null)
        val setToNullLeafNode = setToNullLeafList.getNode(3)
        assertNoChildren(setToNullLeafNode)
        assertEquals("b", setToNullLeafList.set(3, null))
        assertDetached(setToNullLeafNode)

        val oneChildList = sparseListOf(null, "a", "b", null)
        val oneChildNode = oneChildList.getNode(1)
        assertSingleChild(oneChildNode)
        assertEquals("a", oneChildList.removeAt(1))
        assertDetached(oneChildNode)

        val twoChildList = sparseListOf(null, "a", null, "b", null, "c", null)
        val twoChildNode = twoChildList.getNode(3)
        assertTwoChildren(twoChildNode)
        assertEquals("b", twoChildList.removeAt(3))
        assertDetached(twoChildNode)
    }

    @Test
    fun deletionShapesKeepExactPromotionsReplacementsAndCachesCharacterized() {
        run {
            val sparseList = sparseListOf("a", "b", "c")
            val aNode = sparseList.getNode(0)!!
            val bNode = sparseList.getNode(1)!!
            val cNode = sparseList.getNode(2)!!

            assertNodeState(bNode, "b", 1, null, aNode, cNode, 1, 1, 2, "leaf pre root")
            assertNodeState(aNode, "a", 0, bNode, null, null, 0, 0, 1, "leaf pre left")
            assertNodeState(cNode, "c", 2, bNode, null, null, 0, 0, 1, "leaf pre removed")

            assertEquals("c", sparseList.set(2, null))

            assertSparseListState(sparseList, listOf("a", "b", null), "leaf delete")
            assertSame(bNode, root(sparseList))
            assertNodeState(bNode, "b", 1, null, aNode, null, 1, 0, 2, "leaf post root")
            assertNodeState(aNode, "a", 0, bNode, null, null, 0, 0, 1, "leaf post left")
            assertDetached(cNode)
        }

        run {
            val sparseList = sparseListOf("a", "b")
            val aNode = sparseList.getNode(0)!!
            val bNode = sparseList.getNode(1)!!

            assertNodeState(aNode, "a", 0, null, null, bNode, 0, 1, 2, "right-only pre root")
            assertNodeState(bNode, "b", 1, aNode, null, null, 0, 0, 1, "right-only pre promoted")

            assertEquals("a", sparseList.removeAt(0))

            assertSparseListState(sparseList, listOf("b"), "right-only delete")
            assertSame(bNode, root(sparseList))
            assertNodeState(bNode, "b", 0, null, null, null, 0, 0, 1, "right-only post root")
            assertDetached(aNode)
        }

        run {
            val sparseList = sparseListOf("a", "b", "c")
            val aNode = sparseList.getNode(0)!!
            val bNode = sparseList.getNode(1)!!
            val cNode = sparseList.getNode(2)!!

            assertEquals("c", sparseList.removeAt(2))
            assertSparseListState(sparseList, listOf("a", "b"), "left-only setup")
            assertSame(bNode, root(sparseList))
            assertNodeState(bNode, "b", 1, null, aNode, null, 1, 0, 2, "left-only pre root")
            assertNodeState(aNode, "a", 0, bNode, null, null, 0, 0, 1, "left-only pre promoted")
            assertDetached(cNode)

            assertEquals("b", sparseList.removeAt(1))

            assertSparseListState(sparseList, listOf("a"), "left-only delete")
            assertSame(aNode, root(sparseList))
            assertNodeState(aNode, "a", 0, null, null, null, 0, 0, 1, "left-only post root")
            assertDetached(bNode)
        }

        run {
            val sparseList = sparseListOf("a", "b", "c")
            val aNode = sparseList.getNode(0)!!
            val bNode = sparseList.getNode(1)!!
            val cNode = sparseList.getNode(2)!!

            assertNodeState(bNode, "b", 1, null, aNode, cNode, 1, 1, 2, "direct successor pre root")
            assertNodeState(cNode, "c", 2, bNode, null, null, 0, 0, 1, "direct successor pre replacement")

            assertEquals("b", sparseList.removeAt(1))

            assertSparseListState(sparseList, listOf("a", "c"), "direct successor delete")
            assertSame(cNode, root(sparseList))
            assertNodeState(cNode, "c", 1, null, aNode, null, 1, 0, 2, "direct successor post root")
            assertNodeState(aNode, "a", 0, cNode, null, null, 0, 0, 1, "direct successor post left")
            assertDetached(bNode)
        }

        run {
            val sparseList = sparseListOf("a", "b", "c", "d", "e")
            val aNode = sparseList.getNode(0)!!
            val bNode = sparseList.getNode(1)!!
            val cNode = sparseList.getNode(2)!!
            val dNode = sparseList.getNode(3)!!
            val eNode = sparseList.getNode(4)!!

            assertNodeState(bNode, "b", 1, null, aNode, dNode, 1, 3, 3, "deep successor pre root")
            assertNodeState(aNode, "a", 0, bNode, null, null, 0, 0, 1, "deep successor pre left")
            assertNodeState(dNode, "d", 3, bNode, cNode, eNode, 1, 1, 2, "deep successor pre right")
            assertNodeState(cNode, "c", 2, dNode, null, null, 0, 0, 1, "deep successor pre replacement")
            assertNodeState(eNode, "e", 4, dNode, null, null, 0, 0, 1, "deep successor pre far right")

            assertEquals("b", sparseList.removeAt(1))

            assertSparseListState(sparseList, listOf("a", "c", "d", "e"), "deep successor delete")
            assertSame(cNode, root(sparseList))
            assertNodeState(cNode, "c", 1, null, aNode, dNode, 1, 2, 3, "deep successor post root")
            assertNodeState(aNode, "a", 0, cNode, null, null, 0, 0, 1, "deep successor post left")
            assertNodeState(dNode, "d", 2, cNode, null, eNode, 0, 1, 2, "deep successor post right")
            assertNodeState(eNode, "e", 3, dNode, null, null, 0, 0, 1, "deep successor post far right")
            assertDetached(bNode)
        }
    }

    @Test
    fun deletionTriggeredDoubleRotationsStayDeterministic() {
        run {
            val sparseList = sparseListByInsertPositions(0, 0, 0, 0)
            val dNode = sparseList.getNode(0)!!
            val cNode = sparseList.getNode(1)!!
            val bNode = sparseList.getNode(2)!!
            val aNode = sparseList.getNode(3)!!

            assertSparseListState(sparseList, listOf("d", "c", "b", "a"), "lr pre")
            assertSame(bNode, root(sparseList))
            assertNodeState(bNode, "b", 2, null, dNode, aNode, 2, 1, 3, "lr pre root")
            assertNodeState(dNode, "d", 0, bNode, null, cNode, 0, 1, 2, "lr pre left")
            assertNodeState(cNode, "c", 1, dNode, null, null, 0, 0, 1, "lr pre pivot")
            assertNodeState(aNode, "a", 3, bNode, null, null, 0, 0, 1, "lr pre removed")

            assertEquals("a", sparseList.removeAt(3))

            assertSparseListState(sparseList, listOf("d", "c", "b"), "lr post")
            assertSame(cNode, root(sparseList))
            assertNodeState(cNode, "c", 1, null, dNode, bNode, 1, 1, 2, "lr post root")
            assertNodeState(dNode, "d", 0, cNode, null, null, 0, 0, 1, "lr post left")
            assertNodeState(bNode, "b", 2, cNode, null, null, 0, 0, 1, "lr post right")
            assertDetached(aNode)
        }

        run {
            val sparseList = sparseListByInsertPositions(0, 0, 0, 1)
            val cNode = sparseList.getNode(0)!!
            val dNode = sparseList.getNode(1)!!
            val bNode = sparseList.getNode(2)!!
            val aNode = sparseList.getNode(3)!!

            assertSparseListState(sparseList, listOf("c", "d", "b", "a"), "rl pre")
            assertSame(dNode, root(sparseList))
            assertNodeState(dNode, "d", 1, null, cNode, aNode, 1, 2, 3, "rl pre root")
            assertNodeState(cNode, "c", 0, dNode, null, null, 0, 0, 1, "rl pre removed")
            assertNodeState(aNode, "a", 3, dNode, bNode, null, 1, 0, 2, "rl pre right")
            assertNodeState(bNode, "b", 2, aNode, null, null, 0, 0, 1, "rl pre pivot")

            assertEquals("c", sparseList.removeAt(0))

            assertSparseListState(sparseList, listOf("d", "b", "a"), "rl post")
            assertSame(bNode, root(sparseList))
            assertNodeState(bNode, "b", 1, null, dNode, aNode, 1, 1, 2, "rl post root")
            assertNodeState(dNode, "d", 0, bNode, null, null, 0, 0, 1, "rl post left")
            assertNodeState(aNode, "a", 2, bNode, null, null, 0, 0, 1, "rl post right")
            assertDetached(cNode)
        }
    }

    @Test
    fun fixedSeedModelOraclePreservesContentsIndicesAndTreeInvariants() {
        val sparseList = SparseList()
        val expected = mutableListOf<Any?>()
        val random = Random(0x5A17E123L)
        var nextValue = 0

        repeat(400) { step ->
            when (random.nextInt(6)) {
                0 -> {
                    val index = if (expected.isEmpty()) 0 else random.nextInt(expected.size + 1)
                    val value = "v${nextValue++}"
                    sparseList.add(index, value)
                    expected.add(index, value)
                }
                1 -> {
                    val index = if (expected.isEmpty()) 0 else random.nextInt(expected.size + 1)
                    sparseList.add(index, null)
                    expected.add(index, null)
                }
                2 -> {
                    val index = if (expected.isEmpty()) 0 else random.nextInt(expected.size + 1)
                    val length = random.nextInt(3) + 1
                    sparseList.addNulls(index, length)
                    repeat(length) { offset -> expected.add(index + offset, null) }
                }
                3 -> {
                    if (expected.isNotEmpty()) {
                        val index = random.nextInt(expected.size)
                        val replacement = if (random.nextBoolean()) null else "v${nextValue++}"
                        val old = sparseList.set(index, replacement)
                        assertEquals(expected[index], old, "set return mismatch at step $step")
                        expected[index] = replacement
                    }
                }
                4 -> {
                    if (expected.isNotEmpty()) {
                        val index = random.nextInt(expected.size)
                        val removed = sparseList.removeAt(index)
                        assertEquals(expected.removeAt(index), removed, "remove return mismatch at step $step")
                    }
                }
                else -> {
                    if (random.nextInt(9) == 0) {
                        sparseList.clear()
                        expected.clear()
                    }
                }
            }

            assertSparseListState(sparseList, expected, "after random step $step")
        }
    }

    private fun assertSparseListState(sparseList: SparseList, expected: List<Any?>, message: String = "state mismatch") {
        assertEquals(expected.size, sparseList.size, "$message size")
        assertEquals(expected, List(sparseList.size) { sparseList[it] }, "$message contents")
        assertEquals(expected.indexOf("missing"), sparseList.indexOf("missing"), "$message indexOf")
        assertEquals(expected.lastIndexOf("missing"), sparseList.lastIndexOf("missing"), "$message lastIndexOf")

        val root = root(sparseList)
        val expectedTreeSize = expected.lastIndexOfLastNonNull() + 1

        if (root == null) {
            return
        }

        assertEquals(expectedTreeSize, treeSize(sparseList), "$message treeSize")

        val visited = IdentityHashMap<SparseListNode, Int>()
        val nodesInOrder = mutableListOf<SparseListNode>()
        val validation = validateNode(root, null, visited, nodesInOrder, message)
        assertEquals(expectedTreeSize, validation.size, "$message root subtree size")
        assertEquals(expectedTreeSize, rootSize(root), "$message root.size()")
        assertEquals(expected.count { it != null }, validation.nodeCount, "$message non-null node count")
        assertNull(parent(root), "$message root parent")
        assertTrue(nodesInOrder.isNotEmpty(), "$message nodes")
        assertEquals(expectedTreeSize - 1, nodesInOrder.last().index, "$message last node index")

        val expectedNonNullIndices = expected.indices.filter { it < expectedTreeSize && expected[it] != null }
        assertEquals(expectedNonNullIndices, nodesInOrder.map { it.index }, "$message in-order indices")

        expected.indices.forEach { index ->
                val node = sparseList.getNode(index)
                val expectedValue = expected[index]
                if (index < expectedTreeSize && expectedValue != null) {
                    assertNotNull(node, "$message missing node at $index")
                    assertEquals(index, node!!.index, "$message node index at $index")
                    assertEquals(expectedValue, node.value, "$message node value at $index")
                    assertSame(node, sparseList.getNode(node.index), "$message node identity at $index")
                } else {
                    assertNull(node, "$message unexpected node at $index")
                }
            }
        }

    private fun validateNode(
        node: SparseListNode,
        expectedParent: SparseListNode?,
        visited: IdentityHashMap<SparseListNode, Int>,
        nodesInOrder: MutableList<SparseListNode>,
        message: String,
    ): TreeValidation {
        assertNull(visited.put(node, 1), "$message cycle at ${node.index}")
        assertSame(expectedParent, parent(node), "$message parent link at ${node.index}")
        assertNotNull(node.value, "$message null node value at ${node.index}")

        val left = left(node)
        val right = right(node)
        val leftValidation = left?.let { validateNode(it, node, visited, nodesInOrder, message) } ?: TreeValidation(0, 0, 0)
        nodesInOrder += node
        val rightValidation = right?.let { validateNode(it, node, visited, nodesInOrder, message) } ?: TreeValidation(0, 0, 0)

        assertEquals(leftValidation.size, totalLeftSize(node), "$message totalLeftSize at ${node.index}")
        assertEquals(rightValidation.size, totalRightSize(node), "$message totalRightSize at ${node.index}")
        val expectedHeight = 1 + maxOf(leftValidation.height, rightValidation.height)
        assertEquals(expectedHeight, height(node), "$message height at ${node.index}")
        assertTrue(abs(leftValidation.height - rightValidation.height) <= 1, "$message balance at ${node.index}")

        val subtreeSize = leftValidation.size + emptySpace(node) + rightValidation.size + 1
        assertEquals(subtreeSize, rootSize(node), "$message subtree size at ${node.index}")
        return TreeValidation(subtreeSize, expectedHeight, leftValidation.nodeCount + rightValidation.nodeCount + 1)
    }

    private fun assertDetached(node: SparseListNode?) {
        assertNotNull(node)
        assertNull(parent(node!!))
        assertNull(left(node))
        assertNull(right(node))
        assertNull(host(node))
        assertNull(node.value)
        assertEquals(0, emptySpace(node))
        assertEquals(0, totalLeftSize(node))
        assertEquals(0, totalRightSize(node))
        assertEquals(-1, height(node))
    }

    private fun assertNodeState(
        node: SparseListNode?,
        expectedValue: Any?,
        expectedIndex: Int,
        expectedParent: SparseListNode?,
        expectedLeft: SparseListNode?,
        expectedRight: SparseListNode?,
        expectedTotalLeftSize: Int,
        expectedTotalRightSize: Int,
        expectedHeight: Int,
        message: String,
    ) {
        assertNotNull(node, "$message node")
        val actualNode = node!!
        assertEquals(expectedValue, actualNode.value, "$message value")
        assertEquals(expectedIndex, actualNode.index, "$message index")
        assertSame(expectedParent, parent(actualNode), "$message parent")
        assertSame(expectedLeft, left(actualNode), "$message left")
        assertSame(expectedRight, right(actualNode), "$message right")
        assertEquals(0, emptySpace(actualNode), "$message emptySpace")
        assertEquals(expectedTotalLeftSize, totalLeftSize(actualNode), "$message totalLeftSize")
        assertEquals(expectedTotalRightSize, totalRightSize(actualNode), "$message totalRightSize")
        assertEquals(expectedHeight, height(actualNode), "$message height")
    }

    private fun assertNoChildren(node: SparseListNode?) {
        assertNotNull(node)
        assertNull(left(node!!))
        assertNull(right(node))
    }

    private fun assertSingleChild(node: SparseListNode?) {
        assertNotNull(node)
        val nodeValue = node!!
        val childCount = listOf(left(nodeValue), right(nodeValue)).count { it != null }
        assertEquals(1, childCount)
    }

    private fun assertTwoChildren(node: SparseListNode?) {
        assertNotNull(node)
        assertNotNull(left(node!!))
        assertNotNull(right(node))
    }

    private fun List<Any?>.lastIndexOfLastNonNull(): Int = indexOfLast { it != null }

    private fun root(sparseList: SparseList): SparseListNode? = rootField.get(sparseList) as SparseListNode?

    private fun treeSize(sparseList: SparseList): Int = treeSizeField.getInt(sparseList)

    private fun sparseListOf(vararg values: Any?): SparseList =
        SparseList().apply {
            values.forEachIndexed { index, value ->
                if (value == null) {
                    addNulls(index, 1)
                } else {
                    add(index, value)
                }
            }
        }

    private fun sparseListByInsertPositions(vararg positions: Int): SparseList =
        SparseList().apply {
            positions.forEachIndexed { step, index ->
                add(index, ('a'.code + step).toChar().toString())
            }
        }

    private fun parent(node: SparseListNode): SparseListNode? = parentField.get(node) as SparseListNode?

    private fun left(node: SparseListNode): SparseListNode? = leftField.get(node) as SparseListNode?

    private fun right(node: SparseListNode): SparseListNode? = rightField.get(node) as SparseListNode?

    private fun host(node: SparseListNode): SparseList? = hostField.get(node) as SparseList?

    private fun emptySpace(node: SparseListNode): Int = emptySpaceField.getInt(node)

    private fun totalLeftSize(node: SparseListNode): Int = totalLeftSizeField.getInt(node)

    private fun totalRightSize(node: SparseListNode): Int = totalRightSizeField.getInt(node)

    private fun height(node: SparseListNode): Int = heightField.getInt(node)

    private fun rootSize(node: SparseListNode): Int = totalLeftSize(node) + emptySpace(node) + totalRightSize(node) + 1

    private data class TreeValidation(
        val size: Int,
        val height: Int,
        val nodeCount: Int,
    )

    private companion object {
        private val rootField = SparseList::class.java.declaredField("root")
        private val treeSizeField = SparseList::class.java.declaredField("treeSize")
        private val parentField = SparseListNode::class.java.declaredField("parent")
        private val hostField = SparseListNode::class.java.declaredField("host")
        private val leftField = SparseListNode::class.java.declaredField("left")
        private val rightField = SparseListNode::class.java.declaredField("right")
        private val emptySpaceField = SparseListNode::class.java.declaredField("emptySpace")
        private val totalLeftSizeField = SparseListNode::class.java.declaredField("totalLeftSize")
        private val totalRightSizeField = SparseListNode::class.java.declaredField("totalRightSize")
        private val heightField = SparseListNode::class.java.declaredField("height")

        init {
            listOf(
                rootField,
                treeSizeField,
                parentField,
                hostField,
                leftField,
                rightField,
                emptySpaceField,
                totalLeftSizeField,
                totalRightSizeField,
                heightField,
            ).forEach { it.isAccessible = true }
        }

        private fun Class<*>.declaredField(name: String): Field = getDeclaredField(name)
    }
}
