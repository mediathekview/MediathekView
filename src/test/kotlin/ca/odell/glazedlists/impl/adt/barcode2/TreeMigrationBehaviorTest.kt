package ca.odell.glazedlists.impl.adt.barcode2

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.lang.reflect.Field
import java.util.*
import kotlin.math.abs

internal class TreeMigrationBehaviorTest {
    @Test
    fun fourColorCountsMasksAndRankSelectStayCharacterized() {
        val coder = ListToByteCoder(listOf("A", "B", "C", "D"))
        val tree = FourColorTree<Any?>(coder)
        val a = coder.colorToByte("A")
        val b = coder.colorToByte("B")
        val c = coder.colorToByte("C")
        val d = coder.colorToByte("D")
        val allColors = coder.allColorsToByte()
        val acColors = coder.colorsToByte(listOf("A", "C"))
        val bdColors = coder.colorsToByte(listOf("B", "D"))
        val aValue = Any()
        val bValue = Any()
        val cValue = Any()
        val dValue = Any()

        val aNode = tree.add(0, allColors, a, aValue, 3)
        val bNode = tree.add(3, allColors, b, bValue, 2)
        val cNode = tree.add(5, allColors, c, cValue, 1)
        val dNode = tree.add(6, allColors, d, dValue, 2)

        assertEquals("AAABBCDD", tree.asSequenceOfColors())
        assertEquals(8, tree.size(allColors))
        assertEquals(3, tree.size(a))
        assertEquals(2, tree.size(b))
        assertEquals(1, tree.size(c))
        assertEquals(2, tree.size(d))
        assertEquals(4, tree.size(acColors))
        assertEquals(4, tree.size(bdColors))
        assertSame(aNode, tree[1, a])
        assertSame(bNode, tree[0, b])
        assertSame(cNode, tree[5, allColors])
        assertSame(dNode, tree[1, d])
        assertEquals(3, tree.convertIndexColor(0, b, allColors))
        assertEquals(5, tree.convertIndexColor(0, c, allColors))
        assertEquals(7, tree.convertIndexColor(1, d, allColors))
        assertEquals(2, tree.convertIndexColor(3, allColors, a))
        assertEquals(3, tree.convertIndexColor(6, allColors, acColors))
        assertNull(aNode.previous())
        assertSame(bNode, aNode.next())
        assertSame(cNode, bNode.next())
        assertSame(dNode, cNode.next())
        assertNull(dNode.next())
        assertEquals("   A [3]: $aValue\nB [2]: $bValue\n   C [1]: $cValue\n      D [2]: $dValue\n", tree.toString())
        assertFourColorTreeStateMatches(
            tree,
            coder,
            listOf(
                FourColorExpectation("A", 3, aValue),
                FourColorExpectation("B", 2, bValue),
                FourColorExpectation("C", 1, cValue),
                FourColorExpectation("D", 2, dValue),
            ),
        )
    }

    @Test
    fun fourColorNodeSplitMergeIdentityAndRemovalStayCharacterized() {
        val coder = ListToByteCoder(listOf("A", "B", "C", "D"))
        val tree = FourColorTree<Any?>(coder)
        val a = coder.colorToByte("A")
        val b = coder.colorToByte("B")
        val c = coder.colorToByte("C")
        val allColors = coder.allColorsToByte()
        val sharedValue = Any()
        val blueValue = Any()

        val mergedNode = tree.add(0, allColors, a, sharedValue, 2)
        val mergedAgain = tree.add(2, allColors, a, sharedValue, 1)
        assertSame(mergedNode, mergedAgain)
        assertFourColorTreeStateMatches(tree, coder, listOf(FourColorExpectation("A", 3, sharedValue)))

        tree.add(1, allColors, b, blueValue, 2)
        assertEquals(0, tree.indexOfNode(mergedNode, a))
        assertSame(blueValue, mergedNode.next()!!.get())
        assertFourColorTreeStateMatches(
            tree,
            coder,
            listOf(
                FourColorExpectation("A", 1, sharedValue),
                FourColorExpectation("B", 2, blueValue),
                FourColorExpectation("A", 2, sharedValue),
            ),
        )

        tree.remove(1, allColors, 2)
        assertFourColorTreeStateMatches(
            tree,
            coder,
            listOf(
                FourColorExpectation("A", 1, sharedValue),
                FourColorExpectation("A", 2, sharedValue),
            ),
        )

        val nullLeft = tree.add(1, allColors, b, null, 1)
        val nullRight = tree.add(2, allColors, b, null, 1)
        assertFalse(nullLeft === nullRight)

        val distinctLeftValue = String(charArrayOf('v'))
        val distinctRightValue = String(charArrayOf('v'))
        val distinctLeft = tree.add(tree.size(allColors), allColors, c, distinctLeftValue, 1)
        val distinctRight = tree.add(tree.size(allColors), allColors, c, distinctRightValue, 1)

        assertFalse(distinctLeft === distinctRight)
        assertEquals(0, tree.indexOfNode(distinctLeft, c))
        assertEquals(1, tree.indexOfNode(distinctRight, c))
        assertFourColorTreeStateMatches(
            tree,
            coder,
            listOf(
                FourColorExpectation("A", 1, sharedValue),
                FourColorExpectation("B", 1, null),
                FourColorExpectation("B", 1, null),
                FourColorExpectation("A", 2, sharedValue),
                FourColorExpectation("C", 1, distinctLeftValue),
                FourColorExpectation("C", 1, distinctRightValue),
            ),
        )
    }

    @Test
    fun fourColorTransitionsAndIteratorStateStayCharacterized() {
        val coder = ListToByteCoder(listOf("A", "B", "C", "D"))
        val tree = FourColorTree<Any?>(coder)
        val a = coder.colorToByte("A")
        val b = coder.colorToByte("B")
        val c = coder.colorToByte("C")
        val d = coder.colorToByte("D")
        val allColors = coder.allColorsToByte()
        val acColors = coder.colorsToByte(listOf("A", "C"))
        val aValue = Any()
        val bValue = Any()
        val cValue = Any()
        val dValue = Any()

        tree.add(0, allColors, a, aValue, 2)
        val recolored = tree.add(2, allColors, b, bValue, 1)
        tree.add(3, allColors, c, cValue, 2)
        tree.add(5, allColors, d, dValue, 1)

        tree.setColor(recolored, d)
        assertEquals("AADCCD", tree.asSequenceOfColors())
        assertEquals(0, tree.size(b))
        assertEquals(2, tree.size(d))
        assertFourColorTreeStateMatches(
            tree,
            coder,
            listOf(
                FourColorExpectation("A", 2, aValue),
                FourColorExpectation("D", 1, bValue),
                FourColorExpectation("C", 2, cValue),
                FourColorExpectation("D", 1, dValue),
            ),
        )

        val iterator = FourColorTreeIterator(tree, 2, acColors)
        assertTrue(iterator.hasNext(acColors))
        iterator.next(acColors)
        assertSame(cValue, iterator.value())
        assertEquals(c, iterator.color())
        assertEquals(2, iterator.index(acColors))
        assertEquals(2, iterator.nodeStartIndex(acColors))
        assertEquals(4, iterator.nodeEndIndex(acColors))
        assertEquals(0, iterator.nodeSize(b))

        val copy = iterator.copy()
        iterator.next(acColors)
        assertEquals(3, iterator.index(acColors))
        assertEquals(2, copy.index(acColors))
        copy.next(acColors)
        assertEquals(3, copy.index(acColors))
        assertFalse(iterator.hasNextNode(acColors))
        assertFalse(copy.hasNextNode(acColors))

        val dIterator = FourColorTreeIterator(tree)
        assertTrue(dIterator.hasNextNode(d))
        dIterator.nextNode(d)
        assertSame(bValue, dIterator.value())
        assertEquals(0, dIterator.nodeStartIndex(d))
        assertEquals(1, dIterator.nodeEndIndex(d))
        dIterator.nextNode(d)
        assertSame(dValue, dIterator.value())
        assertEquals(1, dIterator.nodeStartIndex(d))
        assertEquals(2, dIterator.nodeEndIndex(d))
    }

    @Test
    fun fourColorBoundaryAndFailureModesStayCharacterized() {
        val coder = ListToByteCoder(listOf("A", "B", "C", "D"))
        val tree = FourColorTree<String>(coder)
        val allColors = coder.allColorsToByte()
        val a = coder.colorToByte("A")

        assertEquals(0, tree.size(allColors))
        assertThrows(IndexOutOfBoundsException::class.java) { tree[0, allColors] }
        assertEquals(0, tree.convertIndexColor(0, allColors, allColors))
        assertThrows(IndexOutOfBoundsException::class.java) { tree.convertIndexColor(1, allColors, allColors) }

        val emptyIterator = FourColorTreeIterator(tree)
        assertFalse(emptyIterator.hasNext(allColors))
        assertFalse(emptyIterator.hasNextNode(allColors))
        assertThrows(NoSuchElementException::class.java) { emptyIterator.next(allColors) }
        assertThrows(NoSuchElementException::class.java) { emptyIterator.nextNode(allColors) }
        assertThrows(NoSuchElementException::class.java) { emptyIterator.index(allColors) }
        assertThrows(NoSuchElementException::class.java) { emptyIterator.nodeStartIndex(allColors) }
        assertThrows(NoSuchElementException::class.java) { emptyIterator.nodeEndIndex(allColors) }
        assertThrows(NullPointerException::class.java) { emptyIterator.nodeSize(allColors) }
        assertThrows(IllegalStateException::class.java) { emptyIterator.color() }
        assertThrows(IllegalStateException::class.java) { emptyIterator.value() }
        assertThrows(IllegalStateException::class.java) { emptyIterator.node() }

        tree.add(0, allColors, a, "only", 1)
        val endIterator = FourColorTreeIterator(tree, tree.size(allColors), allColors)
        assertFalse(endIterator.hasNext(allColors))
        assertFalse(endIterator.hasNextNode(allColors))
    }

    @Test
    fun indexedInsertionRemovalAndLinksStayCharacterized() {
        val tree = SimpleTree<String>()
        val c = tree.add(0, "c", 1)
        val b = tree.add(0, "b", 1)
        val a = tree.add(0, "a", 1)
        val d = tree.add(3, "d", 1)

        assertEquals(listOf("a", "b", "c", "d"), treeValues(tree))
        assertEquals(4, tree.size())
        assertEquals(0, tree.indexOfNode(a, 1))
        assertEquals(1, tree.indexOfNode(b, 1))
        assertEquals(2, tree.indexOfNode(c, 1))
        assertEquals(3, tree.indexOfNode(d, 1))
        assertEquals(2, tree.convertIndexColor(2, 1, 1))

        assertNodeState(rootNode(tree), value = "b", count1 = 4, height = 3, parent = null, left = "a", right = "c")
        assertNodeState(c as SimpleNode<*>, value = "c", count1 = 2, height = 2, parent = "b", left = null, right = "d")
        assertNodeState(d as SimpleNode<*>, value = "d", count1 = 1, height = 1, parent = "c", left = null, right = null)

        tree.remove(2, 1)

        val expectedValues = listOf("a", "b", "d")
        assertEquals(expectedValues, treeValues(tree))
        assertEquals(expectedValues.size, tree.size())
        assertEquals(0, tree.indexOfNode(a, 1))
        assertEquals(1, tree.indexOfNode(b, 1))
        assertEquals(2, tree.indexOfNode(d, 1))
        assertNodeState(rootNode(tree), value = "b", count1 = 3, height = 2, parent = null, left = "a", right = "d")
        assertNodeState(d as SimpleNode<*>, value = "d", count1 = 1, height = 1, parent = "b", left = null, right = null)
        assertTreeStateMatches(tree, expectedValues)

        tree.clear()
        assertEquals(emptyList<String>(), treeValues(tree))
        assertEquals(0, tree.size())
        assertEquals("", tree.toString())
    }

    @Test
    fun sortedInsertionDuplicateIdentityAndUnsortedNodesStayCharacterized() {
        val duplicates = SimpleTree<Int>(Comparator.naturalOrder())
        val firstTwo = duplicates.addInSortedOrder(1, 2, 1)
        duplicates.addInSortedOrder(1, 1, 1)
        val secondTwo = duplicates.addInSortedOrder(1, 2, 1)
        val thirdTwo = duplicates.addInSortedOrder(1, 2, 1)
        duplicates.addInSortedOrder(1, 3, 1)

        assertEquals(listOf(1, 2, 2, 2, 3), treeValues(duplicates))
        assertEquals(1, duplicates.indexOfNode(firstTwo, 1))
        assertEquals(3, duplicates.indexOfNode(secondTwo, 1))
        assertEquals(2, duplicates.indexOfNode(thirdTwo, 1))
        assertEquals(1, duplicates.indexOfValue(2, firstIndex = true, simulated = false, colorsOut = 1))
        assertEquals(3, duplicates.indexOfValue(2, firstIndex = false, simulated = false, colorsOut = 1))
        assertEquals(-1, duplicates.indexOfValue(0, firstIndex = true, simulated = false, colorsOut = 1))
        assertEquals(0, duplicates.indexOfValue(0, firstIndex = true, simulated = true, colorsOut = 1))
        assertEquals(5, duplicates.indexOfValue(4, firstIndex = true, simulated = true, colorsOut = 1))
        assertTreeStateMatches(duplicates, listOf(1, 2, 2, 2, 3))

        val unsorted = SimpleTree<Int>(Comparator.naturalOrder())
        val one = unsorted.addInSortedOrder(1, 1, 1)
        val three = unsorted.addInSortedOrder(1, 3, 1)
        one.sorted = Element.UNSORTED
        val two = unsorted.addInSortedOrder(1, 2, 1)

        assertEquals(listOf(2, 1, 3), treeValues(unsorted))
        assertEquals(0, unsorted.indexOfNode(two, 1))
        assertEquals(1, unsorted.indexOfNode(one, 1))
        assertEquals(2, unsorted.indexOfNode(three, 1))
        assertEquals(Element.UNSORTED, one.sorted)
        assertTreeStateMatches(unsorted, listOf(2, 1, 3))
    }

    @Test
    fun avlInsertionRotationsStayCharacterized() {
        assertSortedRotation(listOf(3, 2, 1))
        assertSortedRotation(listOf(1, 2, 3))
        assertSortedRotation(listOf(3, 1, 2))
        assertSortedRotation(listOf(1, 3, 2))
    }

    @Test
    fun deletionTopologiesAndBalancingStayCharacterized() {
        val twoChildRoot = SimpleTree<Int>(Comparator.naturalOrder())
        val insertedByValue = mutableMapOf<Int, Element<Int>>()
        listOf(4, 2, 6, 1, 3, 5, 7).forEach { value ->
            insertedByValue[value] = twoChildRoot.addInSortedOrder(1, value, 1)
        }
        twoChildRoot.remove(insertedByValue.getValue(4))

        assertTreeStateMatches(twoChildRoot, listOf(1, 2, 3, 5, 6, 7))
        assertNodeState(rootNode(twoChildRoot), value = 3, count1 = 6, height = 3, parent = null, left = 2, right = 6)
        assertNodeState(rootNode(twoChildRoot)?.left, value = 2, count1 = 2, height = 2, parent = 3, left = 1, right = null)
        assertNodeState(rootNode(twoChildRoot)?.right, value = 6, count1 = 3, height = 2, parent = 3, left = 5, right = 7)

        val deletionRebalance = SimpleTree<Int>(Comparator.naturalOrder())
        listOf(1, 2, 3, 4).forEach { value ->
            deletionRebalance.addInSortedOrder(1, value, 1)
        }
        deletionRebalance.remove(0, 1)

        assertTreeStateMatches(deletionRebalance, listOf(2, 3, 4))
        assertNodeState(rootNode(deletionRebalance), value = 3, count1 = 3, height = 2, parent = null, left = 2, right = 4)
    }

    @Test
    fun comparatorEqualDuplicateIdentitySurvivesDeletionAndReinsertion() {
        val duplicates = SimpleTree(compareBy(KeyedValue::key))
        val first = duplicates.addInSortedOrder(1, KeyedValue(2, "first"), 1)
        duplicates.addInSortedOrder(1, KeyedValue(1, "one"), 1)
        val second = duplicates.addInSortedOrder(1, KeyedValue(2, "second"), 1)
        val third = duplicates.addInSortedOrder(1, KeyedValue(2, "third"), 1)
        duplicates.addInSortedOrder(1, KeyedValue(3, "three"), 1)

        duplicates.remove(second)
        val replacement = duplicates.addInSortedOrder(1, KeyedValue(2, "replacement"), 1)

        val values = treeValues(duplicates)
        assertEquals(listOf(1, 2, 2, 2, 3), values.map(KeyedValue::key))
        assertEquals(setOf("first", "third", "replacement"), values.filter { it.key == 2 }.map(KeyedValue::label).toSet())
        assertEquals(1, duplicates.indexOfValue(KeyedValue(2, "probe"), firstIndex = true, simulated = false, colorsOut = 1))
        assertEquals(3, duplicates.indexOfValue(KeyedValue(2, "probe"), firstIndex = false, simulated = false, colorsOut = 1))

        val duplicateIndices = listOf(first, third, replacement).map { duplicates.indexOfNode(it, 1) }
        assertEquals(3, duplicateIndices.toSet().size)
        duplicateIndices.forEach { index -> assertTrue(index in 1..3) }
        listOf(first, third, replacement).forEach { duplicate ->
            assertSame(duplicate, duplicates[duplicates.indexOfNode(duplicate, 1)])
        }

        assertTreeStateMatches(duplicates, values)
    }

    @Test
    fun fixedSeedListOracleKeepsOrderStructureAndIteratorIndices() {
        val random = Random(0x5EEDC0DEL)
        val tree = SimpleTree<Int?>()
        val expected = mutableListOf<Int?>()

        repeat(200) {
            when {
                expected.isEmpty() -> {
                    val value = nextOracleValue(random)
                    tree.add(0, value, 1)
                    expected += value
                }
                else -> {
                    when (random.nextInt(4)) {
                        0 -> {
                            val index = random.nextInt(expected.size + 1)
                            val value = nextOracleValue(random)
                            tree.add(index, value, 1)
                            expected.add(index, value)
                        }

                        1 -> {
                            val index = random.nextInt(expected.size)
                            tree.remove(index, 1)
                            expected.removeAt(index)
                        }

                        2 -> {
                            val index = random.nextInt(expected.size)
                            val element = tree[index]
                            tree.remove(element)
                            expected.removeAt(index)
                        }

                        else -> {
                            val index = random.nextInt(expected.size)
                            val value = nextOracleValue(random)
                            tree.set(index, value, 1)
                            expected[index] = value
                        }
                    }
                }
            }

            assertTreeStateMatches(tree, expected)
        }
    }

    @Test
    fun iteratorStartCopyTraversalAndFailureModesStayCharacterized() {
        val emptyTree = SimpleTree<String>()
        val emptyIterator = SimpleTreeIterator(emptyTree)

        assertFalse(emptyIterator.hasNext())
        assertFalse(emptyIterator.hasNextNode())
        assertThrows(NoSuchElementException::class.java) { emptyIterator.next() }
        assertThrows(NoSuchElementException::class.java) { emptyIterator.nextNode() }
        assertThrows(NoSuchElementException::class.java) { emptyIterator.index() }
        assertThrows(NoSuchElementException::class.java) { emptyIterator.nodeStartIndex() }
        assertThrows(NoSuchElementException::class.java) { emptyIterator.nodeEndIndex() }
        assertThrows(IllegalStateException::class.java) { emptyIterator.value() }
        assertThrows(IllegalStateException::class.java) { emptyIterator.node() }

        val tree = SimpleTree<String>()
        tree.add(0, "a", 1)
        val b = tree.add(1, "b", 1)
        tree.add(2, "c", 1)

        val iterator = SimpleTreeIterator(tree, 1, 1)
        assertTrue(iterator.hasNext())
        iterator.next()
        assertEquals("b", iterator.value())
        assertSame(b, iterator.node())
        assertEquals(1, iterator.index())
        assertEquals(1, iterator.nodeStartIndex())
        assertEquals(2, iterator.nodeEndIndex())

        val copy = iterator.copy()
        iterator.next()
        assertEquals("c", iterator.value())
        copy.next()
        assertEquals("c", copy.value())
        assertFalse(iterator.hasNext())
        assertFalse(iterator.hasNextNode())

        val nodeIterator = SimpleTreeIterator(tree)
        nodeIterator.nextNode()
        assertEquals("a", nodeIterator.value())
        nodeIterator.nextNode()
        assertEquals("b", nodeIterator.value())

        val endIterator = SimpleTreeIterator(tree, tree.size(), 1)
        assertFalse(endIterator.hasNext())
        assertFalse(endIterator.hasNextNode())
    }

    @Test
    fun emptySingletonAndOutOfRangeCasesStayCharacterized() {
        val empty = SimpleTree<String>()
        assertThrows(IndexOutOfBoundsException::class.java) { empty[0] }
        assertThrows(AssertionError::class.java) { empty.add(1, "x", 1) }
        assertEquals(0, empty.convertIndexColor(0, 1, 1))
        assertThrows(IndexOutOfBoundsException::class.java) { empty.convertIndexColor(1, 1, 1) }

        val singleton = SimpleTree<String>()
        val only = singleton.add(0, "only", 1)
        assertEquals(listOf("only"), treeValues(singleton))
        assertSame(only, singleton[0])
        assertNull(only.previous())
        assertNull(only.next())
        assertTreeStateMatches(singleton, listOf("only"))
    }

    private fun <T> treeValues(tree: SimpleTree<T>): List<T> {
        val iterator = SimpleTreeIterator(tree)
        val result = mutableListOf<T>()
        while (iterator.hasNext()) {
            iterator.next()
            result += iterator.value()
        }
        return result
    }

    private fun assertSortedRotation(insertionOrder: List<Int>) {
        val tree = SimpleTree<Int>(Comparator.naturalOrder())
        insertionOrder.forEach { value ->
            tree.addInSortedOrder(1, value, 1)
        }

        assertTreeStateMatches(tree, listOf(1, 2, 3))
        assertNodeState(rootNode(tree), value = 2, count1 = 3, height = 2, parent = null, left = 1, right = 3)
    }

    private fun <T> assertTreeStateMatches(tree: SimpleTree<T>, expected: List<T>) {
        assertEquals(expected, treeValues(tree))
        assertEquals(expected.size, tree.size())

        val root = rootNode(tree)
        if (expected.isEmpty()) {
            assertNull(root)
        } else {
            assertNotNull(root)
        }

        val structure = assertStructure(root, null)
        assertEquals(expected, structure.values)
        assertEquals(expected.size, structure.count)
        assertIteratorState(tree, expected)

        if (expected.isEmpty()) {
            assertEquals(0, tree.convertIndexColor(0, 1, 1))
        } else {
            expected.indices.forEach { index ->
                assertEquals(index, tree.convertIndexColor(index, 1, 1))
            }
        }
    }

    private fun <T> assertIteratorState(tree: SimpleTree<T>, expected: List<T>) {
        val iterator = SimpleTreeIterator(tree)
        val elements = mutableListOf<Element<T>>()
        expected.forEachIndexed { index, value ->
            assertTrue(iterator.hasNext())
            assertTrue(iterator.hasNextNode())
            iterator.next()
            assertEquals(value, iterator.value())
            assertEquals(index, iterator.index())
            assertEquals(index, iterator.nodeStartIndex())
            assertEquals(index + 1, iterator.nodeEndIndex())
            val current = iterator.node()
            elements += current
            assertEquals(index, tree.indexOfNode(current, 1))
        }
        assertFalse(iterator.hasNext())
        assertFalse(iterator.hasNextNode())

        elements.forEachIndexed { index, element ->
            assertSame(elements.getOrNull(index - 1), element.previous())
            assertSame(elements.getOrNull(index + 1), element.next())
        }
    }

    private fun <T> assertStructure(node: SimpleNode<T>?, parent: SimpleNode<T>?): StructureState<T> {
        if (node == null) {
            return StructureState(emptyList(), 0, 0)
        }

        assertSame(parent, node.parent)

        val left = assertStructure(node.left, node)
        val right = assertStructure(node.right, node)
        val expectedCount = left.count + 1 + right.count
        val expectedHeight = maxOf(left.height, right.height) + 1

        assertEquals(expectedCount, node.count1)
        assertEquals(expectedHeight.toByte(), node.height)
        assertTrue(abs(left.height - right.height) < 2, "AVL imbalance at ${node.t0}")

        return StructureState(left.values + node.get() + right.values, expectedCount, expectedHeight)
    }

    private fun assertFourColorTreeStateMatches(
        tree: FourColorTree<*>,
        coder: ListToByteCoder<String>,
        expected: List<FourColorExpectation>,
    ) {
        val allColors = coder.allColorsToByte()
        val flattenedColors = buildString {
            expected.forEach { block -> repeat(block.size) { append(block.label) } }
        }
        assertEquals(flattenedColors, tree.asSequenceOfColors())
        assertEquals(expected.sumOf(FourColorExpectation::size), tree.size(allColors))
        assertEquals(expected.filter { it.label == "A" }.sumOf(FourColorExpectation::size), tree.size(coder.colorToByte("A")))
        assertEquals(expected.filter { it.label == "B" }.sumOf(FourColorExpectation::size), tree.size(coder.colorToByte("B")))
        assertEquals(expected.filter { it.label == "C" }.sumOf(FourColorExpectation::size), tree.size(coder.colorToByte("C")))
        assertEquals(expected.filter { it.label == "D" }.sumOf(FourColorExpectation::size), tree.size(coder.colorToByte("D")))

        val root = fourColorRootNode(tree)
        if (expected.isEmpty()) {
            assertNull(root)
            return
        }

        assertNotNull(root)
        val structure = assertFourColorStructure(root, null)
        val expectedBlocks = expected.map { FourColorNodeState(coder.colorToByte(it.label), it.size, it.value) }
        assertEquals(expectedBlocks, structure.blocks)
        assertEquals(expected.sumOf(FourColorExpectation::size), structure.totalSize)
        assertEquals(expectedBlocks.flatMap { block -> List(block.size) { block.value } }, fourColorValues(tree, allColors))

        val iterator = FourColorTreeIterator(tree)
        val nodeElements = mutableListOf<Element<*>>()
        var flattenedIndex = 0
        expected.forEach { block ->
            repeat(block.size) {
                assertTrue(iterator.hasNext(allColors))
                iterator.next(allColors)
                assertEquals(flattenedIndex, iterator.index(allColors))
                assertEquals(flattenedIndex - it, iterator.nodeStartIndex(allColors))
                assertEquals(flattenedIndex - it + block.size, iterator.nodeEndIndex(allColors))
                assertEquals(block.value, iterator.value())
                assertEquals(coder.colorToByte(block.label), iterator.color())
                flattenedIndex++
            }
            nodeElements += iterator.node()
        }
        assertFalse(iterator.hasNext(allColors))
        assertFalse(iterator.hasNextNode(allColors))

        nodeElements.forEachIndexed { index, element ->
            assertSame(nodeElements.getOrNull(index - 1), element.previous())
            assertSame(nodeElements.getOrNull(index + 1), element.next())
        }
    }

    private fun <T> fourColorValues(tree: FourColorTree<T>, colors: Byte): List<T> {
        val iterator = FourColorTreeIterator(tree)
        val result = mutableListOf<T>()
        while (iterator.hasNext(colors)) {
            iterator.next(colors)
            result += iterator.value()
        }
        return result
    }

    private fun assertFourColorStructure(node: Any?, parent: Any?): FourColorStructureState {
        if (node == null) return FourColorStructureState(emptyList(), intArrayOf(0, 0, 0, 0), 0, 0)

        assertSame(parent, field(node.javaClass, "parent").get(node))

        val left = assertFourColorStructure(field(node.javaClass, "left").get(node), node)
        val right = assertFourColorStructure(field(node.javaClass, "right").get(node), node)
        val color = field(node.javaClass, "color").getByte(node)
        val size = field(node.javaClass, "size").getInt(node)
        val value = field(node.javaClass, "t0").get(node)
        val expectedCounts =
            intArrayOf(
                left.counts[0] + right.counts[0] + if (color.toInt() == 1) size else 0,
                left.counts[1] + right.counts[1] + if (color.toInt() == 2) size else 0,
                left.counts[2] + right.counts[2] + if (color.toInt() == 4) size else 0,
                left.counts[3] + right.counts[3] + if (color.toInt() == 8) size else 0,
            )
        val expectedHeight = maxOf(left.height, right.height) + 1

        assertEquals(expectedCounts[0], field(node.javaClass, "count1").getInt(node))
        assertEquals(expectedCounts[1], field(node.javaClass, "count2").getInt(node))
        assertEquals(expectedCounts[2], field(node.javaClass, "count4").getInt(node))
        assertEquals(expectedCounts[3], field(node.javaClass, "count8").getInt(node))
        assertEquals(expectedHeight.toByte(), field(node.javaClass, "height").getByte(node))
        assertTrue(abs(left.height - right.height) < 2)

        return FourColorStructureState(
            left.blocks + FourColorNodeState(color, size, value) + right.blocks,
            expectedCounts,
            expectedHeight,
            left.totalSize + size + right.totalSize,
        )
    }

    @Suppress("UNCHECKED_CAST")
    private fun fourColorRootNode(tree: FourColorTree<*>): Any? = field(tree.javaClass, "root").get(tree)

    @Suppress("UNCHECKED_CAST")
    private fun <T> rootNode(tree: SimpleTree<T>): SimpleNode<T>? = field(tree.javaClass, "root").get(tree) as SimpleNode<T>?

    private fun assertNodeState(
        node: SimpleNode<*>?,
        value: Any?,
        count1: Int,
        height: Int,
        parent: Any?,
        left: Any?,
        right: Any?,
    ) {
        requireNotNull(node)
        assertEquals(value, node.t0)
        assertEquals(count1, node.count1)
        assertEquals(height.toByte(), node.height)
        assertEquals(parent, node.parent?.t0)
        assertEquals(left, node.left?.t0)
        assertEquals(right, node.right?.t0)
    }

    private fun field(type: Class<*>, name: String): Field =
        runCatching {
            type.getDeclaredField(name).also { it.isAccessible = true }
        }.getOrElse {
            val parent = type.superclass ?: throw it
            field(parent, name)
        }

    private fun nextOracleValue(random: Random): Int? = if (random.nextInt(5) == 0) null else random.nextInt(11) - 5

    private data class KeyedValue(val key: Int, val label: String)

    private data class StructureState<T>(val values: List<T>, val count: Int, val height: Int)

    private data class FourColorExpectation(val label: String, val size: Int, val value: Any?)

    private data class FourColorNodeState(val color: Byte, val size: Int, val value: Any?)

    private class FourColorStructureState(
        val blocks: List<FourColorNodeState>,
        val counts: IntArray,
        val height: Int,
        val totalSize: Int,
    )
}
