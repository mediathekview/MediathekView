package ca.odell.glazedlists.impl.adt.barcode2

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.lang.reflect.Modifier

internal class ElementBehaviorTest {
    @Test
    fun javaTreeNodesExposeKotlinPropertiesAndLinks() {
        val tree = SimpleTree<String?>()
        val first = tree.add(0, "first", 1)
        val second = tree.add(1, "second", 1)

        assertEquals(1.toByte(), first.color)
        assertEquals(Element.SORTED, first.sorted)
        first.sorted = Element.PENDING
        assertEquals(Element.PENDING, first.sorted)

        assertEquals("first", first.get())
        first.set(null)
        assertNull(first.get())

        assertSame(second, first.next())
        assertSame(first, second.previous())
        assertNull(first.previous())
        assertNull(second.next())
    }

    @Test
    fun simpleNodeInternalJvmSurfaceStaysSynthetic() {
        val nodeClass = SimpleNode::class.java

        assertTrue(nodeClass.declaredConstructors.any { Modifier.isPrivate(it.modifiers) })
        assertSyntheticMember(nodeClass, "getT0")
        assertSyntheticMember(nodeClass, "setT0")
        assertSyntheticMember(nodeClass, "getParent")
        assertSyntheticMember(nodeClass, "setParent")
        assertSyntheticMember(nodeClass, "getCount1")
        assertSyntheticMember(nodeClass, "setCount1")
        assertSyntheticMember(nodeClass, "getHeight")
        assertSyntheticMember(nodeClass, "setHeight")
        assertSyntheticMember(nodeClass, "getLeft")
        assertSyntheticMember(nodeClass, "setLeft")
        assertSyntheticMember(nodeClass, "getRight")
        assertSyntheticMember(nodeClass, "setRight")
        assertSyntheticMember(nodeClass, "size")
        assertSyntheticMember(nodeClass, "refreshCounts")
        assertSyntheticMember(nodeClass, "asTree")
    }

    private fun assertSyntheticMember(type: Class<*>, namePrefix: String) {
        assertTrue(type.declaredMethods.any { it.name.startsWith(namePrefix) && it.isSynthetic }, namePrefix)
    }
}
