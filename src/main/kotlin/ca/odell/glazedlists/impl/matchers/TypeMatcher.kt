/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.matchers.Matcher

/** Matches non-null values assignable to at least one configured class. */
internal open class TypeMatcher<E>(private vararg val classes: Class<*>) : Matcher<E> {
    override fun matches(item: E): Boolean {
        if (item == null) return false
        val target = item.javaClass
        return classes.any { it.isAssignableFrom(target) }
    }
}
