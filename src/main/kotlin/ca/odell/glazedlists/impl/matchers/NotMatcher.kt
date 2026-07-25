/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.matchers.Matcher

/** Inverts the result of [parent]. */
internal open class NotMatcher<E>(private val parent: Matcher<E>) : Matcher<E> {

    override fun matches(item: E): Boolean = !parent.matches(item)

    override fun toString(): String = "[NotMatcher parent:$parent]"
}
