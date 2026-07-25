/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.matchers.Matcher

/** Matches when at least one child matcher matches. */
internal open class OrMatcher<E>(private vararg val matchers: Matcher<in E>) : Matcher<E> {
    override fun matches(item: E): Boolean = matchers.any { it.matches(item) }
}
