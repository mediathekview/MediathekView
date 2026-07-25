/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.matchers.Matcher

/** A singleton [Matcher] that matches only null values. */
internal class NullMatcher<E> private constructor() : Matcher<E> {
    override fun matches(item: E): Boolean = item == null

    override fun toString(): String = "[NullMatcher]"

    companion object {
        private val INSTANCE: Matcher<Any?> = NullMatcher()

        @Suppress("UNCHECKED_CAST")
        fun <E> getInstance(): Matcher<E> = INSTANCE as Matcher<E>
    }
}
