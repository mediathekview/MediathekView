/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.matchers.Matcher

/** Matches strings that are neither null nor empty. */
internal open class NonNullAndNonEmptyStringMatcher : Matcher<String?> {
    override fun matches(item: String?): Boolean = !item.isNullOrEmpty()

    companion object {
        private val INSTANCE: Matcher<String?> = NonNullAndNonEmptyStringMatcher()

        fun getInstance(): Matcher<String?> = INSTANCE
    }
}
