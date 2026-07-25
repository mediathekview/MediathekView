/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.filter

import java.util.regex.Matcher
import java.util.regex.Pattern

/** Matches the complete input text against a configured regular expression. */
internal open class RegularExpressionTextSearchStrategy : AbstractTextSearchStrategy() {
    private var matcher: Matcher? = null

    override fun setSubtext(subtext: String) {
        matcher = Pattern.compile(subtext).matcher("")
    }

    override fun indexOf(text: String): Int {
        val currentMatcher = matcher!!
        return if (currentMatcher.reset(text).matches()) currentMatcher.start() else -1
    }
}
