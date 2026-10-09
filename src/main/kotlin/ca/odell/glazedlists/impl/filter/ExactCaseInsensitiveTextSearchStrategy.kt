/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.filter

/**
 * Matches a configured subtext against a complete input string without regard to case.
 *
 * @author James Lemieux
 */
internal open class ExactCaseInsensitiveTextSearchStrategy : StartsWithCaseInsensitiveTextSearchStrategy() {
    private var subtextLength = 0

    override fun setSubtext(subtext: String) {
        super.setSubtext(subtext)
        subtextLength = subtext.length
    }

    override fun indexOf(text: String): Int {
        if (text.length != subtextLength) return -1
        return super.indexOf(text)
    }
}
