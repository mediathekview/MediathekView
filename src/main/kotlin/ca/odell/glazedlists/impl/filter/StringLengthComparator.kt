/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.filter

/** Orders strings by descending length. */
internal class StringLengthComparator : Comparator<String> {
    override fun compare(a: String, b: String): Int = b.length - a.length
}
