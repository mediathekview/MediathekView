/*
 * $Id: TreeSearchable.java 4178 2012-06-20 08:52:11Z kleopatra $
 *
 * Copyright 2008 Sun Microsystems, Inc., 4150 Network Circle,
 * Santa Clara, California 95054, U.S.A. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
package org.jdesktop.swingx.search;

import org.jdesktop.swingx.JXTree;
import org.jdesktop.swingx.decorator.AbstractHighlighter;
import org.jdesktop.swingx.decorator.Highlighter;
import org.jdesktop.swingx.util.Contract;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A searchable targetting the visible rows of a JXTree.
 * 
 * 
 */
public class TreeSearchable extends AbstractSearchable {

    protected JXTree tree;

    /**
     * Instantiates a Searchable for the given JTree.
     * 
     * @param tree the JTree to search, must not be null.
     */
    public TreeSearchable(JXTree tree) {
        this.tree = Contract.asNotNull(tree, "tree must not be null");
    }

    @Override
    protected void findMatchAndUpdateState(Pattern pattern, int startRow,
            boolean backwards) {
        SearchResult searchResult = null;
        if (backwards) {
            for (int index = startRow; index >= 0 && searchResult == null; index--) {
                searchResult = findMatchAt(pattern, index);
            }
        } else {
            for (int index = startRow; index < getSize()
                    && searchResult == null; index++) {
                searchResult = findMatchAt(pattern, index);
            }
        }
        updateState(searchResult);

    }

    @Override
    protected SearchResult findExtendedMatch(Pattern pattern, int row) {
        return findMatchAt(pattern, row);
    }

    /**
     * Matches the cell content at row/col against the given Pattern. Returns an
     * appropriate SearchResult if matching or null if no matching
     * 
     * @param pattern
     * @param row a valid row index in view coordinates a valid column index in
     *        view coordinates
     * @return an appropriate <code>SearchResult</code> if matching or null if
     *         no matching
     */
    protected SearchResult findMatchAt(Pattern pattern, int row) {
        String text = tree.getStringAt(row);
        if ((text != null) && (text.length() > 0)) {
            Matcher matcher = pattern.matcher(text);
            if (matcher.find()) {
                return createSearchResult(matcher, row, 0);
            }
        }
        return null;
    }

    @Override
    protected int getSize() {
        return tree.getRowCount();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public JXTree getTarget() {
        return tree;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void moveMatchMarker() {
        if (markByHighlighter()) {
            moveMatchByHighlighter();
        } else { // use selection
            moveMatchBySelection();
        }
    }

    protected void moveMatchBySelection() {
        // // the common behaviour (JXList, JXTable) is to not
        // // move the selection if not found
        if (!hasMatch()) {
            return;
        }
        tree.setSelectionRow(lastSearchResult.foundRow);
        tree.scrollRowToVisible(lastSearchResult.foundRow);
    }

    /**
     * use and move the match highlighter. PRE: markByHighlighter
     * 
     */
    protected void moveMatchByHighlighter() {
        AbstractHighlighter searchHL = getConfiguredMatchHighlighter();
        // no match
        if (!hasMatch()) {
            return;
        } else {
            ensureInsertedSearchHighlighters(searchHL);
            tree.scrollRowToVisible(lastSearchResult.foundRow);
        }
    }

    /**
     * @param searchHighlighter
     */
    @Override
    protected void removeHighlighter(Highlighter searchHighlighter) {
        tree.removeHighlighter(searchHighlighter);
    }

    /**
     * @return all registered highlighters
     */
    @Override
    protected Highlighter[] getHighlighters() {
        return tree.getHighlighters();
    }

    /**
     * @param highlighter
     */
    @Override
    protected void addHighlighter(Highlighter highlighter) {
        tree.addHighlighter(highlighter);
    }

}