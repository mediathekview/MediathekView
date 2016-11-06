package com.explodingpixels.macwidgets;

/**
 * An interface for listening to {@link SourceListItem} selections.
 */
public interface SourceListSelectionListener {

    /**
     * Called when a {@link SourceListItem} is selected in a {@link SourceList}.
     * @param item the item that was selected.
     */
    void sourceListItemSelected(SourceListItem item);

}
