package com.explodingpixels.widgets;

import javax.swing.JPopupMenu;

/**
 * An interface that is used to popuplate a {@link JPopupMenu}. The
 * {@link #customizePopup(JPopupMenu)} method will be called just prior to each showing of the
 * menu. Thus, the implementor should clear the menu at the beginning of the customization. Here is
 * a simple {@code PopupMenuCustomizer} implementation:
 * <pre>
 * public class MyPopupMenuCustomizer implements PopupMenuCustomizer {
 *     public void customizePopup(JPopupMenu popup) {
 *           popup.removeAll();
 *           JMenuItem menuItem = new JMenuItem(menuString);
 *           menuItem.addActionListener(someActionListener);
 *           popup.add(menuItem);
 *     }
 * }
 * </pre>
 */
public interface PopupMenuCustomizer {

    /**
     * Called just prior the given {@link JPopupMenu} being shown.
     * @param popup the {@code JPopupMenu} about to be shown.
     */
    void customizePopup(JPopupMenu popup);

}
