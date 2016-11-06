package com.explodingpixels.macwidgets;

import java.awt.event.MouseEvent;

/**
 * An interface to hook into clicks on {@link SourceListCategory}s and
 * {@link SourceListItem}s. This interface is similar to what
 * {@link SourceListSelectionListener} offers, but provides mouse clicks
 * including which mouse button was pressed the number of clicks that occured.
 * <p/>
 * It is, for example, possible to handle left mouse button double clicks on an
 * item, which can be done with an implementation like this:
 * <pre>
 * SourceListClickListener clickListener = new SourceListClickListener() {
 *     public void sourceListItemClicked(SourceListItem item, Button button,
 *                                       int clickCount) {
 *         boolean isLeftButton = button == SourceListClickListener.Button.LEFT;
 *         if (isLeftButton && clickCount == 2) {
 *             // do something here.
 *         }
 *     }
 *     public void sourceListCategoryClicked(SourceListCategory category,
 *                                           Button button, int clickCount) {
 *         // no implementation.
 *     }
 * };
 * </pre>
 */
public interface SourceListClickListener {

    /**
     * Called when a {@link SourceListItem} is clicked.
     *
     * @param item       the {@code SourceListItem} that was clicked. Will not be null.
     * @param button     the mouse button that was used to perform the click.
     * @param clickCount the number of times the mouse button was clicked.
     */
    void sourceListItemClicked(SourceListItem item, Button button,
                               int clickCount);

    /**
     * Called when a {@link SourceListCategory} is clicked.
     *
     * @param category   the {@code SourceListCategory} that was clicked. Will not
     *                   be null.
     * @param button     the mouse button that was used to perform the click.
     * @param clickCount the number of times the mouse button was clicked.
     */
    void sourceListCategoryClicked(SourceListCategory category, Button button,
                                   int clickCount);

    /**
     * Corresponds to a button on a mouse.
     */
    enum Button {
        LEFT(MouseEvent.BUTTON1),
        MIDDLE(MouseEvent.BUTTON2),
        RIGHT(MouseEvent.BUTTON3);

        final int fCorrespondingMouseEventButton;

        Button(int correspondingMouseEventButton) {
            fCorrespondingMouseEventButton = correspondingMouseEventButton;
        }

        private int getCorrespondingMouseEventButton() {
            return fCorrespondingMouseEventButton;
        }

        public static Button getButton(int correspondingMouseEventButton) {
            Button retVal = null;

            // loop over each Button comparing it's 
            // correspondingMouseEventButton value to the current Button's
            // correspondingMouseEventButton.
            for (Button button : Button.values()) {
                if (button.getCorrespondingMouseEventButton() == correspondingMouseEventButton) {
                    retVal = button;
                    break;
                }
            }

            // if no matching Button was found, throw an exception.
            if (retVal == null) {
                throw new IllegalArgumentException(
                        correspondingMouseEventButton + " is not a valid MouseEvent button integer.");
            }

            return retVal;
        }
    }

}
