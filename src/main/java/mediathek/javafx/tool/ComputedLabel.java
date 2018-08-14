package mediathek.javafx.tool;

import javafx.scene.control.Label;

/**
 * A JavaFX Label which will adapt its minimum width.
 */
public abstract class ComputedLabel extends Label {
    private double width = 0d;

    /**
     * This sets the text of the label and adjusts the width of the label to prevent "jumping" text.
     */
    public void setComputedText(String text) {
        setText(text);
        double curWidth = getWidth();
        if (curWidth >= width) {
            width = curWidth;
            setMinWidth(width);
        }

    }
}
