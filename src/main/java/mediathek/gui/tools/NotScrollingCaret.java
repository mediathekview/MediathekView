package mediathek.gui.tools;

import java.awt.Rectangle;

import javax.swing.text.DefaultCaret;

/**
 * Überschreibt das Verhalten, das beim setText() auf der View zu der Cursor-Position gescrollt wird. Der Cursor steht beim setText() am
 * Ende des gesetzten Textes, was zur Folge hat, dass hierbei immer ans Ende des Textes gescrollt wird.
 */
@SuppressWarnings("serial")
public final class NotScrollingCaret extends DefaultCaret {

    @Override
    protected void adjustVisibility(Rectangle nloc) {
        // Nichts tun, es soll nicht zum Cursor gescrollt werden.
    }
}
