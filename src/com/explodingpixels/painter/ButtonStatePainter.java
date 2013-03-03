package com.explodingpixels.painter;

import java.awt.Component;
import java.awt.Graphics2D;

import javax.swing.AbstractButton;

/**
 * A {@link MacWidgetsPainter} that can be used to paint the various states of a button. This painter
 * will delegate to the supplied painters based on the current state of the button. That is, if the button is being
 * "rolled over", the {@code rolloverPainter} will be called; if the button is pressed, then the {@code pressedPainter}
 * will be called, etc.
 */
public class ButtonStatePainter<B extends AbstractButton>
        implements MacWidgetsPainter<B> {

    private final MacWidgetsPainter<Component> fDefaultPainter;
    private final MacWidgetsPainter<Component> fRolloverPainter;
    private final MacWidgetsPainter<Component> fPressedPainter;
    private final MacWidgetsPainter<Component> fSelectedPainter;

    /**
     * Creates a painter that will always use the given {@link MacWidgetsPainter} to
     * paint the button.
     * @param defaultPainter the {@code Painter} to use to paint the
     *        button.
     */
    public ButtonStatePainter(MacWidgetsPainter<Component> defaultPainter) {
        this(defaultPainter, defaultPainter, defaultPainter, defaultPainter);
    }

    /**
     * Creates a painter that will delegate to the given painters based on
     * the current state of the button.
     * @param defaultPainter the {@link MacWidgetsPainter} to use when the button has
     *        no specific state.
     * @param rolloverPainter the {@code Painter} to use when the button is
     *        being "rolled over".
     * @param pressedPainter the {@code Painter} to use when the button is
     *        being pressed.
     * @param selectedPainter the {@code Painter} to use when the button has
     *        been selected.
     */
    public ButtonStatePainter(MacWidgetsPainter<Component> defaultPainter,
                              MacWidgetsPainter<Component> rolloverPainter,
                              MacWidgetsPainter<Component> pressedPainter,
                              MacWidgetsPainter<Component> selectedPainter) {
        fDefaultPainter = defaultPainter;
        fRolloverPainter = rolloverPainter;
        fPressedPainter = pressedPainter;
        fSelectedPainter = selectedPainter;
    }

    public void paint(Graphics2D g, B button, int width, int height) {

        if (button.getModel().isRollover()) {
            fRolloverPainter.paint(g, button, width, height);
        } else if (button.getModel().isPressed()) {
            fPressedPainter.paint(g, button, width, height);
        } else if (button.getModel().isSelected()) {
            fSelectedPainter.paint(g, button, width, height);
        } else {
            fDefaultPainter.paint(g, button, width, height);
        }

    }

    // Dummy Painter implementation. /////////////////////////////////////////////////

    /**
     * An implementation of {@link MacWidgetsPainter} that does no painting.
     */
    public static class DummyPainter implements MacWidgetsPainter<Component> {
        public void paint(Graphics2D g, Component component, int width, int height) {
            // do nothing.
        }
    }
}
