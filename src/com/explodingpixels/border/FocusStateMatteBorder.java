package com.explodingpixels.border;

import java.awt.Color;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;

import javax.swing.JComponent;
import javax.swing.border.MatteBorder;

import com.explodingpixels.widgets.WindowUtils;

public class FocusStateMatteBorder extends MatteBorder {

    private final Color fFocusedColor;

    private final Color fUnfocusedColor;

    private final JComponent fComponentToTrackFocusOf;

    public FocusStateMatteBorder(int top, int left, int bottom, int right,
                                 Color focusedColor, Color unfocusedColor,
                                 JComponent componentToTrackFocusOf) {
        super(top, left, bottom, right, focusedColor);
        fFocusedColor = focusedColor;
        fUnfocusedColor = unfocusedColor;
        fComponentToTrackFocusOf = componentToTrackFocusOf;
        updateColor(true);
        WindowUtils.installWeakWindowFocusListener(fComponentToTrackFocusOf, createWindowFocusListener());
    }

    private void updateColor(boolean focused) {
        color = focused ? fFocusedColor : fUnfocusedColor;
        fComponentToTrackFocusOf.repaint();
    }

    private WindowFocusListener createWindowFocusListener() {
        return new WindowFocusListener() {
            public void windowGainedFocus(WindowEvent e) {
                updateColor(true);
            }

            public void windowLostFocus(WindowEvent e) {
                updateColor(false);
            }
        };
    }

}
