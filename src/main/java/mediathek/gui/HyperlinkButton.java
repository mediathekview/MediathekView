package mediathek.gui;

import com.jidesoft.swing.ButtonStyle;
import com.jidesoft.swing.JideButton;

import javax.swing.*;
import java.awt.*;

/**
 * A swing button which displays as a hyperlink in the UI.
 */
public class HyperlinkButton extends JideButton {
    public HyperlinkButton() {
        super();
        setForeground(Color.BLUE);
        setButtonStyle(ButtonStyle.HYPERLINK_STYLE);
        setHorizontalAlignment(SwingConstants.LEFT);
    }
}
