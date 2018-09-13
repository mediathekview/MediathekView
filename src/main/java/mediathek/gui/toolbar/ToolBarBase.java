package mediathek.gui.toolbar;

import mediathek.config.Icons;
import mediathek.config.MVConfig;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;

public abstract class ToolBarBase extends JToolBar {
    protected final Box.Filler filler__10 = new Box.Filler(new Dimension(10, 20), new Dimension(10, 20), new Dimension(10, 32767));
    protected final Box.Filler filler__trenner = new Box.Filler(new Dimension(1, 5), new Dimension(1, 5), new Dimension(32767, 5));
    protected final ArrayList<MVButton> buttonList = new ArrayList<>();
    protected final MVConfig.Configs nrIconKlein = MVConfig.Configs.SYSTEM_ICON_KLEIN;
    private final Dimension dim = new Dimension(40, 40);

    public JButton createFilterButton() {
        JButton filterButton = new JButton();
        filterButton.setToolTipText("Filter anzeigen/ausblenden");
        filterButton.setBorder(null);
        filterButton.setBorderPainted(false);
        filterButton.setHorizontalTextPosition(SwingConstants.CENTER);
        filterButton.setMaximumSize(dim);
        filterButton.setMinimumSize(dim);
        filterButton.setOpaque(false);
        filterButton.setPreferredSize(dim);
        filterButton.setVerticalTextPosition(SwingConstants.BOTTOM);
        filterButton.setIcon(Icons.ICON_BUTTON_FILTER_ANZEIGEN);

        return filterButton;
    }

    protected void setButtonVisibility() {
        for (MVButton b : buttonList) {
            b.setVisible(b.getAnzeigen());
        }
    }
    
    protected void setButtonIcons() {
        for (MVButton b : buttonList) {
            b.setIcon();
        }
    }
}
