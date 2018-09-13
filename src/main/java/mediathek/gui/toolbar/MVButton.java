package mediathek.gui.toolbar;

import mediathek.config.MVConfig;

import javax.swing.*;

public class MVButton extends JButton {
    private final ImageIcon imageIconKlein;
    private final String name;
    private final ImageIcon imageIconNormal;
    private final MVConfig.Configs nrIconKlein = MVConfig.Configs.SYSTEM_ICON_KLEIN;
    private boolean anzeigen = true;

    public MVButton(String name, String toolTip,
                    ImageIcon iimageIconNormal, ImageIcon iimageIconKlein) {
        setToolTipText(toolTip);
        this.name = name;
        imageIconKlein = iimageIconKlein;
        imageIconNormal = iimageIconNormal;
        setOpaque(false);
        setBorder(BorderFactory.createEmptyBorder(8, 8, 8, 8));
        setHorizontalTextPosition(SwingConstants.CENTER);
        setVerticalTextPosition(SwingConstants.BOTTOM);
    }

    public boolean getAnzeigen() {
        return anzeigen;
    }

    public void setAnzeigen(boolean show) {
        anzeigen = show;
    }

    public Icon getSmallIcon() {
        return imageIconKlein;
    }

    public String getName() {
        return name;
    }

    public void setIcon() {
        if (Boolean.parseBoolean(MVConfig.get(nrIconKlein))) {
            this.setIcon(imageIconKlein);
        } else {
            this.setIcon(imageIconNormal);
        }
    }
}