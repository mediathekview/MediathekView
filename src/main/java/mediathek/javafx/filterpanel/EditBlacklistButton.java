package mediathek.javafx.filterpanel;

import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import mediathek.config.Daten;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

import javax.swing.*;

public class EditBlacklistButton extends Button {
    public EditBlacklistButton() {
        super();
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");
        setGraphic(fontAwesome.create(FontAwesome.Glyph.SKYATLAS).size(16d));
        setTooltip(new Tooltip("Blacklist bearbeiten"));
        setOnAction(e -> SwingUtilities.invokeLater(() -> {
            DialogLeer dialog = new DialogLeer(null, true);
            dialog.init("Blacklist", new PanelBlacklist(Daten.getInstance(), null, PanelBlacklist.class.getName() + "_3"));
            dialog.setVisible(true);
        }));
    }
}
