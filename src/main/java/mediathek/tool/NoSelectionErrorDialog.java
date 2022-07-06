package mediathek.tool;

import mediathek.config.Konstanten;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;

public class NoSelectionErrorDialog {

    public static void show(@Nullable Component parent) {
        JOptionPane.showMessageDialog(parent, "Der Befehl kann nicht ausgeführt werden.\n" +
                "Sie haben keinen Tabelleneintrag ausgewählt.",
                Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
    }
}
