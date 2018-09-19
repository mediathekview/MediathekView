package mediathek.javafx.filterpanel;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.layout.VBox;

import javax.swing.*;
import java.awt.*;

public class SwingFilterDialog extends JDialog {
    private final JFXPanel fxPanel = new JFXPanel();

    public SwingFilterDialog(Frame owner, VBox content) {
        super(owner);
        setDefaultCloseOperation(HIDE_ON_CLOSE);
        setTitle("Filter");
        setType(Type.UTILITY);
        //setResizable(false);
        setContentPane(fxPanel);
        Platform.runLater(() -> {
            fxPanel.setScene(new Scene(content));
            SwingUtilities.invokeLater(this::pack);
        });
    }
}
