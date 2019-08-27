package mediathek.javafx.filterpanel;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.layout.VBox;
import mediathek.config.Daten;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

public class SwingFilterDialog extends JDialog {
    private final JFXPanel fxPanel = new JFXPanel();

    public SwingFilterDialog(Frame owner, VBox content) {
        super(owner);
        setDefaultCloseOperation(HIDE_ON_CLOSE);
        setTitle("Filter");
        setType(Type.UTILITY);
        setContentPane(fxPanel);
        Platform.runLater(() -> {
            fxPanel.setScene(new Scene(content));
            SwingUtilities.invokeLater(() -> {
                pack();
                registerWindowSizeListener();
            });
        });

        //GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_FILTER_DIALOG_NEW,this, MediathekGui.ui());

        Daten.getInstance().getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                setEnabled(false);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                setEnabled(true);
            }
        });
    }

    private void registerWindowSizeListener() {
        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentHidden(ComponentEvent e) {
                //GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_FILTER_DIALOG_NEW, SwingFilterDialog.this);
            }
        });

    }
}
