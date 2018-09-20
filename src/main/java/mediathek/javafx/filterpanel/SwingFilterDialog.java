package mediathek.javafx.filterpanel;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.layout.VBox;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.tool.GuiFunktionen;

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

        GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_FILTER_DIALOG_NEW,this, MediathekGui.ui());

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
}
