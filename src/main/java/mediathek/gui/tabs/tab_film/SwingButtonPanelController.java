package mediathek.gui.tabs.tab_film;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.messages.PsetNumberOfButtonsChangedEvent;
import mediathek.tool.Listener;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public final class SwingButtonPanelController implements IButtonPanelController {
    private final GuiFilme guiFilme;
    private final JPanel contentPanel = new JPanel();
    private final JPanel buttonsPanel = new JPanel();
    private final GridBagLayout gridbag = new GridBagLayout();
    private final Insets DEFAULT_INSETS = new Insets(4, 10, 4, 10);
    private final GridBagConstraints c = new GridBagConstraints();
    private ListePset listeButton;

    public SwingButtonPanelController(@NotNull GuiFilme guiFilme, @NotNull JPanel jPanel2) {
        this.guiFilme = guiFilme;

        createContentPanel(jPanel2);
        createCloseButton();
        createButtonsPanel();
        buttonsPanel.addMouseListener(new BeobMausButton());
    }

    private void createContentPanel(JPanel jPanel2) {
        contentPanel.setBorder(new LineBorder(new Color(153, 153, 153)));
        contentPanel.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).gridGap("5", "5"), //NON-NLS
                // columns
                new AC()
                        .fill().gap()
                        .grow().fill(),
                // rows
                new AC()
                        .grow().fill()));
        jPanel2.add(contentPanel, new CC().cell(0, 1));
    }

    private void createButtonsPanel() {
        buttonsPanel.setLayout(gridbag);
        contentPanel.add(buttonsPanel, new CC().cell(1, 0));
    }

    private void createCloseButton() {
        var btnClose = new JButton();
        btnClose.setToolTipText("Buttons ausblenden"); //NON-NLS
        btnClose.setFocusable(false);
        btnClose.setFocusPainted(false);
        btnClose.setRequestFocusEnabled(false);
        btnClose.setBorderPainted(false);
        btnClose.setIcon(IconFontSwing.buildIcon(FontAwesome.TIMES_CIRCLE_O, 16));
        btnClose.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, Boolean.FALSE.toString());
            Listener.notify(Listener.EREIGNIS_LISTE_PSET, GuiFilme.class.getSimpleName());
            guiFilme.getButtonPanelController().setVisible(false);
        });

        contentPanel.add(btnClose, new CC().pad("5 5 3 3").cell(0, 0).alignX("left").alignY("top").grow(0, 0).width("20:20:20").height("20:20:20")); //NON-NLS
    }

    private void initConstraints() {
        c.weightx = 0;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = DEFAULT_INSETS;
    }

    private void addExtraFeld(int i, int spalte, int zeile) {
        c.gridx = spalte;
        c.gridy = zeile;

        var pset = listeButton.get(i);
        final var psetName = pset.arr[DatenPset.PROGRAMMSET_NAME];
        final var psetColor = pset.getFarbe();

        if (pset.isLable()) {
            JLabel label = new JLabel(psetName);
            if (psetColor != null) {
                label.setForeground(psetColor);
            }
            gridbag.setConstraints(label, c);

            buttonsPanel.add(label);
        } else {
            var button = new JButton(psetName);
            button.addActionListener(new BeobOpenPlayer(guiFilme, pset));
            if (psetColor != null) {
                button.setBackground(psetColor);
            }
            gridbag.setConstraints(button, c);

            buttonsPanel.add(button);
        }
    }

    public void setupButtons() {
        // erst sauber machen
        // zum Anlegen der Button:
        // Programmgruppe ohne Namen: Leerfeld
        // Programmgruppe ohen Programme: Label
        // sonst ein Button
        buttonsPanel.removeAll();
        final int maxSpalten = MVConfig.getInt(MVConfig.Configs.SYSTEM_TAB_FILME_ANZAHL_BUTTON); //Anzahl der Spalten der Schalter

        initConstraints();

        int spalte = 0;
        int zeile = 0;
        listeButton = Daten.listePset.getListeButton();
        for (int i = 0; i < listeButton.size(); ++i) {
            if (!listeButton.get(i).isFreeLine()) {
                addExtraFeld(i, spalte, zeile);
            }
            ++spalte;
            if (spalte > maxSpalten - 1) {
                spalte = 0;
                ++zeile;
            }
        }

        // zum zusammenschieben
        c.weightx = 10;
        c.gridx = maxSpalten + 1;
        c.gridy = 0;
        JLabel label = new JLabel();
        gridbag.setConstraints(label, c);

        buttonsPanel.add(label);
        buttonsPanel.updateUI();
    }

    public void setVisible(boolean visible) {
        contentPanel.setVisible(visible);
    }

    private static class BeobMausButton extends MouseAdapter {
        private final JSpinner jSpinner = new JSpinner(new SpinnerNumberModel(4, 2, 10, 1));
        private final EmptyBorder emptyBorder = new EmptyBorder(3, 5, 3, 5);

        public BeobMausButton() {
            int start = MVConfig.getInt(MVConfig.Configs.SYSTEM_TAB_FILME_ANZAHL_BUTTON);
            jSpinner.setValue(start);
            jSpinner.setToolTipText("Damit kann die Anzahl der Button verändert werden");
        }

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            JPopupMenu jPopupMenu = new JPopupMenu();
            jSpinner.addChangeListener(e -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_TAB_FILME_ANZAHL_BUTTON, String.valueOf(((Number) jSpinner.getModel().getValue()).intValue()));
                Daten.getInstance().getMessageBus().publishAsync(new PsetNumberOfButtonsChangedEvent());
            });
            JPanel jPanelAnzahl = new JPanel();
            jPanelAnzahl.setLayout(new BorderLayout());
            jPanelAnzahl.setBorder(emptyBorder);
            jPanelAnzahl.add(new JLabel("Anzahl Button je Zeile: "), BorderLayout.WEST);
            jPanelAnzahl.add(jSpinner, BorderLayout.EAST);

            jPopupMenu.add(jPanelAnzahl);

            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }
}
