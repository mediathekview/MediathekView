package mediathek.gui.tabs.tab_film;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.messages.ButtonPanelVisibilityChangedEvent;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.Listener;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.configuration2.Configuration;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.LineBorder;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

public final class SwingButtonPanelController {
    private final GuiFilme guiFilme;
    private final JPanel contentPanel = new JPanel();
    private final JPanel buttonsPanel = new JPanel();
    private final GridBagLayout gridbag = new GridBagLayout();
    private final Insets DEFAULT_INSETS = new Insets(4, 10, 4, 10);
    private final GridBagConstraints c = new GridBagConstraints();
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private ListePset listeButton;

    public SwingButtonPanelController(@NotNull GuiFilme guiFilme, @NotNull JPanel jPanel2) {
        this.guiFilme = guiFilme;

        createContentPanel(jPanel2);
        createCloseButton();
        createButtonsPanel();

        final int initialColumns = config.getInt(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_MAX_VISIBLE);
        var contextMenu = new ButtonPanelContextMenu(initialColumns);
        contextMenu.getColumnModel().addChangeListener(e -> {
            final int columns = (int) contextMenu.getColumnModel().getValue();
            config.setProperty(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_MAX_VISIBLE, columns);
            setupButtons();
        });
        buttonsPanel.setComponentPopupMenu(contextMenu.getPopupMenu());

        //fill with data...
        setupButtons();

        //register message bus handler
        Daten.getInstance().getMessageBus().subscribe(this);

        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_PSET, SwingButtonPanelController.class.getSimpleName()) {
            @Override
            public void ping() {
                // psets have changed and we need to update the columns
                setupButtons();
            }
        });

        contentPanel.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));

        SwingUtilities.invokeLater(() -> contentPanel.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, String.valueOf(true));
            }

            @Override
            public void componentHidden(ComponentEvent e) {
                MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, String.valueOf(false));
            }
        }));
    }

    @Handler
    private void handleButtonPanelVisibilityChanged(ButtonPanelVisibilityChangedEvent evt) {
        SwingUtilities.invokeLater(() -> contentPanel.setVisible(evt.visible));
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
            final boolean visibility = false;
            MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, Boolean.toString(visibility));
            Daten.getInstance().getMessageBus().publishAsync(new ButtonPanelVisibilityChangedEvent(visibility));
        });

        contentPanel.add(btnClose, new CC().pad("5 5 3 3").cell(0, 0).alignX("left").alignY("top").grow(0, 0).width("20:20:20").height("20:20:20")); //NON-NLS
    }

    private void initConstraints() {
        c.weightx = 0;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = DEFAULT_INSETS;
    }

    private JLabel createLabel(@NotNull String psetName, Color psetColor, int row, int col) {
        var label = new JLabel(psetName);
        if (psetColor != null) {
            label.setForeground(psetColor);
        }

        c.gridx = col;
        c.gridy = row;
        gridbag.setConstraints(label, c);

        return label;
    }

    private JButton createButton(@NotNull DatenPset pset, @NotNull String psetName, Color psetColor, int row, int col) {
        var button = new JButton(psetName);
        button.addActionListener(new BeobOpenPlayer(guiFilme, pset));
        if (psetColor != null) {
            button.setBackground(psetColor);
        }

        c.gridx = col;
        c.gridy = row;
        gridbag.setConstraints(button, c);

        return button;
    }

    private void addExtraFeld(int i, int spalte, int zeile) {
        var pset = listeButton.get(i);
        final var psetName = pset.arr[DatenPset.PROGRAMMSET_NAME];
        final var psetColor = pset.getFarbe();

        if (pset.isLabel()) {
            buttonsPanel.add(createLabel(psetName, psetColor, zeile, spalte));
        } else {
            buttonsPanel.add(createButton(pset, psetName, psetColor, zeile, spalte));
        }
    }

    private void setupButtons() {
        // erst sauber machen
        // zum Anlegen der Button:
        // Programmgruppe ohne Namen: Leerfeld
        // Programmgruppe ohne Programme: Label
        // sonst ein Button
        buttonsPanel.removeAll();
        final int maxColumns = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_MAX_VISIBLE);

        initConstraints();

        int spalte = 0;
        int zeile = 0;
        listeButton = Daten.listePset.getListeButton();
        for (int i = 0; i < listeButton.size(); ++i) {
            if (!listeButton.get(i).isFreeLine()) {
                addExtraFeld(i, spalte, zeile);
            }
            ++spalte;
            if (spalte > maxColumns - 1) {
                spalte = 0;
                ++zeile;
            }
        }

        // zum zusammenschieben
        JLabel label = new JLabel();
        c.weightx = 10;
        c.gridx = maxColumns + 1;
        c.gridy = 0;
        gridbag.setConstraints(label, c);
        buttonsPanel.add(label);

        buttonsPanel.updateUI();
    }

    private static class ButtonPanelContextMenu {
        private final SpinnerNumberModel columnModel = new SpinnerNumberModel(4, 2, 10, 1);
        private final JPopupMenu jPopupMenu = new JPopupMenu();

        public ButtonPanelContextMenu(int start) {
            columnModel.setValue(start);

            createPopupMenu();
        }

        public SpinnerNumberModel getColumnModel() {
            return columnModel;
        }

        public JPopupMenu getPopupMenu() {
            return jPopupMenu;
        }

        private void createPopupMenu() {
            JSpinner jSpinner = new JSpinner(columnModel);
            jSpinner.setToolTipText("Damit kann die Anzahl der Button verändert werden");

            JPanel jPanelAnzahl = new JPanel();
            jPanelAnzahl.setLayout(new BorderLayout());
            jPanelAnzahl.add(new JLabel("Anzahl Button je Zeile: "), BorderLayout.WEST);
            jPanelAnzahl.add(jSpinner, BorderLayout.EAST);

            jPopupMenu.add(jPanelAnzahl);
        }
    }
}
