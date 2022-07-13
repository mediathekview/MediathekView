package mediathek.gui.tabs.tab_film;

import mediathek.config.Daten;
import mediathek.daten.DatenPset;
import mediathek.gui.messages.ProgramSetChangedEvent;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import org.jdesktop.swingx.WrapLayout;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

public class PsetButtonsPanel extends JPanel {
    private static final int DEFAULT_HEIGHT = 90;
    private final JPanel btnPanel = new JPanel();

    private final GuiFilme guiFilme;

    public PsetButtonsPanel(@NotNull GuiFilme guiFilme) {
        this.guiFilme = guiFilme;

        setLayout(new BorderLayout());
        setPreferredSize(new Dimension(Integer.MAX_VALUE, DEFAULT_HEIGHT));
        setMinimumSize(new Dimension(100, DEFAULT_HEIGHT));

        btnPanel.setLayout(new WrapLayout(FlowLayout.LEFT, 5, 5));

        JScrollPane sp = new JScrollPane();
        add(sp, BorderLayout.CENTER);

        sp.setViewportView(btnPanel);

        setupButtonLayout();

        MessageBus.getMessageBus().subscribe(this);
    }

    public void install(@NotNull JTabbedPane tabbedPane) {
        tabbedPane.add("Buttons", this);
    }

    @Handler
    private void handleProgramSetChangedEvent(ProgramSetChangedEvent e) {
        System.out.println("Handle PSET CHANGE");
        SwingUtilities.invokeLater(this::setupButtonLayout);
    }

    protected void setupButtonLayout() {
        btnPanel.removeAll();

        for (var pset : Daten.listePset.getListeButton()) {
            final var psetName = pset.arr[DatenPset.PROGRAMMSET_NAME];
            final var psetColor = pset.getFarbe();
            if (!pset.isFreeLine()) {
                if (pset.isLabel()) {
                    var l = new JLabel(psetName);
                    if (psetColor != null)
                        l.setForeground(psetColor);
                    btnPanel.add(l);
                } else {
                    var b = new JButton(psetName);
                    if (psetColor != null)
                        b.setBackground(psetColor);
                    b.addActionListener(l -> guiFilme.playerStarten(pset));
                    btnPanel.add(b);
                }
            } else {
                btnPanel.add(new JLabel(""));
            }
        }
        validate();
        repaint();
    }
}
