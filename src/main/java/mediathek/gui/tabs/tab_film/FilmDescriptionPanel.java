package mediathek.gui.tabs.tab_film;

import mediathek.daten.DatenFilm;
import mediathek.gui.dialog.DialogFilmBeschreibung;
import mediathek.gui.tabs.AGuiTabPanel;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.sender_icon_cache.MVSenderIconCache;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXHyperlink;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.net.URL;

public class FilmDescriptionPanel extends JPanel {
    private final AGuiTabPanel currentTab;
    private final JScrollPane scrollPane1 = new JScrollPane();
    private final JPopupMenu popupMenu = new JPopupMenu();
    private final JLabel lblIcon = new JLabel();
    private final JLabel lblThema = new JLabel();
    private final JLabel lblTitel = new JLabel();
    private final JTextArea textArea = new JTextArea();
    private final JXHyperlink hyperlink = new JXHyperlink();

    private DatenFilm currentFilm;

    public FilmDescriptionPanel(@NotNull AGuiTabPanel currentTab) {
        this.currentTab = currentTab;

        initComponents();

        createPopupMenu();

        setAllFieldsEmpty();
    }

    private void createPopupMenu() {
        var item = new JMenuItem("Beschreibung ändern...");
        item.addActionListener(l -> {
            DialogFilmBeschreibung dialog = new DialogFilmBeschreibung(MediathekGui.ui(), currentFilm);
            dialog.setVisible(true);
        });
        popupMenu.add(item);
        popupMenu.addSeparator();

        item = new JMenuItem("Beschreibung in Zwischenablage kopieren");
        item.addActionListener(l -> GuiFunktionen.copyToClipboard(currentFilm.getDescription()));
        popupMenu.add(item);

        item = new JMenuItem("Filmbasisinformationen in Zwischenablage kopieren");
        item.addActionListener(l -> {
            String sb = currentFilm.getSender() +
                    " - " +
                    currentFilm.getThema() +
                    " - " +
                    currentFilm.getTitle();

            GuiFunktionen.copyToClipboard(sb);
        });
        popupMenu.add(item);

        setComponentPopupMenu(popupMenu);
        textArea.setComponentPopupMenu(popupMenu);
    }

    private void initComponents() {
        setLayout(new MigLayout(
                new LC().hideMode(3),
                new AC()
                        .fill().gap()
                        .grow().fill(),
                new AC()
                        .gap()
                        .gap()
                        .gap()
        ));

        lblIcon.setMaximumSize(new Dimension(96, 96));
        lblIcon.setPreferredSize(new Dimension(96, 96));
        lblIcon.setVerticalAlignment(SwingConstants.TOP);
        add(lblIcon, new CC().cell(0, 0, 1, 3).alignX("center").alignY("top").grow(0, 0)); //NON-NLS

        lblThema.setFont(lblThema.getFont().deriveFont(lblThema.getFont().getStyle() | Font.BOLD));
        add(lblThema, new CC().cell(1, 0));

        lblTitel.setFont(lblTitel.getFont().deriveFont(lblTitel.getFont().getStyle() | Font.BOLD));
        add(lblTitel, new CC().cell(1, 1));

        scrollPane1.setPreferredSize(new Dimension(299, 75));
        scrollPane1.setMaximumSize(new Dimension(Integer.MAX_VALUE, 75));
        scrollPane1.setMinimumSize(new Dimension(23, 75));
        scrollPane1.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        textArea.setEditable(false);
        textArea.setWrapStyleWord(true);
        textArea.setLineWrap(true);
        scrollPane1.setViewportView(textArea);
        add(scrollPane1, new CC().cell(1, 2).grow());

        //add(hyperlink, new CC().cell(0, 3, 2, 1));
        add(hyperlink, new CC().cell(1, 3));
    }

    /**
     * Determine the image's size while keeping aspect ratio within a given boundary box.
     * @param imgSize The size of the original image.
     * @param boundary The bounds where the image needs to fit into.
     * @return The calculated image dimensions for fitting into boundary.
     */
    public Dimension getScaledDimension(Dimension imgSize, Dimension boundary) {

        int original_width = imgSize.width;
        int original_height = imgSize.height;
        int bound_width = boundary.width;
        int bound_height = boundary.height;
        int new_width = original_width;
        int new_height = original_height;

        // first check if we need to scale width
        if (original_width > bound_width) {
            //scale width to fit
            new_width = bound_width;
            //scale height to maintain aspect ratio
            new_height = (new_width * original_height) / original_width;
        }

        // then check if we need to scale even with the new height
        if (new_height > bound_height) {
            //scale height to fit instead
            new_height = bound_height;
            //scale width to maintain aspect ratio
            new_width = (new_height * original_width) / original_height;
        }

        return new Dimension(new_width, new_height);
    }

    private Image getScaledImage(Image srcImg, int w, int h) {
        BufferedImage resizedImg = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2 = resizedImg.createGraphics();

        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g2.drawImage(srcImg, 0, 0, w, h, null);
        g2.dispose();

        return resizedImg;
    }

    public void install(@NotNull JTabbedPane tabbedPane, @NotNull JTable tabelle) {
        tabbedPane.add("Beschreibung", this);

        tabelle.getSelectionModel().addListSelectionListener(e ->
                currentTab.getCurrentlySelectedFilm().ifPresentOrElse(film -> {
                    showFilmDescription(film);
                    currentFilm = film;
                }, () -> {
                    setAllFieldsEmpty();
                    currentFilm = null;
                }));
    }

    private void setAllFieldsEmpty() {
        hyperlink.setVisible(false);
        hyperlink.setText("");
        hyperlink.setToolTipText("");

        textArea.setText("");
        lblIcon.setIcon(null);
        lblThema.setText("");
        lblTitel.setText("");
    }

    private void showFilmDescription(@NotNull DatenFilm film) {
        lblThema.setText(film.getThema());
        lblTitel.setText(film.getTitle());

        hyperlink.setVisible(true);
        try {
            hyperlink.setURI(new URL(film.getWebsiteLink()).toURI());
            hyperlink.setText("Link zur Webseite");
            hyperlink.setClicked(false);
        }
        catch (Exception e) {
            //logger
            hyperlink.setText("Link nicht verfügbar");
        }
        hyperlink.setToolTipText(film.getWebsiteLink());

        textArea.setText(film.getDescription());
        SwingUtilities.invokeLater(() -> scrollPane1.getVerticalScrollBar().setValue(0));
        MVSenderIconCache.get(film.getSender()).ifPresentOrElse(icon -> {
            var imageDim = new Dimension(icon.getIconWidth(), icon.getIconHeight());
            var boundary = new Dimension(96, 96);
            var destDim = getScaledDimension(imageDim, boundary);
            var newIcon = getScaledImage(icon.getImage(), destDim.width, destDim.height);
            lblIcon.setIcon(new ImageIcon(newIcon));
        }, () -> lblIcon.setIcon(null));
    }
}
