package mediathek.gui.dialog;

import com.explodingpixels.macwidgets.HudWidgetFactory;
import com.explodingpixels.macwidgets.HudWindow;
import com.explodingpixels.macwidgets.plaf.HudPaintingUtils;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import mediathek.daten.DatenFilm;
import org.jdesktop.swingx.JXHyperlink;

import java.net.URISyntaxException;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.daten.DDaten;
import mediathek.tool.EscBeenden;
import mediathek.tool.UrlHyperlinkAction;

/**
 * Display the current film information in a Apple-style HUD window.
 */
public class MVFilmInformation implements ChangeListener {

    private HudWindow hud;
    private JXHyperlink lblUrlThemaField;
    private JXHyperlink lblUrlPicture;
    private JXHyperlink lblUrlThumbnail;
    private JTextArea textAreaBeschreibung;
    private DDaten ddaten;
    private JLabel[] labelArrNames = new JLabel[DatenFilm.FILME_MAX_ELEM];
    private JTextField[] txtArrCont = new JTextField[DatenFilm.FILME_MAX_ELEM];

    public MVFilmInformation(Frame owner, JTabbedPane tabbedPane, DDaten ddaten) {
        hud = new HudWindow("Filminformation", owner);
        hud.makeResizeable();
        this.ddaten = ddaten;
        //JComponent content = buildContent();
        JComponent content = setLable();
        //prevents flickering in JDK7, JDK6 is still buggy :(
        content.setOpaque(false);
        hud.setContentPane(content);
        JDialog dialog = hud.getJDialog();
        dialog.pack();
        Dimension size = dialog.getSize();
        size.width = 600;
        size.height = 500;
        dialog.setSize(size);
        calculateHudPosition();

        tabbedPane.addChangeListener(this);

        new EscBeenden(dialog) {
            @Override
            public void beenden_(JDialog d) {
                d.dispose();
            }
        };
    }

    private JComponent setLable() {
        JPanel panel = new JPanel();
        panel.setDoubleBuffered(true);
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        for (int i = 0; i < DatenFilm.FILME_MAX_ELEM; ++i) {
            labelArrNames[i] = HudWidgetFactory.createHudLabel(DatenFilm.FILME_COLUMN_NAMES[i] + ":");
            labelArrNames[i].setHorizontalAlignment(SwingConstants.RIGHT);
            txtArrCont[i] = HudWidgetFactory.createHudTextField("");
            txtArrCont[i].setEditable(false);
            txtArrCont[i].setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        }
        lblUrlThemaField = new JXHyperlink();
        lblUrlThemaField.setForeground(Color.WHITE);
        try {
            lblUrlThemaField.setAction(new UrlHyperlinkAction(ddaten, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlThemaField.setFont(HudPaintingUtils.getHudFont());
        lblUrlPicture = new JXHyperlink();
        lblUrlPicture.setForeground(Color.WHITE);
        try {
            lblUrlPicture.setAction(new UrlHyperlinkAction(ddaten, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlPicture.setFont(HudPaintingUtils.getHudFont());
        lblUrlThumbnail = new JXHyperlink();
        lblUrlThumbnail.setForeground(Color.WHITE);
        try {
            lblUrlThumbnail.setAction(new UrlHyperlinkAction(ddaten, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlThumbnail.setFont(HudPaintingUtils.getHudFont());
        textAreaBeschreibung = new JTextArea();
        textAreaBeschreibung.setLineWrap(true);
        textAreaBeschreibung.setBackground(Color.BLACK);
        textAreaBeschreibung.setForeground(Color.WHITE);
        textAreaBeschreibung.setOpaque(false);
        textAreaBeschreibung.setRows(4);
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        panel.setLayout(gridbag);
        int zeile = 0;
        for (int i = 0; i < labelArrNames.length; ++i) {
            if (i == DatenFilm.FILM_URL_RTMP_NR
                    || i == DatenFilm.FILM_URL_AUTH_NR) {
                continue;
            }
            c.gridy = zeile;
            addLable(i, gridbag, c, panel);
            ++zeile;
        }
        // zum zusammenschieben
        c.weightx = 0;
        c.gridx = 0;
        c.weighty = 1;
        c.gridy = zeile;
        JLabel label = new JLabel();
        gridbag.setConstraints(label, c);
        panel.add(label);
        return panel;
    }

    private void addLable(int i, GridBagLayout gridbag, GridBagConstraints c, JPanel panel) {
        c.gridx = 0;
        c.weightx = 0;
        gridbag.setConstraints(labelArrNames[i], c);
        panel.add(labelArrNames[i]);
        c.gridx = 1;
        c.weightx = 10;
        if (i == DatenFilm.FILM_URL_THEMA_NR) {
            gridbag.setConstraints(lblUrlThemaField, c);
            panel.add(lblUrlThemaField);
        } else if (i == DatenFilm.FILM_IMAGE_URL_NR) {
            gridbag.setConstraints(lblUrlPicture, c);
            panel.add(lblUrlPicture);
        } else if (i == DatenFilm.FILM_THUMBNAIL_URL_NR) {
            gridbag.setConstraints(lblUrlThumbnail, c);
            panel.add(lblUrlThumbnail);
        } else if (i == DatenFilm.FILM_DESCRIPTION_NR) {
            gridbag.setConstraints(textAreaBeschreibung, c);
            panel.add(textAreaBeschreibung);
        } else {
            gridbag.setConstraints(txtArrCont[i], c);
            panel.add(txtArrCont[i]);
        }
    }

    private void calculateHudPosition() {
        //FIXME calculate the HUD position
    }

    public void show() {
        hud.getJDialog().setVisible(true);
    }

    public boolean isVisible() {
        return hud.getJDialog().isVisible();
    }

    public void updateCurrentFilm(DatenFilm film) {
        for (int i = 0; i < txtArrCont.length; ++i) {
            txtArrCont[i].setText(film.arr[i]);
        }
        if (film.arr[DatenFilm.FILM_DESCRIPTION_NR].equals("")) {
            // sonst müsste die Größe gesetzt werden
            textAreaBeschreibung.setText(" ");
        } else {
            textAreaBeschreibung.setText(film.arr[DatenFilm.FILM_DESCRIPTION_NR]);
        }
        // setup Hyperlink
        // FIXME ist das nicht besser zu lösen?
        try {
            lblUrlThemaField.setAction(new UrlHyperlinkAction(ddaten, film.arr[DatenFilm.FILM_URL_THEMA_NR]));
            lblUrlThemaField.setForeground(Color.WHITE);
            lblUrlPicture.setAction(new UrlHyperlinkAction(ddaten, film.arr[DatenFilm.FILM_IMAGE_URL_NR]));
            lblUrlPicture.setForeground(Color.WHITE);
            lblUrlThumbnail.setAction(new UrlHyperlinkAction(ddaten, film.arr[DatenFilm.FILM_THUMBNAIL_URL_NR]));
            lblUrlThumbnail.setForeground(Color.WHITE);
        } catch (URISyntaxException ignored) {
        }

        hud.getJDialog().repaint();
    }

    @Override
    public void stateChanged(ChangeEvent changeEvent) {
        //Whenever there is a change event, reset HUD info to nothing
        DatenFilm emptyFilm = new DatenFilm();
        updateCurrentFilm(emptyFilm);
    }
}
