package mediathek.gui.dialog;

import com.explodingpixels.macwidgets.HudWidgetFactory;
import com.explodingpixels.macwidgets.HudWindow;
import com.explodingpixels.macwidgets.plaf.HudPaintingUtils;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.net.URISyntaxException;
import java.net.URL;
import javax.imageio.ImageIO;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.daten.Daten;
import mediathek.tool.EscBeenden;
import mediathek.controller.Log;
import mediathek.tool.UrlHyperlinkAction;
import msearch.daten.DatenFilm;
import org.jdesktop.swingx.JXHyperlink;

/**
 * Display the current film information in a Apple-style HUD window.
 */
public class MVFilmInformation implements ChangeListener {

    private HudWindow hud = null;
    private JDialog dialog = null;
    private JXHyperlink lblUrlThemaField;
    private JXHyperlink lblUrlPicture;
    private JTextArea textAreaBeschreibung;
    private Daten ddaten;
    private JLabel[] labelArrNames = new JLabel[DatenFilm.MAX_ELEM];
    private JTextField[] txtArrCont = new JTextField[DatenFilm.MAX_ELEM];
    private ViewImage viewImage = new ViewImage();
    private JButton buttonBild = new JButton("Bild laden");
    private Color foreground, background;
    private DatenFilm aktFilm = new DatenFilm();
    private JFrame parent;

    public MVFilmInformation(JFrame owner, JTabbedPane tabbedPane, Daten ddaten) {
        this.ddaten = ddaten;
        parent = owner;
//        if (Funktionen.getOs() == Funktionen.OS_LINUX) {
//            foreground = Color.WHITE;
//            background = Color.BLACK;
//            //dialog = new JDialog(ddaten.mediathekGui);
//            dialog = new JDialog();
//            dialog.setTitle("Filminformation");
//
//            for (int i = 0; i < DatenFilm.MAX_ELEM; ++i) {
//                labelArrNames[i] = new JLabel(DatenFilm.COLUMN_NAMES[i] + ":");
//                labelArrNames[i].setHorizontalAlignment(SwingConstants.RIGHT);
//                labelArrNames[i].setDoubleBuffered(true);
//                labelArrNames[i].setForeground(foreground);
//                labelArrNames[i].setBackground(background);
//
//                txtArrCont[i] = new JTextField("");
//                txtArrCont[i].setEditable(false);
//                txtArrCont[i].setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
//                txtArrCont[i].setDoubleBuffered(true);
//                txtArrCont[i].setForeground(foreground);
//                txtArrCont[i].setBackground(background);
//            }
//            JComponent content = setLable();
//            content.setBackground(background);
//            content.setForeground(foreground);
//            dialog.add(content);
//            dialog.repaint();
//        } else {
        foreground = Color.WHITE;
        background = Color.BLACK;
        hud = new HudWindow("Filminformation", owner);
        hud.makeResizeable();
        for (int i = 0; i < DatenFilm.MAX_ELEM; ++i) {
            labelArrNames[i] = HudWidgetFactory.createHudLabel(DatenFilm.COLUMN_NAMES[i] + ":");
            labelArrNames[i].setHorizontalAlignment(SwingConstants.RIGHT);
            labelArrNames[i].setDoubleBuffered(true);
            txtArrCont[i] = HudWidgetFactory.createHudTextField("");
            txtArrCont[i].setEditable(false);
            txtArrCont[i].setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
            txtArrCont[i].setDoubleBuffered(true);
        }
        JComponent content = setLable();
        content.setDoubleBuffered(true);
        //prevents flickering in JDK7, JDK6 is still buggy :(
        content.setOpaque(false);
        hud.setContentPane(content);
        dialog = hud.getJDialog();
//        }
        viewImage.setDoubleBuffered(true);
        buttonBild.setOpaque(false);
        buttonBild.setBackground(background);
        buttonBild.setForeground(foreground);
        buttonBild.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent ae) {
                setAktFilm(true);
            }
        });
        dialog.pack();
        Dimension size = dialog.getSize();
        size.width = 600;
        size.height = 600;
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
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        lblUrlThemaField = new JXHyperlink();
        lblUrlThemaField.setDoubleBuffered(true);
        lblUrlThemaField.setForeground(foreground);
        try {
            lblUrlThemaField.setAction(new UrlHyperlinkAction(parent, ddaten, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlThemaField.setFont(HudPaintingUtils.getHudFont());
        lblUrlPicture = new JXHyperlink();
        lblUrlPicture.setDoubleBuffered(true);
        lblUrlPicture.setForeground(foreground);
        try {
            lblUrlPicture.setAction(new UrlHyperlinkAction(parent, ddaten, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlPicture.setFont(HudPaintingUtils.getHudFont());
        textAreaBeschreibung = new JTextArea();
        textAreaBeschreibung.setDoubleBuffered(true);
        textAreaBeschreibung.setLineWrap(true);
        textAreaBeschreibung.setWrapStyleWord(true);
        textAreaBeschreibung.setBackground(background);
        textAreaBeschreibung.setForeground(foreground);
        textAreaBeschreibung.setOpaque(false);
        textAreaBeschreibung.setRows(4);
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        c.weighty = 0;
        panel.setLayout(gridbag);
        int zeile = 0;
        for (int i = 0; i < labelArrNames.length; ++i) {
            if (i == DatenFilm.FILM_URL_RTMP_NR
                    || i == DatenFilm.FILM_URL_AUTH_NR
                    || i == DatenFilm.FILM_URL_HD_NR
                    || i == DatenFilm.FILM_URL_RTMP_HD_NR
                    || i == DatenFilm.FILM_URL_KLEIN_NR
                    || i == DatenFilm.FILM_URL_RTMP_KLEIN_NR
                    || i == DatenFilm.FILM_ABSPIELEN_NR
                    || i == DatenFilm.FILM_AUFZEICHNEN_NR
                    || i == DatenFilm.FILM_DATUM_LONG_NR
                    || i == DatenFilm.FILM_REF_NR) {
                continue;
            }
            c.gridy = zeile;
            addLable(i, gridbag, c, panel);
            ++zeile;
        }
        // Bild einfügen
        c.weightx = 0;
        c.gridx = 1;
        c.gridy = zeile++;
        gridbag.setConstraints(buttonBild, c);
        panel.add(buttonBild);
        c.gridy = zeile++;
        gridbag.setConstraints(viewImage, c);
        panel.add(viewImage);

        // zum zusammenschieben
        c.weightx = 0;
        c.gridx = 0;
        c.weighty = 2;
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
        if (i == DatenFilm.FILM_WEBSEITE_NR) {
            gridbag.setConstraints(lblUrlThemaField, c);
            panel.add(lblUrlThemaField);
        } else if (i == DatenFilm.FILM_IMAGE_URL_NR) {
            gridbag.setConstraints(lblUrlPicture, c);
            panel.add(lblUrlPicture);
        } else if (i == DatenFilm.FILM_BESCHREIBUNG_NR) {
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
        setAktFilm(false);
        dialog.setVisible(true);
    }

    public boolean isVisible() {
        return dialog.isVisible();
    }

    public void updateCurrentFilm(DatenFilm film) {
        aktFilm = film;
        if (this.isVisible()) {
            setAktFilm(false);
        }
    }

    private void setAktFilm(boolean bild) {
        if (aktFilm == null) {
            for (int i = 0; i < txtArrCont.length; ++i) {
                txtArrCont[i].setText("");
            }
            textAreaBeschreibung.setText(" ");
            lblUrlThemaField.setText("");
            lblUrlThemaField.setForeground(foreground);
            // bei den Bildern wird nur eins gesetzt
            lblUrlPicture.setText("");
            lblUrlPicture.setForeground(foreground);
            // wenns kein Bild gibt brauchts auch nichts zum Anzeigen
            buttonBild.setVisible(false);
            viewImage.setImage(""); // zum löschen
        } else {
            for (int i = 0; i < txtArrCont.length; ++i) {
                txtArrCont[i].setText(aktFilm.arr[i]);
            }
            if (aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG_NR].equals("")) {
                // sonst müsste die Größe gesetzt werden
                textAreaBeschreibung.setText(" ");
            } else {
                textAreaBeschreibung.setText(aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG_NR]);
            }
            lblUrlThemaField.setText(aktFilm.arr[DatenFilm.FILM_WEBSEITE_NR]);
            lblUrlThemaField.setForeground(foreground);
            // bei den Bildern wird nur eins gesetzt
            lblUrlPicture.setText(aktFilm.arr[DatenFilm.FILM_IMAGE_URL_NR]);
            lblUrlPicture.setForeground(foreground);
            if (aktFilm.arr[DatenFilm.FILM_IMAGE_URL_NR].equals("")) {
                // wenns kein Bild gibt brauchts auch nichts zum Anzeigen
                buttonBild.setVisible(false);
                viewImage.setImage(""); // zum löschen
            } else {
                if (aktFilm.arr[DatenFilm.FILM_SENDER_NR].equals("")) {
                    buttonBild.setText("Bild laden");
                } else {
                    buttonBild.setText("Bild vom Sender  " + aktFilm.arr[DatenFilm.FILM_SENDER_NR] + "  laden");
                }
                buttonBild.setVisible(true);
                if (bild) {
                    if (!aktFilm.arr[DatenFilm.FILM_IMAGE_URL_NR].equals("")) {
                        viewImage.setImage(aktFilm.arr[DatenFilm.FILM_IMAGE_URL_NR]);
                    }
                } else {
                    viewImage.setImage(""); // zum löschen
                }
            }
        }
        dialog.repaint();
    }

    @Override
    public void stateChanged(ChangeEvent changeEvent) {
        //Whenever there is a change event, reset HUD info to nothing
        DatenFilm emptyFilm = new DatenFilm();
        updateCurrentFilm(emptyFilm);
    }

    private class ViewImage extends JComponent {

        private BufferedImage image = null;

        public void setImage(String urlStr) {
            if (urlStr.equals("")) {
                image = null;
                this.setMinimumSize(new Dimension(10, 10));
                this.setPreferredSize(new Dimension(10, 10));
                return;
            }
            try {
                image = ImageIO.read(new URL(urlStr));
                image = scale(image, new Dimension(250, 250));
                this.setMinimumSize(new Dimension(image.getWidth(), image.getHeight()));
                this.setPreferredSize(new Dimension(image.getWidth(), image.getHeight()));
                this.setSize(new Dimension(image.getWidth(), image.getHeight()));
            } catch (Exception ex) {
                Log.fehlerMeldung(919302497, Log.FEHLER_ART_PROG, MVFilmInformation.class.getName(), ex);
            }
            repaint();
        }

        @Override
        protected void paintComponent(Graphics g) {
            if (image != null) {
                g.drawImage(image, 0, 0, image.getWidth(), image.getHeight(), this);
                super.paintComponent(g);
            } else {
                super.paintComponent(g);
            }
        }

        private BufferedImage scale(BufferedImage img, Dimension d) {
            float factor = getFactor(img.getWidth(), img.getHeight(), d);
            // create the image
            int w = (int) (img.getWidth() * factor);
            int h = (int) (img.getHeight() * factor);
            BufferedImage scaled = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);

            Graphics2D g = scaled.createGraphics();
            g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            g.drawImage(img, 0, 0, w, h, null);
            g.dispose();
            return scaled;
        }

        float getFactor(int width, int height, Dimension dim) {
            float sx = dim.width / (float) width;
            float sy = dim.height / (float) height;
            return Math.min(sx, sy);
        }
    }
}
