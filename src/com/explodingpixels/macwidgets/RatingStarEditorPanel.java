package com.explodingpixels.macwidgets;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JPanel;

/**
 * This editor panel emulates iTunes Star rating editor functionality
 * @author Paul Connolly paulcconnolly gmail
 */
public class RatingStarEditorPanel
        extends JPanel
        implements MouseListener, MouseMotionListener {


    public RatingStarEditorPanel() {
        setBorder(BorderFactory.createLineBorder(Color.BLACK, 1));

        repaint();
    }

    private int lastX = Integer.MIN_VALUE;
    private int lastY = Integer.MIN_VALUE;
    private int xoffset = 5; //these offsets are there for display purposes, to position the stars correctly
    private int yoffset = 2;


    public void mouseClicked(MouseEvent e) {
        processCoordinates(e);
        repaint();
    }

    /**
     * keep track of the last mouse position.
     *
     * @param e
     */
    private void recordCoordinates(MouseEvent e) {
        lastX = e.getX();
        lastY = e.getY();
    }

    /**
     * Before recording coordinates, we have to make sure that they fall in the right range.
     *
     * @param e
     * @return
     */
    private boolean validateCoordinates(MouseEvent e) {
        Point location = getLocation();
        Dimension size = getSize();
        if (e.getX() < location.getX() || e.getX() > location.getX() + size.getWidth()) {
            return false;
        }
        if (e.getY() < location.getY() || e.getY() > location.getY() + size.getHeight()) {
            return false;
        }
        return true;
    }

    public void mousePressed(MouseEvent e) {
        processCoordinates(e);
        repaint();
    }


    private void processCoordinates(MouseEvent e) {
        if (e != null) {
            if (validateCoordinates(e)) {
                recordCoordinates(e);
            }
        }
    }

    public void mouseReleased(MouseEvent e) {
        processCoordinates(e);
        repaint();
    }

    public void mouseEntered(MouseEvent e) {

    }

    public void mouseExited(MouseEvent e) {

    }

    /**
     * Calculate the level between 0-5. This correlates to the star rating system.
     *
     * @return
     */
    public int getLevel() {
        if (lastX == Integer.MIN_VALUE || lastY == Integer.MIN_VALUE) {
            return 0;
        }

        int min = (int) getLocation().getX();
        int max = min + (int) getSize().getWidth();
        //essentially, calculate a percentile for what section of this component the mouse
        //is positioned within.
        return (int) Math.ceil(((double) (lastX - min) / (max - min)) * 5);
    }

    @Override
    protected void paintComponent(Graphics g) {

        Graphics2D g2 = ((Graphics2D) g);
        Image star = new ImageIcon(ITunesRatingTableCellRenderer.class.getResource(
                "/com/explodingpixels/macwidgets/images/itunes_star_unselected.png")).getImage();

        int level = getLevel();

        g2.clearRect(0, 0, getWidth(), getHeight());
        for (int i = 0; i < level; i++) {
            g2.drawImage(star, (i * star.getWidth(null)) + xoffset, yoffset, null, null);
        }
    }

    public void mouseDragged(MouseEvent e) {
        processCoordinates(e);
        repaint();
    }

    public void mouseMoved(MouseEvent e) {
        //To change body of implemented methods use File | Settings | File Templates.
    }
}
