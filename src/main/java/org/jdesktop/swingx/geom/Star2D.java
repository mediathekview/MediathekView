/*
 * $Id: Star2D.java 3863 2010-10-26 02:53:32Z kschaefe $
 *
 * Dual-licensed under LGPL (Sun and Romain Guy) and BSD (Romain Guy).
 *
 * Copyright 2005 Sun Microsystems, Inc., 4150 Network Circle,
 * Santa Clara, California 95054, U.S.A. All rights reserved.
 *
 * Copyright (c) 2006 Romain Guy <romain.guy@mac.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.jdesktop.swingx.geom;

import java.awt.*;
import java.awt.geom.*;

/**
 * <p>This class provides a star shape. A star is defined by two radii and a
 * number of branches. Each branch spans between the two radii. The inner
 * radius is the distance between the center of the star and the origin of the
 * branches. The outer radius is the distance between the center of the star
 * and the tips of the branches.</p>
 *
 * @author Romain Guy <romain.guy@mac.com>
 */

public class Star2D implements Shape {
    private Shape starShape;
    private double x;
    private double y;
    private double innerRadius;
    private double outerRadius;
    private int branchesCount;

    /**
     * <p>Creates a new star whose center is located at the specified
     * <code>x</code> and <code>y</code> coordinates. The number of branches
     * and their length can be specified.</p>
     *
     * @param x the location of the star center
     * @param y the location of the star center
     * @param innerRadius the distance between the center of the star and the
     *   origin of the branches
     * @param outerRadius the distance between the center of the star and the
     *   tip of the branches
     * @param branchesCount the number of branches in this star; must be &gt;= 3
     * @throws IllegalArgumentException if <code>branchesCount<code> is < 3 or
     *   if <code>innerRadius</code> is &gt;= <code>outerRadius</code>
     */
    public Star2D(double x, double y,
                  double innerRadius, double outerRadius,
                  int branchesCount) {
        if (branchesCount < 3) {
            throw new IllegalArgumentException("The number of branches must" +
                                               " be >= 3.");
        } else if (innerRadius >= outerRadius) {
            throw new IllegalArgumentException("The inner radius must be < " +
                                               "outer radius.");
        }

        this.x = x;
        this.y = y;
        this.innerRadius = innerRadius;
        this.outerRadius = outerRadius;
        this.branchesCount = branchesCount;

        starShape = generateStar(x, y, innerRadius, outerRadius, branchesCount);
    }

    private static Shape generateStar(double x, double y,
                                      double innerRadius, double outerRadius,
                                      int branchesCount) {
        GeneralPath path = new GeneralPath();

        double outerAngleIncrement = 2 * Math.PI / branchesCount;

        double outerAngle = branchesCount % 2 == 0 ? 0.0 : -(Math.PI / 2.0);
        double innerAngle = (outerAngleIncrement / 2.0) + outerAngle;

        float x1 = (float) (Math.cos(outerAngle) * outerRadius + x);
        float y1 = (float) (Math.sin(outerAngle) * outerRadius + y);

        float x2 = (float) (Math.cos(innerAngle) * innerRadius + x);
        float y2 = (float) (Math.sin(innerAngle) * innerRadius + y);

        path.moveTo(x1, y1);
        path.lineTo(x2, y2);

        outerAngle += outerAngleIncrement;
        innerAngle += outerAngleIncrement;

        for (int i = 1; i < branchesCount; i++) {
            x1 = (float) (Math.cos(outerAngle) * outerRadius + x);
            y1 = (float) (Math.sin(outerAngle) * outerRadius + y);

            path.lineTo(x1, y1);

            x2 = (float) (Math.cos(innerAngle) * innerRadius + x);
            y2 = (float) (Math.sin(innerAngle) * innerRadius + y);

            path.lineTo(x2, y2);

            outerAngle += outerAngleIncrement;
            innerAngle += outerAngleIncrement;
        }

        path.closePath();
        return path;
    }

    /**
     * <p>Sets the inner radius of the star, that is the distance between its
     * center and the origin of the branches. The inner radius must always be
     * lower than the outer radius.</p>
     *
     * @param innerRadius the distance between the center of the star and the
     *   origin of the branches
     * @throws IllegalArgumentException if the inner radius is &gt;= outer radius
     */
    public void setInnerRadius(double innerRadius) {
        if (innerRadius >= outerRadius) {
            throw new IllegalArgumentException("The inner radius must be <" +
                                               " outer radius.");
        }

        this.innerRadius = innerRadius;
        starShape = generateStar(getX(), getY(), innerRadius, getOuterRadius(),
                                 getBranchesCount());
    }

    /**
     * <p>Sets location of the center of the star.</p>
     *
     * @param x the x location of the center of the star
     */
    public void setX(double x) {
        this.x = x;
        starShape = generateStar(x, getY(), getInnerRadius(), getOuterRadius(),
                                 getBranchesCount());
    }

    /**
     * <p>Sets the location of the center of the star.</p>
     *
     * @param y the x location of the center of the star
     */
    public void setY(double y) {
        this.y = y;
        starShape = generateStar(getX(), y, getInnerRadius(), getOuterRadius(),
                                 getBranchesCount());
    }

    /**
     * <p>Sets the outer radius of the star, that is the distance between its
     * center and the tips of the branches. The outer radius must always be
     * greater than the inner radius.</p>
     *
     * @param outerRadius the distance between the center of the star and the
     *   tips of the branches
     * @throws IllegalArgumentException if the inner radius is &gt;= outer radius
     */
    public void setOuterRadius(double outerRadius) {
        if (innerRadius >= outerRadius) {
            throw new IllegalArgumentException("The outer radius must be > " +
                                               "inner radius.");
        }

        this.outerRadius = outerRadius;
        starShape = generateStar(getX(), getY(), getInnerRadius(), outerRadius,
                                 getBranchesCount());
    }

    /**
     * <p>Sets the number branches of the star. A star must always have at least
     * 3 branches.</p>
     *
     * @param branchesCount the number of branches
     * @throws IllegalArgumentException if <code>branchesCount</code> is &lt;=2
     */
    public void setBranchesCount(int branchesCount) {
        if (branchesCount <= 2) {
            throw new IllegalArgumentException("The number of branches must" +
                                               " be >= 3.");
        }

        this.branchesCount = branchesCount;
        starShape = generateStar(getX(), getY(), getInnerRadius(),
                                 getOuterRadius(), branchesCount);
    }

    /**
     * <p>Returns the location of the center of star.</p>
     *
     * @return the x coordinate of the center of the star
     */
    public double getX() {
        return x;
    }

    /**
     * <p>Returns the location of the center of star.</p>
     *
     * @return the y coordinate of the center of the star
     */
    public double getY() {
        return y;
    }

    /**
     * <p>Returns the distance between the center of the star and the origin
     * of the branches.</p>
     *
     * @return the inner radius of the star
     */
    public double getInnerRadius() {
        return innerRadius;
    }

    /**
     * <p>Returns the distance between the center of the star and the tips
     * of the branches.</p>
     *
     * @return the outer radius of the star
     */
    public double getOuterRadius() {
        return outerRadius;
    }

    /**
     * <p>Returns the number of branches of the star.</p>
     *
     * @return the number of branches, always &gt;= 3
     */
    public int getBranchesCount() {
        return branchesCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Rectangle getBounds() {
        return starShape.getBounds();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Rectangle2D getBounds2D() {
        return starShape.getBounds2D();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(double x, double y) {
        return starShape.contains(x, y);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(Point2D p) {
        return starShape.contains(p);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean intersects(double x, double y, double w, double h) {
        return starShape.intersects(x, y, w, h);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean intersects(Rectangle2D r) {
        return starShape.intersects(r);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(double x, double y, double w, double h) {
        return starShape.contains(x, y, w, h);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(Rectangle2D r) {
        return starShape.contains(r);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PathIterator getPathIterator(AffineTransform at) {
        return starShape.getPathIterator(at);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PathIterator getPathIterator(AffineTransform at, double flatness) {
        return starShape.getPathIterator(at, flatness);
    }
}
