/*
 * @(#)ThreadCheckingRepaintManager.java 4/18/2006
 *
 * Copyright 2002 - 2006 JIDE Software Inc. All rights reserved.
 */

package com.jidesoft.utils;

import javax.swing.*;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

/**
 * For usage of this class, please refer to http://weblogs.java.net/blog/alexfromsun/archive/2006/02/debugging_swing.html
 * <p> To use it, call RepaintManager.setCurrentManager(new ThreadCheckingRepaintManager()) then watch the print out
 * from the console of all threading violations. </p>
 */
public class ThreadCheckingRepaintManager extends RepaintManager {
    // it is recommended to pass the complete check
    private boolean completeCheck = true;
    private boolean checkIsShowing = false;

    /**
     * Creates ThreadCheckingRepaintManager. You can create one and set it using RepaintManager.setCurrentManager(new
     * ThreadCheckingRepaintManager()).
     */
    public ThreadCheckingRepaintManager() {
        super();
    }

    /**
     * Creates ThreadCheckingRepaintManager. You can create one and set it using RepaintManager.setCurrentManager(new
     * ThreadCheckingRepaintManager()).
     *
     * @param checkIsShowing true to only check showing components.
     */
    public ThreadCheckingRepaintManager(boolean checkIsShowing) {
        super();
        this.checkIsShowing = checkIsShowing;
    }

    /**
     * Initially there was a rule that it is safe to create and use Swing components until they are realized but this
     * rule is not valid any more, and now it is recommended to interact with Swing from EDT only.
     * <p/>
     * That's why completeCheck flag is used - if you test the old program switch it to false, but new applications
     * should be tested with completeCheck set to true*
     *
     * @return true or false. By default, it is false.
     */
    public boolean isCompleteCheck() {
        return completeCheck;
    }

    /**
     * @param completeCheck true or false.
     *
     * @see #isCompleteCheck()
     */
    public void setCompleteCheck(boolean completeCheck) {
        this.completeCheck = completeCheck;
    }

    @Override
    public synchronized void addInvalidComponent(JComponent jComponent) {
        checkThreadViolations(jComponent);
        super.addInvalidComponent(jComponent);
    }

    @Override
    public synchronized void addDirtyRegion(JComponent jComponent, int i, int i1, int i2, int i3) {
        checkThreadViolations(jComponent);
        super.addDirtyRegion(jComponent, i, i1, i2, i3);
    }

    private void checkThreadViolations(JComponent c) {
        if (!SwingUtilities.isEventDispatchThread() && (completeCheck || checkIsShowing(c))) {
            Exception exception = new Exception();
            boolean repaint = false;
            boolean fromSwing = false;
            StackTraceElement[] stackTrace = exception.getStackTrace();
            for (StackTraceElement st : stackTrace) {
                if (repaint && st.getClassName().startsWith("javax.swing.")) {
                    fromSwing = true;
                }
                if ("repaint".equals(st.getMethodName())) {
                    repaint = true;
                }
            }
            if (repaint && !fromSwing) {
                //no problems here, since repaint() is thread safe
                return;
            }
            System.out.println("----------Wrong Thread START");
            System.out.println(getStrackTraceAsString(exception));
            System.out.println("----------Wrong Thread END");
        }
    }

    @SuppressWarnings({"SimplifiableIfStatement"})
    private boolean checkIsShowing(JComponent c) {
        if (this.checkIsShowing) {
            return c.isShowing();
        }
        else {
            return true;
        }
    }

    private String getStrackTraceAsString(Exception e) {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        PrintStream printStream = new PrintStream(byteArrayOutputStream);
        e.printStackTrace(printStream);
        printStream.flush();
        return byteArrayOutputStream.toString();
    }
}
