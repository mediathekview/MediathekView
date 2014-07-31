/*
 *  ExceptionUtil, utility class for exceptions.
 *  Copyright (C) 2004 - 2013 Achim Westermann.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  If you modify or optimize the code in a useful way please let me know.
 *  Achim.Westermann@gmx.de
 *
 */
package info.monitorenter.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;

/**
 * Nice static helpers for working with Strings.
 * <p>
 * Maybe not always the fastest solution to call in here, but working. Also
 * usable for seeing examples and cutting code for manual inlining.
 * <p>
 * 
 * @author Achim.Westermann@gmx.de
 * 
 * @version $Revision: 1.2 $
 */
public final class ExceptionUtil {
  
  /** Singleton instance. */
  private static ExceptionUtil instance = null;

  /**
   * Returns an input stream that contains what will written in this Application to {@link System#err}.<p>
   * 
   * <b>Caution</b><br/>
   * If you do not consume the bytes to read from the result you may block the whole application. Do only use this for debugging purposes or end to end test 
   * code!<p>
   * 
   * @return an input stream that contains what will written in this Application to {@link System#err}.
   * 
   * @throws IOException if something goes wrong. 
   */
  public static InputStream captureSystemErrForDebuggingPurposesOnly() throws IOException{
    PipedOutputStream pipeOut = new PipedOutputStream();
    PipedInputStream pipeIn = new PipedInputStream(pipeOut);
    PrintStream streamOut = new PrintStream(pipeOut);
    System.setErr(streamOut);
    return pipeIn;
  }
  
  /**
   * Returns an input stream that contains what will written in this Application to {@link System#out}.<p>
   * 
   * <b>Caution</b><br/>
   * If you do not consume the bytes to read from the result you may block the whole application. Do only use this for debugging purposes or end to end test 
   * code!<p>
   * 
   * @return an input stream that contains what will written in this Application to {@link System#out}.
   * 
   * @throws IOException if something goes wrong. 
   */
  public static InputStream captureSystemOutForDebuggingPurposesOnly() throws IOException{
    PipedOutputStream pipeOut = new PipedOutputStream();
    PipedInputStream pipeIn = new PipedInputStream(pipeOut);
    PrintStream streamOut = new PrintStream(pipeOut);
    System.setOut(streamOut);
    return pipeIn;
  }
  
  /**
   * Prints out the current Thread stack to the given stream.<p>
   * 
   *   @see Thread#getStackTrace()
   *   
   * @param outprint the stream to print to (e.g. <code>{@link System#err}</code>).
   */
  public static void dumpThreadStack(PrintStream outprint) {
    StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
    String stackTraceString = StringUtil.arrayToString(stackTrace,"\n");
    outprint.println(stackTraceString);
  }
  
  /**
   * Returns the singleton instance of this class.
   * <p>
   * 
   * This method is useless for now as all methods are static. It may be used in
   * future if VM-global configuration will be put to the state of the instance.
   * <p>
   * 
   * 
   * @return the singleton instance of this class.
   */
  public static ExceptionUtil instance() {
    if (ExceptionUtil.instance == null) {
      ExceptionUtil.instance = new ExceptionUtil();
    }
    return ExceptionUtil.instance;
  }

}
