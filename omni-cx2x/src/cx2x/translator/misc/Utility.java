/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.misc;

import xcodeml.util.XmOption;

import java.util.Iterator;
import java.util.List;

/**
 * This class contains utilities methods that are useful in the whole project.
 * Mostly to implements java 1.8 features that are not present in java 1.7.
 * 
 * @author clementval
 */
public class Utility {
  /**
   * Join an array of String elements into a single String.
   * @param delimiter Delimiter to be placed between each element.
   * @param elements  Array of String elements.
   * @return A joined string of all elements separated by the delimiter.
   */
  public static String join(String delimiter, String[] elements){
    StringBuilder ret = new StringBuilder();
    boolean firstIteration = true;
    for(String s : elements){
      if(!firstIteration){
        ret.append(delimiter);
      } else {
        firstIteration = false;
      }
      ret.append(s);
    }
    return ret.toString();
  }

  /**
   * Join a list of String elements into a single String.
   * @param delimiter Delimiter to be placed between each element.
   * @param elements  List of String elements.
   * @param <T>       Type of element in the list.
   * @return A joined string of all elements separated by the delimiter.
   */
  public static <T> String join(String delimiter, List<T> elements){
    StringBuilder ret = new StringBuilder();
    Iterator<T> it = elements.iterator();
    while(it.hasNext()){
      ret.append(it.next().toString());
      if(it.hasNext()) {
        ret.append(delimiter);
      }
    }
    return ret.toString();
  }

  /**
   * Print debugging information on the standard output if option is active.
   * @param value Value to be printed.
   */
  public static void debug(String value){
    if(XmOption.isDebugOutput()) {
      System.out.println(value);
    }
  }
}
