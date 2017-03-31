/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.common;

import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.accelerator.AcceleratorGenerator;
import cx2x.translator.language.helper.target.Target;
import xcodeml.util.XmOption;

import java.util.*;

/**
 * This class contains utilities methods that are useful in the whole project.
 * Mostly to implements java 1.8 features that are not present in java 1.7.
 *
 * @author clementval
 */
public class Utility {

  /**
   * Join an array of String elements into a single String.
   *
   * @param delimiter Delimiter to be placed between each element.
   * @param elements  Array of String elements.
   * @return A joined string of all elements separated by the delimiter.
   */
  public static String join(String delimiter, String[] elements) {
    StringBuilder ret = new StringBuilder();
    boolean firstIteration = true;
    for(String s : elements) {
      if(!firstIteration) {
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
   *
   * @param delimiter Delimiter to be placed between each element.
   * @param elements  List of String elements.
   * @param <T>       Type of element in the list.
   * @return A joined string of all elements separated by the delimiter.
   */
  public static <T> String join(String delimiter, List<T> elements) {
    StringBuilder ret = new StringBuilder();
    Iterator<T> it = elements.iterator();
    while(it.hasNext()) {
      ret.append(it.next().toString());
      if(it.hasNext()) {
        ret.append(delimiter);
      }
    }
    return ret.toString();
  }

  /**
   * Print debugging information on the standard output if option is active.
   *
   * @param value Value to be printed.
   */
  public static void debug(String value) {
    if(XmOption.isDebugOutput()) {
      System.out.println(value);
    }
  }

  /**
   * Convert an object back to a List<String>
   *
   * @param rawObject Raw object to be converted.
   * @return A list of string values. If the raw object is null or not an
   * original list of string, the returned list will be empty.
   */
  public static List<String> convertToList(Object rawObject) {
    List<String> strList = new ArrayList<>();
    if(rawObject != null && rawObject instanceof ArrayList) {
      List rawList = (ArrayList) rawObject;
      for(Object object : rawList) {
        strList.add(Objects.toString(object, null));
      }
    }
    return strList;
  }

  /**
   * Check if there is an intersection between the set and the list.
   *
   * @param set  A given set.
   * @param list A given list.
   * @param <T>  Type of the objects in set or list.
   * @return True if there is an intersection. False otherwise.
   */
  public static <T> boolean hasIntersection(Set<T> set, List<T> list) {
    for(T n : set) {
      if(list.contains(n)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Print a string with a specified indentation at the beginning.
   * @param indent Number of spaces to indent.
   * @param value  Text value to be printed.
   */
  public static void printWithIdent(int indent, String value){
    for(int i = 0; i < indent; ++i) {
      System.out.print(" ");
    }
    System.out.println(value);
  }

  /**
   * Get a formatted suffix for the CLAW module file including the directive
   * and target of the current transformation.
   *
   * .<directive>.<target>.claw
   *
   * @param target    The current transformation target.
   * @param directive The current accelerator directive used in transformation.
   * @return A formatted string for the CLAW module file name.
   */
  public static String formattedModuleFilePrefix(Target target,
                                                 AcceleratorDirective directive)
  {
    return "." + directive.toString() +
        "." + target.toString() +
        ClawConstant.CLAW_MOD_SUFFIX;
  }


}
