/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

import java.util.*;

/**
 * This class contains utilities methods that are useful in the whole project.
 * Mostly to implements java 1.8 features that are not present in java 1.7.
 *
 * @author clementval
 */
public final class Utility {

  // Avoid potential instantiation of this class
  private Utility() {
  }

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
   * Convert an object back to a list of string
   *
   * @param rawObject Raw object to be converted.
   * @return A list of string values. If the raw object is null or not an
   * original list of string, the returned list will be empty.
   */
  public static List<String> convertToList(Object rawObject) {
    List<String> strList = new ArrayList<>();
    if(rawObject instanceof ArrayList) {
      List rawList = (ArrayList) rawObject;
      for(Object object : rawList) {
        strList.add((String) object);
      }
    }
    return strList;
  }

  /**
   * Convert an object back to a map of String, String.
   *
   * @param rawObject Raw object to be converted.
   * @return A map of string keys and values. If the raw object is null or
   * not an original map of string, the returned map will be empty.
   */
  public static Map<String, String> convertToMap(Object rawObject) {
    Map<String, String> map = new HashMap<>();
    if(rawObject instanceof HashMap) {
      Map rawMap = (HashMap) rawObject;
      for(Object object : rawMap.keySet()) {
        String key = (String) object;
        map.put(key, (String) rawMap.get(key));
      }
    }
    return map;
  }

  /**
   * Check if there is an intersection between the set and the list.
   *
   * @param set  A given set.
   * @param list A given list.
   * @param <T>  Type of the objects in set or list.
   * @return True if there is an intersection. False otherwise.
   */
  public static <T> boolean hasIntersection(Set<T> set, Set<T> list) {
    for(T n : set) {
      if(list.contains(n)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Print a string with a specified indentation at the beginning.
   *
   * @param indent Number of spaces to indent.
   * @param value  Text value to be printed.
   */
  public static void printWithIndent(int indent, String value) {
    StringBuilder str = new StringBuilder();
    for(int i = 0; i < indent; ++i) {
      str.append(" ");
    }
    str.append(value);
    System.out.println(str.toString());
  }

  /**
   * Count the number of occurrences of pattern in source string.
   * Case insensitive.
   *
   * @param source  String in which the pattern is search for.
   * @param pattern Pattern to be found.
   * @return Number of occurrences.
   */
  public static int countOccurrences(String source, String pattern) {
    if(source == null || source.isEmpty()
        || pattern == null || pattern.isEmpty())
    {
      return 0;
    }
    String newStr = source.toLowerCase().replace(pattern.toLowerCase(), "");
    return (source.length() - newStr.length()) / pattern.length();
  }
}
