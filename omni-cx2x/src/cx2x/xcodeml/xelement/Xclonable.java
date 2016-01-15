/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

/**
 * Clonable interface defines methods that class that can be clone must
 * implement.
 * @param <T> Derived class of XbaseElement
 *
 * @author clementval
 */
 
public interface Xclonable<T extends XbaseElement> {
  T cloneObject();
}
