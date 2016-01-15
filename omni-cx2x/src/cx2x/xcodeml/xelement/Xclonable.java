package cx2x.xcodeml.xelement;

/**
 * Clonable interface defines methods that class that can be clone must
 * implement.
 *
 * @author Valentin Clement
 *
 * @param <T> Derived class of XbaseElement
 */
public interface Xclonable<T extends XbaseElement> {
  T cloneObject();
}
