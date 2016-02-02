package cx2x.xcodeml.xelement;

/**
 * Creatable interface defines methods that class that can be created must
 * implement.
 * @param <T> Derived class of XbaseElement
 *
 * @author clementval
 */
public interface Xcreatable<T extends XbaseElement> {

  /**
   * Construct an empty element of the defined type.
   * @return A new object of type T that is empty.
   */
  T createEmpty(XcodeProgram program);
}
