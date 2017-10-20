/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

/**
 * The Xintent represents the possible value for the intent attribute in XcodeML
 * intermediate representation.
 * <p>
 * Possible value are: in, out, inout
 *
 * @author clementval
 */
public enum Xintent {
  NONE,
  IN,
  OUT,
  INOUT,
  ANY; // Represents any intent as well as no intent.

  /**
   * Convert string value to enum.
   *
   * @param value String value.
   * @return Corresponding enum value.
   */
  public static Xintent fromString(String value) {
    if(value == null) {
      return NONE;
    }
    switch(value.toLowerCase()) {
      case Xname.INTENT_IN:
        return IN;
      case Xname.INTENT_OUT:
        return OUT;
      case Xname.INTENT_INOUT:
        return INOUT;
    }
    return NONE;
  }

  /**
   * Convert current enum to String value.
   *
   * @return Corresponding String value.
   */
  public String toString() {
    switch(this) {
      case IN:
        return Xname.INTENT_IN;
      case OUT:
        return Xname.INTENT_OUT;
      case INOUT:
        return Xname.INTENT_INOUT;
      default:
        return "";
    }
  }

  /**
   * Check if it is an in intent.
   *
   * @return True if IN or INOUT. False otherwise.
   */
  public boolean isIntentIn() {
    return this == IN || this == INOUT;
  }

  /**
   * Check if it is an out intent.
   *
   * @return True if OUT or INOUT. False otherwise.
   */
  public boolean isIntentOut() {
    return this == OUT || this == INOUT;
  }

  /**
   * Check whether an intent is specified
   *
   * @return True if IN, OUT or INOUT. False otherwise.
   */
  public boolean isIntent() {
    return isIntentIn() || isIntentOut();
  }


  /**
   * Check if intent is compatible with the given one.
   *
   * @param intent Intent to check with.
   * @return True if intents are compatible.
   */
  public boolean isCompatible(Xintent intent) {
    return intent != null && (this == ANY || (intent == ANY)
        || (isIntentIn() && intent.isIntentIn())
        || (isIntentOut() && intent.isIntentOut())
        || (this == NONE && intent == NONE));

  }
}
