package claw.python;

/* These modules are provided by Jython */
import org.python.core.PyObject;

public class PythonFactory {

    private PyObject transformClass;

    /**
     * Create a new PythonInterpreter object, then use it to
     * execute some python code. In this case, we want to
     * import the python module that we will coerce.
     *
     * Once the module is imported than we obtain a reference to
     * it and assign the reference to a Java variable
     */

    public PythonFactory()
    {
    }

}
