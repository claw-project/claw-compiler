/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.translator;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
import java.util.Map;

import claw.shenron.transformation.TransformationGroup;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Message;
import claw.tatsu.common.Target;
import claw.tatsu.primitive.Pragma;
import claw.tatsu.xcodeml.exception.IllegalDirectiveException;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.ClawConstant;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.configuration.GroupConfiguration;

/**
 * ClawTranslatorDriver is the class driving the translation. It creates the
 * translator and pass to it all the directives it can manage. It is also in
 * charge of decompiling the XcodeML/F after the translation took place.
 *
 * @author clementval
 */
public class ClawTranslatorDriver
{

    private boolean _canTransform = false;
    private ClawTranslator _translator;
    private XcodeProgram _translationUnit = null;
    private final Configuration _cfg;

    public Configuration cfg()
    {
        return _cfg;
    }

    public Context context()
    {
        return _cfg.context();
    }

    /**
     * ClawTranslatorDriver ctor.
     *
     * @throws Exception If translator cannot be created.
     */
    public ClawTranslatorDriver(Configuration cfg) throws Exception
    {
        _cfg = cfg;
        // Create translator
        String translatorClassPath = cfg.getParameter(Configuration.TRANSLATOR);
        if (translatorClassPath == null || translatorClassPath.equals(""))
        {
            throw new Exception("Translator not set in configuration");
        }

        try
        {
            // Check if class is there
            Class<?> translatorClass = Class.forName(translatorClassPath);
            Constructor<?> ctor = translatorClass.getConstructor(Configuration.class);
            _translator = (ClawTranslator) ctor.newInstance(cfg);
        } catch (ClassNotFoundException e)
        {
            throw new Exception("Cannot create translator", e);
        }
    }

    /**
     * Analysis the XcodeML/F directives and categorized them in corresponding
     * transformation with the help of the translator.
     * 
     * @throws Exception
     */
    public void analyze(InputStream inStrm) throws Exception
    {
        _translationUnit = XcodeProgram.createFromStream(inStrm, context());

        if (_translationUnit.hasErrors())
        {
            flushErrors();
            throw new Exception("Translation unit has errors");
        }

        if (cfg().getCurrentDirective() == CompilerDirective.OPENMP && cfg().getCurrentTarget() == Target.CPU)
        {
            _translationUnit.addWarning("Fine grain OpenMP directive generation " + "is not advised for CPU target.",
                    0);
        }

        try
        {
            // Check all pragma found in the translation unit
            for (Xnode pragma : _translationUnit.matchAll(Xcode.F_PRAGMA_STATEMENT))
            {

                /*
                 * Since OMNI Compiler 1.2.2, any pragma that are just between the declaration
                 * and the execution part will be placed in the declaration part. This is not
                 * what is best for all the current CLAW directives. Therefore, we move them
                 * back to the execution block.
                 */
                // TODO MODEL_CONFIG: Pragma.moveInExecution(pragma);

                // Pragma can be handled by the translator so let it do its job.
                if (_translator.isHandledPragma(pragma))
                {
                    _translator.generateTransformation(_translationUnit, pragma);
                } else
                {
                    // Check if the pragma is a compile guard
                    if (context().getGenerator().isCompileGuard(pragma.value()))
                    {
                        pragma.delete();
                    } else
                    {
                        // Handle special transformation of OpenACC line continuation
                        for (GroupConfiguration gc : cfg().getGroups())
                        {
                            if (gc.getTriggerType() == GroupConfiguration.TriggerType.DIRECTIVE
                                    && Pragma.getPrefix(pragma).equals(gc.getDirective()))
                            {
                                generateTransformation(gc, new ClawPragma(pragma));
                            }
                        }
                    }
                }
            }

            _translator.finalizeTranslation(_translationUnit);

        } catch (IllegalDirectiveException e)
        {
            _translationUnit.addError(e.getMessage(), e.getDirectiveLine());
            flushErrors();
            throw e;
        } catch (IllegalTransformationException e)
        {
            _translationUnit.addError(e.getMessage(), e.getStartLine());
            flushErrors();
            throw e;
        }

        // Generate transformation for translation_unit trigger type
        for (GroupConfiguration gc : cfg().getGroups())
        {
            if (gc.getTriggerType() == GroupConfiguration.TriggerType.TRANSLATION_UNIT)
            {
                generateTransformation(gc, null);
            }
        }

        // Analysis done, the transformation can be performed.
        _canTransform = true;
    }

    /**
     * Instantiate correct transformation class from group configuration.
     *
     * @param gc     Group configuration for the
     * @param pragma Pragma associated with the transformation.
     * @throws Exception
     */
    private void generateTransformation(GroupConfiguration gc, ClawPragma pragma) throws Exception
    {
        try
        {
            Class<?> groupClass = gc.getTransformationClass();
            ClawTransformation transformation;
            if (pragma != null)
            {
                Constructor<?> ctor = groupClass.getConstructor(ClawPragma.class);
                transformation = (ClawTransformation) ctor.newInstance(pragma);
            } else
            {
                Constructor<?> ctor = groupClass.getConstructor();
                transformation = (ClawTransformation) ctor.newInstance();
            }
            _translator.addTransformation(_translationUnit, transformation);
        } catch (Exception ex)
        {
            PrintStream err = context().getErrorStream();
            err.println("Cannot generate transformation " + gc.getName());
            err.println(ex.getMessage());
            flushErrors();
            throw ex;
        }
    }

    /**
     * Apply all the transformation in the pipeline.
     * 
     * @throws Exception
     */
    public void transform(OutputStream outputStream) throws Exception
    {
        try
        {
            if (outputStream != null && !_canTransform)
            {
                _translationUnit.write(outputStream, ClawConstant.INDENT_OUTPUT);
                return;
            }

            for (Map.Entry<Class<?>, TransformationGroup> entry : _translator.getGroups().entrySet())
            {
                Message.debug(context(), "Apply transformation: " + entry.getValue().transformationName() + " - "
                        + entry.getValue().count());

                try
                {
                    entry.getValue().applyTransformations(_translationUnit, _translator);
                    Message.warnings(context(), _translationUnit);
                } catch (IllegalTransformationException itex)
                {
                    _translationUnit.addError(itex.getMessage(), itex.getStartLine());
                    flushErrors();
                    throw itex;
                } catch (Exception ex)
                {
                    _translationUnit.addError("Unexpected error: " + ex.getMessage(), 0);
                    if (context().getXmOption().isDebugOutput())
                    {
                        StringWriter errors = new StringWriter();
                        ex.printStackTrace(new PrintWriter(errors));
                        _translationUnit.addError(errors.toString(), 0);
                    }
                    flushErrors();
                    throw ex;
                }
            }

            if (outputStream != null)
            {
                // Write transformed IR to file
                _translationUnit.write(outputStream, ClawConstant.INDENT_OUTPUT);
            }
        } catch (Exception ex)
        {
            context().getErrorStream().println("Transformation exception: " + ex.getMessage());
            throw ex;
        }
    }

    /**
     * Print all the errors stored in the XcodeML object and abort the program.
     */
    private void flushErrors()
    {
        Message.errors(context(), _translationUnit);
    }

    /**
     * Flush all information stored in the translator.
     *
     * @throws IllegalTransformationException If module cache cannot be written.
     * @throws IOException
     */
    public void flush() throws IllegalTransformationException, IOException
    {
        context().getModuleCache().write(context(), ClawConstant.INDENT_OUTPUT);
    }

    /**
     * Get the current translator associated with this translation.
     *
     * @return Get the current translator.
     */
    public ClawTranslator getTranslator()
    {
        return _translator;
    }

    /**
     * Get the XcodeProgram object representing the Fortran code translated.
     *
     * @return Current XcodeProgram object.
     */
    public XcodeProgram getTranslationUnit()
    {
        return _translationUnit;
    }
}
