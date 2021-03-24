/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

import java.io.PrintStream;

import claw.tatsu.directive.configuration.AcceleratorConfiguration;
import claw.tatsu.directive.configuration.OpenAccConfiguration;
import claw.tatsu.directive.configuration.OpenMpConfiguration;
import claw.tatsu.directive.generator.DirectiveGenerator;
import claw.tatsu.directive.generator.DirectiveNone;
import claw.tatsu.directive.generator.OpenAcc;
import claw.tatsu.directive.generator.OpenMp;
import claw.tatsu.xcodeml.module.ModuleCache;
import xcodeml.util.IXmOption;
import xcodeml.util.XmOptionLocal;

/**
 * Class holding all information needed during a translation.
 *
 * @author clementval
 */
public class Context
{

    private int _maxColumns;
    private DirectiveGenerator _directiveGenerator;
    private AcceleratorConfiguration _acceleratorConfiguration;
    private CompilerDirective _compilerDirective;
    private Target _target;
    private ModuleCache _moduleCache;
    private PrintStream _errorStream;
    private final IXmOption _xmOption;

    protected Context()
    {
        _errorStream = System.err;
        _xmOption = new XmOptionLocal();
    }

    public Context(PrintStream errorStream, IXmOption xmOption)
    {
        _errorStream = errorStream;
        _xmOption = xmOption;
    }

    public void setErrorStream(PrintStream errorStream)
    {
        _errorStream = errorStream;
    }

    /**
     *
     * @param compilerDirective        Compiler directive to used.
     * @param target                   Target for the current translation.
     * @param acceleratorConfiguration Configuration for the accelerator
     *                                 translation.
     * @param maxColumns               Max columns.
     */
    public Context(CompilerDirective compilerDirective, Target target,
            AcceleratorConfiguration acceleratorConfiguration, int maxColumns, PrintStream errorStream,
            IXmOption xmOption)
    {
        _errorStream = errorStream;
        _xmOption = xmOption;
        init(compilerDirective, target, acceleratorConfiguration, maxColumns);
    }

    public void init(CompilerDirective compilerDirective, Target target,
            AcceleratorConfiguration acceleratorConfiguration, int maxColumns)
    {
        if (compilerDirective != null)
        {
            _compilerDirective = compilerDirective;
            if (compilerDirective == CompilerDirective.OPENACC)
            {
                OpenAcc gen = new OpenAcc(this);
                _directiveGenerator = gen;
                if (acceleratorConfiguration != null)
                {
                    gen.setExecutionMode(((OpenAccConfiguration) acceleratorConfiguration).getMode());
                }
            } else if (compilerDirective == CompilerDirective.OPENMP)
            {
                OpenMp gen = new OpenMp(this);
                _directiveGenerator = gen;
                if (acceleratorConfiguration != null)
                {
                    gen.setExecutionMode(((OpenMpConfiguration) acceleratorConfiguration).getMode());
                }
            } else
            {
                _directiveGenerator = new DirectiveNone();
            }
        } else
        {
            _compilerDirective = CompilerDirective.NONE;
            _directiveGenerator = new DirectiveNone();
        }
        _acceleratorConfiguration = acceleratorConfiguration;

        if (target == null)
        {
            _target = Target.NONE;
        } else
        {
            _target = target;
        }

        _maxColumns = maxColumns;
        _moduleCache = new ModuleCache();
    }

    public PrintStream getErrorStream()
    {
        return _errorStream;
    }

    public IXmOption getXmOption()
    {
        return _xmOption;
    }

    public int getMaxColumns()
    {
        return _maxColumns;
    }

    public Target getTarget()
    {
        return _target;
    }

    public CompilerDirective getCompilerDirective()
    {
        return _compilerDirective;
    }

    public DirectiveGenerator getGenerator()
    {
        return _directiveGenerator;
    }

    public ModuleCache getModuleCache()
    {
        return _moduleCache;
    }

    public AcceleratorConfiguration getAcceleratorConfig()
    {
        return _acceleratorConfiguration;
    }

    /**
     * Check is current target is corresponding to the given one.
     *
     * @param value Target value to check against.
     * @return True if the target is identical to the given one. False otherwise.
     */
    public boolean isTarget(Target value)
    {
        return getTarget() == value;
    }
}
