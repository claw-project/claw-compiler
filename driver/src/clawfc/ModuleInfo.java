/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.util.Collection;

public interface ModuleInfo
{
    public String getName();

    public boolean isProgram();

    public boolean isModule();

    public boolean isInput();

    public boolean usesCLAW();

    public boolean hasSource();

    public Collection<String> getUsedModules();

    public clawfc.depscan.FortranModuleInfo getModuleSrcInfo();

    public FileInfo getSrcFileInfo();

    public FileInfo getModFileInfo();
}
