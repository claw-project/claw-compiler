/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import java.util.Map;
import java.util.Set;

public interface BuildOrder
{
    public Map<String, ProgramUnitInfo> getUsedModules();

    public Set<String> getTargetModules();

    public Set<String> getProcessedModules();

    public boolean done();

    public String next();

    public void onProcessed(String modName);
}
