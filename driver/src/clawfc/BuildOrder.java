/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.util.Map;
import java.util.Set;

public interface BuildOrder
{
    public Map<String, ModuleInfo> getUsedModules();

    public Set<String> getTargetModules();

    public Set<String> getProcessedModules();

    public boolean done();

    public String next();

    public void onProcessed(String modName);
}
