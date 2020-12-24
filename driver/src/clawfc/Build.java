/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import clawfc.depscan.FortranSemanticException;

public class Build
{
    public static String moduleNameWithLocation(String name, Map<String, ModuleInfo> availModules)
    {
        ModuleInfo info = availModules.get(name);
        return moduleNameWithLocation(info);
    }

    public static String moduleNameWithLocation(ModuleInfo info)
    {
        if (info.hasSource())
        {
            Path srcFilePath = info.getSrcPath();
            long lineNum = info.getModuleSrcInfo().getStartLineIdx() + 1;
            return String.format("%s (%s:%s)", info.getName(), srcFilePath, lineNum);
        } else if (info.getXMod() != null && info.getXMod().getFilePath() != null)
        {
            Path xmodFilePath = info.getXMod().getFilePath();
            return String.format("%s (%s)", info.getName(), xmodFilePath);
        } else
        {
            return String.format("%s (no source or xmod file)", info.getName());
        }
    }

    static class SanityChecker
    {
        final Map<String, ModuleInfo> availModules;
        final Set<String> targetModuleNames;
        final Set<String> visited;
        final Set<String> stackSet;
        final List<String> stack;

        public SanityChecker(Map<String, ModuleInfo> availModules, Set<String> targetModuleNames)
        {
            this.availModules = availModules;
            this.targetModuleNames = targetModuleNames;
            visited = new HashSet<String>();
            stackSet = new HashSet<String>();
            stack = new ArrayList<String>();
        }

        /**
         * @return List of visited modules
         */
        public Set<String> run() throws FortranSemanticException
        {
            for (String target : targetModuleNames)
            {
                startDFS(target);
            }
            return Collections.unmodifiableSet(visited);
        }

        void startDFS(String root) throws FortranSemanticException
        {
            stackSet.clear();
            stack.clear();
            dfs(root);
        }

        String moduleWithLocation(String name)
        {
            ModuleInfo info = availModules.get(name);
            Path srcFilePath = info.getSrcPath();
            if (info.getModuleSrcInfo() != null)
            {
                long lineNum = info.getModuleSrcInfo().getStartLineIdx() + 1;
                return String.format("%s (%s:%s)", name, srcFilePath, lineNum);
            } else
            {
                return String.format("%s (%s)", name, srcFilePath);
            }
        }

        void dfs(String modName) throws FortranSemanticException
        {
            ModuleInfo info = availModules.get(modName);
            if (info != null)
            {
                if (stackSet.add(modName))
                {
                    if (visited.add(modName))
                    {
                        stack.add(modName);
                        if (info.hasSource())
                        {
                            for (String depModName : info.getModuleSrcInfo().getUsedModuleNames())
                            {
                                dfs(depModName);
                            }
                        }
                        stack.remove(stack.size() - 1);
                    }
                    stackSet.remove(modName);
                } else
                {
                    String reqModName = stack.get(stack.size() - 1);
                    String errMsg = String.format("Circle dependency between modules %s and %s",
                            moduleWithLocation(reqModName), moduleWithLocation(modName));
                    stack.add(modName);
                    StringBuilder stackStr = new StringBuilder();
                    stackStr.append("\nInclude stack:\n");
                    int offset = 0;
                    for (String sModName : stack)
                    {
                        stackStr.append(String.join("", Collections.nCopies(offset, " ")));
                        if (modName.equals(sModName))
                        {
                            stackStr.append("[" + moduleWithLocation(sModName) + "]\n");
                        } else
                        {
                            stackStr.append(moduleWithLocation(sModName) + "\n");
                        }
                        offset += 1;
                    }
                    errMsg += stackStr.toString();
                    throw new FortranSemanticException(errMsg);
                }
            } else
            {
                String reqModName = stack.get(stack.size() - 1);
                String errMsg = String.format(
                        "Module \"%s\" used by module %s is not defined in any file under given search path", modName,
                        moduleWithLocation(reqModName));
                StringBuilder stackStr = new StringBuilder();
                stackStr.append("\nInclude stack:\n");
                int offset = 0;
                for (String sModName : stack)
                {
                    stackStr.append(String.join("", Collections.nCopies(offset, " ")));
                    stackStr.append(moduleWithLocation(sModName));
                    offset += 1;
                }
                errMsg += stackStr.toString();
                throw new FortranSemanticException(errMsg);
            }
        }
    }

    public static void sanityCheck(Map<String, ModuleInfo> availModules, Set<String> targetModuleNames)
            throws FortranSemanticException
    {
        SanityChecker checker = new SanityChecker(availModules, targetModuleNames);
        checker.run();
    }

    public static Map<String, ModuleInfo> removeUnreferencedModules(Map<String, ModuleInfo> availModules,
            Set<String> targetModuleNames) throws FortranSemanticException
    {
        SanityChecker checker = new SanityChecker(availModules, targetModuleNames);
        Set<String> visited = checker.run();
        Map<String, ModuleInfo> res = new LinkedHashMap<String, ModuleInfo>();
        for (String modName : visited)
        {
            res.put(modName, availModules.get(modName));
        }
        return Collections.unmodifiableMap(res);
    }

    static class ParallelOrder implements BuildOrder
    {
        final Map<String, ModuleInfo> usedModules;
        final Set<String> targetModuleNames;
        final Queue<String> waiting;
        final Set<String> currentSet;
        final Map<String, Set<String>> deps;
        final Map<String, Set<String>> revDeps;
        final Set<String> processed;

        @Override
        public Map<String, ModuleInfo> getUsedModules()
        {
            return usedModules;
        }

        @Override
        public Set<String> getTargetModules()
        {
            return targetModuleNames;
        }

        @Override
        public synchronized Set<String> getProcessedModules()
        {
            return Collections.unmodifiableSet(new HashSet<String>(processed));
        }

        @Override
        public synchronized boolean done()
        {
            return processed.size() == usedModules.size();
        }

        public ParallelOrder(Map<String, ModuleInfo> usedModules, Set<String> targetModuleNames)
        {
            this.usedModules = Collections.unmodifiableMap(usedModules);
            this.targetModuleNames = Collections.unmodifiableSet(targetModuleNames);
            deps = getDependencies(usedModules);
            revDeps = reverseDependencies(usedModules);
            waiting = new LinkedList<String>(getStartSet(usedModules));
            currentSet = new LinkedHashSet<String>();
            processed = new LinkedHashSet<String>();
        }

        @Override
        public synchronized String next()
        {
            String modName = waiting.poll();
            if (modName == null)
            {
                return null;
            } else
            {
                currentSet.add(modName);
                return modName;
            }
        }

        @Override
        public synchronized void onProcessed(String modName)
        {
            if (!currentSet.remove(modName))
            {
                throw new RuntimeException(String.format("Module \"%s\" is not in the current set", modName));
            }
            if (!processed.add(modName))
            {
                throw new RuntimeException(String.format("Module \"%s\" was already processed", modName));
            }
            Set<String> depModules = revDeps.get(modName);
            for (final String depModName : depModules)
            {
                Set<String> depModDeps = deps.get(depModName);
                if (!depModDeps.remove(modName))
                {
                    throw new RuntimeException(String.format(
                            "Module \"%s\" already removed from dependencies of Module \"%s\" ", modName, depModName));
                }
                if (depModDeps.isEmpty())
                {
                    waiting.add(depModName);
                }
            }
        }

        static Set<String> getStartSet(Map<String, ModuleInfo> usedModules)
        {
            Set<String> startSet = new LinkedHashSet<String>();
            for (String modName : usedModules.keySet())
            {
                startSet.add(modName);
            }
            for (Map.Entry<String, ModuleInfo> entry : usedModules.entrySet())
            {
                String modName = entry.getKey();
                ModuleInfo info = entry.getValue();
                if (info.hasSource())
                {
                    List<String> depModules = info.getModuleSrcInfo().getUsedModuleNames();
                    if (!depModules.isEmpty())
                    {
                        startSet.remove(modName);
                    }
                }
            }
            return startSet;
        }

        static Map<String, Set<String>> getDependencies(Map<String, ModuleInfo> usedModules)
        {
            Map<String, Set<String>> deps = new HashMap<String, Set<String>>();
            for (Map.Entry<String, ModuleInfo> entry : usedModules.entrySet())
            {
                String modName = entry.getKey();
                ModuleInfo info = entry.getValue();
                Set<String> modDeps = new LinkedHashSet<String>();
                if (info.hasSource())
                {
                    List<String> depModules = info.getModuleSrcInfo().getUsedModuleNames();
                    modDeps.addAll(depModules);
                }
                deps.put(modName, modDeps);
            }
            return deps;
        }

        static Map<String, Set<String>> reverseDependencies(Map<String, ModuleInfo> usedModules)
        {
            Map<String, Set<String>> revDeps = new HashMap<String, Set<String>>();
            for (String modName : usedModules.keySet())
            {
                revDeps.put(modName, new LinkedHashSet<String>());
            }
            for (Map.Entry<String, ModuleInfo> entry : usedModules.entrySet())
            {
                String modName = entry.getKey();
                ModuleInfo info = entry.getValue();
                if (info.hasSource())
                {
                    List<String> depModules = info.getModuleSrcInfo().getUsedModuleNames();
                    for (String depModName : depModules)
                    {
                        revDeps.get(depModName).add(modName);
                    }
                }
            }
            return revDeps;
        }
    }

    public static BuildOrder getParallelOrder(Map<String, ModuleInfo> usedModules, Set<String> targetModuleNames)
    {
        return new ParallelOrder(usedModules, targetModuleNames);
    }
};