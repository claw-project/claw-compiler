/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.module;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import claw.tatsu.common.Context;
import claw.tatsu.primitive.Xmod;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.fortran.FortranModule;

/**
 * Translator interface
 *
 * Translator stores all the transformation to be applied by a translator.
 *
 * @author clementval
 */
public class ModuleCache
{

    private final Map<String, FortranModule> _moduleCache;
    private final List<String> _searchPaths;

    /**
     * Constructs a new empty module cache.
     */
    public ModuleCache()
    {
        _moduleCache = new HashMap<>();
        _searchPaths = new ArrayList<>();
    }

    /**
     * Check whether a module is in the cache.
     *
     * @param moduleName Name of the module.
     * @return True if the module is in the cache. False otherwise.
     */
    public boolean isModuleLoaded(String moduleName)
    {
        return _moduleCache.containsKey(moduleName.toLowerCase());
    }

    /**
     * Add a module in the cache by its name. If already present, the module is
     * overwritten.
     *
     * @param moduleName Name of the module.
     * @param module     Xmod object.
     */
    public void add(String moduleName, FortranModule module)
    {
        _moduleCache.remove(moduleName.toLowerCase());
        _moduleCache.put(moduleName.toLowerCase(), module);
    }

    /**
     * Get a module in the cache by its name.
     *
     * @param moduleName Name of the module.
     * @return The cached module.
     */
    public FortranModule get(String moduleName)
    {
        return _moduleCache.get(moduleName.toLowerCase());
    }

    /**
     * Write all modules in the cache to files.
     *
     * @param ident Number of spaces used to indent the XML file.
     * @throws IllegalTransformationException If module cannot be written.
     * @throws IOException
     */
    public void write(Context context, int ident) throws IllegalTransformationException, IOException
    {
        String suffix = Xmod.getSuffix(context);
        for (Map.Entry<String, FortranModule> pair : _moduleCache.entrySet())
        {
            FortranModule module = pair.getValue();
            String newModuleName = module.getPath() + module.getName() + suffix;
            try (OutputStream out = Files.newOutputStream(Paths.get(newModuleName)))
            {
                module.write(out, ident);
            }
        }
    }

    /**
     * Add a path to the current list of possible search paths.
     *
     * @param path Path.
     */
    public void addSearchPath(String path)
    {
        _searchPaths.add(path);
    }

    /**
     * Get the list of possible search paths.
     *
     * @return Current list of search paths.
     */
    public List<String> getSearchPaths()
    {
        return _searchPaths;
    }
}
