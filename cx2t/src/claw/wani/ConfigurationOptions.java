/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.wani;

import static java.lang.String.format;

import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

public class ConfigurationOptions
{
    final String _targetPlatform;
    final Path _configFile;
    final Path _configDir;
    final String _accDirLanguage;
    final Path _modelConfigFile;
    final List<String> _cfgKeyOverrides;
    final boolean _showDebugOutput;
    final Integer _maxFLineLength;
    final boolean _suppressPPLineDirectives;
    final boolean _exitOnPureFunction;
    final boolean _addParenToBinOpts;
    final List<Path> _transSetPaths;

    public ConfigurationOptions(String targetPlatform, Path configFile, Path configDir, String accDirLanguage,
            Path modelConfigFile, List<String> cfgKeyOverrides, boolean showDebugOutput, Integer maxFLineLength,
            boolean suppressPPLineDirectives, boolean exitOnPureFunction, boolean addParenToBinOpts,
            List<Path> transSetPaths)
    {
        this._targetPlatform = targetPlatform;
        this._configFile = configFile;
        this._configDir = configDir;
        this._accDirLanguage = accDirLanguage;
        this._modelConfigFile = modelConfigFile;
        this._cfgKeyOverrides = cfgKeyOverrides;
        this._showDebugOutput = showDebugOutput;
        this._maxFLineLength = maxFLineLength;
        this._suppressPPLineDirectives = suppressPPLineDirectives;
        this._exitOnPureFunction = exitOnPureFunction;
        this._addParenToBinOpts = addParenToBinOpts;
        this._transSetPaths = transSetPaths;
    }

    public List<Path> transSetPaths()
    {
        return _transSetPaths;
    }

    public String targetPlatform()
    {
        return _targetPlatform;
    }

    public Path configFilePath()
    {
        return _configFile;
    }

    public Path configDirPath()
    {
        return _configDir;
    }

    public String accDirectiveLanguage()
    {
        return _accDirLanguage;
    }

    public Path modelConfigFilePath()
    {
        return this._modelConfigFile;
    }

    public List<String> cfgKeysOverrides()
    {
        return _cfgKeyOverrides;
    }

    public boolean showDebugOutput()
    {
        return _showDebugOutput;
    }

    public Integer maxFortranLineLength()
    {
        return _maxFLineLength;
    }

    public boolean suppressPreprocLineDirectives()
    {
        return _suppressPPLineDirectives;
    }

    public boolean exitOnPureFunction()
    {
        return _exitOnPureFunction;
    }

    public boolean addParenToBinaryOpts()
    {
        return _addParenToBinOpts;
    }

    protected String toString(List<Path> paths)
    {
        final String res = String.join("\n\t",
                paths.stream().map((path) -> path.toString()).collect(Collectors.toList()));
        return res != "" ? "\t" + res + "\n" : res;
    }

    @Override
    public String toString()
    {
        StringBuilder res = new StringBuilder();
        res.append(format("Suppress preprocessor line directives in decompiled source: %s\n",
                suppressPreprocLineDirectives()));
        res.append(format("Configuration directory: %s\n", configDirPath()));
        res.append(format("Translator configuration file: %s\n", configFilePath()));
        res.append(format("Target platform: %s\n", targetPlatform()));
        res.append(format("Accelerator directive language: %s\n", accDirectiveLanguage()));
        res.append(format("Print debug output: %s\n", showDebugOutput()));
        res.append(format("Max Fortran line length: %s\n", maxFortranLineLength()));
        res.append(format("Exit on pure function: %s\n", exitOnPureFunction()));
        res.append(format("Model Config file: %s\n", modelConfigFilePath()));
        res.append(format("Config overrides: \n\t%s\n", String.join("\n\t", cfgKeysOverrides())));
        res.append(format("Add parenthesis to binary opts: %s\n", addParenToBinaryOpts()));
        return res.toString();
    }
}
