/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw;

import static claw.Options.parseCmdlineArguments;
import static claw.wani.x2t.configuration.Configuration.CONFIG_XSD;
import static claw.wani.x2t.configuration.Configuration.DEFAULT_CONFIG_FILE;
import static claw.wani.x2t.configuration.Configuration.SET_XSD;
import static java.lang.String.format;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.xcodeml.backend.OmniBackendDriver;
import claw.wani.report.ClawTransformationReport;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.translator.ClawTranslatorDriver;
import xcodeml.util.IXmOption;
import xcodeml.util.XmOptionLocal;

/**
 * ClawX2T is the entry point of any CLAW XcodeML/F translation.
 *
 * @author clementval
 */
public class ClawX2T
{
    public static void printTargets()
    {
        final String s = String.join("\n", Target.availableTargets());
        System.out.println(s);
    }

    public static void printDirectiveLanguages()
    {
        final String s = String.join("\n", CompilerDirective.availableDirectiveLanguage());
        System.out.println(s);
    }

    public static void printVersion()
    {
        System.out.println(claw.ClawVersion.VERSION);
    }

    public static void verifyTargetOption(final String target) throws Exception
    {
        if (target != null && !Target.availableTargets().contains(target))
        {
            throw new Exception(format("Input target \"%s\" is not supported", target));
        }
    }

    public static void verifyDirectiveOption(final String dirLang) throws Exception
    {
        if (dirLang != null && !CompilerDirective.availableDirectiveLanguage().contains(dirLang))
        {
            throw new Exception(format("Input accelerator directive language \"%s\" is not supported", dirLang));
        }
    }

    public static boolean dirExists(Path path)
    {
        return Files.exists(path) && Files.isDirectory(path);
    }

    public static boolean fileExists(Path path)
    {
        return Files.exists(path) && !Files.isDirectory(path);
    }

    public static void verifyConfigDir(final Path configDirPath) throws Exception
    {
        if (configDirPath != null)
        {
            if (!dirExists(configDirPath))
            {
                throw new Exception(
                        format("Configuration directory \"%s\" does not exist or is not a directory", configDirPath));
            }
            if (!fileExists(configDirPath.resolve(DEFAULT_CONFIG_FILE)))
            {
                throw new Exception(format("Configuration directory \"%s\" does not contain default config file \"%s\"",
                        configDirPath, DEFAULT_CONFIG_FILE));
            }
            if (!fileExists(configDirPath.resolve(CONFIG_XSD)))
            {
                throw new Exception(format("Configuration directory \"%s\" does not contain config schema file \"%s\"",
                        configDirPath, CONFIG_XSD));
            }
            if (!fileExists(configDirPath.resolve(SET_XSD)))
            {
                throw new Exception(
                        format("Configuration directory \"%s\" does not contain transformation set schema file \"%s\"",
                                configDirPath, SET_XSD));
            }
        }
    }

    public static void verifyConfigFile(final Path configFilePath) throws Exception
    {
        if (configFilePath != null && !fileExists(configFilePath))
        {
            throw new Exception(format("Configuration file \"%s\" not found", configFilePath));
        }
    }

    public static void verifyModelConfigFile(final Path modelConfigFilePath) throws Exception
    {
        if (modelConfigFilePath != null && !fileExists(modelConfigFilePath))
        {
            throw new Exception(format("Model configuration file \"%s\" not found", modelConfigFilePath));
        }
    }

    public static void verifyInputFile(final Path inputFilePath) throws Exception
    {
        if (inputFilePath != null && !fileExists(inputFilePath))
        {
            throw new Exception(format("Input file \"%s\" not found", inputFilePath));
        }
    }

    public static void verifyOutputXcodeMLFile(final Path outputFilePath) throws Exception
    {
        if (outputFilePath != null)
        {
            verifyOutputFile(outputFilePath);
        }
    }

    public static void verifyOutputFortranFile(final Path outputFilePath) throws Exception
    {
        if (outputFilePath != null)
        {
            verifyOutputFile(outputFilePath);
        }
    }

    public static void verifyOutputFile(final Path outputFilePath) throws Exception
    {
        if (fileExists(outputFilePath))
        {
            try
            {
                Files.delete(outputFilePath);
            } catch (IOException e)
            {
                throw new Exception(format("Output file \"%s\" is not writable", outputFilePath), e);
            }
        }
        final Path outDirPath = outputFilePath.getParent();
        try
        {
            Files.createDirectories(outDirPath);
        } catch (IOException e)
        {
            throw new Exception(format("Could not create output directory \"%s\"", outDirPath), e);
        }
        Path tmpFilePath = null;
        try
        {
            tmpFilePath = Files.createTempFile(outDirPath, null, null);
        } catch (IOException e)
        {
            throw new Exception(format("Output directory \"%s\" is not writable", outDirPath), e);
        } finally
        {
            Files.deleteIfExists(tmpFilePath);
        }
    }

    static void verifyOptions(Options opts) throws Exception
    {
        verifyTargetOption(opts.targetPlatform());
        verifyDirectiveOption(opts.accDirectiveLanguage());
        verifyConfigDir(opts.configDirPath());
        verifyConfigFile(opts.configFilePath());
        verifyModelConfigFile(opts.modelConfigFilePath());
        if (opts.printCfg())
        {
            return;
        }
        verifyInputFile(opts.inputFilePath());
        verifyOutputXcodeMLFile(opts.outputTranslatedXastFilePath());
        verifyOutputFortranFile(opts.outputFortranFilePath());
    }

    static InputStream getInputAsStream(Path inputFilePath) throws IOException
    {
        if (inputFilePath != null)
        {
            return Files.newInputStream(inputFilePath);
        } else
        {
            return System.in;
        }
    }

    static OutputStream getXCodeMLOutputAsStream(Path outputFilePath) throws IOException
    {
        if (outputFilePath != null)
        {
            return Files.newOutputStream(outputFilePath);
        } else
        {
            return System.out;
        }
    }

    /**
     * Main point of entry of the program.
     *
     * @param args Arguments of the program.
     * @throws Exception if translation failed.
     */
    public static void main(String[] args) throws Exception
    {
        final Options opts = parseCmdlineArguments(args);
        if (opts == null)
        {// Helper screen
            return;
        } else if (opts.printOptions())
        {
            System.out.println(opts.toString());
            return;
        } else if (opts.printVersion())
        {
            printVersion();
            return;
        } else if (opts.printTargets())
        {
            printTargets();
            return;
        } else if (opts.printDirectives())
        {
            printDirectiveLanguages();
            return;
        }
        verifyOptions(opts);

        // Set decompiler options
        IXmOption xmOption = new XmOptionLocal();
        if (opts.suppressPreprocLineDirectives())
        {
            xmOption.setIsSuppressLineDirective(true);
        }
        if (opts.showDebugOutput())
        {
            xmOption.setDebugOutput(true);
        }
        if (opts.addParenToBinaryOpts())
        {
            xmOption.setAddPar(true);
        }
        // Prepare configuration
        final Context transContext = new Context(System.err, xmOption);
        final Configuration cfg = Configuration.load(opts.configDirPath(), opts.configFilePath(),
                opts.modelConfigFilePath(), opts.targetPlatform(), opts.accDirectiveLanguage(),
                opts.maxFortranLineLength(), transContext);
        for (String keyValue : opts.cfgKeysOverrides())
        {
            String key = keyValue.substring(0, keyValue.indexOf(":"));
            String value = keyValue.substring(keyValue.indexOf(":") + 1);
            cfg.overrideConfigurationParameter(key, value);
        }
        if (opts.exitOnPureFunction())
        {
            cfg.setForcePure();
        }
        if (opts.printCfg())
        {
            System.out.println(cfg);
            return;
        }
        if (cfg.getCurrentTarget() == Target.FPGA)
        {
            throw new Exception("FPGA target is not supported");
        }
        // Setup translation context
        for (Path xmodSrchPath : opts.moduleIncludeDirs())
        {
            transContext.getModuleCache().addSearchPath(xmodSrchPath.toString());
        }
        // Perform transformations

        ClawTranslatorDriver translatorDriver = new ClawTranslatorDriver(cfg);

        try (InputStream in = getInputAsStream(opts.inputFilePath()))
        {
            translatorDriver.analyze(in);
        }
        try (OutputStream out = getXCodeMLOutputAsStream(opts.outputTranslatedXastFilePath()))
        {
            translatorDriver.transform(out);
        }
        translatorDriver.flush();

        if (opts.translationReportFilePath() != null)
        {
            ClawTransformationReport report = new ClawTransformationReport(opts.translationReportFilePath());
            report.generate(args, translatorDriver, cfg);
        }

        if (opts.outputFortranFilePath() != null)
        {
            try (OutputStream out = Files.newOutputStream(opts.outputFortranFilePath()))
            {
                OmniBackendDriver backend = new OmniBackendDriver(OmniBackendDriver.Lang.FORTRAN);
                backend.decompile(out, translatorDriver.getTranslationUnit().getDocument(), cfg.getUserMaxColumns(),
                        opts.suppressPreprocLineDirectives(), xmOption);
            } catch (Exception e)
            {
                throw new Exception("Failed to decompile XcodeML to Fortran", e);
            }
        }
    }
}
