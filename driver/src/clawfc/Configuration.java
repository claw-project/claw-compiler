/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;
import java.util.logging.Logger;

public class Configuration
{
    public static enum FortranCompilerVendor {
        CRAY, INTEL, GNU, PORTLAND, NAG
    }

    public static final String PROPERTIES_FILENAME = "/config/clawfc.properties";
    private final static Logger LOGGER = Logger.getLogger(Configuration.class.getName());

    public String name()
    {
        return cfgProperties.getProperty("claw.name");
    }

    public String version()
    {
        return cfgProperties.getProperty("claw.version");
    }

    public String commit()
    {
        return cfgProperties.getProperty("claw.commit");
    }

    public FortranCompilerVendor defaultFortranCompilerVendor()
    {
        return FortranCompilerVendor.valueOf(cfgProperties.getProperty("claw.fc.default.vendor").toUpperCase());
    }

    public String defaultFortranCompilerCmd()
    {
        return cfgProperties.getProperty("claw.fc.default.cmd");
    }

    public Path installRoot()
    {
        return Paths.get(cfgProperties.getProperty("claw.home"));
    }

    public Path configDir()
    {
        return installRoot().resolve(cfgProperties.getProperty("claw.cfg.dir"));
    }

    public String omniVersion()
    {
        return cfgProperties.getProperty("omni.version.string");
    }

    public String omniVersionTag()
    {
        return cfgProperties.getProperty("omni.version.tag");
    }

    public Path omniInstallRoot()
    {
        if (OMNI_HOME_ENV != null)
        {
            return Paths.get(OMNI_HOME_ENV).normalize();
        } else
        {
            return Paths.get(cfgProperties.getProperty("omni.home")).normalize();
        }
    }

    public Path omniFrontEnd()
    {
        return omniInstallRoot().resolve(cfgProperties.getProperty("omni.fortran.frontend")).normalize();
    }

    public Path omniDefaultStdXmodDir()
    {
        return omniInstallRoot().resolve("fincludes");
    }

    public Path defaultStdXmodDir()
    {
        return installRoot().resolve("fincludes");
    }

    public Configuration() throws Exception
    {
        cfgProperties = loadProperties();
    }

    Properties loadProperties() throws Exception
    {
        Properties props = new Properties();
        InputStream inStream;
        try
        {
            inStream = Configuration.class.getResourceAsStream(PROPERTIES_FILENAME);
            props.load(inStream);
        } catch (Exception e)
        {
            LOGGER.severe(String.format("Failed to load properties config file \"%s\"", PROPERTIES_FILENAME));
            throw e;
        }
        return props;
    }

    public String toString()
    {
        StringBuilder s = new StringBuilder();
        s.append(String.format("claw.name : %s\n", name()));
        s.append(String.format("claw.version : %s\n", version()));
        s.append(String.format("claw.commit : %s\n", commit()));
        s.append(String.format("claw.defaultFortranCompilerType : %s\n", defaultFortranCompilerVendor()));
        s.append(String.format("claw.defaultFortranCompilerCmd : %s\n", defaultFortranCompilerCmd()));
        s.append(String.format("claw.installRoot : %s\n", installRoot()));
        s.append(String.format("claw.cfg.dir : %s [%s]\n", configDir(), cfgProperties.getProperty("claw.cfg.dir")));
        s.append(String.format("omni.version.string : %s\n", omniVersion()));
        s.append(String.format("omni.version.tag : %s\n", omniVersionTag()));
        s.append(String.format("omni.home : %s [%s]\n", omniInstallRoot().toString(),
                cfgProperties.getProperty("omni.home")));
        s.append(String.format("omni.fortran.frontend : %s [%s]\n", omniFrontEnd().toString(),
                cfgProperties.getProperty("omni.fortran.frontend")));
        return s.toString();
    }

    Properties cfgProperties;
    static final String OMNI_HOME_ENV = System.getenv("OMNI_HOME");
};