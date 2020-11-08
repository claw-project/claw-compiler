package clawfc;

import java.lang.RuntimeException;
import java.lang.ProcessBuilder;

import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.*;
import java.nio.file.*;

import java.io.InputStream;

import claw.ClawX2T;

public class Driver
{
    final static Logger LOGGER = Logger.getLogger(Driver.class.getName());

    public static void main(String[] args) throws Exception
    {
    	Driver driver = new Driver();
        driver.verifyInstall();
        Options opts = Options.parseCmdlineArguments(args);
        driver.execute(opts);
        System.exit(0);
    }

    final Configuration cfg;

    Driver() throws Exception
    {
        cfg = new Configuration();
    }

    static String getCmdOutput(String[] args) throws Exception
    {
        ProcessBuilder pb = new ProcessBuilder(args);
        pb.redirectErrorStream(true);
        Process p = pb.start();
        final int retCode = p.waitFor();
        if (retCode != 0)
        {
            throw new RuntimeException(
                    String.format("Cmd \"%s\" failed with return code ", String.join(" ", args), retCode));
        }
        String result = null;
        try (InputStream istrm = p.getInputStream(); Scanner s = new Scanner(istrm).useDelimiter("\\A"))
        {
            result = s.hasNext() ? s.next() : null;
        }
        return result;
    }

    void verifyInstall()
    {
        if (!Files.isDirectory(cfg.installRoot()))
        {
            LOGGER.severe(String.format("CLAW install directory \"%s\" does not exist or is not a directory",
                    cfg.installRoot()));
            System.exit(1);
        }
        if (!Files.isDirectory(cfg.omniInstallRoot()))
        {
            LOGGER.severe(
                    String.format("OMNI XCodeML Tools install directory \"%s\" does not exist or is not a directory",
                            cfg.omniInstallRoot()));
            System.exit(1);
        }
        if (!Files.isExecutable(cfg.omniFrontEnd()))
        {
            LOGGER.severe(
                    String.format("OMNI XCodeML Tools Fortran Frontend \"%s\" does not exist or is not a directory",
                            cfg.omniFrontEnd()));
            System.exit(1);
        }
        {
            String omniVersionTag = null;
            try
            {
                omniVersionTag = getCmdOutput(new String[] { cfg.omniFrontEnd().toString(), "--version-tag" });
            } catch (Exception e)
            {
                LOGGER.severe("Failed to get OMNI XCodeML Tools version: " + e.getMessage());
                System.exit(1);
            }
            if (!cfg.omniVersionTag().equals(omniVersionTag))
            {
                LOGGER.severe(String.format("OMNI XCodeML Tools version mismatch\n\texpected: \"%s\"\n\tgot: \"%s\"",
                        cfg.omniVersionTag(), omniVersionTag));
                System.exit(1);
            }
        }
    }

    void execute(Options opts) throws Exception
    {
        if (opts.printInstallCfg())
        {
            System.out.print(cfg.toString());
        } else if (opts.printVersion())
        {
            printVersion();
        } else if(opts.printTargets())
        {
            ClawX2T.main(new String[] {"--target-list"});
        } else if(opts.printDirectives())
        {
            ClawX2T.main(new String[] {"--directive-list"});
        } else if(opts.printCfg())
        {
            ArrayList<String> args = new ArrayList<String>(Arrays.asList("--show-config", "--config-path=" + cfg.configDir()));
            String cfg = opts.configFile();
            if(cfg != null)
            { args.add("--config=" + cfg); }
            ClawX2T.main(args.stream().toArray(String[]::new));
        } else if(opts.printOptions())
        {
            System.out.println(opts);
        }
        else
        {
            
        }
    }

    void printVersion()
    {
        String vStr = String.format("%s %s \"%s\" %s ", cfg.name(), cfg.version(), cfg.commit(), cfg.omniVersion());
        System.out.print(vStr);
    }
};