package clawfc;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;

public class Configuration
{
    public static final String PROPERTIES_FILENAME = "/config/clawfc.properties";
    private final static Logger LOGGER = Logger.getLogger(Configuration.class.getName());

    public Configuration() throws IOException
    {
	cfgProperties = loadProperties();
    }

    Properties loadProperties() throws IOException
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

    Properties cfgProperties;
};