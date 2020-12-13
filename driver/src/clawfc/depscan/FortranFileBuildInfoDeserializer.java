/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.InputStream;

import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import clawfc.Configuration;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Unmarshaller;

public class FortranFileBuildInfoDeserializer
{
    final Unmarshaller _unmarshaller;
    final String SCHEMA_FILE = "/clawfc/depscan/serial/file_build_info.xsd";

    public FortranFileBuildInfoDeserializer(boolean validate) throws Exception
    {
        JAXBContext contextObj = JAXBContext.newInstance(clawfc.depscan.serial.FortranFileBuildInfo.class);
        _unmarshaller = contextObj.createUnmarshaller();
        if (validate)
        {
            Schema schema;
            try (InputStream schemaStream = Configuration.class.getResourceAsStream(SCHEMA_FILE))
            {
                SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
                schema = sf.newSchema(new StreamSource(schemaStream));
            }
            _unmarshaller.setSchema(schema);
        }
    }

    public clawfc.depscan.FortranFileBuildInfo deserialize(InputStream in) throws Exception
    {
        FortranFileBuildInfo deObj = new FortranFileBuildInfo(
                (clawfc.depscan.serial.FortranFileBuildInfo) _unmarshaller.unmarshal(in));
        return deObj;
    }
}
