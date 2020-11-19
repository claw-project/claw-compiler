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

public class FortranFileSummaryDeserializer
{
    final Unmarshaller _unmarshaller;
    final String SCHEMA_FILE = "/clawfc/depscan/serial/dep_info.xsd";

    public FortranFileSummaryDeserializer(boolean validate) throws Exception
    {
        JAXBContext contextObj = JAXBContext.newInstance(clawfc.depscan.serial.FortranFileSummary.class);
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

    public clawfc.depscan.FortranFileSummary deserialize(InputStream in) throws Exception
    {
        FortranFileSummary deObj = new FortranFileSummary(
                (clawfc.depscan.serial.FortranFileSummary) _unmarshaller.unmarshal(in));
        return deObj;
    }
}
