/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
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

public class FortranFileProgramUnitInfoDeserializer
{
    final Unmarshaller _unmarshaller;
    final String SCHEMA_FILE = "/clawfc/depscan/serial/file_unit_info.xsd";

    public FortranFileProgramUnitInfoDeserializer(boolean validate) throws Exception
    {
        JAXBContext contextObj = JAXBContext.newInstance(clawfc.depscan.serial.FortranFileProgramUnitInfo.class);
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

    public clawfc.depscan.FortranFileProgramUnitInfo deserialize(InputStream in) throws Exception
    {
        FortranFileProgramUnitInfo deObj = new FortranFileProgramUnitInfo(
                (clawfc.depscan.serial.FortranFileProgramUnitInfo) _unmarshaller.unmarshal(in));
        return deObj;
    }
}
