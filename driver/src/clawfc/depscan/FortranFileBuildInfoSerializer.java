/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.OutputStream;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Marshaller;

public class FortranFileBuildInfoSerializer
{
    final Marshaller _marshaller;

    public FortranFileBuildInfoSerializer() throws Exception
    {
        JAXBContext contextObj = JAXBContext.newInstance(clawfc.depscan.serial.FortranFileBuildInfo.class);
        _marshaller = contextObj.createMarshaller();
        _marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
    }

    public void serialize(clawfc.depscan.FortranFileBuildInfo summary, OutputStream out) throws Exception
    {
        _marshaller.marshal(summary.getData(), out);
    }
}
