/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.OutputStream;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Marshaller;

public class FortranFileSummarySerializer
{
    final Marshaller _marshaller;

    public FortranFileSummarySerializer() throws Exception
    {
        JAXBContext contextObj = JAXBContext.newInstance(clawfc.depscan.serial.FortranFileSummary.class);
        _marshaller = contextObj.createMarshaller();
        _marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
    }

    public void serialize(clawfc.depscan.FortranFileSummary summary, OutputStream out) throws Exception
    {
        _marshaller.marshal(summary.data(), out);
    }
}
