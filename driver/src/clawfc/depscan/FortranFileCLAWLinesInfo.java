/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import java.util.*;

public class FortranFileCLAWLinesInfo
{
    public final List<Integer> clawDirectives;
    public final List<Integer> clawGuards;
    
    public FortranFileCLAWLinesInfo(List<Integer> clawDirectives,
                                    List<Integer> clawGuards)
    {
        this.clawDirectives = Collections.unmodifiableList(clawDirectives);
        this.clawGuards = Collections.unmodifiableList(clawGuards);        
    }
    
    @Override
    public boolean equals(Object obj) 
    {
        if(this == obj)
        { return true; }
        if(obj == null)
        { return false; }
        if (getClass() != obj.getClass())
        { return false; }
        FortranFileCLAWLinesInfo other = (FortranFileCLAWLinesInfo) obj;
        if(!clawDirectives.equals(other.clawDirectives))
        { return false; }
        if(!clawGuards.equals(other.clawGuards))
        { return false; }
        return true;
    }
}
