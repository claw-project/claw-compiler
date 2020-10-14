/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;

public class FortranModuleDependencies
{
	String name;
	ArrayList<String> preqNames;
	
	public FortranModuleDependencies(String name, LinkedHashSet<String> preqNames)
	{
		this.name = name;
		this.preqNames = new ArrayList<String>(preqNames.size());
		for(String s: preqNames)
		{ this.preqNames.add(s); }
	}
	
	public FortranModuleDependencies(String name, String[] preqNames)
	{
		this.name = name;
		this.preqNames = new ArrayList<String>(Arrays.asList(preqNames));
	}
	
	public FortranModuleDependencies()
	{
		this.name = null;
		this.preqNames = new ArrayList<String>();
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
        FortranModuleDependencies other = (FortranModuleDependencies) obj;
        if(name == null)
        {
        	if(other.name != null)
        	{ return false; }
        }
        else
        {
            if(!name.equals(other.name))
            { return false; }        	
        }
        if(!preqNames.equals(other.preqNames))
        { return false; }
        return true;
    }
}
