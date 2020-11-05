/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;
import java.util.*;

public class FortranModuleDependencies
{
    String _name;
    List<String> _usedModuleNames;
    
	public String name() { return _name; }
	public List<String> usedModuleNames() {return _usedModuleNames; }
	
	public FortranModuleDependencies(String name, LinkedHashSet<String> usedModuleNames)
	{
		this._name = name;
		this._usedModuleNames = new ArrayList<String>(usedModuleNames.size());
		for(String s: usedModuleNames)
		{ this._usedModuleNames.add(s); }
		this._usedModuleNames = Collections.unmodifiableList(this._usedModuleNames);
	}
	
	public FortranModuleDependencies(String name, String[] usedModuleNames)
	{
		this._name = name;
		this._usedModuleNames = Collections.unmodifiableList(new ArrayList<String>(Arrays.asList(usedModuleNames)));
	}
	
	/*public FortranModuleDependencies()
	{
		this._name = null;
		this._usedModuleNames = new ArrayList<String>();
	}*/
	
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
        if(name() == null)
        {
        	if(other.name() != null)
        	{ return false; }
        }
        else
        {
            if(!name().equals(other.name()))
            { return false; }        	
        }
        if(!usedModuleNames().equals(other.usedModuleNames()))
        { return false; }
        return true;
    }
}
