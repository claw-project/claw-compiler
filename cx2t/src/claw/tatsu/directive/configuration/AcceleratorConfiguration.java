/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.configuration;

import java.util.Map;

/**
 * Base configuration class related to accelerator options.
 *
 * @author peclatj, clementval
 */
public class AcceleratorConfiguration
{

    private static final String ACCELERATOR_DATA_STRATEGY = "accelerator_data_strategy";
    private static final String ACCELERATOR_LOCAL_STRATEGY = "accelerator_local_strategy";
    private static final String ACCELERATOR_COLLAPSE = "accelerator_collapse";

    private AcceleratorDataStrategy _dataStrategy = AcceleratorDataStrategy.PRESENT;
    private AcceleratorLocalStrategy _localStrategy = AcceleratorLocalStrategy.PRIVATE;
    private boolean _collapseStrategy = false;

    /**
     * Constructs a AcceleratorConfiguration object holding accelerator
     * configuration information.
     *
     * @param parameters Map of all configuration parameters.
     */
    public AcceleratorConfiguration(Map<String, String> parameters)
    {
        if (parameters.containsKey(ACCELERATOR_DATA_STRATEGY))
        {
            _dataStrategy = AcceleratorDataStrategy.fromString(parameters.get(ACCELERATOR_DATA_STRATEGY));
        }
        if (parameters.containsKey(ACCELERATOR_LOCAL_STRATEGY))
        {
            _localStrategy = AcceleratorLocalStrategy.fromString(parameters.get(ACCELERATOR_LOCAL_STRATEGY));
        }
        if (parameters.containsKey(ACCELERATOR_COLLAPSE))
        {
            _collapseStrategy = Boolean.parseBoolean(parameters.get(ACCELERATOR_COLLAPSE));
        }
    }

    /**
     * Get the accelerator data strategy value.
     *
     * @return accelerator data strategy. Present by default.
     */
    public AcceleratorDataStrategy getDataStrategy()
    {
        return _dataStrategy;
    }

    /**
     * Get the accelerator local array strategy value.
     *
     * @return accelerator local array strategy. Private by default.
     */
    public AcceleratorLocalStrategy getLocalStrategy()
    {
        return _localStrategy;
    }

    /**
     * Get the accelerator collapse strategy.
     *
     * @return True if collapse strategy is on. False otherwise.
     */
    public boolean hasCollapseStrategy()
    {
        return _collapseStrategy;
    }
}
