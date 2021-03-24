/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.configuration;

import claw.tatsu.directive.generator.OpenAccExecutionMode;

import java.util.Map;

/**
 * OpenACC specific configuration options.
 *
 * @author clementval
 */
public class OpenAccConfiguration extends AcceleratorConfiguration
{

    private static final String OPENACC_NUM_WORKERS = "acc_num_workers";
    private static final String OPENACC_NUM_GANGS = "acc_num_gangs";
    private static final String OPENACC_VECTOR_LENGTH = "acc_vector_length";
    private static final String OPENACC_EXECUTION_MODE = "acc_execution_mode";

    private int _numWorkers = 0;
    private int _numGangs = 0;
    private int _vectorLength = 0;

    private OpenAccExecutionMode _mode = OpenAccExecutionMode.VECTOR;

    /**
     * Constructs a OpenAccConfiguration object holding OpenACC configuration
     * information.
     *
     * @param parameters Map of all configuration parameters.
     */
    public OpenAccConfiguration(Map<String, String> parameters)
    {
        super(parameters);

        if (parameters.containsKey(OPENACC_NUM_WORKERS))
        {
            _numWorkers = Integer.parseInt(parameters.get(OPENACC_NUM_WORKERS));
        }
        if (parameters.containsKey(OPENACC_NUM_GANGS))
        {
            _numGangs = Integer.parseInt(parameters.get(OPENACC_NUM_GANGS));
        }
        if (parameters.containsKey(OPENACC_VECTOR_LENGTH))
        {
            _vectorLength = Integer.parseInt(parameters.get(OPENACC_VECTOR_LENGTH));
        }
        if (parameters.containsKey(OPENACC_EXECUTION_MODE))
        {
            _mode = OpenAccExecutionMode.fromString(parameters.get(OPENACC_EXECUTION_MODE));
        }
    }

    /**
     * Get the num_workers value.
     *
     * @return num_workers value. 0 if not defined.
     */
    public int getNumWorkers()
    {
        return _numWorkers;
    }

    /**
     * Get the num_gangs value.
     *
     * @return num_gangs value. 0 if not defined.
     */
    public int getNumGangs()
    {
        return _numGangs;
    }

    /**
     * Get the vector_length value.
     *
     * @return vector_length value. 0 if not defined.
     */
    public int getVectorLength()
    {
        return _vectorLength;
    }

    /**
     * Get the OpenACC execution mode value.
     *
     * @return OpenACC execution mode value. NONE if not defined.
     */
    public OpenAccExecutionMode getMode()
    {
        return _mode;
    }
}
