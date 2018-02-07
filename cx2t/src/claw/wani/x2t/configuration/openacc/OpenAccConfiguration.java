/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration.openacc;

import claw.tatsu.directive.generator.openacc.OpenAccExecutionMode;

import java.util.Map;

/**
 * @author clementval
 */
public class OpenAccConfiguration {

  static final String LOCAL_STRATEGY_PRIVATE = "private";
  static final String LOCAL_STRATEGY_PROMOTE = "promote";

  static final String DATA_STRATEGY_NONE = "none";
  static final String DATA_STRATEGY_PRESENT = "present";
  static final String DATA_STRATEGY_KERNEL = "kernel";

  private static final String OPENACC_NUM_WORKERS = "openacc_num_workers";
  private static final String OPENACC_NUM_GANGS = "openacc_num_gangs";
  private static final String OPENACC_VECTOR_LENGTH = "openacc_vector_length";
  private static final String OPENACC_EXECUTION_MODE = "openacc_execution_mode";
  private static final String OPENACC_DATA_STRATEGY = "openacc_data_strategy";
  private static final String OPENACC_LOCAL_STRATEGY = "openacc_local_strategy";
  private static final String OPENACC_COLLAPSE = "openacc_collapse";

  private int _numWorkers = 0;
  private int _numGangs = 0;
  private int _vectorLength = 0;
  private boolean _collapseStrategy = false;
  private OpenAccExecutionMode _mode = OpenAccExecutionMode.VECTOR;
  private OpenAccDataStrategy _dataStrategy = OpenAccDataStrategy.PRESENT;
  private OpenAccLocalStrategy _localStrategy = OpenAccLocalStrategy.PRIVATE;

  /**
   * Constructs a OpenAccConfiguration object holding OpenACC configuration
   * information.
   *
   * @param parameters Map of all configuration parameters.
   */
  public OpenAccConfiguration(Map<String, String> parameters) {
    if(parameters.containsKey(OPENACC_NUM_WORKERS)) {
      _numWorkers = Integer.parseInt(parameters.get(OPENACC_NUM_WORKERS));
    }
    if(parameters.containsKey(OPENACC_NUM_GANGS)) {
      _numGangs = Integer.parseInt(parameters.get(OPENACC_NUM_GANGS));
    }
    if(parameters.containsKey(OPENACC_VECTOR_LENGTH)) {
      _vectorLength = Integer.parseInt(parameters.get(OPENACC_VECTOR_LENGTH));
    }
    if(parameters.containsKey(OPENACC_EXECUTION_MODE)) {
      _mode = OpenAccExecutionMode.
          fromString(parameters.get(OPENACC_EXECUTION_MODE));
    }
    if(parameters.containsKey(OPENACC_DATA_STRATEGY)) {
      _dataStrategy = OpenAccDataStrategy.
          fromString(parameters.get(OPENACC_DATA_STRATEGY));
    }
    if(parameters.containsKey(OPENACC_LOCAL_STRATEGY)) {
      _localStrategy = OpenAccLocalStrategy.
          fromString(parameters.get(OPENACC_LOCAL_STRATEGY));
    }
    if(parameters.containsKey(OPENACC_COLLAPSE)) {
      _collapseStrategy =
          Boolean.parseBoolean(parameters.get(OPENACC_COLLAPSE));
    }
  }

  /**
   * Get the num_workers value.
   *
   * @return num_workers value. 0 if not defined.
   */
  public int getNumWorkers() {
    return _numWorkers;
  }

  /**
   * Get the num_gangs value.
   *
   * @return num_gangs value. 0 if not defined.
   */
  public int getNumGangs() {
    return _numGangs;
  }

  /**
   * Get the vector_length value.
   *
   * @return vector_length value. 0 if not defined.
   */
  public int getVectorLength() {
    return _vectorLength;
  }

  /**
   * Get the OpenACC execution mode value.
   *
   * @return OpenACC execution mode value. NONE if not defined.
   */
  public OpenAccExecutionMode getMode() {
    return _mode;
  }

  /**
   * Get the OpenACC data strategy value.
   *
   * @return OpenACC data strategy. PRESENT by default.
   */
  public OpenAccDataStrategy getDataStrategy() {
    return _dataStrategy;
  }

  /**
   * Get the OpenACC local array strategy value.
   *
   * @return OpenACC local array strategy. PRIVATE by default.
   */
  public OpenAccLocalStrategy getLocalStrategy() {
    return _localStrategy;
  }

  /**
   * Get the OpenACC collapse strategy.
   *
   * @return True if collapse strategy is on. False otherwise.
   */
  public boolean hasCollapseStrategy() {
    return _collapseStrategy;
  }
}
