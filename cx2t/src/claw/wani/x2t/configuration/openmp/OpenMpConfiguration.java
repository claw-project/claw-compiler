/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration.openmp;

import claw.tatsu.directive.generator.openmp.OpenMpExecutionMode;

import java.util.Map;

/**
 * @author peclatj
 */
public class OpenMpConfiguration {

  static final String LOCAL_STRATEGY_PRIVATE = "private";
  static final String LOCAL_STRATEGY_PROMOTE = "promote";

  static final String DATA_STRATEGY_NONE = "none";
  static final String DATA_STRATEGY_PRESENT = "present";
  static final String DATA_STRATEGY_KERNEL = "kernel";

  private static final String OPENMP_NUM_THREADS = "openmp_num_threads";
  private static final String OPENMP_NUM_TEAMS = "openmp_num_teams";
  private static final String OPENMP_SCHEDULER_CHUNK_SIZE =
      "openmp_scheduler_chunk_size";
  private static final String OPENMP_EXECUTION_MODE = "openmp_execution_mode";
  private static final String OPENMP_DATA_STRATEGY = "openmp_data_strategy";
  private static final String OPENMP_LOCAL_STRATEGY = "openmp_local_strategy";
  private static final String OPENMP_COLLAPSE = "openmp_collapse";

  private int _numThreads = 0;
  private int _numTeams = 0;
  private int _schedulerChunkSize = 0;
  private boolean _collapseStrategy = false;
  private OpenMpExecutionMode _mode = OpenMpExecutionMode.TEAMS_DISTRIBUTE;
  private OpenMpDataStrategy _dataStrategy = OpenMpDataStrategy.PRESENT;
  private OpenMpLocalStrategy _localStrategy = OpenMpLocalStrategy.PRIVATE;

  /**
   * Constructs a OpenMpConfiguration object holding OpenMP configuration
   * information.
   *
   * @param parameters Map of all configuration parameters.
   */
  public OpenMpConfiguration(Map<String, String> parameters) {
    if(parameters.containsKey(OPENMP_NUM_THREADS)) {
      _numThreads = Integer.parseInt(parameters.get(OPENMP_NUM_THREADS));
    }
    if(parameters.containsKey(OPENMP_NUM_TEAMS)) {
      _numTeams = Integer.parseInt(parameters.get(OPENMP_NUM_TEAMS));
    }
    if(parameters.containsKey(OPENMP_SCHEDULER_CHUNK_SIZE)) {
      _schedulerChunkSize =
          Integer.parseInt(parameters.get(OPENMP_SCHEDULER_CHUNK_SIZE));
    }
    if(parameters.containsKey(OPENMP_EXECUTION_MODE)) {
      _mode = OpenMpExecutionMode.
          fromString(parameters.get(OPENMP_EXECUTION_MODE));
    }
    if(parameters.containsKey(OPENMP_DATA_STRATEGY)) {
      _dataStrategy = OpenMpDataStrategy.
          fromString(parameters.get(OPENMP_DATA_STRATEGY));
    }
    if(parameters.containsKey(OPENMP_LOCAL_STRATEGY)) {
      _localStrategy = OpenMpLocalStrategy.
          fromString(parameters.get(OPENMP_LOCAL_STRATEGY));
    }
    if(parameters.containsKey(OPENMP_COLLAPSE)) {
      _collapseStrategy =
          Boolean.parseBoolean(parameters.get(OPENMP_COLLAPSE));
    }
  }

  /**
   * Get the num_threads value.
   *
   * @return num_threads value. 0 if not defined.
   */
  public int getNumThreads() {
    return _numThreads;
  }

  /**
   * Get the num_teams value.
   *
   * @return num_teams value. 0 if not defined.
   */
  public int getNumTeams() {
    return _numTeams;
  }

  /**
   * Get the scheduler_chunk_size value.
   *
   * @return scheduler_chunk_size value. 0 if not defined.
   */
  public int getSchedulerChunkSize() {
    return _schedulerChunkSize;
  }

  /**
   * Get the OpenMP execution mode value.
   *
   * @return OpenMP execution mode value. NONE if not defined.
   */
  public OpenMpExecutionMode getMode() {
    return _mode;
  }

  /**
   * Get the OpenMP data strategy value.
   *
   * @return OpenMP data strategy. PRESENT by default.
   */
  public OpenMpDataStrategy getDataStrategy() {
    return _dataStrategy;
  }

  /**
   * Get the OpenMP local array strategy value.
   *
   * @return OpenMP local array strategy. PRIVATE by default.
   */
  public OpenMpLocalStrategy getLocalStrategy() {
    return _localStrategy;
  }

  /**
   * Get the OpenMP collapse strategy.
   *
   * @return True if collapse strategy is on. False otherwise.
   */
  public boolean hasCollapseStrategy() {
    return _collapseStrategy;
  }
}
