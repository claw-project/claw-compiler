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

  private static final String OPENMP_NUM_THREADS = "openmp_num_threads";
  private static final String OPENMP_NUM_TEAMS = "openmp_num_teams";
  private static final String OPENMP_SCHEDULER_CHUNK_SIZE =
      "openmp_scheduler_chunk_size";
  private static final String OPENMP_EXECUTION_MODE = "openmp_execution_mode";

  private int _numThreads = 0;
  private int _numTeams = 0;
  private int _schedulerChunkSize = 0;
  private OpenMpExecutionMode _mode = OpenMpExecutionMode.TEAMS_DISTRIBUTE;

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
}
