/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration.gpu;

import java.util.Map;

/**
 * @author peclatj
 */
public class GpuConfiguration {

  private static final String GPU_DATA_STRATEGY = "gpu_data_strategy";
  private static final String GPU_LOCAL_STRATEGY = "gpu_local_strategy";
  private static final String GPU_COLLAPSE = "gpu_collapse";

  private GpuDataStrategy _dataStrategy = GpuDataStrategy.PRESENT;
  private GpuLocalStrategy _localStrategy = GpuLocalStrategy.PRIVATE;
  private boolean _collapseStrategy = false;

  /**
   * Constructs a GpuConfiguration object holding GPU configuration information.
   *
   * @param parameters Map of all configuration parameters.
   */
  public GpuConfiguration(Map<String, String> parameters) {
    if(parameters.containsKey(GPU_DATA_STRATEGY)) {
      _dataStrategy = GpuDataStrategy.
          fromString(parameters.get(GPU_DATA_STRATEGY));
    }
    if(parameters.containsKey(GPU_LOCAL_STRATEGY)) {
      _localStrategy = GpuLocalStrategy.
          fromString(parameters.get(GPU_LOCAL_STRATEGY));
    }
    if(parameters.containsKey(GPU_COLLAPSE)) {
      _collapseStrategy = Boolean.parseBoolean(parameters.get(GPU_COLLAPSE));
    }
  }

  /**
   * Get the GPU data strategy value.
   *
   * @return GPU data strategy. PRESENT by default.
   */
  public GpuDataStrategy getDataStrategy() { return _dataStrategy; }

  /**
   * Get the GPU local array strategy value.
   *
   * @return GPU local array strategy. PRIVATE by default.
   */
  public GpuLocalStrategy getLocalStrategy() { return _localStrategy; }

  /**
   * Get the GPU collapse strategy.
   *
   * @return True if collapse strategy is on. False otherwise.
   */
  public boolean hasCollapseStrategy() { return _collapseStrategy; }
}
