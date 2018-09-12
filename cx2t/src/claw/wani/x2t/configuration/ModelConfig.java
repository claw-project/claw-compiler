/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import net.consensys.cava.toml.Toml;
import net.consensys.cava.toml.TomlArray;
import net.consensys.cava.toml.TomlParseResult;

import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * SCA Specific model configuration. This class reads and holds all
 * information about the model dimensions and promotion layouts that can be
 * applied in the SCA transformation.
 *
 * @author clementval
 */
public class ModelConfig {

  private static ModelConfig _instance = null;

  private static final String KEY_DIMENSIONS = "dimensions";
  private static final String KEY_DIMENSION_ID = "id";
  private static final String KEY_DIMENSION_SIZE = "size";

  private static final String KEY_LAYOUTS = "dimensions";

  private ModelConfig() {
  }

  public static ModelConfig get() {
    if(_instance == null) {
      _instance = new ModelConfig();
    }
    return _instance;
  }

  public void load(String configPath) throws Exception {
    if(_instance == null) {
      _instance = new ModelConfig();
    }

    Path source = Paths.get(configPath);
    TomlParseResult result = Toml.parse(source);
    if(result.hasErrors()) {
      throw new Exception("Model configuration file not formatted correctly.");
    } else {
      // Validate the information in the configuration file

      TomlArray dimensions = result.getArray(KEY_DIMENSIONS);
      

      for(Object value : dimensions.toList()) {

      }

      TomlArray layouts = result.getArray(KEY_LAYOUTS);

    }
  }

}
