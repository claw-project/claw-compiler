/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import net.consensys.cava.toml.Toml;
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

  private ModelConfig() {
    _instance = new ModelConfig();
  }

  public static void load(String configPath) throws Exception {
    if(_instance == null) {
      _instance = new ModelConfig();
    }

    Path source = Paths.get(configPath);
    TomlParseResult result = Toml.parse(source);
    if(result.hasErrors()) {
      throw new Exception("Model configuration file not formatted correctly.");
    } else {
      // Validate the information in the configuration file
    }
  }

}
