/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import net.consensys.cava.toml.Toml;
import net.consensys.cava.toml.TomlArray;
import net.consensys.cava.toml.TomlParseResult;
import net.consensys.cava.toml.TomlTable;

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;

/**
 * SCA Specific model configuration. This class reads and holds all information
 * about the model dimensions and promotion layouts that can be applied in the
 * SCA transformation.
 *
 * @author clementval
 */
public class ModelConfig
{

    private static final String KEY_DIMENSIONS = "dimensions";
    private static final String KEY_DIMENSION_ID = "id";
    private static final String KEY_DIMENSION_SIZE = "size";
    private static final String KEY_DIMENSION_LB = "lower";
    private static final String KEY_DIMENSION_UB = "upper";
    private static final String KEY_DIMENSION_STEP = "step";
    private static final String KEY_DIMENSION_ITERATION = "iteration";
    private static final String KEY_LAYOUTS = "layouts";
    private static final String KEY_LAYOUT_POSITION = "position";

    private static final String DEFAULT_LAYOUT_ID = "default";
    private static final String DEFAULT_LOWER_BOUND = "1";

    static final String ERR_MALFORMATTED = "Model configuration file not formatted correctly.";
    static final String ERR_NO_DIMENSIONS = "No dimension defined in the model configuration %s.";
    static final String ERR_NO_SIZE = "Size information missing in dimension %s";
    static final String ERR_NO_UPPER = "Upper information missing in size dimension %s";
    static final String ERR_NO_LAYOUTS = "No layouts defined in the model configuration %s"
            + ". At least \"default\" should be defined!";
    static final String ERR_LAYOUT_NO_ID = "Layout is missing identifier information";
    static final String ERR_LAYOUT_NO_POSITION = "Layout %s is missing position information";
    static final String ERR_DIM_NOT_AVAIL = "Dimension %s defined in layout "
            + "%s in not available in this configuration.";
    static final String ERR_NO_BASE_DIM = "Layout %s is missing the base dimension \":\"";

    // Dotted key from the configuration file
    private static final String KEY_MODEL_NAME = "model.name";

    private final Map<String, DimensionDefinition> _dimensions;
    private final Map<String, List<DimensionDefinition>> _layouts;

    private String _modelName;
    private boolean _isLoaded;

    /**
     * Private ctor to avoid instantiation of this class.
     */
    public ModelConfig()
    {
        _dimensions = new LinkedHashMap<>();
        _layouts = new HashMap<>();
        _isLoaded = false;
    }

    /**
     * Load a model configuration file and read its content.
     *
     * @param configPath Path to the model configuration file.
     * @throws Exception If the configuration does not conform to the specification.
     */
    void load(String configPath) throws Exception
    {
        _dimensions.clear();
        _layouts.clear();
        load(new FileInputStream(configPath));
    }

    void load(InputStream is) throws Exception
    {
        TomlParseResult result = Toml.parse(is);
        if (result.hasErrors())
        {
            throw new Exception(ERR_MALFORMATTED);
        } else
        {
            _modelName = result.getString(KEY_MODEL_NAME);
            readDimensions(result);
            readLayouts(result);
        }
        _isLoaded = true;
    }

    /**
     * Read all the dimensions defined in the model configuration.
     *
     * The dimension can be defined as follows:<br>
     * <br>
     *
     * [[dimensions]] # Definition of dimensions that can be used in layouts<br>
     * id = "horizontal"<br>
     * [dimensions.size]<br>
     * lower = 1 # if not specified, 1 by default<br>
     * upper = "nproma" # mandatory information<br>
     * [dimensions.iteration]<br>
     * lower = "pstart" # if not specified size.lower by default<br>
     * upper = "pend" # if not specified size.upper by default<br>
     * step = 1 # if not specified, 1 by default<br>
     * <br>
     *
     * @param result The current TOML parse result object.
     * @throws Exception If the result is not conform to the specifications.
     */
    private void readDimensions(TomlParseResult result) throws Exception
    {
        TomlArray dimensions = result.getArray(KEY_DIMENSIONS);
        if (dimensions == null)
        {
            throw new Exception(String.format(ERR_NO_DIMENSIONS, _modelName));
        }

        for (int i = 0; i < dimensions.size(); ++i)
        {
            TomlTable dimension = dimensions.getTable(i);
            String dimId = dimension.getString(KEY_DIMENSION_ID);

            TomlTable dimSize = dimension.getTable(KEY_DIMENSION_SIZE);
            if (dimSize == null)
            {
                throw new Exception(String.format(ERR_NO_SIZE, dimId));
            }

            String lowerBound = readBoundOrDefault(dimSize);
            String upperBound = readStringOrInt(dimSize, KEY_DIMENSION_UB);
            if (upperBound.isEmpty())
            {
                throw new Exception(String.format(ERR_NO_UPPER, dimId));
            }

            TomlTable dimIt = dimension.getTable(KEY_DIMENSION_ITERATION);

            if (dimIt == null)
            {
                _dimensions.put(dimId, new DimensionDefinition(dimId, lowerBound, upperBound));
            } else
            {
                String lowerItBound = readBoundOrNull(dimIt, KEY_DIMENSION_LB);
                String upperItBound = readBoundOrNull(dimIt, KEY_DIMENSION_UB);
                String stepItBound = readBoundOrNull(dimIt, KEY_DIMENSION_STEP);

                _dimensions.put(dimId, new DimensionDefinition(dimId, lowerBound, upperBound, lowerItBound,
                        upperItBound, stepItBound));
            }
        }
    }

    /**
     * Read all the layouts defined in the model configuration.
     *
     * The layouts can be defined as follows:
     *
     * [[layouts]] id = "id" position = [ "dim1", ":" ]
     *
     * The ":" dimension is representing the base dimension (currently present
     * dimensions)
     *
     * @param result The current TOML parse result object.
     * @throws Exception If the result is not conform to the specifications.
     */
    private void readLayouts(TomlParseResult result) throws Exception
    {
        TomlArray layouts = result.getArray(KEY_LAYOUTS);
        if (layouts == null)
        {
            throw new Exception(String.format(ERR_NO_LAYOUTS, _modelName));
        }

        for (int i = 0; i < layouts.size(); ++i)
        {
            TomlTable layout = layouts.getTable(i);
            String layoutId = layout.getString(KEY_DIMENSION_ID);
            if (layoutId == null)
            {
                throw new Exception(ERR_LAYOUT_NO_ID);
            }

            TomlArray position = layout.getArray(KEY_LAYOUT_POSITION);
            if (position == null)
            {
                throw new Exception(String.format(ERR_LAYOUT_NO_POSITION, layoutId));
            }

            List<DimensionDefinition> dimensions = new ArrayList<>();

            for (int j = 0; j < position.size(); ++j)
            {
                String pos = position.getString(j);

                if (pos.equals(DimensionDefinition.BASE_DIM))
                {
                    dimensions.add(DimensionDefinition.BASE_DIMENSION);
                } else
                {
                    if (!_dimensions.containsKey(pos))
                    {
                        throw new Exception(String.format(ERR_DIM_NOT_AVAIL, pos, layoutId));
                    }
                    dimensions.add(_dimensions.get(pos));
                }
            }

            if (!dimensions.contains(DimensionDefinition.BASE_DIMENSION))
            {
                throw new Exception(String.format(ERR_NO_BASE_DIM, layoutId));
            }

            DimensionDefinition.flagInsertPosition(dimensions);
            dimensions.remove(DimensionDefinition.BASE_DIMENSION);

            _layouts.put(layoutId, dimensions);
        }
    }

    /**
     * Read value if present or return null.
     *
     * @param table     Current table to read in.
     * @param dottedKey Dotted key to get the value.
     * @return String representation of the value if present. Null otherwise.
     */
    private String readBoundOrNull(TomlTable table, String dottedKey)
    {
        String value = readStringOrInt(table, dottedKey);
        if (value.isEmpty())
        {
            return null;
        }
        return value;
    }

    /**
     * Read a value if present or set it to the default.
     *
     * @param table Current table to read in.
     * @return String representation of the value if present. Default value
     *         otherwise.
     */
    private String readBoundOrDefault(TomlTable table)
    {
        String value = readStringOrInt(table, KEY_DIMENSION_LB);
        if (value.isEmpty())
        {
            return DEFAULT_LOWER_BOUND;
        }
        return value;
    }

    /**
     * Read a value in the TOML file that can be a String or an Integer.
     *
     * @param table     Current table to read in.
     * @param dottedKey Dotted key to get the value.
     * @return String representation of the actual value if present. Empty string
     *         otherwise.
     */
    private String readStringOrInt(TomlTable table, String dottedKey)
    {
        Object value = table.get(dottedKey);
        if (value == null)
        {
            return "";
        }
        if (value instanceof String)
        {
            return (String) value;
        }
        return String.valueOf(value);
    }

    /**
     * Return the model name defined in the configuration file.
     *
     * @return String value representing the model name.
     */
    public String getName()
    {
        return _modelName;
    }

    /**
     * Get a dimension by its identifier.
     *
     * @param id Dimension identifier.
     * @return The named dimension if present. Null otherwise.
     */
    public DimensionDefinition getDimension(String id)
    {
        if (id == null)
        {
            return null;
        }
        if (_dimensions.containsKey(id.toLowerCase()))
        {
            return _dimensions.get(id.toLowerCase());
        }
        return null;
    }

    /**
     * Check if a dimension is available from its identifier.
     *
     * @param id Identifier of the dimension to query.
     * @return True if the dimension is available. False otherwise.
     */
    public boolean hasDimension(String id)
    {
        return id != null && _dimensions.containsKey(id.toLowerCase());
    }

    /**
     * Add a dimension in the model configuration.
     *
     * @param d Dimension to add.
     */
    public void putDimension(DimensionDefinition d)
    {
        if (d != null)
        {
            _dimensions.put(d.getIdentifier().toLowerCase(), d);
        }
    }

    /**
     * Get the number of dimensions defined in the configuration.
     *
     * @return Number of dimensions.
     */
    public int getNbDimensions()
    {
        return _dimensions.size();
    }

    /**
     * Check whether a layout is available.
     *
     * @param id Id of the layout to query.
     * @return True if the layout is available. False otherwise.
     */
    public boolean hasLayout(String id)
    {
        return id != null && _layouts.containsKey(id.toLowerCase());
    }

    /**
     * Add a layout in the model configuration. If dimensions in layout are not
     * present, they are automatically added.
     *
     * @param id         Unique identifier of the layout.
     * @param dimensions List of dimensions composing the layout.
     */
    public void putLayout(String id, List<DimensionDefinition> dimensions)
    {
        _layouts.put(id.toLowerCase(), dimensions);
        for (DimensionDefinition dim : dimensions)
        {
            if (!_dimensions.containsKey(dim.getIdentifier()))
            {
                putDimension(dim);
            }
        }
    }

    /**
     * Add a default layout in the model configuration.
     *
     * @param dimensions List of dimensions composing the layout.
     */
    public void putDefaultLayout(List<DimensionDefinition> dimensions)
    {
        putLayout(DEFAULT_LAYOUT_ID, dimensions);
    }

    /**
     * Get a layout by its identifier.
     *
     * @param id Layout identifier.
     * @return The named layout if present. Empty list otherwise.
     */
    public List<DimensionDefinition> getLayout(String id)
    {
        if (id == null)
        {
            return Collections.emptyList();
        }
        if (_layouts.containsKey(id.toLowerCase()))
        {
            return _layouts.get(id.toLowerCase());
        }
        return Collections.emptyList();
    }

    /**
     * Get the default layout. Defined with "default" identifier in the
     * configuration.
     *
     * @return The default layout information.
     */
    public List<DimensionDefinition> getDefaultLayout()
    {
        return getLayout(DEFAULT_LAYOUT_ID);
    }

    /**
     * Generate the default layout when not present. Used for in directive dimension
     * definition.
     */
    public void generateDefaultLayout()
    {
        if (!_dimensions.isEmpty() && !hasLayout(DEFAULT_LAYOUT_ID))
        {
            putLayout(DEFAULT_LAYOUT_ID, new ArrayList<>(_dimensions.values()));
        }
    }

    /**
     * Get the number of layouts defined in the configuration.
     *
     * @return Number of layout.
     */
    public int getNbLayouts()
    {
        return _layouts.size();
    }

    /**
     * Check if the model configuration has been loaded.
     *
     * @return True if the model configuration is loaded. False otherwise.
     */
    public boolean isLoaded()
    {
        return _isLoaded;
    }

}
