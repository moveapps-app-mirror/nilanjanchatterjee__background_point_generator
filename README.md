# Background Point Generator 

MoveApps

Github repository: https://github.com/nilanjanchatterjee/Background_point_generator

## Description

This app allows users to generate background points that can the be passed to MoveApps' resource selection function (RSF) App. For RSFs, *observed* points are the locations where the animal(s) were detected and *background* points are randomly generated locations located within a given extent.


## Documentation

This App is rather simple and performs no analysis. It simply generates the background locations that will be used, along with the observed locations, to fit a resource selection function (RSF) using MoveApps' Basic RSF App. 

### Application scope

#### Generality of App usability

This app was developed and tested using a terrestrial system, but should work for any ecosystem.

#### Required data properties

The App should work for any kind of (location) data. The greater the ratio of observed versus background points, the longer the app will take to run.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
`observed_background_points.png`: a basic plot containing the locations of observed and background points. This should just give you an idea of how many background points were generated and where they occurred.

### Settings 

*Scale (`scale`):* Specify whether you want to generate the background locations at the population or individual scale. This selection should be consistent with the scale at which you intend to fit the RSF. *Population* will generate the background points for the whole area used by the individuals and *Individual* will generate background points based on the range of each individual. Default value is *Population*.

*Ratio of used versus background points (`points_ratio`):* The ratio of observed versus background points that you want to use for the resource selection function. Default value is *10*.

**Extent Type (`extent_type`):** Specify where you want to generate background points for the resource selection function. *Bounding Box* will generate the background points for the whole bounding box rectangle area used by the individuals/population and *Minimum Convex Polygon* (MCP) will generate the background points in the MCP used by the individuals/population. If your scale is 'Population', you can also select *Custom Extent* and upload a shape file under the Custom Extent GPKG File setting. Default value is *Bounding Box*.

**Custom Extent GPKG File (`custom_extent_file`):** Upload your local GPKG file (ending in .gpkg) containing a polygon that defines the area in which you want the background points to be generated. The polygon will be projected into the projection of the input move2 object. The custom extent is only available if scale is "Population".

**Extent Buffer (`extent_buffer`):** Provide an additional buffer to extend the extent selected above. The buffer will be applied in the map units present in the move2 data object (typically meters). The default value is 0, i.e., no buffer.

**Custom Mask GPKG File (`custom_mask_file`):** Upload your local GPKG file (ending in .gpkg) containing the polygons in which you do NOT want the app to generate background points. The polygons should indicate known non-habitat areas. Note that for now observed points within the mask areas will still be preserved. A custom mask can be used with any scale and extent type.



### Changes in output data
The output data is the original input data combined with the background points data. There will be a new column, `case` (value 1 or 0), that indicates whether the point is observed or background.


### Most common errors

- There is an uploaded custom extent and the scale is not set to "Population".
- There is an uploaded custom extent and the extent type is not set to  "Custom Extent".

### Null or error handling

If a custom extent is uploaded but the scale is set to "Individual", the app will log an error and return nothing. If the custom extent does not overlap with the observed data at all, you will be able to tell this is happening by looking at the plot in `observed_background_points.png`.