Next step on sprites:
How to handle non-standard sizes:

we will do bin-packing and generate a registry, this can then feed spriteSheetregion

when adding, the object will fill the span of tiles it's covered.
idea: horizontal and vertical percent (of tiles) for opaque and blocked.
We will need to account for making part of the image transparent for non-blocking parts when PC sprites walk behind them. Sigh.

For sprite grids we define a script to take either a list of indices or a start index + width and height, this will then join these into a single image that can be bin-packed.

Change sprites from:

type SpriteRef =
    | SheetRegion of SpriteSheetRegion // arbitrary pixel region on a sheet
    | SheetCell of SpriteSheetCell     // grid address on a registered sheet
    | SheetSpan of SpriteSheetSpan     // rectangular block of cells
    | SheetCells of SpriteSheetCell[]  // Ordered list for scrambled/irregular multi-cells
    | TextureId of int                 // maps to a loaded Texture2D managed externally
    | Scene of string   

to

type SpriteRef =
    | SheetRegion of SpriteSheetRegions // arbitrary pixel region on a sheet
    | SheetCell of SpriteSheetCell     // grid address on a registered sheet
    | NonTileSheetCell of NonTileSheetCell // sheet cell that has a grid cell size different from tile


Irregular lists are processed into single images. SheetSpans are also processed into single images. in some cases, they can be gathered into their own newly sized grid cells.

Example Registry Entry:

Name/ID: 5
Sheet: 2
Rect: (128, 0, 64, 96)
Metadata:
Blocked: (20% of width, 10% of height)
Opaque: (10% of width, 50% of height)
These are then discretized into the grid, where 10% becomes non blocking. Ie we need to account for how many tiles 10% of height covers and within each their %, which rounding decides if blocking.

Things to keep in mind:
- NonTileSheetCell fields given it's non-standard grids
- Pivot and anchor points.
- sheet region registry as id based approach