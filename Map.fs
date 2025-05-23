namespace AspectGameEngine

//struct 
[<Struct>]
type TileImageLoc =
    val AtlasIndex: int
    val Row: int
    val Column: int
    new(atlasIndex, row, column) = { AtlasIndex = atlasIndex; Row = row; Column = column }

type Item = 
//struct record
[<Struct>] type Tile = { 
    

}

module Say =
    let hello name =
        printfn "Hello %s" name
