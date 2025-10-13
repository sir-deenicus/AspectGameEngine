namespace AspectGameEngine.Localization

open System.IO
open System
open System.Globalization
open System.Collections.Generic
open System.Text

type Key = string

[<Struct>]
type Placeholder = { Name: string; Format: string voption }

[<Struct>]
type Piece =
    | Text of text: string
    | Hole of placeholder: Placeholder

type SimpleMessage = Piece list

[<Struct>]
type VariantLabel =
    | Exact of value: int
    | Category of name: string

[<Struct>]
type Variant = { Label: VariantLabel; Message: SimpleMessage }

[<Struct>]
type Message =
    | Simple of pieces: SimpleMessage
    | Plural of selector: string * variants: Variant array
    | Select of selector: string * variants: Variant array
    | Alias of target: Key

[<Struct>]
type Entry = { Key: Key; Msg: Message; Note: string voption }

[<Struct>]
type Meta =
    { Locale: string voption
      Fallback: string array
      Props: IReadOnlyDictionary<string, string> }

[<Struct>]
type AglFile =
    { Meta: Meta
      Entries: Entry array }

[<Struct>]
type PackEntry =
    { Key: Key
      Msg: Message }

[<Struct>]
type AglPack =
    { Locale: string
      Keys: string array
      Index: IReadOnlyDictionary<string, int>
      Messages: Message array }


module private Str =
    let isIdentChar (c: char) =
        Char.IsLetterOrDigit c || c = '_' || c = '.' || c = '-' 

    let trimEndCR (s: string) =
        if s.Length > 0 && s[s.Length-1] = '\r' then s.Substring(0, s.Length-1) else s

type private Cursor(text: string) =
    let mutable i = 0
    let len = text.Length

    member _.At = i
    member _.Eof = i >= len
    member _.Peek() = if i < len then text[i] else '\u0000'
    member _.Peek2() = if i+1 < len then text[i+1] else '\u0000'
    member _.Peek3() = if i+2 < len then text[i+2] else '\u0000'
    member _.Take() =
        let c = text[i]
        i <- i + 1
        c

    member _.StartsWith(str: string) =
        if i + str.Length > len then false
        else
            let slice = text.AsSpan(i, str.Length)
            slice.SequenceEqual(str.AsSpan())
    member this.SkipWhile(pred) =
        while not this.Eof && pred (this.Peek()) do
            this.Take() |> ignore
    member this.SkipSpaces() =
        this.SkipWhile(fun c -> c = ' ' || c = '\t')
    member this.SkipSpacesAndTabs() = this.SkipSpaces()
    member this.SkipWhitespace() =
        this.SkipWhile(Char.IsWhiteSpace)
    member this.SkipLine() =
        while not this.Eof && this.Peek() <> '\n' do this.Take() |> ignore
        if not this.Eof && this.Peek() = '\n' then this.Take() |> ignore
    member this.Expect(ch: char) =
        let c = this.Take()
        if c <> ch then failwithf "Expected '%c' but got '%c' at %d" ch c i
    member this.TryTake(ch: char) =
        if not this.Eof && this.Peek() = ch then this.Take() |> ignore; true else false

module private Parse =

    let parseQuoted (cur: Cursor) : string =
        if not (cur.Peek() = '"') then failwith "parseQuoted: expected opening quote"

        // Triple-quoted string
        if cur.StartsWith("\"\"\"") then
            cur.Take() |> ignore
            cur.Take() |> ignore
            cur.Take() |> ignore
            let sb = StringBuilder()
            let mutable completed = false
            while not completed do
                if cur.Eof then failwith "Unterminated triple-quoted string"
                if cur.StartsWith("\"\"\"") then
                    cur.Take() |> ignore
                    cur.Take() |> ignore
                    cur.Take() |> ignore
                    completed <- true
                else
                    sb.Append(cur.Take()) |> ignore
            sb.ToString()
        else
            // Normal quoted string with escapes
            cur.Expect('"')
            let sb = StringBuilder()
            let mutable completed = false
            while not completed do
                if cur.Eof then failwith "Unterminated string"
                let c = cur.Take()
                match c with
                | '\\' ->
                    if cur.Eof then failwith "Unterminated escape sequence"
                    let n = cur.Take()
                    match n with
                    | 'n' -> sb.Append('\n') |> ignore
                    | 'r' -> sb.Append('\r') |> ignore
                    | 't' -> sb.Append('\t') |> ignore
                    | '"' -> sb.Append('"') |> ignore
                    | '\\' -> sb.Append('\\') |> ignore
                    | '{' -> sb.Append('{') |> ignore
                    | '}' -> sb.Append('}') |> ignore
                    | other -> sb.Append(other) |> ignore
                | '"' -> completed <- true
                | other -> sb.Append(other) |> ignore
            sb.ToString()

    let parseIdentifier (cur: Cursor) =
        let sb = StringBuilder()
        while not cur.Eof && Str.isIdentChar (cur.Peek()) do
            sb.Append(cur.Take()) |> ignore
        if sb.Length = 0 then failwith "Expected identifier"
        sb.ToString()

    let parseKey (cur: Cursor) = parseIdentifier cur

    let parseVariantLabel (cur: Cursor) : VariantLabel =
        if cur.TryTake('=') then
            // exact number
            let sb = StringBuilder()
            let mutable neg = false
            if cur.TryTake('-') then neg <- true
            while not cur.Eof && Char.IsDigit(cur.Peek()) do
                sb.Append(cur.Take()) |> ignore
            if sb.Length = 0 then failwith "Expected number after '='"
            let v = Int32.Parse(sb.ToString(), CultureInfo.InvariantCulture)
            Exact(if neg then -v else v)
        else
            Category (parseIdentifier cur)

    let parsePieces (text: string) : SimpleMessage =
        // Parse {name[:format]} with backslash escapes for { and }
        let s = text.AsSpan()
        let sb = StringBuilder()
        let pieces = ResizeArray<Piece>()
        let mutable i = 0
        let inline flushText () =
            if sb.Length > 0 then
                pieces.Add(Text(sb.ToString()))
                sb.Clear() |> ignore
        while i < s.Length do
            let c = s[i]
            if c = '\\' then
                if i+1 < s.Length then
                    let n = s[i+1]
                    match n with
                    | '{' | '}' | '\\' | '"' ->
                        sb.Append(n) |> ignore; i <- i + 2
                    | 'n' -> sb.Append('\n') |> ignore; i <- i + 2
                    | 'r' -> sb.Append('\r') |> ignore; i <- i + 2
                    | 't' -> sb.Append('\t') |> ignore; i <- i + 2
                    | _ -> sb.Append(n) |> ignore; i <- i + 2
                else
                    sb.Append(c) |> ignore; i <- i + 1
            elif c = '{' then
                // parse placeholder
                flushText()
                i <- i + 1
                let start = i
                let mutable seen = false
                while i < s.Length && s[i] <> '}' do
                    if s[i] = '\n' || s[i] = '\r' then failwith "Unterminated placeholder"
                    i <- i + 1
                    seen <- true
                if not seen || i >= s.Length then failwith "Unterminated placeholder"
                let inner = s.Slice(start, i - start).ToString()
                i <- i + 1 // consume '}'
                let name, fmt =
                    match inner.IndexOf(':') with
                    | -1 -> inner, ValueNone
                    | p ->
                        let n = inner.Substring(0, p)
                        let f = inner.Substring(p+1)
                        n.Trim(), ValueSome(f.Trim())
                if String.IsNullOrWhiteSpace(name) then failwith "Empty placeholder name"
                pieces.Add(Hole { Name = name; Format = fmt })
            else
                sb.Append(c) |> ignore; i <- i + 1
        flushText()
        pieces |> List.ofSeq

    let parseMeta (lines: string array) (startIdx: int) =
        let dict = Dictionary<string,string>(StringComparer.OrdinalIgnoreCase)
        let mutable i = startIdx
        let mutable locale = ValueNone
        let fallbacks = ResizeArray<string>()
        let mutable finished = false
        while i < lines.Length && not finished do
            let line = Str.trimEndCR (lines[i].Trim())
            if line.StartsWith("[") then
                // do not consume the '[' line; stop parsing meta
                finished <- true
            elif line.StartsWith("#") then
                i <- i + 1
            elif String.IsNullOrWhiteSpace(line) then
                finished <- true
            else
                let eq = line.IndexOf('=')
                if eq < 0 then failwith "[meta] expected key = value"
                let key = line.Substring(0, eq).Trim()
                let value = line.Substring(eq+1).Trim()
                if value.StartsWith("[") then
                    // array
                    let inner = value.Trim().TrimStart('[').TrimEnd(']')
                    for part in inner.Split(',', StringSplitOptions.RemoveEmptyEntries) do
                        let v = part.Trim().Trim('"')
                        if key.Equals("fallback", StringComparison.OrdinalIgnoreCase) then fallbacks.Add(v)
                        else dict[key] <- v
                else
                    let v =
                        if value.StartsWith("\"") then
                            let cur = Cursor(value)
                            parseQuoted cur
                        else value.Trim('"')
                    if key.Equals("locale", StringComparison.OrdinalIgnoreCase) then locale <- ValueSome v
                    elif key.Equals("fallback", StringComparison.OrdinalIgnoreCase) then () // handled above
                    dict[key] <- v
                i <- i + 1
        let meta : Meta =
            { Locale = locale
              Fallback = fallbacks.ToArray()
              Props = dict :> IReadOnlyDictionary<_,_> }
        meta, i

    let parse (text: string) : AglFile =
        let lines = text.Replace("\r\n", "\n").Split('\n')
        let entries = ResizeArray<Entry>()
        let mutable i = 0
        let mutable pendingNote: string voption = ValueNone
        let mutable meta: Meta =
            { Locale = ValueNone; 
              Fallback = Array.empty; 
              Props = Dictionary<string,string>(StringComparer.OrdinalIgnoreCase) :> IReadOnlyDictionary<string,string> }

        while i < lines.Length do
            let raw = lines[i]
            let line = Str.trimEndCR (raw.Trim())
            // Skip empty or comments
            if String.IsNullOrWhiteSpace(line) || line.StartsWith("#") then
                i <- i + 1
            elif line.StartsWith("///") then
                // dev note, capture until next non-empty
                let note = line.Substring(3).Trim()
                pendingNote <- ValueSome note
                i <- i + 1
            elif line.StartsWith("[meta]") then
                let m, nextIdx = parseMeta lines (i+1)
                meta <- m
                i <- nextIdx
            else
                // Accumulate logical entry (handle blocks and triple quotes)
                let countTriples (s: string) =
                    let mutable count = 0
                    let mutable idx = s.IndexOf("\"\"\"", StringComparison.Ordinal)
                    while idx >= 0 do
                        count <- count + 1
                        idx <- s.IndexOf("\"\"\"", idx + 3, StringComparison.Ordinal)
                    count
                let mutable acc = StringBuilder()
                acc.Append(line) |> ignore
                let mutable inTriple = (countTriples line) % 2 = 1
                // If line contains an opening '{' without '}', keep appending until we close,
                // or while inside a triple-quoted string.
                let opens = line.ToCharArray() |> Array.filter ((=) '{') |> Array.length
                let closes = line.ToCharArray() |> Array.filter ((=) '}') |> Array.length
                let mutable openCount = opens - closes
                while (openCount > 0 || inTriple) && i + 1 < lines.Length do
                    i <- i + 1
                    let ln = Str.trimEndCR lines[i]
                    acc.AppendLine() |> ignore
                    acc.Append(ln) |> ignore
                    let o = ln.ToCharArray() |> Array.filter ((=) '{') |> Array.length
                    let c = ln.ToCharArray() |> Array.filter ((=) '}') |> Array.length
                    openCount <- openCount + o - c
                    if (countTriples ln) % 2 = 1 then
                        inTriple <- not inTriple
                let entryText = acc.ToString().Trim()
                // Parse the entryText
                let cur = Cursor(entryText)
                // key
                let key = parseKey cur
                cur.SkipSpacesAndTabs()
                if cur.TryTake('(') then
                    // block: key(selector) { variants }
                    cur.SkipSpaces()
                    let selector = parseIdentifier cur
                    cur.SkipSpaces()
                    cur.Expect(')')
                    cur.SkipSpaces()
                    cur.Expect('{')
                    // variants
                    let vars = ResizeArray<Variant>()
                    let mutable needOther = true
                    let mutable completed = false
                    while not cur.Eof && not completed do
                        cur.SkipWhitespace()
                        if cur.TryTake('}') then
                            completed <- true
                        else
                            // label :
                            let label = parseVariantLabel cur 
                            cur.SkipWhitespace()
                            cur.Expect(':')
                            cur.SkipWhitespace()
                            // value string
                            let msgText =
                                if cur.Peek() = '"' && cur.Peek2() = '"' && cur.Peek3() = '"' then parseQuoted cur
                                elif cur.Peek() = '"' then parseQuoted cur
                                else failwith "Expected quoted string for variant"
                            let pieces = parsePieces msgText
                            vars.Add({ Label = label; Message = pieces })
                            match label with
                            | Category c when c.Equals("other", StringComparison.OrdinalIgnoreCase) -> needOther <- false
                            | _ -> ()
                            // optional comma or newline whitespace
                            cur.SkipWhitespace()
                            cur.TryTake(',') |> ignore 
                            cur.SkipWhitespace()
                    if needOther then failwithf "Entry '%s' missing 'other' variant" key
                    let msg =
                        // Heuristic: numeric selector name implies plural
                        if selector.Equals("count", StringComparison.OrdinalIgnoreCase) then
                            Plural(selector, vars.ToArray())
                        else Select(selector, vars.ToArray())
                    entries.Add({ Key = key; Msg = msg; Note = pendingNote })
                    pendingNote <- ValueNone
                else
                    cur.SkipSpaces()
                    cur.Expect('=')
                    cur.SkipSpaces()
                    if cur.TryTake('@') then
                        // alias to other key
                        let target = parseKey cur
                        entries.Add({ Key = key; Msg = Alias target; Note = pendingNote })
                        pendingNote <- ValueNone
                    else
                        // simple quoted
                        let msgText =
                            if cur.Peek() = '"' && cur.Peek2() = '"' && cur.Peek3() = '"' then parseQuoted cur
                            elif cur.Peek() = '"' then parseQuoted cur
                            else failwith "Expected quoted string"
                        let pieces = parsePieces msgText
                        entries.Add({ Key = key; Msg = Simple pieces; Note = pendingNote })
                        pendingNote <- ValueNone

                i <- i + 1

        // check duplicates
        let dup =
            entries
            |> Seq.groupBy (fun e -> e.Key)
            |> Seq.tryFind (fun (_, g) -> Seq.length g > 1)
        match dup with
        | Some (k, _) -> failwithf "Duplicate key '%s'" k
        | None -> ()
        { Meta = meta; Entries = entries.ToArray() }

type AglParser =
    static member Parse(text: string) = Parse.parse text
    static member ParseFile(path: string) =
        let text = File.ReadAllText(path)
        Parse.parse text
 
module private Pack =
    let build (file: AglFile) : AglPack =
        let locale = file.Meta.Locale |> ValueOption.defaultValue "und"
        let keys = file.Entries |> Array.map (fun e -> e.Key)
        let dict = Dictionary<string,int>(keys.Length, StringComparer.Ordinal)
        for i = 0 to keys.Length - 1 do
            dict[keys[i]] <- i
        let messages = file.Entries |> Array.map (fun e -> e.Msg)
        { Locale = locale
          Keys = keys
          Index = dict :> IReadOnlyDictionary<_,_>
          Messages = messages }

module private ZigZag =
    let inline encode (value: int) =
        let u = uint32 value
        int ((u <<< 1) ^^^ (u >>> 31))

    let inline decode (value: int) =
        let u = uint32 value
        let shifted = int (u >>> 1)
        let sign = int (u &&& 1u)
        shifted ^^^ -sign

module private Bin =
    let encoding = Encoding.UTF8

    let write7 (bw: BinaryWriter) (value: int) =
        let mutable v = uint32 value
        while v >= 0x80u do
            let chunk = byte (int ((v &&& 0x7Fu) ||| 0x80u))
            bw.Write(chunk)
            v <- v >>> 7
        bw.Write(byte (int v))

    let read7 (br: BinaryReader) =
        let mutable shift = 0
        let mutable result = 0u
        let mutable cont = true
        while cont do
            let b = br.ReadByte()
            let payload = uint32 (int b &&& 0x7F)
            result <- result ||| (payload <<< shift)
            cont <- (int b &&& 0x80) <> 0
            shift <- shift + 7
        int result

    let writeString (bw: BinaryWriter) (value: string) =
        if String.IsNullOrEmpty(value) then
            write7 bw 0
        else
            let bytes = encoding.GetBytes(value)
            write7 bw (bytes.Length + 1)
            bw.Write(bytes)

    let readString (br: BinaryReader) =
        let len = read7 br
        if len = 0 then ""
        else
            let bytes = br.ReadBytes(len - 1)
            encoding.GetString(bytes)

    let writePieces (bw: BinaryWriter) (pieces: SimpleMessage) =
        let arr = pieces |> List.toArray
        write7 bw arr.Length
        for piece in arr do
            match piece with
            | Text text ->
                bw.Write(byte 0)
                writeString bw text
            | Hole placeholder ->
                bw.Write(byte 1)
                writeString bw placeholder.Name
                writeString bw (placeholder.Format |> ValueOption.defaultValue "")

    let readPieces (br: BinaryReader) : SimpleMessage =
        let count = read7 br
        let items = ResizeArray<Piece>(count)
        for _ = 1 to count do
            let tag = br.ReadByte()
            match tag with
            | 0uy ->
                let text = readString br
                items.Add(Text text)
            | 1uy ->
                let name = readString br
                let fmt = readString br
                let fmtOpt = if String.IsNullOrEmpty(fmt) then ValueNone else ValueSome fmt
                items.Add(Hole { Name = name; Format = fmtOpt })
            | _ -> failwith "Invalid piece tag"
        items |> List.ofSeq

    let writeVariants (bw: BinaryWriter) (variants: Variant array) =
        write7 bw variants.Length
        for variant in variants do
            match variant.Label with
            | Exact value ->
                bw.Write(byte 0)
                write7 bw (ZigZag.encode value)
            | Category name ->
                bw.Write(byte 1)
                writeString bw name
            writePieces bw variant.Message

    let readVariants (br: BinaryReader) =
        let count = read7 br
        Array.init count (fun _ ->
            let tag = br.ReadByte()
            let label =
                match tag with
                | 0uy ->
                    let encoded = read7 br
                    VariantLabel.Exact (ZigZag.decode encoded)
                | 1uy ->
                    let name = readString br
                    VariantLabel.Category name
                | _ -> failwith "Invalid variant label"
            let message = readPieces br
            { Label = label; Message = message })

type AglPacker =
    static member Build(file: AglFile) : AglPack =
        Pack.build file

    static member WriteBinary(pack: AglPack) : byte[] =
        use ms = new MemoryStream()
        use bw = new BinaryWriter(ms, Bin.encoding, true)
        bw.Write(Bin.encoding.GetBytes("AGLB1"))
        Bin.writeString bw pack.Locale
        Bin.write7 bw pack.Keys.Length
        for i = 0 to pack.Keys.Length - 1 do
            Bin.writeString bw pack.Keys[i]
            match pack.Messages[i] with
            | Simple pieces ->
                bw.Write(byte 0)
                Bin.writePieces bw pieces
            | Plural (selector, variants) ->
                bw.Write(byte 1)
                Bin.writeString bw selector
                Bin.writeVariants bw variants
            | Select (selector, variants) ->
                bw.Write(byte 2)
                Bin.writeString bw selector
                Bin.writeVariants bw variants
            | Alias target ->
                bw.Write(byte 3)
                Bin.writeString bw target
        bw.Flush()
        ms.ToArray()

    static member ReadBinary(bytes: byte[]) : AglPack =
        use ms = new MemoryStream(bytes)
        use br = new BinaryReader(ms, Bin.encoding, true)
        let magic = br.ReadBytes(5)
        if Bin.encoding.GetString(magic) <> "AGLB1" then
            failwith "Invalid AGLB1 header"
        let locale = Bin.readString br
        let count = Bin.read7 br
        let keys = Array.zeroCreate<string> count
        let messages = Array.zeroCreate<Message> count
        for i = 0 to count - 1 do
            keys[i] <- Bin.readString br
            let tag = br.ReadByte()
            messages[i] <-
                match tag with
                | 0uy ->
                    let pieces = Bin.readPieces br
                    Simple pieces
                | 1uy ->
                    let selector = Bin.readString br
                    let variants = Bin.readVariants br
                    Plural(selector, variants)
                | 2uy ->
                    let selector = Bin.readString br
                    let variants = Bin.readVariants br
                    Select(selector, variants)
                | 3uy ->
                    let target = Bin.readString br
                    Alias target
                | _ -> failwith "Invalid message tag"
        let dict = Dictionary<string,int>(count, StringComparer.Ordinal)
        for i = 0 to count - 1 do
            dict[keys[i]] <- i
        { Locale = locale
          Keys = keys
          Index = dict :> IReadOnlyDictionary<_,_>
          Messages = messages }


// ====================== Localizer (public runtime API) ======================

type Args = IReadOnlyDictionary<string, obj>

module private Args =
    let ofList (pairs: (string * obj) list) =
        let d = Dictionary<string,obj>(StringComparer.Ordinal)
        for (k,v) in pairs do d[k] <- v
        d :> IReadOnlyDictionary<_,_>

module private Render =
    let renderPieces (pieces: SimpleMessage) (args: Args voption) (fp: IFormatProvider) : string =
        let sb = StringBuilder()
        let tryGet (name: string) =
            match args with
            | ValueNone -> ValueNone
            | ValueSome a ->
                match a.TryGetValue name with
                | true, v -> ValueSome v
                | _ -> ValueNone
        for p in pieces do
            match p with
            | Text t -> sb.Append(t) |> ignore
            | Hole ph ->
                match tryGet ph.Name with
                | ValueSome v ->
                    match ph.Format, v with
                    | ValueSome fmt, (:? IFormattable as f) -> sb.Append(f.ToString(fmt, fp)) |> ignore
                    | ValueSome _, _ -> sb.Append(v.ToString()) |> ignore
                    | ValueNone, (:? IFormattable as f) -> sb.Append(f.ToString(null, fp)) |> ignore
                    | ValueNone, _ -> sb.Append(v.ToString()) |> ignore
                | ValueNone ->
                    // Keep placeholder visible for diagnostics
                    sb.Append('{').Append(ph.Name) |> ignore
                    match ph.Format with
                    | ValueSome fmt -> sb.Append(':').Append(fmt) |> ignore
                    | _ -> ()
                    sb.Append('}') |> ignore
        sb.ToString()

    let pickCategory (variants: Variant array) (cat: string) =
        let found =
            variants
            |> Array.tryFind (fun v ->
                match v.Label with
                | Category c -> c.Equals(cat, StringComparison.OrdinalIgnoreCase)
                | _ -> false)
        match found with
        | Some v -> Some v.Message
        | None ->
            variants
            |> Array.tryFind (fun v ->
                match v.Label with
                | Category c -> c.Equals("other", StringComparison.OrdinalIgnoreCase)
                | _ -> false)
            |> Option.map (fun v -> v.Message)

    let pickPlural (variants: Variant array) (n: int64) =
        // exact match first
        match variants |> Array.tryFind (fun v -> match v.Label with | Exact m -> int64 m = n | _ -> false) with
        | Some v -> Some v.Message
        | None ->
            // minimal English categories: zero/one/other
            let cat =
                if n = 0L then "zero"
                elif n = 1L then "one"
                else "other"
            pickCategory variants cat

type Localizer(pack: AglPack, ?fallbacks: AglPack array, ?formatProvider: IFormatProvider) =
    let fp = defaultArg formatProvider (CultureInfo.CurrentCulture :> IFormatProvider)
    let packs =
        match fallbacks with
        | Some arr when arr.Length > 0 -> Array.append [| pack |] arr
        | _ -> [| pack |]

    // Find a message by key across pack + fallbacks (in order)
    member private _.TryFindMessage(key: string) : struct (AglPack * int * Message) voption =
        let mutable res = ValueNone
        let mutable i = 0
        while i < packs.Length && ValueOption.isNone res do
            let p = packs[i]
            match p.Index.TryGetValue(key) with
            | true, idx ->
                res <- ValueSome (struct (p, idx, p.Messages[idx]))
            | _ -> ()
            i <- i + 1
        res

    // Resolve alias chain to a Simple message's pieces (iterative, cycle-safe)
    member private this.ResolveSimple(key: string) : SimpleMessage voption =
        let visited = HashSet<string>(StringComparer.Ordinal)
        let mutable current = key
        let mutable result : SimpleMessage voption = ValueNone
        let mutable complete = false
        while not complete do
            if not (visited.Add(current)) then
                // cycle detected
                complete <- true
            else
                match this.TryFindMessage(current) with
                | ValueSome (struct (_,_, Simple pieces)) ->
                    result <- ValueSome pieces
                    complete <- true
                | ValueSome (struct (_,_, Alias target)) ->
                    current <- target
                | _ ->
                    complete <- true
        result

    member _.Locale = pack.Locale

    // Simple retrieval without args
    member this.TryGet(key: string) : string voption =
        match this.ResolveSimple(key) with
        | ValueSome pieces -> ValueSome (Render.renderPieces pieces ValueNone fp)
        | _ -> ValueNone

    member this.Get(key: string) : string =
        this.TryGet(key) |> ValueOption.defaultValue key

    // Format with args for Simple messages (or alias to simple)
    member this.Format(key: string, args: IReadOnlyDictionary<string,obj>) : string =
        match this.ResolveSimple(key) with
        | ValueSome pieces -> Render.renderPieces pieces (ValueSome args) (CultureInfo.CurrentCulture)
        | _ -> key

    // Select variants (iterative alias resolution)
    member this.Select(key: string, label: string, args: IReadOnlyDictionary<string,obj>) : string =
        let visited = HashSet<string>(StringComparer.Ordinal)
        let mutable current = key
        let mutable output = key
        let mutable complete = false
        while not complete do
            if not (visited.Add(current)) then complete <- true
            else
                match this.TryFindMessage(current) with
                | ValueSome (struct (_,_, Select (_, vars))) ->
                    match Render.pickCategory vars label with
                    | Some msg ->
                        output <- Render.renderPieces msg (ValueSome args) (CultureInfo.CurrentCulture)
                        complete <- true
                    | None -> output <- current; complete <- true
                | ValueSome (struct (_,_, Alias target)) -> current <- target
                | _ -> output <- current; complete <- true
        output

    // Plural variants (iterative alias resolution; inject count)
    member this.Plural(key: string, count: int64, args: IReadOnlyDictionary<string,obj>) : string =
        let visited = HashSet<string>(StringComparer.Ordinal)
        let mutable current = key
        let mutable output = key
        let mutable complete = false
        while not complete do
            if not (visited.Add(current)) then complete <- true
            else
                match this.TryFindMessage(current) with
                | ValueSome (struct (_,_, Plural (_, vars))) ->
                    match Render.pickPlural vars count with
                    | Some msg ->
                        // build overlay with count; small fast path
                        let dict = Dictionary<string,obj>(args, StringComparer.Ordinal)
                        dict.["count"] <- box count
                        output <- Render.renderPieces msg (ValueSome dict) (CultureInfo.CurrentCulture)
                        complete <- true
                    | None -> output <- current; complete <- true
                | ValueSome (struct (_,_, Alias target)) -> current <- target
                | _ -> output <- current; complete <- true
        output


module Localization =
    // High-level helpers for common entry points

    // From .agl text
    let loadAgl (aglText: string) =
        let file = AglParser.Parse aglText
        let pack = AglPacker.Build file
        Localizer(pack)

    // From .agl file path
    let loadAglFile (path: string) =
        let text = File.ReadAllText(path)
        loadAgl text

    // From AGLB1 binary bytes
    let fromBinary (bytes: byte[]) =
        let pack = AglPacker.ReadBinary bytes
        Localizer(pack)

    // With fallback packs (in priority order after primary)
    let fromBinaryWithFallbacks (primary: byte[]) (fallbacks: byte[][]) =
        let p = AglPacker.ReadBinary primary
        let f = fallbacks |> Array.map AglPacker.ReadBinary
        Localizer(p, fallbacks = f)         