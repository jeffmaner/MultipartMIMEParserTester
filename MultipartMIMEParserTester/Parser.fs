namespace MultipartMIMEParser

open FParsec
open System.IO

type Header  = { headerName   : string
               ; headerValue  : string
               ; headerParams : (string * string) list }

type Headers = Header list // TODO: Not strictly necessary.
(* type Content = Content of string
             | Post of Post
and Post = { postHeaders : Headers
           ; postContent : Content } *)
type Content = string // TODO: Not strictly necessary.
type Post = { postHeaders : Headers
            ; postContent : Content }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal MParserAncillary =
  let private ($) f x = f x

  let internal readStream s =
      use r = new StreamReader(s:Stream)
       in r.ReadToEnd()

  /// Splits line on character c.
  //let private splitLine c = spaces >>. (sepBy $ manyChars (noneOf (c+"\r\n")) $ pstring c)
  let private splitLine c e =
      let eol = preturn () .>> e
      let part = spaces >>. (manyTill $ noneOf c $ (attempt (preturn () .>> pstring c) <|> eol)) |>> System.String.Concat
       in many part

  let private splitParameters = splitLine ";" $ parray 2 newline

  /// Runs parser p on input s with failure message m.
  let private runP p s m =
      match run p s with
      | Success (r,_,_) -> r
      | Failure (e,_,_) -> failwith $ sprintf m e

  /// Returns all but the first of the header parameters.
  let private parseHeaderParams s =
      let makeParams xs =
          match xs with
          | (n::v::[]) -> (n,v)
          | [""]       -> ("","") // TODO: Is this necessary?
          | _          -> failwith $ sprintf "Failure to parse header parameter: %A." xs
      let parseParams = splitLine "=" newline |>> makeParams
      let splitParams = splitParameters |>> List.tail
       in runP splitParams s "Failure to parse header parameters: %s."
          |> List.map (fun x -> runP parseParams x "%A.")
          |> List.map (fun (n,v) -> (n,v.Trim('"'))) // TODO: Replace call to Trim() with between parser?

  /// Returns the first of the header parameters.
  let private chopHeaderParams s =
      let splitParams = splitParameters |>> List.head
       in runP splitParams s "Failure to chop header parameters: %s."

  /// Splits POST s into headers and post content.
  let internal parsePost s =
      let blankField = parray 2 newline
      let makeHeader xs =
          match xs with
          | (n::v::[]) -> { headerName=n; headerValue=chopHeaderParams v; headerParams=parseHeaderParams v }
          | _          -> failwith $ sprintf "Failure to parse header name and value. %A" xs
      let pHeader = splitLine ":" blankField |>> makeHeader
      let pHeaders = many1Till pHeader (attempt (preturn () .>> blankField))
      let pContent = manyChars anyChar
      let pp = pipe2 pHeaders pContent $ fun h c -> { postHeaders=h; postContent=c }
       in runP pp s "Failure to parse post: %s."

  /// Splits POST content s on boundary b.
  let internal parseContent s b =
      let str (cs:char list) = System.String.Concat cs
      let boundary = "--" + b
      let section = many1Till anyChar (attempt (preturn () .>> pstring boundary) <|> eof) |>> str
      let sections = skipString boundary >>. many section |>> List.map (fun s -> s.Trim())
       in runP sections s "Failure to split content into parts on boundary: %s."

/// Multipart POST parser.
type MParser(s:Stream) =
  // TODO: Pass stream to parsers instead of string.
  let text = MParserAncillary.readStream s
  let post = MParserAncillary.parsePost text
  let contentType = post.postHeaders |> List.find (fun h -> h.headerName.ToLower() = "content-type")
  let boundary = contentType.headerParams |> List.find (fun p -> (fst p).ToLower() = "boundary") |> snd
  let f s = use ms = new MemoryStream()
            use sw = new StreamWriter(ms)
            do sw.Write (s:string)
            do sw.Flush ()
            ms.Position <- 0L
            new MParser(ms)
  member x.Boundary = boundary
  member x.ContentType = contentType.headerValue
  member x.Headers = post.postHeaders
  member x.Length = text.Length // N.b. Not strictly necessary. Just getting testing up and running.
  member x.Parts = MParserAncillary.parseContent post.postContent boundary
                   |> List.filter (fun s -> not (s.StartsWith "--"))
                   |> List.map f
  member x.Post = post
  override x.ToString() = text

// vim:ft=fs