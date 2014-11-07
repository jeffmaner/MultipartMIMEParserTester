namespace MultipartMIMEParser

open FParsec
open System.IO

type Header  = { headerName   : string
               ; headerValue  : string
               ; headerParams : (string * string) list }

type Headers = Header list
type Content = string

type Post = { postHeaders : Headers
            ; postContent : Content }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MParserAncillary =
  let private ($) f x = f x

  let internal readStream s =
      use r = new StreamReader(s:Stream)
       in r.ReadToEnd()

  let private splitLine c = spaces >>. (sepBy $ manyChars (noneOf (c+"\r\n")) $ pstring c)

  let private splitParams = splitLine ";"

  let private runP p s m =
      match run p s with
      | Success (r,_,_) -> r
      | Failure (e,_,_) -> failwith $ sprintf m e

  let private parseHeaderParams s =
      let makeParams xs =
          match xs with
          | (n::v::[]) -> (n,v)
          | _          -> failwith $ sprintf "Failure to parse header parameter: %A." xs
      let parseParams = splitLine "=" |>> makeParams
      let splitParams = splitParams |>> List.tail
       in runP splitParams s "Failure to parse header parameters: %s."
          |> List.map (fun x -> runP parseParams x "%A.")
          |> List.map (fun (n,v) -> (n,v.Trim('"')))

  let private chopHeaderParams s =
      let splitParams = splitParams |>> List.head
       in runP splitParams s "Failure to chop header parameters: %s."

  let internal parsePost s =
      let blankField = parray 2 newline
      let makeHeader xs =
          match xs with
          | (n::v::[]) -> { headerName=n; headerValue=chopHeaderParams v; headerParams=parseHeaderParams v }
          | _          -> failwith $ sprintf "Failure to parse header name and value. %A" xs
      let pHeader = splitLine ":" |>> makeHeader
      let pHeaders = many1Till pHeader blankField
      let pContent = manyChars anyChar
      let pp = pipe2 pHeaders pContent $ fun h c -> { postHeaders=h; postContent=c }
       in runP pp s "Failure to parse post: %s."

  let internal parseContent s b =
      let splitParts = spaces >>. (sepEndBy $ manyChars anyChar $ pstring ("--" + b)) .>> spaces
       in runP splitParts s "Failure to split content into parts on boundary: %s."

type MParser(s:Stream) =
  let text = MParserAncillary.readStream s
  let post = MParserAncillary.parsePost text
  let contentType = post.postHeaders |> List.find (fun h -> h.headerName.ToLower() = "content-type")
  let boundary = contentType.headerParams |> List.find (fun p -> (fst p).ToLower() = "boundary") |> snd
  member x.Boundary = boundary
  member x.ContentType = contentType.headerValue
  member x.Headers = post.postHeaders
  member x.Length = text.Length // N.b. Not strictly necessary. Just getting testing up and running.
  member x.Parts = MParserAncillary.parseContent post.postContent boundary
  member x.Post = post
  override x.ToString() = text

// vim:ft=fs