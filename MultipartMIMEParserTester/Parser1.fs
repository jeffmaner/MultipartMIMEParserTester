namespace MultipartMIMEParser

open FParsec
open System.IO

type Header1 = { name  : string
               ; value : string
               ; addl  : (string * string) list option }

type private Post1 = { contentType : string
                     ; boundary    : string
                     ; subtype     : string
                     ; content     : string }

type Content2 = Content2 of string
              | Post2 of Post2 list
and Post2 = { headers : Header1 list
            ; content : Content2 }

type UserState = { Boundary : string }
  with static member Default = { Boundary="" }

module internal P =
  let ($) f x = f x
  let undefined = failwith "Undefined."
  let ascii = System.Text.Encoding.ASCII
  let str cs = System.String.Concat (cs:char list)

  let makeHeader ((n,v),nvps) = { name=n; value=v; addl=nvps}

  let runP p s = match runParserOnStream p UserState.Default "" s ascii with
                 | Success (r,_,_) -> r
                 | Failure (e,_,_) -> failwith (sprintf "%A" e)

  let blankField = parray 2 newline

  let delimited d e =
      let pEnd = preturn () .>> e
      let part = spaces >>. (manyTill $ noneOf d $ (attempt (preturn () .>> pstring d) <|> pEnd)) |>> str
       in part .>>. part

  let delimited3 firstDelimiter secondDelimiter thirdDelimiter endMarker =
      delimited firstDelimiter endMarker
      .>>. opt (many (delimited secondDelimiter endMarker
                      >>. delimited thirdDelimiter endMarker))

  let pHeader =
      let includesBoundary s = undefined
      let setBoundary b = { Boundary=b }
       in delimited3 ":" ";" "=" blankField
          |>> makeHeader
          >>. fun stream -> if includesBoundary
                            then stream.UserState <- setBoundary b
                                 Reply ()
                            else Reply ()

  let pHeaders = manyTill pHeader $ attempt (preturn () .>> blankField)

  let rec pContent2 boundary =
      match boundary with
      | "" -> // Content is text.
              let line = restOfLine false
               in pipe2 pHeaders (manyTill line $ attempt (preturn () .>> blankField))
                  $ fun h c -> { headers=h
                               ; content=Content2 $ System.String.Join (System.Environment.NewLine,c) }
      | _  -> // Content contains boundaries.
              let b = "--"+boundary
              let p = pipe2 pHeaders (pContent2 b) $ fun h c -> { headers=h; content=c }
               in skipString b >>. manyTill p (attempt (preturn () .>> blankField))

  let pStream = runP (pipe2 pHeaders pContent2 $ fun h c -> { headers=h; content=c })


type MParser1(s:Stream) =
  let ($) f x = f x
  let undefined = failwith "Undefined."
  let ascii = System.Text.Encoding.ASCII
  let str cs = System.String.Concat (cs:char list)
  let q = "\""
  let qP = pstring q
  let pSemicolon = pstring ";"
  let manyNoDoubleQuote = many $ noneOf q
  let enquoted = between qP qP manyNoDoubleQuote |>> str
  let skip = skipStringCI
  let pContentType = skip "content-type: "
                     >>. manyTill anyChar (attempt $ preturn () .>> pSemicolon)
                     |>> str
  let pBoundary = skip " boundary=" >>. enquoted
  let pSubtype = opt $ pSemicolon >>. skip " type=" >>. enquoted
  let pContent = many anyChar |>> str // TODO: The content parser needs to recurse on the stream.
  let pStream = pipe4 pContentType pBoundary pSubtype pContent
                      $ fun c b t s -> { contentType=c; boundary=b; subtype=t; content=s }
  let result s = P.runP pStream s
  let r = result s
  member p.ContentType = r.contentType
  member p.Boundary = r.boundary
  member p.ContentSubtype = r.subtype
  member p.Content = r.content

type MParser2 (s:Stream) =
  let r = P.pStream s

  let findHeader name =
    match r.headers |> List.tryFind (fun h -> h.name.ToLower() = name) with
    | Some h -> h.value
    | None   -> ""

  member p.Boundary =
    let isBoundary ((s:string),_) = s.ToLower() = "boundary"
    let header = r.headers
                 |> List.tryFind (fun h -> if h.addl.IsSome
                                           then h.addl.Value |> List.exists isBoundary
                                           else false)
     in match header with
        | Some h -> h.addl.Value |> List.find isBoundary |> snd
        | None   -> ""
  member p.ContentID = findHeader "content-id"
  member p.ContentLocation = findHeader "content-location"
  member p.ContentSubtype = findHeader "type"
  member p.ContentTransferEncoding = findHeader "content-transfer-encoding"
  member p.ContentType = findHeader "content-type"
  member p.Content = r.content
  member p.Headers = r.headers
  member p.MessageID = findHeader "message-id"
  member p.MimeVersion = findHeader "mime-version"

// vim:ft=fs