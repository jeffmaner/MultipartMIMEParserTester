namespace MultipartMIMEParser

open FParsec
open System.IO

type Header1 = { name  : string
               ; value : string
               ; addl  : (string * string) list option }

type Content2 = Content2 of string
              | Post2 of Post2 list
and Post2 = { headers : Header1 list
            ; content : Content2 }

type UserState = { Boundary : string }
  with static member Default = { Boundary="" }

module internal P =
  let ($) f x = f x
  let undefined = failwith "Undefined." // TODO: Remove when no longer used.
  let ascii = System.Text.Encoding.ASCII // TODO: Move this if it is only used once.
  let str cs = System.String.Concat (cs:char list) // TODO: Move this if it is only used once.

  let runP p s = match runParserOnStream p UserState.Default "" s ascii with
                 | Success (r,_,_) -> r
                 | Failure (e,_,_) -> failwith (sprintf "%A" e)

  let blankField = parray 2 newline

  let delimited d e =
      let pEnd = preturn () .>> e
      let part = spaces
                 >>. (manyTill
                      $ noneOf d
                      $ (attempt (preturn () .>> pstring d)
                                  <|> pEnd)) |>> str
       in part .>>. part

  let delimited3 firstDelimiter secondDelimiter thirdDelimiter endMarker =
      delimited firstDelimiter endMarker
      .>>. opt (many (delimited secondDelimiter endMarker
                      >>. delimited thirdDelimiter endMarker))

  let isBoundary ((n:string),_) = n.ToLower() = "boundary"

  let pHeader =
      let makeHeader ((n,v),nvps) = { name=n; value=v; addl=nvps}
      let includesBoundary (h:Header1) = match h.addl with
                                         | Some xs -> xs |> List.exists isBoundary
                                         | None    -> false
      let setBoundary b = { Boundary=b }
       in delimited3 ":" ";" "=" blankField
          |>> makeHeader
          >>= fun header stream -> if includesBoundary header
                                   then
                                     stream.UserState <- setBoundary (header.addl.Value
                                                                      |> List.find isBoundary
                                                                      |> snd)
                                     Reply ()
                                   else Reply ()

  let pHeaders = manyTill pHeader $ attempt (preturn () .>> blankField)

  let rec pContent2 (stream:CharStream<UserState>) =
      match stream.UserState.Boundary with
      | "" -> // Content is text.
              let nl = System.Environment.NewLine
              let unlines (ss:string list) = System.String.Join (nl,ss)
              let line = restOfLine false
              let lines = manyTill line $ attempt (preturn () .>> blankField)
               in pipe2 pHeaders lines
                        $ fun h c -> { headers=h
                                     ; content=Content2 $ unlines c }
      | _  -> // Content contains boundaries.
              let b = "--" + stream.UserState.Boundary
              let p = pipe2 pHeaders pContent2 $ fun h c -> { headers=h; content=c }
               in skipString b
                  >>. manyTill p (attempt (preturn () .>> blankField))

  let pStream = runP (pipe2 pHeaders pContent2 $ fun h c -> { headers=h; content=c })

  let rec pContent3 (stream:CharStream<UserState>) =
      match stream.UserState.Boundary with
      | "" -> // Content is text.
              let nl = System.Environment.NewLine
              let unlines (ss:string list) = System.String.Join (nl,ss)
              let line = restOfLine false
              let lines = manyTill line $ attempt (preturn () .>> blankField)
               in pipe2 pHeaders lines
                        $ fun h c -> [{ headers=h
                                      ; content=Content2 $ unlines c }]
      | _  -> // Content contains boundaries.
              let b = "--" + stream.UserState.Boundary
              let p = pipe2 pHeaders (fun stream -> Reply (pContent3 stream)) $ fun h c -> { headers=h; content=Post2 $ List.map runP c }
               in skipString b
                  >>. manyTill p (attempt (preturn () .>> blankField))

  let pStream2 = runP (pipe2 pHeaders (fun stream -> pContent3 stream) $ fun h c -> { headers=h; content=c })

  let pParts = pipe4 pPart1 pPart2 pPart3 pPart4 $ fun p1 p2 p3 p4 -> [p1;p2;p3;p4]
  let pMain = pipe2 pBody pParts $ fun b ps -> { headers=b; content=Post2 ps}
  let pStream3 = runP pMain

type MParser2 (s:Stream) =
  let r = P.pStream s

  let findHeader name =
    match r.headers |> List.tryFind (fun h -> h.name.ToLower() = name) with
    | Some h -> h.value
    | None   -> ""

  member p.Boundary =
    let header = r.headers
                 |> List.tryFind (fun h -> match h.addl with
                                           | Some xs -> xs |> List.exists P.isBoundary
                                           | None    -> false)
     in match header with
        | Some h -> h.addl.Value |> List.find P.isBoundary |> snd
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