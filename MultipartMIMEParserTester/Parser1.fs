namespace MultipartMIMEParser

open FParsec
open System.IO

type private Post1 = { contentType : string; boundary : string; subtype : string; content : string }

module Parser1 =
  ()

type MParser1(s:Stream) =
  let ($) f x = f x
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
  let pContent = many anyChar |>> str // TODO: The content parser needs to be recursive on the stream.
  let pStream = pipe4 pContentType pBoundary pSubtype pContent
                      $ fun c b t s -> { contentType=c; boundary=b; subtype=t; content=s }
  let result s = match runParserOnStream pStream () "" s ascii with
                  | Success (r,_,_) -> r
                  | Failure (e,_,_) -> failwith (sprintf "%A" e)
  let r = result s
  member p.ContentType = r.contentType
  member p.Boundary = r.boundary
  member p.ContentSubtype = r.subtype
  member p.Content = r.content

// vim:ft=fs