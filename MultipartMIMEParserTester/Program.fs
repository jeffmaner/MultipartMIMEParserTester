open MultipartMIMEParser
open System.IO

[<EntryPoint>]
let main args =
    printfn "Given an input stream:"

    let path = @"C:\Users\jeff.maner\Documents\Projects\RosettaNet\"
    let file = "sampleRNIFReceiptAck.txt"
    //let file = "samplePart1.txt"
    let text = File.ReadAllText(path+file)
    use s = new FileStream(path+file, FileMode.Open)
    let parser = new MParser(s)

    printfn "when I ask for the entire stream as a string, I get it."
    printfn "%A" (parser.ToString() = text)

    printfn "when I ask for the length of the stream, I get it."
    printfn "%A" (parser.Length = text.Length)

    printfn "when I ask for the post, I get it."
    printfn "%A" (parser.Post.postHeaders)
    printfn "%A" (parser.Post.postContent)

    printfn "when I ask for the boundary, I get it."
    printfn "%A" (parser.Boundary = "RN-Http-Body-Boundary")

    printfn "when I ask for the count of parts, I get one."
    printfn "%A" parser.Parts
    printfn "%A" (parser.Parts.Length = 1)

    System.Console.ReadKey() |> ignore
    0

// vim:ft=fs