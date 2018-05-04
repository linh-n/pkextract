open Extractor

[<EntryPoint>]
let main _ =
    let lines = 
        [|1 .. 99|]
        |> Array.collect (requestHsSearch >> getHtmlDocuments)
        |> Array.map extract

    let wr = new System.IO.StreamWriter("data.csv")
    wr.WriteLine "com,proDesc,proCode,email,phone,ntn,add,web,contper"
    for line in lines do
        wr.WriteLine line

    0