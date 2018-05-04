module Extractor

open FSharp.Data
open FSharp.Data.JsonExtensions

let requestHsSearch hsCode =
    printfn "Processing hs %d" hsCode
    Http.RequestString(
        "http://pakistanexportersdirectory.gov.pk/includes/retrieve-Hscode.php",
        httpMethod = "POST",
        body = FormValues [ "hscode", hsCode |> sprintf "%02d"; "HsLikeEx", "Like" ])

let getHtmlDocuments (response: string) =
    match response.Length with
    | 0 -> [||]
    | _ ->
        let json = JsonValue.Parse(response)
        let data = json?data
        data.AsArray()
        |> Array.map ((fun item -> item.[4]) >> (fun html -> html.ToString()))

let extract (html: string) =
    match html.Length with
    | 0 -> ""
    | _ ->
        let doc = HtmlDocument.Parse(html)
        let spans = 
            doc.Descendants ["span"]
            |> Seq.map (fun e -> e.TryGetAttribute("id"), e.InnerText())
            |> Seq.filter (fun (id, _) -> id.IsSome)
            |> Seq.map (fun (id, name) -> id.Value.Value().Replace(@"\""", ""), name)
        
        let (_, com) = spans |> Seq.find (fun (id, _) -> id.StartsWith("com"))
        let (_, proDesc) = spans |> Seq.find (fun (id, _) -> id.StartsWith("proDesc"))
        let (_, proCode) = spans |> Seq.find (fun (id, _) -> id.StartsWith("proCode"))
        let (_, email) = spans |> Seq.find (fun (id, _) -> id.StartsWith("email"))
        let (_, phone) = spans |> Seq.find (fun (id, _) -> id.StartsWith("phone"))
        let (_, ntn) = spans |> Seq.find (fun (id, _) -> id.StartsWith("ntn"))
        let (_, add) = spans |> Seq.find (fun (id, _) -> id.StartsWith("add"))
        let (_, web) = spans |> Seq.find (fun (id, _) -> id.StartsWith("web"))
        let (_, contper) = spans |> Seq.find (fun (id, _) -> id.StartsWith("contper"))

        [|com; proDesc.Replace("-", "").Trim(); proCode; email; phone; ntn; add; web; contper|]
        |> Array.map (function
                        | value when value.Length > 0 -> sprintf "\"%s\"" value
                        | _ -> "")
        |> String.concat ","
