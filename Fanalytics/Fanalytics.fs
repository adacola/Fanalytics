namespace Adacola

open System
open System.Net
open Basis.Core

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Fanalytics =

    let private customCampaignValuesOrderList = ["utmcsr"; "utmccn"; "utmcmd"; "utmcct"; "utmctr"; "utmgclid"]

    let private toUnixTime =
        let unixEpoch = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
        fun (dateTime : DateTime) -> (dateTime.ToUniversalTime() - unixEpoch).TotalSeconds |> int64

    module Default =
        let getTimestamp() = DateTime.UtcNow

        let getUniqueID =
            let random = Random()
            fun () -> random.Next() |> uint32 |> string

        let customCampaignValues =
            [
                "utmcsr", "(direct)"
                "utmccn", "(direct)"
                "utmcmd", "(none)"
            ] |> Map.ofList

    type CreateAnalyticsCookiesArg = {
        Domain : string
        DomainHash : string
        RequestLimit : int option
        IncludesUtmc : bool
        Utmv : string option
        CustomCampaignValues : Map<string, string>
        Path : string
        SessionCount : int
        AccessCount : int
        CampaignCount : int
        OutboundCountdown : int
        FirstTimestamp : int64 option
        LastTimestamp : int64 option
        GetDateTime : unit -> DateTime
        GetUniqueID : unit -> string
    } with
        static member Default = {
            Domain = ""
            DomainHash = ""
            RequestLimit = None
            IncludesUtmc = false
            Utmv = None
            CustomCampaignValues = Default.customCampaignValues
            Path = "/"
            SessionCount = 1
            AccessCount = 1
            CampaignCount = 1
            OutboundCountdown = 10
            FirstTimestamp = None
            LastTimestamp = None
            GetDateTime = Default.getTimestamp
            GetUniqueID = Default.getUniqueID
        }
        static member Create(domain, domainHash, ?requestLimit, ?includesUtmc) =
            { CreateAnalyticsCookiesArg.Default with
                Domain = domain
                DomainHash = domainHash
                RequestLimit = requestLimit
                IncludesUtmc = includesUtmc |> Option.getOr false
            }

    /// <summary>
    /// Analytics関係のクッキーが格納されたCoockeCollectionを返します。
    /// </summary>
    /// <param name="argument">引数レコード</param>
    /// <returns>(Analytics関係のクッキーが格納されたCookieCollection, 次回呼び出し時にargに渡す値)</returns>
    let createAnalyticsCookies arg =
        let time = arg.GetDateTime()
        let timestamp = time |> toUnixTime
        let firstTimestamp = arg.FirstTimestamp |> Option.getOr timestamp
        let lastTimestamp = arg.LastTimestamp |> Option.getOr timestamp

        let utma =
            let value = sprintf "%s.%s.%d.%d.%d.%d" arg.DomainHash (arg.GetUniqueID()) firstTimestamp lastTimestamp timestamp arg.SessionCount
            let expires = time.AddYears 2
            Cookie("__utma", value, arg.Path, arg.Domain, Expires = expires) |> Some

        let utmb =
            let value = sprintf "%s.%d.%d.%d" arg.DomainHash arg.AccessCount arg.OutboundCountdown timestamp
            let expires = time.AddMinutes 30.
            Cookie("__utmb", value, arg.Path, arg.Domain, Expires = expires) |> Some

        let utmc =
            if arg.IncludesUtmc then
                let value = sprintf "%s" arg.DomainHash
                Cookie("__utmc", value, arg.Path, arg.Domain) |> Some
            else None

        let utmt = option {
            let! requestLimit = arg.RequestLimit
            let value = sprintf "%d" requestLimit
            let expires = time.AddMinutes 10.
            return Cookie("__utmt", value, arg.Path, arg.Domain, Expires = expires)
        }

        let utmv = option {
            let! value = arg.Utmv
            let expires = time.AddYears 2
            return Cookie("__utmv", value, arg.Path, arg.Domain, Expires = expires)
        }

        let utmz =
            let rec customCampaignValuesToString result orderList values =
                match orderList with
                | [] -> values |> Map.toSeq |> Seq.fold (fun rs v -> v::rs) result
                | order::rest ->
                    match Map.tryFind order values with
                    | Some v -> customCampaignValuesToString ((order, v)::result) rest (Map.remove order values)
                    | None -> customCampaignValuesToString result rest values
            let value =
                customCampaignValuesToString [] customCampaignValuesOrderList arg.CustomCampaignValues
                |> List.rev |> List.map ((<||) (sprintf "%s=%s")) |> String.concat "|"
                |> sprintf "%s.%d.%d.%d.%s" arg.DomainHash timestamp arg.SessionCount arg.CampaignCount
            let expires = time.AddMonths 6
            Cookie("__utmz", value, arg.Path, arg.Domain, Expires = expires) |> Some
   
        let cc = CookieCollection()
        [utma; utmb; utmc; utmt; utmv; utmz] |> List.iter (Option.iter cc.Add)
        let nextArg =
            { arg with
                FirstTimestamp = Some firstTimestamp
                LastTimestamp = Some timestamp
                AccessCount = arg.AccessCount + 1
            }
        cc, nextArg

    /// <summary>
    /// CookieContainerにAnalytics系のCookieを追加・更新します。
    /// </summary>
    /// <param name="argCreateAnalyticsCookies">Analytics系Cookieを作成する関数に渡す引数</param>
    /// <param name="cookieContainer">更新対象のCookieContainer</param>
    /// <returns>次回呼び出し時にargCreateAnalyticsCookiesに渡す値</returns>
    let updateCookieContainer argCreateAnalyticsCookies (cookieContainer : CookieContainer) =
        let nextCookies, nextArg = createAnalyticsCookies argCreateAnalyticsCookies
        cookieContainer.Add nextCookies
        nextArg
        
    type CookieContainerManager(?initialArg) =
        let mutable arg = initialArg |> Option.getOr CreateAnalyticsCookiesArg.Default
        member __.Update cookieContainer =
            let nextArg = updateCookieContainer arg cookieContainer
            arg <- nextArg
        member x.Create() =
            let cc = CookieContainer()
            x.Update cc
            cc
