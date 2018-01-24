module Lambda.TwitterBot
open Lambda.Parser
open Lambda.Ast
open Lambda.Eval
open Lambda.Utils
open Lambda.Exception
open Lambda.Session

open System
open System.Reactive
open System.Threading
open System.Threading.Tasks
open System.Drawing
open System.Drawing.Imaging
open System.Reflection
open System.Diagnostics
open System.IO
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Control
open FSharp.Control.Reactive

open CoreTweet
open CoreTweet.Streaming
open System.Runtime.Serialization
open System.Text.RegularExpressions
open System.Xml
open Mono.Terminal
open ImageBuilder

type AsyncBuilder with
  member x.Bind(t:Task<'T>, f:'T -> Async<'R>) : Async<'R> = 
    async.Bind(Async.AwaitTask t, f)
end

type Microsoft.FSharp.Control.Async with
  static member AwaitFun (t : unit -> 'T, finalizer : 'T -> unit, timeout : int) =
    async {
      use cts = new CancellationTokenSource()
      let task = Task.Run t
      use timer = Task.Delay (timeout, cts.Token)
      let! completed = Async.AwaitTask <| Task.WhenAny(task, timer)
      if completed = (task :> Task) then
        do cts.Cancel ()
        let! result = Async.AwaitTask task
        return Some result
      else
        do task.ContinueWith(fun (x: Task<'T>) -> finalizer x.Result; x.Dispose()) |> ignore;
        return None
    }
end

[<Literal>]
let ownerId : int64 = 3054246138L
let p = Process.GetCurrentProcess ()
 
(*
let t =
  let x = new DataContractSerializer(typeof<Tokens>) in
  if(File.Exists("bot.xml")) then
    use y = XmlReader.Create("bot.xml") in
        x.ReadObject(y) :?> Tokens
  else
    let se = OAuth.Authorize("8brJ4ZmOc5fHxY89AcOLFdhy7", "3N1Mp6BjesoyLgbdiutksFalVXJ1UoTBiTOxNa0nBfaHJ0xUsB") in
    Console.WriteLine(se.AuthorizeUri);
    Console.Write("pin> ");
    let g = se.GetTokens(Console.ReadLine()) in
    let s = XmlWriterSettings() in
    s.Encoding <- System.Text.UTF8Encoding(false);
    use y = XmlWriter.Create("bot.xml", s) in
        x.WriteObject(y, g)
    g
*)

let t =
  let token = Environment.GetEnvironmentVariable "ACCESSTOKEN" in
  let secret = Environment.GetEnvironmentVariable "ACCESSSECRET" in
  Tokens.Create ("8brJ4ZmOc5fHxY89AcOLFdhy7", "3N1Mp6BjesoyLgbdiutksFalVXJ1UoTBiTOxNa0nBfaHJ0xUsB", token, secret)

let me = t.Account.VerifyCredentials()

let hash () = 
  let hashbase = [|'a'..'z'|] in
  let rand = Random () in
  List.map (fun _ -> hashbase.[rand.Next(hashbase.Length)]) [1..3] |> String.Concat |> sprintf "[%s]"

let report s =
  cprintfn ConsoleColor.Blue "report: %s" s
  t.DirectMessages.NewAsync(ownerId, text = s)

let createText name msg img =
  let trial = sprintf "%s %s %s" name msg (hash ()) in
  let ic = match img with Some (RenderedImages xs) -> List.length xs | _ -> 0 in
  let count = TwitterText.count trial + 24 * ic in
  if count <= TwitterText.limit then
    trial
  else
    match img with
      | Some (RenderedImages _) ->
        sprintf "%s (image) %s" name (hash())
      | Some TooBig ->
        sprintf "%s Error: too big result to show. %s" name (hash())
      | None ->
        let n = (msg.Length - count + TwitterText.limit - 10) in
        let s = sprintf "%s %s %s" name (msg.[..n]) (hash()) in
        s

let work (session: Session) s =
  try match s with
      | Regex @".*@b_rdct\s*((?:\s-noimage|\s-ni)?)\s+(.+)\s+:b=\s+(.+)" [b; n; t] ->
        match (session.parse t |> session.eval) with
          | (i, Some res) ->
            let s = sprintf "%s :b= %s" n (to_s res) in
            if (String.IsNullOrEmpty b) then 
              (session.defMeta n res, s, None)
            else
              (session.defMeta n res, s, Some i)
          | (i, None) -> (session, "Infinite reduction", Some i)
      | Regex @".*@b_rdct\s+([^ ]+)\s*:=\s*(.+)" [n; t] ->
        let e = session.parse t in
        (session.defMeta n e, sprintf "%s := %s" n (to_s e), None)
      | Regex @".*@b_rdct\s+(?:-showvars|-sv).*" [] ->
        let mutable ib = ImageBuilder.create 8 StringFormat.standard in
        use font = new Font ("STIX", float32 24) in 
        for (k, v) in session.dict |> Map.toSeq do
          ib <- ib |> iprintfn font Brushes.Black "&%s := %s" k (to_s v)
        done
        let i = ib |> ImageBuilder.render Color.White |> List.singleton |> RenderedImages in
        (session, "(image)", Some i)
      | Regex @".*@b_rdct\s*((?:\s-noimage|\s-ni)?)\s+(.*)" [b; t] ->
        match (session.parse t |> session.eval) with
          | (i, Some res) ->
            if (String.IsNullOrEmpty b) then 
              (session, to_s res, Some i)
            else
              (session, to_s res, None)
          | (i, None) -> (session, "Infinite reduction", Some i)
      | _ -> (session, "Wrong syntax", None)
  with
    | LambdaException (msg, i, _) -> (session, msg, i)
    | e -> (session, String.Format("Native error: {0}", e.Message), None)

let withTimeout seconds a f =
  Async.AwaitFun(a, f, seconds * 1000)

let reply (session: Session) (s: Status) =
  async {
    let! cr = withTimeout 30 
                          (fun () -> work session (s.Text.Replace("&amp;","&"))) 
                          (function 
                             | (_, _, Some (RenderedImages xs)) -> xs |> List.iter (fun x -> x.Dispose())
                             | _ -> ()
                          ) in
    let (newSession, msg, img) = 
      match cr with
        | Some res -> res
        | None -> (session, "Computation timeout", None)
    in
    let txt = createText ("@" + s.User.ScreenName) msg img in
    let id = Nullable s.Id in
    try
      let! imgIds =
        let upload i (img : Bitmap) = 
          async {
            let bs =
              use stream = new MemoryStream () in
              let (enc, eprms) = ImageHelper.genSaveParams ImageFormat.Jpeg [(Encoder.Quality, 0L)] in
              img.Save (stream, enc, eprms);
              cprintfn ConsoleColor.Green "[%i] --> %gMB" i (float stream.Length / 1000.0 / 1000.0);
              stream.ToArray ()
            in
            do img.Dispose ();
            let! m = t.Media.UploadAsync(media = bs) in
            return m.MediaId
          }
        in
        match img with
          | Some (RenderedImages imgs) ->
            imgs |> List.mapi upload |> Async.Parallel
          | _ -> async { return [| |] }
      in
      let! _ =
        if Array.isEmpty imgIds then
          t.Statuses.UpdateAsync(status=txt, in_reply_to_status_id=id)
        else
          t.Statuses.UpdateAsync(status=txt, in_reply_to_status_id=id, media_ids=imgIds)
      in ()
    with
      | :? AggregateException as e ->
        match (e.InnerExceptions.[0]) with
          | :? TwitterException as e ->
            t.Statuses.UpdateAsync(sprintf "@%s Twitter error: %s %s" s.User.ScreenName e.Message  (hash()), in_reply_to_status_id=id) |> ignore
          | e -> raise e
    do GC.Collect();
    return newSession
  }

let restart () =
  sprintf "notice: process restarting at %A" DateTime.Now |> report |> ignore;
  Process.Start(Assembly.GetEntryAssembly().Location) |> ignore;
  Environment.Exit(0)

let dm (session: Session) (d: DirectMessage) =
  async {
    match d.Text with
      | Regex @".*status.*" [] ->
        let msg =
          sprintf "info: running since %A (for %A). %i threads. %gMB RAM."
                  p.StartTime
                  (DateTime.Now - p.StartTime)
                  p.Threads.Count
                  (float p.PrivateMemorySize64 / 1000.0 / 1000.0)
        in
        report msg |> ignore
      | Regex @".*restart.*" [] -> restart ()
      | _ -> ()
    return session
  }

let rec start _session =
  let session = ref _session in
  sprintf "notice: connected at %A" DateTime.Now |> report |> ignore;
  let obs = t.Streaming.UserAsObservable() in
  let obsr = obs
                 //|> Observable.delaySubscription (TimeSpan.FromSeconds 1.0)
                 //|> Observable.retry
                 //|> Observable.catch <| obs
                 //|> Observable.repeat
                 |> Observable.publish in

  let inline sub x =
    match x with
      | Choice1Of2 newSession -> session := newSession
      | Choice2Of2 err -> to_s err |> report |> ignore
  in

  use replies =
    obsr |> Observable.choose (function :? StatusMessage as s -> Some s.Status | _ -> None)
         |> Observable.filter (fun x -> x.InReplyToUserId = me.Id && x.RetweetedStatus = null)
         |> Observable.flatmapTask (reply !session >> Async.Catch >> Async.StartAsTask)
         |> Observable.subscribe sub
  in
  use dms =
    obsr |> Observable.choose (function :? DirectMessageMessage as d -> Some d.DirectMessage | _ -> None)
         |> Observable.filter (fun x -> x.Sender.Id.Value = ownerId)
         |> Observable.flatmapTask (dm !session >> Async.Catch >> Async.StartAsTask)
         |> Observable.subscribe sub
  in
  use cn = Observable.connect obsr in
  obs |> Observable.wait |> ignore;
  sprintf "notice: connection lost at %A, reconnecting" DateTime.Now |> report |> ignore;
  Thread.Sleep 1000;
  start !session
