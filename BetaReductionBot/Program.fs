namespace BetaReductionBot
open BetaReductionBot.Ast
open BetaReductionBot.Transform
open BetaReductionBot.Parser
open BetaReductionBot.Exception
open BetaReductionBot.Utils
open CoreTweet
open CoreTweet.Streaming
open FSharp.Control.Reactive
open FSharp.Control.Reactive.Observable
open System
open System.Linq
open System.IO
open System.Reflection
open System.Reactive
open System.Reactive.Linq
open System.Diagnostics
open System.Runtime.Serialization
open System.Text.RegularExpressions
open System.Xml
open System.Threading
open System.Threading.Tasks
open System.Drawing
open System.Drawing.Imaging

  module Main =
    begin

      let mutable count = 0
      let mutable error = 0
      let ownerId : int64 = 3054246138L
      let hashbase = ['a'..'z'].ToArray ()
      let rand = Random ()
      let p = Process.GetCurrentProcess ()
      let x = new DataContractSerializer(typeof<Tokens>)

      let parser = MainParser ()
      let tc = TermConverter None

      let t =
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

      let hash () = Seq.map (fun _ -> hashbase.[rand.Next(hashbase.Length)]) [1..3] |> String.Concat

      let report s =
        t.DirectMessages.NewAsync(ownerId, text = s) |> ignore;
        Console.WriteLine(s)

      let reduce s b =
        let t = parser.parse s in
        let br = BetaReducer () in
        let res = tc.toTermI t |> br.reduce |> tc.toTerm in
        let img = br.toBitmap () in
        ((if (res.ToString().Length > 80 && b) then "(image)" else res.ToString()), if b then Some img else None)

      let decvar n s =
        let t  = parser.parse s in
        tc.addMetaVariable n (tc.toTermI t);
        (new Task(fun () ->
          Thread.Sleep(TimeSpan.FromMinutes(30.0));
          tc.removeMetaVariable n |> ignore
        )).Start();
        (n + " := " + (t.ToString()) + " (will expire in 30 minutes)", None)

      let reduceanddec n s b =
        let t = parser.parse s |> tc.toTermI in
        let br = BetaReducer () in
        let res = t |> br.reduce in
        let img = br.toBitmap () in
        tc.addMetaVariable n res;
        (new Task(fun () ->
          Thread.Sleep(TimeSpan.FromMinutes(30.0));
          tc.removeMetaVariable n |> ignore
        )).Start();
        let res = res |> tc.toTerm in
        (n + " := " + (if (res.ToString().Length > 80 && b) then "(image)" else res.ToString()), if b then Some img else None)

      let showvars () =
        let img = ImageBuilder.build ((tc.MetaDic.Select(fun i -> i.Key + " := " + (i.Value |> tc.toTerm).ToString())) |> Enumerable.ToArray) None None in
        ("(image)", Some img)

      let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
      
      let parse s =
        try
          match s with
            | Regex @".*@b_rdct\s*((?:\s-noimage|\s-ni)?)\s+(.+)\s+:b=\s+(.+)" [b; n; t] ->
              reduceanddec n t (String.IsNullOrEmpty b)
            | Regex @".*@b_rdct\s+([^ ]+)\s*:=\s*(.+)" [n; t] ->
              decvar n t
            | Regex @".*@b_rdct\s+(?:-showvars|-sv).*" [] ->
              showvars ()
            | Regex @".*@b_rdct\s*((?:\s-noimage|\s-ni)?)\s+(.*)" [b; t] ->
              reduce t (String.IsNullOrEmpty b)
            | _ -> ("Wrong syntax", None)
        with
          | :? BetaReducerException as e -> (e.Message, e.ErrorImage)
          | e -> (String.Format("Native error: {0}", e.Message), None)
          
      
      let tryupdate text (i : Bitmap option) id =
        Console.WriteLine ("try: " + text);
        let id = Nullable id in
        match i with
          | Some i ->
            let ic = ImageConverter() in
            let bs = ic.ConvertTo(i, typeof<byte[]>) :?> byte[]
            let me = t.Media.Upload(media = bs) in
            t.Statuses.Update(status=text, in_reply_to_status_id=id, media_ids=[me.MediaId])
          | None -> t.Statuses.Update(status=text, in_reply_to_status_id=id)
        |> ignore
      
      let reply (s : Status) =
        let name = "@" + s.User.ScreenName in
        let res = parse(s.Text.Replace("&amp;","&"))
        let (msg, img) = res in
        let text = name + " " + msg in
        let h = hash () in
        let text = (if text.Length > 130 then text.Substring(0, 130) else text) + String.Format(" [{0}]", h) in
        count <- count + 1;
        try 
          tryupdate text img s.Id
        with
          | e when e.Message.Contains("validation of media") ->
            try 
              t.Statuses.UpdateAsync(status = String.Format("{0} Too big result (try using -ni option) [{1}]", name, h), in_reply_to_status_id = Nullable s.Id) |> ignore
            with 
                | _ -> ()
          | e ->
            error <- error + 1;
            report ("err: failed to send on " + (DateTime.Now.ToString()) + " to " + s.Id.ToString() + "\n" + e.Message)
     
      let restart () =
        Process.Start(Assembly.GetEntryAssembly().Location) |> ignore;
        Environment.Exit(0)
             
      let operate s =
        match s with
          | Regex @".*status.*" [] -> String.Format("info: running since {0} (for {1}), {2} replies sent ({3} errors)", p.StartTime, (DateTime.Now - p.StartTime), count, error) |> report
          | Regex @".*restart.*" [] -> restart ()
          | _ -> ()

      [<EntryPoint>]
      let main argv =
        let debug = argv.Contains("--debug") in
        if not debug then
          report("notice: application started on " + DateTime.Now.ToString());
          let rec loop () =
            try
              let u = t.Streaming.User() in
              for m in u do
                match m with
                  | :? StatusMessage as s ->
                    let s = s.Status in
                    if (s.Text.Contains("@b_rdct") && s.RetweetedStatus = null) then
                      let rep () =
                        let th = new Thread(fun () -> reply s) in
                        th.Start();
                        if th.Join(TimeSpan.FromSeconds(60.0)) |> not then
                          th.Abort();
                          let h = hash () in
                          tryupdate (String.Format("@{0} Unreducible expression (computation timeout) [{1}]", s.User.ScreenName, h)) None s.Id
                      in (new Task(fun () -> rep ())).Start();
                  | :? DirectMessageMessage as m ->
                    if (m.DirectMessage.Sender.Id.Value = ownerId) then operate m.DirectMessage.Text
                  | _ -> ()
            with
              | ex -> report("notice: an exception occured: " + ex.ToString())
            report("notice: receiving loop is going to restart on " + DateTime.Now.ToString());
            loop ()
          in loop ()
        else
          while true do
            Console.Write(">");
            Console.ReadLine() |> parser.parse |> tc.toTermI |> BetaReducer().reduce |> tc.toTerm |> Console.WriteLine
        0

    end