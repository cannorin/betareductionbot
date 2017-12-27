namespace BetaReductionBot.Utils
open System
open System.Linq
open System.Collections.Generic
open System.Drawing
open System.Drawing.Text
open System.Drawing.Imaging
open FSharp.Data

module TwitterText =
  begin
    let config = JsonProvider<"https://raw.githubusercontent.com/twitter/twitter-text/master/config/v2.json">.GetSample()

    let count (text : string) =
      let measure c =
        let i = int c in
        config.Ranges |> Array.tryFind (fun r -> r.Start <= i && r.End >= i)
                      |> Option.map (fun r -> r.Weight / config.Scale)
                      |> defaultArg <| (config.DefaultWeight / config.Scale)
      in
      [for c in text -> c] |> List.fold (fun i c -> i + measure c) 0
 
    let limit = config.MaxWeightedTweetLength

    let isValid text =
      count text <= limit

  end

type RenderResult = Images of Bitmap list | TooBig

module ImageBuilder =
  begin
    
    let measure font text =
      use i = new Bitmap (1, 1) in
      use g = Graphics.FromImage i in
      let size = g.MeasureString (text, font) in
      (int32 size.Width, int32 size.Height)

    let build lines fontsize marginscale =
      let defaultfs = 
        let wmax = lines |> Array.map String.length |> Array.max in
        let h = Array.length lines in
        let wlen = (2000 * 2) / (wmax + 2) in
        let hlen = (2000 * 4) / (h + 2) in
        let len = 
          if h > wmax * 6 then
            hlen
          else
            wlen
        in
        min 16 <| len |> max 6
      in

      let fs = defaultArg fontsize defaultfs in
      let ms = defaultArg marginscale 2 in
      let margin = ms * fs / 2 in
      
      use font = new Font ("STIX", float32 fs) in 
      
      let render text =
        let (width, height) =
          let (w, h) = measure font text in
          (w + margin * 2, h + margin * 2)
        in
        let b = new Bitmap (width, height) in
        use g = Graphics.FromImage b in
        g.Clear Color.White;
        g.TextRenderingHint <- TextRenderingHint.AntiAlias;
        g.DrawString (text, font, Brushes.Black, float32 margin, float32 margin);
        b
      in
      
      let join ss = String.concat Environment.NewLine ss in
      let o = join lines in
      let (ow, oh) = o |> measure font in
      printfn "(%i, %i)" ow oh
      if ow > 8000 then
        TooBig
      else if oh > 8000 then
        let n = oh / 8000 + 1 in
        if n > 4 then
          TooBig
        else
          let chunk = (lines.Length + (n - lines.Length % n)) / n in
          let rec takeToEnd n arr =
            if Array.length arr > n then
              (arr |> Array.take n) :: takeToEnd n (arr |> Array.skip n)
            else
              [arr]
          in
          lines |> takeToEnd chunk |> List.map (join >> render) |> Images
      else
        Images [render o]
  end
