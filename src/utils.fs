module Lambda.Utils
open System
open System.Linq
open System.Collections.Generic

open Mono.Terminal 

let editor = new LineEditor ("lambda", 300)
let inline scan prompt = editor.Edit(prompt, "")

module TwitterText =
  begin
    open FSharp.Data
    
    let config = JsonProvider<"https://raw.githubusercontent.com/twitter/twitter-text/master/config/v2.json">.GetSample()

    let count (text : string) =
      let measure c =
        let i = int c in
        config.Ranges |> Array.tryFind (fun r -> r.Start <= i && r.End >= i)
                      |> Option.map (fun r -> r.Weight / config.Scale)
                      ?| (config.DefaultWeight / config.Scale)
      in
      [for c in text -> c] |> List.fold (fun i c -> i + measure c) 0
 
    let limit = config.MaxWeightedTweetLength

    let isValid text =
      count text <= limit

  end

open System.Drawing
open System.Drawing.Text
open System.Drawing.Imaging
 
type RenderResult = RenderedImages of Bitmap list | TooBig

module ImageHelper =
  begin
    open ImageBuilder
   
    let inline stixFont size = new Font("STIX", float32 size)
    
    let measure font text =
      use i = new Bitmap (1, 1) in
      use g = Graphics.FromImage i in
      let size = g.MeasureString (text, font) in
      (int32 size.Width, int32 size.Height)

    let estimateFontSize lengthes = 
      let wmax = lengthes |> List.max in
      let h = List.length lengthes in
      let wlen = (1000 * 2) / (wmax + 2) in
      let hlen = (1000 * 4) / (h + 2) in
      let len = 
        if h > wmax * 6 then
          hlen
        else
          wlen
      in
      min 24 <| len |> max 3
    
    let inline getEncoder (x: ImageFormat) =
      ImageCodecInfo.GetImageEncoders() |> Array.find (fun y -> y.FormatID = x.Guid)
    
    let genSaveParams format prms =
      let eprms = new EncoderParameters(List.length prms) in
      prms |> List.iteri (fun i (k, v: int64) -> eprms.Param.[i] <- new EncoderParameter (k, v));
      (getEncoder format, eprms)
  end

open Microsoft.FSharp.Collections

[<Struct>]
type HashedList<'a> when 'a : comparison =
  { set: Set<'a>;  list: 'a list }

let hashedlist xs =
  { set = Set.ofList xs; list = xs }
 
module HashedList =
  begin
    let add item hl =
      { hl with set = hl.set |> Set.add item; list = item :: hl.list }
    
    let contains item hl =
      hl.set |> Set.contains item

    let empty =
      { set = Set.empty; list = [] }
  end
