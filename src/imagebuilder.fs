module ImageBuilder
open System.Drawing
open System.Drawing.Text
open System.Drawing.Imaging

type ImageBuilder = 
  {
    operations: (Graphics -> Graphics) list;
    size: int * int;
    cursor: int * int;
    margin: int;
    format: StringFormat
  }

let private measure sf font text =
  use i = new Bitmap (1, 1) in
  use g = Graphics.FromImage i in
  let size = g.MeasureString (text, font, PointF.Empty, sf) in
  (int32 size.Width, int32 size.Height)

let private genOp sf (brush, font) (x, y) text (g: Graphics) =
  g.TextRenderingHint <- TextRenderingHint.AntiAlias;
  g.DrawString (text, font, brush, float32 x, float32 y, sf);
  g

let inline private bimap f (x, y) = (f x, f y)

let iprintf font brush format =
  let f s b =
    let (sw, sh) = measure b.format font s in
    let w = max (fst b.size) (fst b.cursor + sw + b.margin * 2) in
    let h = max (snd b.size) (snd b.cursor + b.margin * 2) in
    let cursor = (fst b.cursor + sw, snd b.cursor) in
    { b with operations = genOp b.format (brush, font) (b.cursor |> bimap ((+) b.margin)) s :: b.operations;
             cursor = cursor;
             size = (w, h) }
  Printf.kprintf f format

let iprintfn font brush format =
  let f s b =
    let (sw, sh) = measure b.format font s in
    let w = max (fst b.size) (fst b.cursor + sw + b.margin * 2) in
    let h = max (snd b.size) (snd b.cursor + sh + b.margin * 2) in
    let cursor = (0, snd b.cursor + sh) in
    { b with operations = genOp b.format (brush, font) (b.cursor |> bimap ((+) b.margin)) s :: b.operations;
             cursor = cursor;
             size = (w, h) }
  Printf.kprintf f format

module StringFormat =
  begin
    let standard =
      new StringFormat (StringFormatFlags.MeasureTrailingSpaces)

    let map f (fm: StringFormat) =
      new StringFormat(fm) |> f

    let fromFlags (fs: StringFormatFlags) =
      new StringFormat(fs)

    let addFlag fg (sf: StringFormat) =
      let sf' = new StringFormat(sf) in
      sf'.FormatFlags <- sf.FormatFlags ||| fg;
      sf'
  end

module ImageBuilder =
  begin
    let create margin format =
      { 
        operations = []; 
        cursor = (0, 0); 
        size = (0, 0); 
        margin = margin;
        format = format
      }

    let render backColor builder =
      let b = new Bitmap (fst builder.size, snd builder.size) in
      use g = Graphics.FromImage b in
      g.Clear backColor;
      for op in builder.operations |> List.rev do
        op g |> ignore
      done
      b

  end

(*
let ib = ImageBuilder.create 8 StringFormat.standard in
use font = new Font ("STIX", float32 36) in 

let img = ib |> iprintfn font Brushes.Black "Hello, world!"
             |> iprintfn font Brushes.Red "%i * %i = %i <-- %A!" 4 5 (4*5) true
             |> iprintf font Brushes.Black "The ultimate answer is "
             |> iprintfn font Brushes.Blue "%i." 42
             |> iprintf font Brushes.Black "The quick "
             |> iprintf font Brushes.Brown "brown fox "
             |> iprintf font Brushes.Black "jumps over the "
             |> iprintf font Brushes.Green "lazy "
             |> iprintfn font Brushes.Black "dog."
             |> ImageBuilder.render Color.White 
in

img.Save("a.png") 
*)
