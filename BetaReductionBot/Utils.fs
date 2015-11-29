namespace BetaReductionBot
open System
open System.Linq
open System.Collections.Generic
open System.Drawing
open System.Drawing.Imaging

module Utils =
  begin
    
    module ImageBuilder =
      begin
        let build (lines : string[]) fontsize marginscale =
          let fs = defaultArg fontsize 15 in
          let ms = defaultArg marginscale 2 in
          use f = new Font ("STIX", float32 fs) in
          let margin = ms * fs / 2 in
          let width = (lines.Select(fun s -> s.Length).Max() + 5) * fs * 6 / 10 + margin * 2 in
          let height = (lines.Length + 1) * fs * 3 / 2 + margin * 2 in
          let b = new Bitmap(width, height) in
          use g = Graphics.FromImage b in
          let r = RectangleF(float32 margin, float32 margin, float32 (width - margin) * 2.0f, float32 (height - margin) * 2.0f) in
          g.FillRectangle(Brushes.White, g.ClipBounds);
          g.DrawString(String.Join("\n", lines), f, Brushes.Black, r);
          b

      end

  end
