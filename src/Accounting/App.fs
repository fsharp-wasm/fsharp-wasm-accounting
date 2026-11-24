namespace Accounting

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Scene
open FSharp.Graphics.Widgets

/// Hosts the program.  The scene's event handlers call `Dispatch`, which runs `update` at once; the next frame's `Build` draws the new model.  `clock` reads milliseconds: the host's
/// `Gfx.now` in the browser, a stopwatch under .NET.
type App(text: ITextEngine, lines: int, clock: unit -> float) =
    let mutable model = fst (Model.init lines clock)
    let mutable version = 0
    let mutable lastMs = 0.0

    member _.Model = model
    /// Counts the messages `update` has seen: a host that draws only what changed compares it between frames.
    member _.Version = version

    /// What the last message cost, in milliseconds (opening the lines behind a cell is one pass over the journal; turning the table builds the cube again).
    member _.LastUpdateMs = lastMs

    member _.Dispatch(msg: Msg) =
        let t0 = clock ()
        let m', _ = Model.update clock model msg
        lastMs <- clock () - t0
        model <- m'
        version <- version + 1

    /// The scene of the current model for a canvas of `size`.  A canvas of another size is told to the model first: the table is held inside its window.
    member this.Build(time: float32, size: Vec2) : Scene =
        if size.X <> model.Viewport.X || size.Y <> model.Viewport.Y then this.Dispatch(Resize size)
        let ui : Ui<Msg> = { Theme = Theme.dark; Text = text; State = model.Ui; Time = time; Lift = UiMsg; Send = this.Dispatch }
        View.view ui size model
