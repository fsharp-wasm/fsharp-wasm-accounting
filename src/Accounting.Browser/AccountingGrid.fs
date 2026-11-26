/// The module that becomes AccountingGrid.wasm: the exports are what runtime/gfx.js (`mount`) calls, plus two that read what the page measured.
///   start()                         bind the canvas
///   onResize(w, h, ratio)           CSS size of the canvas and the device pixel ratio
///   onPointer(kind, x, y, button)   kind 0 down, 1 up, 2 move, 3 leave
///   onWheel(dx, dy)
///   onKey(kind)                     kind 0 down, 1 up; the key name is fetched with Gfx.pendingKey
///   frame(nowMs)                    build, resolve, draw (when the model changed, an event was taken: see CanvasHost.Frame)
///   lines()                         how many journal lines there are
///   generatedMs(), pivotMs()        what making the lines and the last turn of the table took
///   updateMs()                      what the last message took
module Accounting.Browser.AccountingGrid

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Scene
open FSharp.Graphics.Canvas
open Accounting

/// A million journal lines: the size at which a pivot table is worth having an engine for.
[<Literal>]
let Lines = 1000000

/// All mutable state of the page, in one object (a module-level `let mutable` is a wasm global; this keeps it together).
type Page() =
    let text = CanvasTextEngine() :> ITextEngine
    let host = CanvasHost text
    let app = App(text, Lines, Gfx.now)
    let mutable seen = -1

    member _.App = app
    member _.Resize(w: float32, h: float32, r: float32) = host.Resize(w, h, r)
    member _.Input(evt: InputEvent) = host.Input evt
    member _.Frame(nowMs: float) =
        // the model changes in `app.Dispatch`, which a handler calls; this keeps the picture true whatever calls Dispatch
        if app.Version <> seen then
            seen <- app.Version
            host.Invalidate()
        host.Frame(nowMs, false, 0.0f, fun time size -> app.Build(time, size))

let private page = Page()

[<CompiledName("start")>]
let start () : unit = Gfx.init "gfx"

[<CompiledName("onResize")>]
let onResize (w: float) (h: float) (ratio: float) : unit = page.Resize(float32 w, float32 h, float32 ratio)

[<CompiledName("onPointer")>]
let onPointer (kind: int) (x: float) (y: float) (button: int) : unit =
    let p = Vec2.create (float32 x) (float32 y)
    let evt =
        match kind with
        | 0 -> PointerDown(p, button)
        | 1 -> PointerUp(p, button)
        | 2 -> PointerMove p
        | _ -> PointerLeave p
    page.Input evt |> ignore

[<CompiledName("onWheel")>]
let onWheel (dx: float) (dy: float) : unit = page.Input(Scroll(Vec2.create (float32 dx) (float32 dy))) |> ignore

[<CompiledName("onKey")>]
let onKey (kind: int) : unit =
    let key = Gfx.pendingKey ()
    page.Input(if kind = 0 then KeyDown key else KeyUp key) |> ignore

[<CompiledName("lines")>]
let lines () : int = page.App.Model.Facts.Count

[<CompiledName("generatedMs")>]
let generatedMs () : float = page.App.Model.GenMs

[<CompiledName("pivotMs")>]
let pivotMs () : float = page.App.Model.CubeMs

/// What is on the page, as numbers (a canvas has no DOM to read): the lines and columns that are shown, the tabs, the pivot and the measure, and the selected cell (-1 when none).
[<CompiledName("updateMs")>]
let updateMs () : float = page.App.LastUpdateMs

[<CompiledName("rowsShown")>]
let rowsShown () : int = page.App.Model.Rows.Length

[<CompiledName("colsShown")>]
let colsShown () : int = page.App.Model.Cols.Length

[<CompiledName("tabCount")>]
let tabCount () : int = 2 + page.App.Model.Details.Length

[<CompiledName("pivot")>]
let pivot () : int = page.App.Model.Pivot

[<CompiledName("measure")>]
let measure () : int = page.App.Model.Measure

/// The tab on show: 0 is the pivot table, 1 the history of the plan, n + 1 the n-th tab of lines.
[<CompiledName("activeTab")>]
let activeTab () : int =
    let m = page.App.Model
    if m.Tab = "pivot" then 0 elif m.Tab = "history" then 1 else 2 + (m.Details |> List.findIndex (fun d -> d.Id = m.Tab))

/// How far the table on show is scrolled, in pixels.
[<CompiledName("offsetY")>]
let offsetY () : float =
    let m = page.App.Model
    match m.Details |> List.tryFind (fun d -> d.Id = m.Tab) with
    | Some d -> float d.Grid.OffsetY
    | None -> float m.Main.OffsetY

[<CompiledName("offsetX")>]
let offsetX () : float =
    let m = page.App.Model
    match m.Details |> List.tryFind (fun d -> d.Id = m.Tab) with
    | Some d -> float d.Grid.OffsetX
    | None -> float m.Main.OffsetX

[<CompiledName("selectedRow")>]
let selectedRow () : int =
    match page.App.Model.Main.Selected with
    | Some(r, _) -> r
    | None -> -1

[<CompiledName("selectedCol")>]
let selectedCol () : int =
    match page.App.Model.Main.Selected with
    | Some(_, c) -> c
    | None -> -1

[<CompiledName("frame")>]
let frame (nowMs: float) : unit = page.Frame nowMs

/// The plan: how many cells hold something, how many steps are on the tape, whether a cell is being written and the text typed so far, and the grand total of the Budget measure.
[<CompiledName("planCells")>]
let planCells () : int = Planning.count page.App.Model.G

[<CompiledName("planSteps")>]
let planSteps () : int = page.App.Model.Steps

[<CompiledName("planEditing")>]
let planEditing () : bool = page.App.Model.Editing.IsSome

/// How many characters have been typed into the cell that is being written (a string does not cross the export as a value the test can compare).
[<CompiledName("planEditLength")>]
let planEditLength () : int = match page.App.Model.Editing with Some e -> e.Text.Length | None -> -1

[<CompiledName("budgetTotal")>]
let budgetTotal () : float = FSharp.Wasm.Frame.Cube.value page.App.Model.Cube 1 0 0

/// What the last step did: the cells that came out with another value, the calcs it reached, and how many of those ran.
[<CompiledName("lastChanged")>]
let lastChanged () : int = match page.App.Model.LastRecalc with Some r -> r.Changed | None -> -1

[<CompiledName("lastReached")>]
let lastReached () : int = match page.App.Model.LastRecalc with Some r -> r.Reached | None -> -1

[<CompiledName("lastRan")>]
let lastRan () : int = match page.App.Model.LastRecalc with Some r -> r.Ran | None -> -1
