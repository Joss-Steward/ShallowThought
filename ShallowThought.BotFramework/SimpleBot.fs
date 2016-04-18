namespace ShallowThought.BotFramework

open System
open System.IO
open System.Net.Sockets
open System.Text.RegularExpressions

type Message =
    | PING of string
    | ERROR of string                       // description
    | NOTICE of string
    | COMMAND of string * string * string   // number, who, text
    | PRIVMSG of string * string * string   // user, dest, content
    | JOIN of string * string               // user, channel
    | QUIT of string * string               // user, reason
    | OTHER

module Util = 
    let (|Match|_|) (pat:string) (inp:string) =
        let m = Regex.Match(inp, pat) in
        if m.Success then
            Some (List.tail [for g in m.Groups -> g.Value])
        else None

    let (|Message|_|) (line: string) =
        match line with
        | Match @"PING :(.+)$" [what] -> Some(PING(what))
        | Match @"ERROR :(.+)$" [desc] -> Some(ERROR(desc))
        | Match @":[^ ]+ NOTICE [^ ]+ :(.+)$" [text] -> Some(NOTICE(text))
        | Match @":[^ ]+ (\d\d\d) ([^:]+)(?: :(.+))?$" [number; who; text] -> Some(COMMAND(number, who, text))
        | Match @":([^!]+)[^ ]+ PRIVMSG ([^ ]+) :(.+)$" [user; dest; content] -> Some(PRIVMSG(user, dest, content))
        | Match @":([^!]+)[^ ]+ JOIN :([^ ]+)$" [sender; channel] -> Some(JOIN(sender, channel))
        | Match @":([^!]+)[^ ]+ QUIT :(.+)$" [user; reason] -> Some(QUIT(user, reason))
        | _ -> None

// Formatting

module Format =
    let BOLD = "\x02"
    let UNDERLINE = "\x1f"
    let RESET = "\x0f"
    let colors = [| "white"; "black"; "blue"; "green"; "red"; "brown"; "purple"; "orange"; "yellow"; "light green"; "teal"; "cyan"; "light blue"; "pink"; "grey"; "light grey" |]

    let COLOR fore =
        match (colors |> Array.tryFindIndex (fun c -> c = fore)) with
        | Some(i) -> (sprintf "\x03%x" i)
        | _ -> RESET

    let BGCOLOR fore back =
        let color = COLOR fore
        match (colors |> Array.tryFindIndex (fun c -> c = fore)) with
        | Some(i) -> color + (sprintf ",%x" i)
        | _ -> RESET
        
type Connection(server: string, port: int, nick: string) =

    let client = new TcpClient()

    member this.Start() =
        client.Connect(server, port)
        let reader = new StreamReader(client.GetStream())
        let writer = new StreamWriter(client.GetStream())
        writer.AutoFlush <- true

        let read = fun () ->
            let line = reader.ReadLine()
            printfn "< %s" line
            line

        let write (line: string) =
            writer.WriteLine line
            printfn "> %s" line

        let connected = fun () -> not reader.EndOfStream

        (read, write, connected)

    member x.Close() =
        client.Close()


type SimpleBot(server: string, port: int, nick: string, channels: string list, pass: string,
               msgHandler: string -> (string -> unit) -> unit) =

    let rec connect (read: unit -> string) (write: string -> unit) =
        write (sprintf "PASS %s" pass)
        write (sprintf "USER %s %s %s %s" nick nick nick nick)
        write (sprintf "NICK %s" nick)

        let rec connect_ = fun () ->
            match read() with
            | Util.Message (PING(what)) -> write (sprintf "PONG %s" what); connect_() // If server pings we pong
            | Util.Message (COMMAND("001", _,  _)) -> // If server welcome we join the channels
                channels |> List.iter (fun channel -> write (sprintf "JOIN %s" channel))
            | Util.Message (COMMAND("433", _,  _)) -> // If nickname in use keep trying
                connect read write
            | _ -> connect_()

        do connect_()

    member this.Start = fun () ->
        let conn = new Connection(server, port, nick)
        let read, write, connected = conn.Start()
        connect read write

        while (connected()) do
            let line = read()
            match line with
            | Util.Message (PING(what)) -> write (sprintf "PONG :%s" what)
            | Util.Message (ERROR(desc)) -> conn.Close(); this.Start()
            | _ -> msgHandler line write
