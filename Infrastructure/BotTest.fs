module BotTest
open DSharpPlus
//open System.Threading.Tasks
let True x y = x

let tok = "Njk1NjIwMzc5ODM3ODU3ODQy.XomlZw.QqjsUvfuy_VfluuOM_5L2-f857M"


let awaitTask (t:System.Threading.Tasks.Task) = 
    t.ContinueWith (fun x -> ()) |> Async.AwaitTask

let config (): DiscordConfiguration = 
    let cfg = new DiscordConfiguration ()
    cfg.set_TokenType TokenType.Bot
    cfg.set_Token tok
    cfg.set_AutomaticGuildSync true
    cfg

let client (): DiscordClient = new DiscordClient (config ())

let make_connection () = 
    printfn "%A okay i'll try " "yes"
    let connect = async {
        let cl = (client ())
        let make_connection = cl.ConnectAsync ()
        let! login_result = awaitTask make_connection

        printfn "look at this: %A" <| cl.GatewayUrl.ToString ()
        printfn "so: %A" <| cl.Ping

        return cl
    }
    let do_stuff = async {
        let! connected = connect
        let! target_channel = 
            connected.GetChannelAsync (uint64 623431943408582676I)
            |> Async.AwaitTask
        let! sended_message =
            connected.SendMessageAsync (target_channel, "Hello, buddy. How are you?")
            |> Async.AwaitTask
        let timestampt = 
            (sended_message.CreationTimestamp.Date.ToUniversalTime ()).ToLongTimeString ()
        let! logout = 
            connected.DisconnectAsync ()
            |> Async.AwaitTask
        printfn "okay, i was sent message, and %A" timestampt
        return connected
    }


    printfn "It's working? :|"
    client () |> (fun x -> printf "%A" <| x.GetType ())
    Async.RunSynchronously do_stuff