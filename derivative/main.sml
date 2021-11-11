structure Main : sig

  val main : unit -> int
  
end = struct

structure R = RegEx
structure D = Derivative
structure I = IO

val u1 = "derive [RegEx] [Symbol] [Order | All]\n\n"
val u2 = "-e  The regular expression to compute the derivative for\n"
val u3 = "-x  Symbol to compute derivative with respect to\n"
val u4 = "-n  Order of derivative\n"
val u5 = "-u  Compute all unique derivatives (-n option ignored)\n"
val u6 = "-l Compute the minimun length of a regex.\n"
val usage = u1 ^ u2 ^ u3 ^ u3 ^ u4 ^ u5

fun panic(msg) = 
  let
    val _ = print msg
  in
    OS.Process.exit(OS.Process.failure)
  end

fun unique(a, r) = 
  let
    val i = ref 0;
  in
    map (fn x => print(Int.toString((i := !i + 1; !i)) ^ ": " ^ R.tos(x) ^ "\n")) 
        (D.all(1, a, r, []))
  end

fun parseArgs([]) = (#" ", "", ~1, false, false)
  | parseArgs("-x"::a::args) = (case parseArgs(args) of (_, b, c, d, e) => (String.sub(a, 0), b, c, d, e))
  | parseArgs("-e"::b::args) = (case parseArgs(args) of (a, _, c, d, e) => (a, b, c, d, e))
  | parseArgs("-n"::c::args) = (case parseArgs(args) of (a, b, _, d, e) => (a, b, valOf (Int.fromString(c)), d, e))
  | parseArgs("-l"::args) = (case parseArgs(args) of (a, b, c, d, _) => (a, b, c, d, true))
  | parseArgs("-u"::args) = (case parseArgs(args) of (a, b, c, _, e) => (a, b, c, true, e))
  | parseArgs(_) = panic(usage)

fun validateArgs(a, r, n, u, l) = 
    if l andalso r <> " " then ()
    else if a = #" " 
            orelse r = "" 
            orelse (n = ~1 andalso u = false)
         then panic(usage)
         else ()

fun main() =
    let
      val args = CommandLine.arguments()
      val (a, r, n, u, l) = parseArgs args
      val _ = validateArgs(a, r, n, u, l)
      val parsed = Parse.parse(r)
      val _ = if l then print(Int.toString(R.minlen(parsed))) else ()
      val r' = if l = false then R.tos (D.nth(n, a, parsed)) else ""
      val _ = if u andalso l = false then () else print(r' ^ "\n")
      val _ = if u andalso l = false then unique(a, parsed) else []
    in
      OS.Process.exit(OS.Process.success)
    end

val _ = main()

end