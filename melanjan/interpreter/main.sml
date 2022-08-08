signature MAIN = sig
  val main : string * string list -> OS.Process.status
end

structure Main : MAIN = struct
  fun main (name, argv) =
    (print "Hello, World!\n";
     OS.Process.success)
end

val () =
  if MLton.isMLton then
    OS.Process.exit (Main.main
      (CommandLine.name (), CommandLine.arguments ()))
  else ()
