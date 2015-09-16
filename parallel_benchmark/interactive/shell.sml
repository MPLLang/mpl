open LamIFut
open LamISysFutSusp
open Posix.Process
open Posix.IO
open Posix.FileSys;;
open Posix.TTY;;

val home = Posix.SysDB.Passwd.home
               (Posix.SysDB.getpwuid (Posix.ProcEnv.getuid ()))

val termios = TC.getattr Posix.FileSys.stdout;;
val {iflag, oflag, cflag, lflag, cc, ispeed, ospeed} =
    Posix.TTY.fieldsOf termios;;
fun term_default () =
    TC.setattr (Posix.FileSys.stdout, TC.sanow, termios)

fun term_shell () =
    let val new_fields = {iflag = iflag,
                          oflag = oflag,
                          cflag = cflag,
                          lflag = L.flags [],
                          cc = cc (* V.update (cc, [(V.erase, #"\b")]) *),
                          ispeed = ispeed,
                          ospeed = ospeed}
    val new_termios = Posix.TTY.termios new_fields in
        TC.setattr (Posix.FileSys.stdout, TC.sanow, new_termios)
    end

val _ = term_shell ()

val _ = OS.Process.atExit term_default

val _ = Posix.ProcEnv.setpgid {pid = NONE, pgid = NONE}
val mypid = Posix.ProcEnv.getpid ()

datatype status = Running | Stopped | Done
type child = {jid: int,
              cmd: string,
              pid: pid,
              status: status,
              fg: bool}
type state = {children: child list,
              history: string list}

fun exit2 _ = (term_default();
               OS.Process.exit OS.Process.success)

fun echo_chars l t =
    case l of
        [] => t
      | h::tl => ftr (fn () => (SOME h, echo_chars tl t))

fun stream_string s t = echo_chars (String.explode s) t

fun next_jid children : int =
    let fun next_jid_rec n =
            if List.exists (fn {jid = jid, ...} => jid = n) children then
                next_jid_rec (n + 1)
            else n
    in next_jid_rec 1
    end

fun job_from_jid jobs jid : child option =
    List.find (fn {jid = j,...} => jid = j) jobs

fun change_status jobs jid status fg : child list =
    case jobs of
        [] => []
      | (h as {jid = j, cmd = cmd, pid = pid, ...})::t =>
        if j = jid then
            {jid = j, cmd = cmd, pid = pid, status = status, fg = fg}::t
        else h::(change_status t jid status fg)

fun get_fg (children: child list) : child option =
    ((* print ((Int.toString (List.length children))^"\n"); *)
      List.find (fn {fg = fg, ...} => fg) children)

fun get_done (children: child list) : child list * child list =
    (* print ((Int.toString (List.length children))^"\n"); *)
    List.foldl (fn (x as {status = status, ...}, (done, rest)) =>
                   case status of
                       Done => (x::done, rest)
                     | _ => (done, x::rest)) ([], []) children

fun delete_fg children : child list =
    List.filter (fn {fg = fg, ...} => not fg) children

fun tokenize (s: string) : string list =
    let fun scanner (c, (quote, esc, fst, rest)) =
            case (c, quote, esc) of
                (#" ", false, false) => (false, false, "", fst::rest)
              | (#" ", _, _) => (quote, false, fst^" ", rest)
              | (#"\"", false, false) => (true, false, fst, rest)
              | (#"\"", true, false) => (false, false, fst, rest)
              | (#"\"", _, true) => (quote, false, fst^"\"", rest)
              | (#"\\", _, false) => (quote, true, fst, rest)
              | (#"\\", _, true) => (quote, false, fst^"\\", rest)
              | (c, _, _) => (quote, false, fst^(String.str c), rest)
    in
        if String.size s = 0 then [] else
        let
            val res = List.foldl scanner (false, false, "", []) (String.explode s)
        in
            List.rev ((#3 res)::(#4 res))
        end
    end

fun complete (line: string) =
    let val lastword = Substring.taker (fn c => c <> #" ")
                                       (Substring.full line)
        val (lookin, rest) = Substring.splitr (fn c => c <> #"/") lastword
        val prog = if Substring.isPrefix "./" rest then
                       Substring.triml 2 rest else rest
        val str = Substring.string prog
        val lookins = if Substring.isPrefix "~" lookin then
                          home^"/"^(Substring.string (Substring.triml 2 lookin))
                      else Substring.string lookin
        val dir = opendir
                      (if lookins = "" then OS.FileSys.getDir ()
                       else lookins)
        fun isdir f = ST.isDir (stat f) handle OS.SysErr _ => false
        fun matching ds =
            (case readdir ds of
                 SOME s => if String.isPrefix str s then
                               ((String.extract (s, String.size str, NONE))^
                                (if isdir (lookins^s)
                                 then "/" else ""))
                               ::(matching ds)
                           else matching ds
               | NONE => [])
            handle OS.SysErr (err, _) => (print (err^"\n"); [])
        val matches = matching dir
        fun max_prefix (l1, l2) =
            if size l1 = 0 orelse size l2 = 0 then ""
            else if String.sub (l1, 0) = String.sub (l2, 0) then
                (String.str (String.sub (l1, 0))^
                 (max_prefix (String.extract (l1, 1, NONE),
                              String.extract (l2, 1, NONE))))
            else ""
    in
        case matches of
            [] => ""
          | h::[] => h
          | h::t => List.foldl max_prefix h t
    end
    handle OS.SysErr _ => ""


fun process_cmdline (s: string) : (string * string list * bool) option =
    let val strings = tokenize s in
        case strings of
            [] => NONE
          | h::t => SOME (h, List.filter (fn x => x <> "&") strings,
                          List.exists (fn x => x = "&") t)
    end

fun jobs children =
    let fun print_status Stopped = "Stopped"
          | print_status Running = "Running"
          | print_status Done = "Done"
        fun print_job {jid = jid, cmd = cmd, status = status, ...} =
            "["^(Int.toString jid)^"]\t "^(print_status status)^
            "\t"^cmd^"\n"
    in
        List.foldl (fn (c, s) => (print_job c)^s) "" children
    end

fun new_gid () =
    Posix.ProcEnv.wordToGid(pidToWord (Posix.ProcEnv.getpid ()))

fun exec_job (path: string) (args: string list) =
    (*create {args = args,
    env = NONE,
    path = path,
    stderr = Param.self,
    stdin = Param.null,
    stdout = Param.pipe} *)
    let val {infd = infd1, outfd = outfd1} = pipe ()
        val {infd = infd2, outfd = outfd2} = pipe ()in
        case fork () of
            NONE => (* Child *)
            (((*dup2 {old = outfd1, new = stdout};
      dup2 {old = outfd2, new = stdin}; *)
                (* Posix.ProcEnv.setpgid {pid = NONE, pgid = NONE}; *)
                execp (path, args))
             handle OS.SysErr (err, _) =>
                    (print ("fsh: "^path^": "^err^"\n");
                     OS.Process.exit OS.Process.failure))
          | SOME pid => (* Parent *)
            let val so = dup stdout in
                (* dup2 {old = stdout, new = outfd1};
dup2 {old = stdin, new = infd2}; *)
                Posix.ProcEnv.setpgid {pid = SOME pid, pgid = SOME pid};
                (pid, so)
            end
    end


(*
    let val (pid, so) = exec_job path args
(*fun waitc s =
    case MLton.TextIO.inputLine s of
NONE => ()
      | SOME l => (print (l^"\n"); waitc s) in
waitc (Child.textIn (getStdout c));
children *)
val (_, status) = waitpid (W_CHILD pid, []) in
(dup2 {old = so, new = stdout};
case status of
    W_EXITED => children
  | W_EXITSTATUS _ => children
  | W_SIGNALED _ => children
  | W_STOPPED _ => {pid = pid, status = Stopped}::children)
    end
*)

fun set_fg pgid =
    let val fd = Posix.FileSys.stdout in
        if Posix.ProcEnv.isatty fd then
            Posix.TTY.TC.setpgrp (fd, pgid)
        else
            ()
    end

(*
fun apprompt p s =
   hd s (fn h => stream (h, fn _ => next s p (apprompt p)))

val input_prompt : (string, unit) stream result modref =
    bindstream (apprompt "fsh> " input)
*)

fun cur_prompt () =
    let val cwd = Posix.FileSys.getcwd ()
        val rel = OS.Path.mkRelative {path = cwd, relativeTo = home}
        val path = if String.isPrefix ".." rel then cwd else
                   if rel = "." then "~" else ("~/"^rel)
    in
        "fsh:"^path^"$ "
    end

fun reap children =
    case children of
        [] => []
      | (job as {pid = pid, cmd = cmd, jid = jid, fg = fg,... })::t =>
        ((* set_fg (Posix.ProcEnv.getpid ()); *)
          (case waitpid_nh (W_CHILD pid, [W.untraced]) of
               NONE => job::(reap t)
             | SOME (_, W_STOPPED _) =>
               {jid = jid, cmd = cmd, pid = pid, status = Stopped,
                fg = false}::(reap t)
             | SOME (_, _) =>
               if not fg then
                   {jid = jid, cmd = cmd, pid = pid, status = Done,
                    fg = false}::(reap t)
               else reap t)
          handle OS.SysErr _ =>
                 if not fg then
                     {jid = jid, cmd = cmd, pid = pid, status = Done,
                      fg = false}::(reap t)
                 else reap t)

and waitfg job (st as {children, history}) s_input s_kills () =
    let val {pid = pid, jid = jid, ...} = job
        val _ = print "waitfg\n"
        val (si, t) = equery s_kills ()
    in
        (NONE,
        if si = Posix.Signal.chld then
            let val new_children = reap children
                val new_st = {children = new_children,
                              history = history} in
                case get_fg new_children of
                    NONE =>
                    (set_fg (mypid);
                     term_shell ();
                     stream_string
                         (cur_prompt ())
                         (ftr (inploop "" new_st 0 (""::history) [] s_kills
                                       s_input)))
                  | SOME job =>
                    ftr (waitfg job new_st s_input s_kills)
            end
        else
            (kill (K_GROUP pid, si);
             ftr (waitfg job st s_input s_kills))
        )
    end
and mainloop (st as {children, history}) s_input s_kills =
    let val (done, children) = get_done (reap children)
        val st = {children = children, history = history}
        (* val (s_kills1, s_kills2) = split s_kills in
        val (s_input1, s_input2) = split s_input in *)
        fun nextprompt () =
            stream_string
                (cur_prompt ())
                (ftr (inploop "" st 0 (""::history) [] s_kills s_input))
        fun exec_fg_job (path: string) (args: string list) cmd  =
            let val (pid, so) = exec_job path args
                val new_children = {jid = next_jid children, cmd = cmd,
                                    pid = pid, status = Running,
                                    fg = true}::children
                val new_st = {children = new_children,
                              history = history}
            in
                term_default ();
                set_fg pid;
                ftr (waitfg (List.hd new_children) new_st s_input s_kills)
            end
        fun exec_bg_job (path: string) (args: string list) cmd =
            let val (pid, so) = exec_job path args
                val jid = next_jid children
                val new_children = {jid = jid, cmd = cmd,
                                    pid = pid, status = Running,
                                    fg = false}::children
                val new_st = {children = new_children,
                              history = history}
            in
                stream_string
                    ("["^(Int.toString jid)^"] "^
                     (Int.toString (SysWord.toInt (pidToWord pid)))^"\n"^
                     (cur_prompt ()))
                    (ftr (inploop "" new_st 0 (""::history) [] s_kills
                                  s_input))
            end
        fun make_fg (job as {jid = jid, pid = pid, cmd = cmd,...}) =
            let val new_children = change_status children jid Running true
                val new_st = {children = new_children,
                              history = history}
            in
                stream_string
                    (cmd^"\n")
                    (ftr (fn () =>
                             (term_default ();
                              set_fg pid;
                              kill (K_PROC pid, Posix.Signal.cont);
                              (NONE, ftr (waitfg job new_st s_input s_kills)))))
            end
        fun make_bg (job as {jid = jid, pid = pid, cmd = cmd,...}) =
            let val new_children = change_status children jid Running false
                val new_st = {children = new_children,
                              history = history}
            in
                kill (K_PROC pid, Posix.Signal.cont);
                stream_string
                    ("["^(Int.toString jid)^"] "^
                     (Int.toString (SysWord.toInt (pidToWord pid)))^"\n"^
                     (cur_prompt ()))
                    (ftr (inploop "" new_st 0 (""::history) [] s_kills s_input))
        end
    val line = List.hd history
    in
    stream_string (jobs done) (
          case process_cmdline line of
              NONE => nextprompt ()
            | SOME (path, args, bg) =>
              if path = "quit" then exit2 ()
              else if path = "jobs" then
                  stream_string (jobs children) (nextprompt ())
              else if path = "fg" orelse path = "bg" then
                  if List.length args < 2 then
                      stream_string ("fsh: "^path^": Invalid job id")
                                    (nextprompt ())
                  else
                      case Int.fromString (List.nth (args, 1)) of
                          NONE => stream_string "Invalid job id\n"
                                                (nextprompt ())
                        | SOME jid =>
                          (case job_from_jid children jid of
                               NONE => stream_string "Invalid job id\n"
                                                     (nextprompt ())
                             | SOME job => (if path = "fg" then make_fg
                                            else make_bg) job)
              else if path = "cd" then
              let val path = if List.length args < 2 then "" else
                     let val dirs =
                         Substring.full (List.nth (args, 1))
                     in
                                         if Substring.isPrefix "~" dirs then
                         home^(Substring.string (Substring.triml 1 dirs))
                     else
                         Substring.string dirs
                     end
              in
              (OS.FileSys.chDir path;
               nextprompt ())
              handle OS.SysErr (err, _) =>
                 stream_string ("fsh: cd: "^err^"\n") (nextprompt ())
              end
          else
              (if bg then exec_bg_job else exec_fg_job)
              path args line)
    end
and inploop line (st as {children, history}) pos hf hr s_kills s_inchar () =
    let val esc = [#"\^[", #"["]
    val erase = esc@[#"K"]
    val left = esc@[#"D"]
    val right = esc@[#"C"]
    val up = esc@[#"A"]
    val down = esc@[#"B"]
    fun repeat n l = if n = 0 then [] else l@(repeat (n - 1) l)
    val eraseline = (repeat pos left)@erase
    val len = String.size line
    fun proc_char c s_inchar =
        if c = #"\n" then
            (SOME c, mainloop {children = children,
                               history = line::history}
                              s_inchar s_kills)
        else if c = #"\b" orelse c = #"\127"
        then if len > 0 andalso pos > 0 then
                 let val newline =
                         (String.substring (line, 0, pos - 1))^
                         (String.extract (line, pos, NONE)) in
                     query
                     (echo_chars
                         (eraseline@
                          (String.explode newline)@
                          (repeat (len - pos) left))
                         (ftr (inploop newline st (pos - 1)
                                       hf hr s_kills s_inchar))) ()
                 end
             else inploop line st pos hf hr s_kills s_inchar ()
        else if c = #"\t" then
            let val rest = complete line in
                query
                (stream_string (complete line)
                              (ftr (inploop (line^rest) st
                                            (pos + (String.size rest))
                                            hf hr s_kills s_inchar))) ()
            end
        else if c = #"\^[" then
            let val (c2, s) = equery s_inchar ()
            in
                if c2 = #"[" then
                    let val (c3, s) = equery s ()
                        val (newline, newpos, echo, newhf, newhr) =
                         case c3 of
                            #"D" => if pos > 0 then
                                        (line, pos - 1, left, hf, hr)
                                    else
                                        (line, pos, [], hf, hr)
                          | #"C" => if pos < len then
                                        (line, pos + 1, right, hf, hr)
                                    else
                                        (line, pos, [], hf, hr)
                          | #"A" => (case hf of
                                         _::[] => (line, pos, [], hf, hr)
                                       | c::h::t => (h, String.size h,
                                                     eraseline@(String.explode h),
                                                     h::t, c::hr))
                          | #"B" => (case hr of
                                         [] => (line, pos, [], hf, hr)
                                       | h::t => (h, String.size h,
                                                  eraseline@(String.explode h),
                                                  h::hf, t))
                          | _ => (line, pos, [], hf, hr)
                        in
                            query
                            (echo_chars
                                echo
                                (ftr (inploop newline st newpos newhf newhr
                                              s_kills s_inchar ))) ()
                    end
                else
                    (SOME c, ftr (inploop (line^(String.str c)) st
                                          (pos + 1) hf hr s_kills s_inchar))
            end
        else
            (SOME c, ftr (inploop (line^(String.str c)) st
                                  (pos + 1) hf hr s_kills s_inchar))
    val (c, s_inchar') = equery s_inchar ()
    in
        proc_char c s_inchar'
    end;;

val result = (stream_string
                  (cur_prompt ())
                  (ftr (inploop "" {children = [],  history = []} 0 [""]
                                [] signals inchar)))

val _ = capture_signals [Posix.Signal.int, Posix.Signal.tstp, Posix.Signal.ttin,
                         Posix.Signal.chld];;
val _ = MLton.Signal.setHandler (Posix.Signal.ttou, MLton.Signal.Handler.ignore)

fun runc f =
    let val (c, r) = query f ()
    in
        (case c of
             SOME c =>
             (TextIO.StreamIO.output1 (TextIO.getOutstream TextIO.stdOut, c);
              TextIO.StreamIO.flushOut (TextIO.getOutstream TextIO.stdOut))
           | NONE => ());
        runc r
    end

val _ = runc result
