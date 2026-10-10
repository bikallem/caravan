(** Stop signals, such as Ctrl-C.

    With several domains a signal can be delivered to any of them, so it is
    taken by a dedicated system thread instead: {!install} blocks the signals,
    and {!wait} waits for them inside affect. {!Server.run} does both for you.
*)

(** The type for installed stop signals. *)
type t

val install : ?signals:int list -> unit -> t
(** [install ~signals ()] starts taking [signals] (default [Sys.sigint] and
    [Sys.sigterm]) for {!wait}. The first two signals are taken; a third one
    gets its default action, usually ending the process, so that a program stuck
    despite them can still be stopped. Call it before [Affect_unix.Unix.main],
    in the main thread: only domains and threads created afterwards have the
    signals blocked. Installing again returns the same value, taking the signals
    again if they were {!release}d.

    After a fork, call [install] again in the child, and use only the value that
    it gives: the value of the parent is not valid in the child, and {!wait} and
    {!release} raise [Invalid_argument] for it. In the child, [install] starts a
    new thread and a new pipe, also if the parent installed other signals. It
    closes the file descriptors that the child got for the pipe of the parent,
    and the signals of the parent that the child does not install get their
    action again. A child of a process that started a thread has no preemption
    tick (OCaml 5.5): the new thread runs only while the main thread blocks or
    waits, as an affect program does.

    A program that you start while the signals are installed inherits them
    blocked, thus it does not get them
    ({{:https://github.com/bikallem/caravan/issues/33}#33}).

    @raise Invalid_argument
      if signals were already installed with a different [signals]. *)

val wait : t -> int
(** [wait t] waits for the next signal received since the last [wait] and is
    that signal. Like any affect wait, it can be cancelled.

    @raise Invalid_argument if [t] comes from the parent of a fork. *)

val release : t -> unit
(** [release t] stops taking the signals: they get their default action again,
    usually ending the process, and signals not yet waited for are forgotten.
    Call it from the thread that called {!install}, after
    [Affect_unix.Unix.main] has returned.

    @raise Invalid_argument if [t] comes from the parent of a fork. *)
