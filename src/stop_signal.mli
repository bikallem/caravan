(** Stop signals, such as Ctrl-C.

    With several domains a signal can be delivered to any of them, so it is
    taken by a dedicated system thread instead: {!install} blocks the
    signals, and {!wait} waits for them inside affect. {!Server.run} does
    both for you. *)

type t
(** The type for installed stop signals. *)

val install : ?signals:int list -> unit -> t
(** [install ~signals ()] starts taking [signals] (default [Sys.sigint] and
    [Sys.sigterm]) for {!wait}. The first two signals are taken; a third one
    gets its default action, usually ending the process, so that a program
    stuck despite them can still be stopped. Call it before [Affect_unix.Unix.main], in the
    main thread: only domains and threads created afterwards have the signals
    blocked. Installing again returns the same value, taking the signals
    again if they were {!release}d.

    @raise Invalid_argument if signals were already installed with a different
    [signals]. *)

val wait : t -> int
(** [wait t] waits for the next signal received since the last [wait] and is
    that signal. Like any affect wait, it can be cancelled. *)

val release : t -> unit
(** [release t] stops taking the signals: they get their default action
    again, usually ending the process, and signals not yet waited for are
    forgotten. Call it from the thread that called {!install}, after
    [Affect_unix.Unix.main] has returned. *)
