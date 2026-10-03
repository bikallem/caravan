(** Stop signals, such as Ctrl-C.

    With several domains a signal can be delivered to any of them, so it is
    taken by a dedicated system thread instead: {!install} blocks the
    signals, and {!wait} waits for them inside affect. {!Server.run} does
    both for you. *)

type t
(** The type for installed stop signals. *)

val install : ?signals:int list -> unit -> t
(** [install ~signals ()] starts taking [signals] (default [Sys.sigint] and
    [Sys.sigterm]) for {!wait}. Call it before [Affect_unix.Unix.main], in the
    main thread: only domains and threads created afterwards have the signals
    blocked. Installing again returns the same value.

    @raise Invalid_argument if signals were already installed with a different
    [signals]. *)

val wait : t -> unit
(** [wait t] waits for the next signal received since the last [wait]. Like
    any affect wait, it can be cancelled. *)
