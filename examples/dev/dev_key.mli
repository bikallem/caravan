(** The key of the sessions during development. In production, give each process
    the same key of the configuration, and never a new key.

    {!session} takes its random bytes from
    [Mirage_crypto_rng.default_generator]: set it first. *)

val session : ?log:(string -> unit) -> unit -> Caravan_session.Key.t
(** [session ()] is the key of the environment variable [SESSION_KEY]: 32 bytes
    or more in base64url, with no padding.

    If [SESSION_KEY] is not set or is empty, it is a new key of
    {!Caravan_session.Key.generate}, and [log] (default [prerr_endline]) gets
    one line. The line tells that the sessions end at a restart, and gives a
    shell command that makes a key for [SESSION_KEY]. The line does not show the
    new key: a log can go to a file or to other persons, and a person who reads
    the key can sign a session cookie for each user.

    @raise Invalid_argument
      if [SESSION_KEY] is set, but is not base64url or has less than 32 bytes.
      Thus a program with a bad key stops at the start, and does not use a new
      key. *)
