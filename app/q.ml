type 'a t =
  { buffer : 'a array
  ; mutable rdpos : int
  ; mutable wrpos : int
  ; lock : Mutex.t
  ; non_empty : Miou_unix.Cond.t
  ; non_full : Miou_unix.Cond.t
  }

let make size v =
  let lock = Mutex.create () in
  { buffer = Array.make size v
  ; lock
  ; rdpos = 0
  ; wrpos = 0
  ; non_empty = Miou_unix.Cond.make ~mutex:lock ()
  ; non_full = Miou_unix.Cond.make ~mutex:lock ()
  }

let is_empty t =
  Mutex.lock t.lock;
  let is_empty = t.rdpos = t.wrpos in
  Mutex.unlock t.lock;
  is_empty

let put t v =
  let predicate () = (t.wrpos + 1) mod Array.length t.buffer = t.rdpos in
  let fn () =
    t.buffer.(t.wrpos) <- v;
    t.wrpos <- (t.wrpos + 1) mod Array.length t.buffer;
    Miou_unix.Cond.signal t.non_empty
  in
  Miou_unix.Cond.until ~predicate ~fn t.non_full

let get t =
  let predicate () = t.wrpos = t.rdpos in
  let fn () =
    let v = t.buffer.(t.rdpos) in
    t.rdpos <- (t.rdpos + 1) mod Array.length t.buffer;
    Miou_unix.Cond.signal t.non_full;
    v
  in
  Miou_unix.Cond.until ~predicate ~fn t.non_empty
